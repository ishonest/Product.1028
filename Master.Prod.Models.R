# -------------------------------------------------------------------------
# Initialization
# -------------------------------------------------------------------------
source("./Master.Settings.R")

prod.models <- data.frame()
for (algoId in Parms$algoIds)
{
  prod.models <- bind_rows(prod.models, 
                           readRDS(paste0("./Data/Summary/", algoId, ".Production.Models.rds")) %>%
                            rename(algo.ID = algoId)
                           )
  rm(algoId)
}

# Selecting Models in buyalgos or having a position
if(file.exists("./Data/Trading/00.Latest.rds"))
{
  in.hand <- readRDS("./Data/Trading/00.Latest.rds") %>% ungroup() %>%
              filter(account == Parms[["acctCode"]]) %>% select(-account)

  prod.models <- prod.models %>% 
                  left_join(in.hand, by = c("ticker", "algo.ID", "model.ID")) %>%
                  filter(algo.ID %in% Parms$buyalgos | !is.na(units)) %>%
                  select(-units)
  
  rm(in.hand)
}

# -------------------------------------------------------------------------
# Incremental Data Pull: Works in the middle of the trading day
# -------------------------------------------------------------------------
source("./Functions/M00.Data.Pull.R")
hist.d1 <- readRDS("./Data/Summary/Clean.Prices.rds") %>% 
            filter(ticker %in% unique(prod.models$ticker))

all.d1 <- Get.Incremental.Data(stocks = unique(hist.d1$ticker), first.date = max(hist.d1$ds))
rm(hist.d1, Get.Data.Clean, Get.Incremental.Data)
saveRDS(all.d1, "./Data/Summary/Latest.Prices.rds")

# -------------------------------------------------------------------------
# Rescoring with New Data
# -------------------------------------------------------------------------
source("./Functions/M01.Scoring.R")
do.call(unlink, list(list.files(c("./Data/Process.Tracker/", "./Data/Scores/"), full.names = TRUE)))

stocks <- setdiff(unique(all.d1$ticker),
                  gsub(".rds", "", list.files("./Data/Process.Tracker/")))

foreach(ticker = stocks
        , .export = c(lsf.str())
        , .packages = c("dplyr", "foreach", "rpart")
        , .multicombine = TRUE, .inorder = FALSE, .errorhandling = 'remove' 
        ) %dopar%
        {
          # ticker <- "AYX"
          d1 <- all.d1 %>% filter(ticker == !!ticker)
          T.models <- prod.models %>% filter(ticker == !!ticker)
          T.scores <- foreach(a = unique(T.models$algo.ID)
                              , .combine = bind_rows, .errorhandling = 'remove') %do%
                      {
                        # a = unique(T.models$algo.ID)[1]
                        T.scores <- get(paste0("Get.Scores.", a))(ticker, d1, T.models)
                        rm(a)
                        return(T.scores)
                      }
          
          saveRDS(T.scores, paste0("./Data/Scores/", ticker, ".rds"))
          saveRDS(ticker, paste0("./Data/Process.Tracker/", ticker, ".rds"))
          
          rm(ticker, d1, T.models, T.scores)
          gc()
        }

rm(list = lsf.str())
rm(stocks)

# -------------------------------------------------------------------------
# Get Targets: Stocks in active zone + price points 
# Filters investment in more than 3 models
# Assigns the #units bought/sold
# -------------------------------------------------------------------------

source("./Functions/M02.Simulation.R")

do.call(unlink,
        list(list.files(c("./Data/Process.Tracker/", "./Data/Simulation/"), full.names = TRUE)))

stocks <- setdiff(gsub(".rds", "", list.files("./Data/Scores/")), 
                  gsub(".rds", "", list.files("./Data/Process.Tracker/")))

targets <- foreach(ticker = stocks
                   , .combine = bind_rows
                   , .export = c(lsf.str()), .packages = c("dplyr", "foreach")
                   , .multicombine = TRUE, .inorder = FALSE, .errorhandling = 'remove'
                   ) %dopar%
            {
              # ticker = "BMA"
              scores <- readRDS(paste0("./Data/Scores/", ticker, ".rds")) 
              models <- prod.models %>% filter(ticker == !!ticker)
              d1 <- all.d1 %>% filter(ticker == !!ticker) %>% 
                    select(ds, ticker, volume, open, low, high, close)
              
              scores <- scores %>%
                        left_join(models, by = c("algo.ID", "ticker", "model.ID", "R")) %>%
                        left_join(d1, by = c("ticker", "ds"))
              
              sim <- foreach(a = unique(scores$algo.ID)
                              , .combine = bind_rows, .errorhandling = 'remove') %do%
                      {
                        # a = unique(scores$algo.ID)[2]
                        sim <- get(paste0("Simulate.", a))(xscore = scores %>% filter(algo.ID == a))
                        return(sim)
                      }
              
              targets <- AF.Action.Today(sim)
              
              saveRDS(sim, paste0("./Data/Simulation/", ticker, ".rds"))
              saveRDS(ticker, paste0("./Data/Process.Tracker/", ticker, ".rds"))
              rm(scores, models, d1, sim, a, ticker)
              
              return(targets)
            }

table(targets$action)
saveRDS(targets, paste0("./Data/Trading/01.Targets.rds"))
View(targets)
# -------------------------------------------------------------------------
do.call(unlink, list(list.files(c("./Data/Process.Tracker/"), full.names = TRUE)))

stopCluster(cl)
rm(list = ls())
gc()
