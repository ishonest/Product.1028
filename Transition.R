# ------------------------------------------------------------------
x <- readRDS("./Data/Trading/00.Latest.rds") %>%
      ungroup() %>%
      mutate(DP.Method = stringr::str_pad(DP.Method, width = 4, side = "right"), 
             MA.Type = stringr::str_pad(MA.Type, width = 5, side = "right"), 
             Period = stringr::str_pad(Period, width = 3, side = "left"),
             model.ID = paste(DP.Method, MA.Type, Period, sep = "|")
             ) %>%
      select(account, ticker, algoId, model.ID, units)


saveRDS(x, "./Data/Trading/00.Latest.rds")
rm(x)

# ------------------------------------------------------------------
x <- readRDS("./Data/Summary/Overview.rds") %>% rename(ticker.ID = tickerID)
saveRDS(x, "./Data/Summary/Overview.rds")

# ------------------------------------------------------------------
file = "./Data/Trading/00.Latest.rds"
x <- readRDS(file) %>%
      rename(algo.ID = algoId)

saveRDS(x, file)

# ------------------------------------------------------------------
file = "./Data/Trading/02.Historical.Activity.rds"
x <- readRDS(file) %>%
      rename(algo.ID = algoId) %>%
      mutate(DP.Method = stringr::str_pad(DP.Method, width = 4, side = "right"), 
             MA.Type = stringr::str_pad(MA.Type, width = 5, side = "right"), 
             Period = stringr::str_pad(Period, width = 3, side = "left"),
             model.ID = paste(DP.Method, MA.Type, Period, sep = "|")
             ) %>%
      select(ticker, algo.ID, model.ID, Type, Situation, IB.action, 
             order.ts, units, price, account)

saveRDS(x, file)

# ------------------------------------------------------------------
file = "./Data/Trading/03.Historical.Orders.rds"
x <- readRDS(file) %>% rename(algo.ID = algoId)

saveRDS(x, file)
