# -------------------------------------------------------------------------
# Associated Functions
# -------------------------------------------------------------------------
AF.MA.Compressed <- function(d1, periods = seq(10, 180, 10))
{
  library(TTR)
  library(foreach)
  library(dplyr)
  
  d1[is.na(d1)] <- 0
  
  suppressWarnings(
    z <- foreach(n = periods, .combine = bind_rows, .errorhandling = 'remove') %:%
      foreach(name = c("open", "low", "high", "close")
              , .combine = bind_rows, .errorhandling = 'remove') %do%
              {
                z <- d1 %>%
                  mutate(Period = n,
                         MA.Var = name,
                         SMA := lag(SMA(get(name), n))/lag(close),
                         EMA := lag(EMA(get(name), n))/lag(close),
                         EMAW := lag(EMA(get(name), n, wilder = TRUE))/lag(close),
                         ZLEMA := lag(ZLEMA(get(name), n))/lag(close),
                         HMA := lag(HMA(get(name), n))/lag(close)
                  ) %>%
                  select(ds, ticker, Period, MA.Var, SMA, EMA, EMAW, ZLEMA, HMA) %>%
                  na.omit()
                
                rm(n, name)
                return(z)
              }
  )
  
  return(z)
}

AF.LN <- function(d2, d1x, MA.Type, n)
{
  d2x <- d2 %>% filter(Period == !!n) %>%
    select(ds, ticker, MA.Var, (!!MA.Type)) %>%
    rename(MA = MA.Type)
  
  if(min(d2x$MA) <= 0 ){ return(data.frame())}
  
  d2x <- d2x %>%
    mutate(LN = log(MA)) %>%
    select(-MA) %>% data.frame() %>%
    reshape(idvar = c("ds", "ticker"), timevar = "MA.Var", direction = "wide") %>%
    left_join(d1x, by = c("ticker", "ds"))
  
  return(d2x)
  
}

AF.MALN <- function(d2, d1x, MA.Type, n)
{
  d2x <- d2 %>%
    filter(Period == !!n) %>%
    select(ds, ticker, MA.Var, (!!MA.Type)) %>%
    rename(MA = MA.Type)
  
  if(min(d2x$MA) <= 0 ){ return(data.frame())}
  
  d2x <- d2x %>%
    mutate(LN = log(MA)) %>%
    data.frame() %>%
    reshape(idvar = c("ds", "ticker"), timevar = "MA.Var", direction = "wide") %>%
    left_join(d1x, by = c("ticker", "ds"))
  
  return(d2x)
  
}

AF.Tree <- function(d2, variable, last.dev.date)
{
  # variable = "Bare.l"
  d3 <- d2 %>% na.omit() %>%
    filter(ds <= last.dev.date) %>%
    mutate(Bare = get(variable)) %>%
    select(-c("ticker", "ds", "volume", "open", "high", "low", "close", 
              "ROI.l", "ROI.h", "Bare.l", "Bare.h"))
  
  fit <- tryCatch(rpart::rpart(Bare ~ ., data = d3, method = "class", 
                        control = rpart.control(minbucket = 0.1*nrow(d3)))
                  , error = function(w) {return(NULL)}
                  , warning = function(w) {return(NULL)})
  
  # rattle::fancyRpartPlot(fit)
  vars <- setdiff(levels(fit$frame$var), "<leaf>")
  if(!is.null(fit) && !is.null(vars) && length(vars) > 0)
  {
    all <- predict(fit, newdata = d2, type = "prob") %>% 
      data.frame()
  } else {all <- data.frame()}
  
  rm(variable, d3, fit, vars)
  return(all)
}

# -------------------------------------------------------------------------
# Production Score Function: Model ID 20191007
# -------------------------------------------------------------------------
Get.Scores.20191007 <- function(ticker, d1, T.models)
{
  # -------------------------------------------------------------------------
  # Inputs: For Testing
  # -------------------------------------------------------------------------
  # library(foreach)
  # library(dplyr)
  # # all.d1 <- readRDS(paste0(data.folder, "IB/Clean.Prices.rds"))
  # ticker <- "MDB"
  # d1 <- all.d1 %>% filter(ticker == !!ticker)

  # -------------------------------------------------------------------------
  # Raw Data Extraction
  # -------------------------------------------------------------------------
  in.days = 5L
  last.dev.date <- as.Date("2019-01-31")
  T.models <- T.models %>% filter(ticker == !!ticker, algo.ID == "20191007")
  d1x <- d1 %>% mutate(last.close = lag(close)
                       , ROI.h = zoo::rollmax(high, in.days, fill = NA, align = "left")/last.close
                       , ROI.l = -zoo::rollmax(-low, in.days, fill = NA, align = "left")/last.close
                       , ROI.c = lead(adjusted, in.days-1)/lag(adjusted)
                       , Bare = as.factor(ifelse(ROI.c > 1, 1, 0))
                       ) %>%
          select(-c(adjusted, last.close, ROI.c))

  d2 <- AF.MA.Compressed(d1, periods = seq(10, 180, 10))

  # -------------------------------------------------------------------------
  # Trend Models
  # -------------------------------------------------------------------------
  # profvis::profvis({

  T.scores <- data.frame()

  for(i in 1:nrow(T.models))
  {
    # i = 1
    DP.Method <- trimws(substr(T.models$model[i], 1, 4))
    MA.Type <- trimws(substr(T.models$model[i], 6, 10))
    n <- as.numeric(substr(T.models$model[i], 12, 14))
    
    d2x <- get(paste0("AF.", DP.Method))(d2, d1x, MA.Type, n)

    if(nrow(d2x) == 0)
    {
      rm(i, MA.Type, n, DP.Method, d2x)
      next()
    }

    # -------------------------------------------------------------
    tdata <- d2x %>% filter(ds <= last.dev.date) %>%
      select(-c("ds", "ds.N", "ticker", "ROI.h", "ROI.l", "volume", "open", "high", "low", "close"))

    m2 <- tryCatch(glm(Bare ~ ., family = "binomial", data = tdata) %>%
                     MASS::stepAIC(direction = "both", trace = FALSE)
                   , error = function(w) {return(NULL)}
                   , warning = function(w) {return(NULL)})

    # -------------------------------------------------------------
    # Checking Validity of Models
    # -------------------------------------------------------------
    if(is.null(m2) || grepl("LN.", paste(names(m2$coefficients), collapse = ", ")) == FALSE)
    {
      rm(i, MA.Type, n, DP.Method, d2x, tdata, m2)
      next()
    }

    all <- d2x %>% arrange(ds.N) %>%
            mutate(algo.ID = T.models$algo.ID[i],
                   model.ID = T.models$model.ID[i],
                   Score = predict(m2, newdata = ., type = "response"),
                   Score = round(Score, 4)) %>%
            ungroup() %>% select(algo.ID, model.ID, ticker, ds, Score)

    all <- all %>% 
          filter(ds <= last.dev.date) %>%
          mutate(R = ntile(Score, 10)) %>%
          group_by(ticker, R) %>%
          summarise(R.low = min(Score)) %>%
          mutate(R.low = ifelse(R == min(R), 0, R.low),
                 R.high = ifelse(R == max(R), 1, lead(R.low) - 0.0001)) %>%
          full_join(all, by = "ticker") %>%
          filter(Score >= R.low, Score <= R.high) %>%
          select(algo.ID, model.ID, ticker, ds, Score, R) %>%
          arrange(ds)

    T.scores <- bind_rows(T.scores, all)

    rm(i, DP.Method, MA.Type, n, d2x, tdata, m2, all)
  }

  # })  # For Profvis
  # -------------------------------------------------------------------------
  # Saving and Cleaning
  # -------------------------------------------------------------------------
  
  rm(d1x, d2, in.days, last.dev.date)
  return(T.scores)

}

# -------------------------------------------------------------------------
# Production Score Function: Model ID 20191024
# -------------------------------------------------------------------------
Get.Scores.20191024 <- function(ticker, d1, T.models)
{
  # -------------------------------------------------------------------------
  # Inputs: For Testing
  # -------------------------------------------------------------------------
  # library(foreach)
  # library(dplyr)
  # library(rpart)
  # ticker = "MDB"
  # -------------------------------------------------------------------------
  # Data Preparation
  # -------------------------------------------------------------------------
  last.dev.date <- as.Date("2019-01-31")
  T.models <- T.models %>% filter(ticker == !!ticker, algo.ID == "20191024")
  
  if( as.numeric(last.dev.date - min(d1$ds)) < 180) {return(NULL)}
  
  for(days in c(seq(5, 80, 5)))
  {
    d1 <- d1 %>% mutate(!!paste0("P.", days) := lag(close, 1)/lag(close, days+1))
    rm(days)
  }
  
  # -------------------------------------------------------------------------
  # Trend Models
  # -------------------------------------------------------------------------
  T.scores <- data.frame()
  for(i in 1:nrow(T.models))
  {
    # i = 1
    edge <- as.numeric(substr(T.models$model.ID[i], 3, 6))
    horizon <- as.numeric(substr(T.models$model.ID[i], 10, 11))
    
    d2 <- d1 %>%
      # select(-starts_with('P.')) %>%
      mutate(ROI.c = round(lead(close, horizon - 1)/lag(close), 4),
             ROI.l = -zoo::rollmax(-low, horizon, fill = NA, align = "left")/lag(close),
             ROI.h =  zoo::rollmax(high, horizon, fill = NA, align = "left")/lag(close),
             Bare.l = ifelse(ROI.l <= 1 - edge, "S", "NS"),
             Bare.h = ifelse(ROI.h >= 1 + edge, "L", "NL") )
    
    l <- AF.Tree(d2, variable = "Bare.l", last.dev.date)
    h <- AF.Tree(d2, variable = "Bare.h", last.dev.date)
    
    if(nrow(l) > 0 & nrow(h) > 0)
    {
      all.x <- bind_cols(l, h) %>%
                mutate(E = L - S,
                       R = data.table::frank(-E, ties.method = "dense")) %>%
                bind_cols(d2) %>%
                mutate(algo.ID = T.models$algo.ID[i],
                       model.ID = T.models$model.ID[i],
                       Score = E) %>%
                select(algo.ID, model.ID, ticker, ds, Score, R)
      
      T.scores <- bind_rows(T.scores, all.x)
      rm(all.x)
    }
    
    rm(i, edge, horizon, d2, l, h)
  }
  
  # -------------------------------------------------------------------------
  # Summarization, Saving and Cleaning
  # -------------------------------------------------------------------------
  rm(last.dev.date)
  return(T.scores)
}
