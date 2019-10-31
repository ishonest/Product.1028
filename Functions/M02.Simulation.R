source("./Functions/F.Trading.Simulation.R")
overview <- readRDS("./Data/Summary/Overview.rds") %>% 
            select(ticker, Exchange, ticker.ID) %>% distinct()

# -------------------------------------------------------------------------
Simulate.20191007 <- function(xscore, Process = "Production")
{
  if(nrow(xscore) == 0) {return(NULL)}

  sim2 <- foreach(i = unique(xscore$model.ID), .errorhandling = 'remove', .combine = bind_rows) %do%
  {
    # i = unique(xscore$model.ID)[1]
    sim2 <- xscore %>%
          filter(model.ID == i) %>% ungroup() %>% arrange(ds) %>%
          mutate(buy.window = case_when(R.buy > 0 ~ 1)) %>%
          AF.roll(df = ., var = "buy.window", width = 3) %>%
          mutate(buy.bin = case_when(!is.na(R.buy) ~ R)
                 , sell.window = ifelse(!is.na(lag(buy.window)) | !is.na(buy.window), 1, NA)
                 , buy.bin = sell.window*zoo::na.locf(buy.bin, na.rm = FALSE)
                 
                 , Type = zoo::na.locf(Type, na.rm = FALSE)
                 , Type = case_when(sell.window == 1 ~ Type)
                 , signal = 1.0*AF.signal(window = sell.window, Type)

                 , buy.price  = round(R.buy*lag(close), 2)
                 , buy.price  = buy.window*zoo::na.locf(buy.price, na.rm = FALSE)

                 , sell.price = round(R.sell*lag(close), 2)
                 , sell.price = sell.window*zoo::na.locf(sell.price, na.rm = FALSE)

                 , stop.price = ifelse(Type == "LONG",
                                       buy.price*(1 - R.stop),
                                       buy.price*(1 + R.stop))
                 , stop.price = sell.window*zoo::na.locf(stop.price, na.rm = FALSE)
                 , stop.price = round(stop.price, 2)

                 , last.sell = case_when(sell.window == 1 & is.na(buy.window) & !is.na(close) ~ close,
                                         sell.window == 1 & is.na(buy.window) & is.na(close) ~ 0,
                                         (Type == "LONG" & lag(Type) == "SHRT") |
                                           (Type == "SHRT" & lag(Type) == "LONG") ~ open )
          ) %>%
          AF.Sim.Core.001(.)

    rm(i)
    if(!all(is.na(sim2$ROI))) {return(sim2)}
  }

  return(sim2)
}

# -------------------------------------------------------------------------
Simulate.20191024 <- function(xscore, Process = "Production")
{
  if(nrow(xscore) == 0) {return(data.frame())}
  
  sim2 <- foreach(m = unique(xscore$model.ID), .combine = bind_rows, .errorhandling = 'remove') %do%
    {
      # m = unique(xscore$model.ID)[11]
      horizon <- as.numeric(substr(m, 10, 11))
      
      sim2 <- xscore %>%
        filter(model.ID == m) %>% group_by(model.ID) %>% arrange(ds) %>%
        mutate(buy.window = case_when(R.buy > 0 ~ 1)) %>%
        AF.roll(df = ., var = "buy.window", width = horizon - 1) %>%
        mutate(buy.bin = case_when(!is.na(R.buy) ~ R)
               , sell.window = ifelse(!is.na(lag(buy.window)) | !is.na(buy.window), 1, NA)
               , buy.bin = sell.window*zoo::na.locf(buy.bin, na.rm = FALSE)
               
               , Type = zoo::na.locf(Type, na.rm = FALSE)
               , Type = case_when(sell.window == 1 ~ Type)
               , signal = 1.0*AF.signal(window = sell.window, Type)
               
               , buy.price  = round(R.buy*lag(close), 2)
               , buy.price  = buy.window*zoo::na.locf(buy.price, na.rm = FALSE)
               
               , sell.price = round(R.sell*lag(close), 2)
               , sell.price = sell.window*zoo::na.locf(sell.price, na.rm = FALSE)
               
               , stop.price = ifelse(Type == "LONG",
                                     buy.price*(1 - R.stop),
                                     buy.price*(1 + R.stop))
               , stop.price = sell.window*zoo::na.locf(stop.price, na.rm = FALSE)
               , stop.price = round(stop.price, 2)
               
               , last.sell = case_when(sell.window == 1 & is.na(buy.window) & !is.na(close) ~ close,
                                       sell.window == 1 & is.na(buy.window) & is.na(close) ~ 0,
                                       (Type == "LONG" & lag(Type) == "SHRT") |
                                         (Type == "SHRT" & lag(Type) == "LONG") ~ open )
        ) %>%
        AF.Sim.Core.001(.)
      
      rm(m, horizon)
      if(!all(is.na(sim2$ROI))) {return(sim2)}
    }
  
  return(sim2)
}

# -------------------------------------------------------------------------
AF.Action.Today <- function(sim)
{
  bought <- sum(!is.na(sim %>% filter(ds == max(ds)) %>% pull(in.hand)))
  can.buy <- floor(max(Parms$invest.max.ticker/Parms$invest.max.model - bought, 0))

  today <- sim %>%
            group_by(algo.ID, model.ID) %>%
            arrange(algo.ID, model.ID, ds) %>%
            mutate(account = Parms[["acctCode"]],
                   action2 = case_when(grepl("BUY", action) ~ "BUY",
                                       grepl("SELL", action) ~ "SELL",
                                       TRUE ~ action),
                   last.close = lag(close),
                   missed.signal = ifelse(!is.na(action) & action == "MISSED BUY", 1, 0),
                   missed.signal = missed.signal*cumsum(missed.signal),
                   # Units is +ve for buy, -ve for sell
                   units = case_when(grepl("SELL", action) ~ -in.hand,
                                     grepl("BUY", action) & Type == "LONG" ~
                                       floor(pmin(Parms[["max.capacity"]]*lag(volume),
                                                  Parms[["invest.max.model"]]/buy.price)),
                                     grepl("BUY", action) & Type == "SHRT" ~
                                       -floor(pmin(Parms[["max.capacity"]]*lag(volume),
                                                  Parms[["invest.max.model"]]/buy.price))
                                     )) %>%
            filter(ds == max(ds) & !is.na(action) & !is.na(units)) %>%
            # # buy if higher than last close (for LONG) 
            # # sell if lesser than last.close (for SHORT)
            filter((action2 == "BUY" & Type == "LONG" & buy.price < last.close) | 
                   (action2 == "BUY" & Type == "SHRT" & buy.price > last.close) |
                   (action2 == "SELL") ) %>%
            # # Push Missed Buys for 3 days max
            filter((action == "MISSED BUY" & missed.signal <= 3) | action != "MISSED BUY") %>%
            # # Filter within capacity only
            # # Top rankers have lowest buy price (LONG) & highest buy price (SHRT)
            group_by(Type, action2) %>%
            mutate(rank = case_when(Type == "LONG" ~ rank(buy.price, ties.method = "last"),
                                    Type == "SHRT" ~ rank(-buy.price, ties.method = "last") ),
                   rank = ifelse(action2 == "BUY", rank, 0) ) %>%
            filter(rank <= can.buy) %>% ungroup() %>% 
            # Other Sanity Checks
            rename(active.day = signal) %>%
            left_join(overview, by = "ticker") %>%
            mutate(buy.price = case_when(grepl("BUY", action) ~ buy.price),
                   sell.price = case_when(grepl("SELL", action) ~ sell.price),
                   last.sell = case_when(grepl("SELL", action) ~ last.sell),
                   Exchange = ifelse(Exchange == "NASDAQ", "ISLAND", Exchange),
                   Type = case_when(grepl("BUY", action) & units > 0 ~ "LONG",
                                    grepl("SELL", action) & units < 0 ~ "LONG",
                                    grepl("BUY", action) & units < 0 ~ "SHRT",
                                    grepl("SELL", action) & units > 0 ~ "SHRT")
                   ) %>%
            select(ticker, ds, Type, action, units, buy.price, sell.price, stop.price, last.sell,
                   active.day, algo.ID, model.ID, account, Exchange, ticker.ID) %>%
            filter(!is.na(Type)) %>%
            arrange(ticker, active.day)

  rm(bought, can.buy)
  return(today)
}
