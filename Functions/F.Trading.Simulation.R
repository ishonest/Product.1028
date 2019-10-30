# -------------------------------------------------------------------------
# Supporting Functions
# -------------------------------------------------------------------------
AF.roll <- function(df, var, width)
{
  # df = y
  # width = 4
  # var = "buy.window"
  
  df$op <- NA
  
  for(i in 0:width)
  {df$op <- pmax(lag(df[[var]], i), df$op, na.rm = TRUE)}
  
  df <- df %>% select(-var) %>% rename(!!var := op)
  
  rm(var, width, i)
  return(df)
}

AF.signal <- function(window, Type)
{
  # window = s$sell.window
  # Type = s$Type
  N <- length(window)
  strength <- rep(NA, N)
  op <- 0
  for(i in 1:N)
  {
    if(is.na(window[i])) {op <- 0}
    else if(i == 1) {op <- 1}
    else if(!is.na(Type[i-1]) & !is.na(Type[i]) & Type[i-1] != Type[i]) {op <- 1}
    else {op <- op + 1}
    
    if(op > 0) {strength[i] <- op}
    rm(i)
  }
  
  rm(N, op, window, Type)
  return(strength)
}

# -------------------------------------------------------------------------
# Core function for Simulation
# -------------------------------------------------------------------------
AF.Sim.Core.001 <- function(sim2, Process = "Production")
{
  # Process = "Production"
  sim3 <- sim2 %>% ungroup() %>% 
          arrange(ds) %>%
          mutate(buy.signal = case_when(Type == "LONG" & buy.price >= low ~ TRUE,
                                        Type == "SHRT" & buy.price <= high ~ TRUE,
                                        TRUE ~ FALSE),
                 sell.signal = case_when(Type == "LONG" & sell.price <= high ~ TRUE,
                                         Type == "SHRT" & sell.price >= low ~ TRUE,
                                         TRUE ~ FALSE),
                 stop.signal = case_when(Type == "LONG" & stop.price >= low ~ TRUE,
                                         Type == "SHRT" & stop.price <= high ~ TRUE,
                                         TRUE ~ FALSE) )
  
  buy.signal <- sim3$buy.signal
  sell.signal <- sim3$sell.signal
  stop.signal <- sim3$stop.signal
  
  clean.x <- function(x) 
  {
    x[is.na(x) | x < 0] <- 0
    return(x)
  }
  
  buy.price <- clean.x(sim3$buy.price)
  sell.price <- clean.x(sim3$sell.price)
  stop.price <- clean.x(sim3$stop.price)
  last.sell <- clean.x(sim3$last.sell)
  
  cost <- 0
  type <- ""
  holding.days <- 0
  trade.val <- 0
  continue <- 1 # To Prevent Buying After Stoploss in a series
  buy.bin <- NA
  
  action <- rep(NA, nrow(sim3))
  capacity <- rep(NA, nrow(sim3))
  ROI <- rep(NA, nrow(sim3))
  invest.period <- rep(NA, nrow(sim3))
  continue.trading <- rep(NA, nrow(sim3))
  bin <- rep(NA, nrow(sim3))
  
  for(i in 1:nrow(sim3))
  {
    if(sell.price[i] == 0) {continue <- 1}
    continue.trading[i] <- continue
    
    # Buy Or Sell. If buy, don't sell (Because we don't know the sequence of low and high)
    # Do Not Stop Loss on Purchase Day
    
    if(buy.signal[i] & cost == 0 & continue > 0)           # BUY
    {
      action[i] <- "BUY"
      type <- sim3$Type[i]
      cost <- buy.price[i]
      holding.days <- 1
      trade.val <- round(sim3$volume[i]*(sim3$open[i] + sim3$close[i])/2, 0)
      capacity[i] <- trade.val
      
      buy.bin <- sim3$R[i]
      bin[i] <- buy.bin
      
    } else
      if(sell.signal[i] & cost > 0)                        # Sell
      {
        action[i] <- "TARGET SELL"
        capacity[i] <- trade.val
        ROI[i] <- case_when(type =="LONG" ~ sell.price[i]/cost,
                            type =="SHRT" ~ 2 - sell.price[i]/cost)
        invest.period[i] <- holding.days + 1
        bin[i] <- buy.bin
        
        cost <- 0
        type <- ""
        holding.days <- 0
        trade.val <- 0
        buy.bin <- NA
        
      } else
        if(i < nrow(sim3) & cost > 0 & stop.signal[i])     # Sell @ stop loss
        {
          action[i] <- "STOP SELL"
          capacity[i] <- trade.val
          ROI[i] <- case_when(type =="LONG" ~ stop.price[i]/cost,
                              type =="SHRT" ~ 2 - stop.price[i]/cost)
          
          invest.period[i] <- holding.days + 1
          bin[i] <- buy.bin
          
          cost <- 0
          type <- NA
          holding.days <- 0
          trade.val <- 0
          continue <- 0
          buy.bin <- NA
        } else
          if(i < nrow(sim3) & cost > 0 & last.sell[i] > 0) # Sell @ Close if still to be sold
          {
            action[i] <- "EOD SELL"
            capacity[i] <- trade.val
            ROI[i] <- last.sell[i]/cost
            ROI[i] <- case_when(type =="LONG" ~ last.sell[i]/cost,
                                type =="SHRT" ~ 2 - last.sell[i]/cost)
            invest.period[i] <- holding.days + 1
            bin[i] <- buy.bin
            
            cost <- 0
            type <- NA
            holding.days <- 0
            trade.val <- 0
            buy.bin <- NA
            
          } else
            if(cost > 0) # Hold
            {
              action[i] <- "HOLD"
              holding.days <- holding.days + 1
              if(trade.val > 0) {capacity[i] <- trade.val}
              bin[i] <- buy.bin
            }
    
  }
  
  op <- sim3 %>% 
        bind_cols(data.frame(action, ROI, invest.period, capacity, stringsAsFactors = FALSE)) %>% 
        select(algo.ID, model.ID, R, buy.bin, ticker, ds, Type,
               Score, signal, buy.price, sell.price, stop.price, last.sell, 
               action, ROI, invest.period, capacity, volume, open, low, high, close) %>%
        mutate(ROI = ifelse(!is.na(ROI) & ROI < 0, 0, ROI),
               action = ifelse(!is.na(ROI) & ROI < 0, "STOP SELL", action)) %>%
        arrange(ds)
    
  if(Process == "Production")
  {
      if(file.exists("./Data/Trading/00.Latest.rds"))
      {
        op <- left_join(op, readRDS("./Data/Trading/00.Latest.rds") %>% 
                            filter(account == Parms[["acctCode"]]) %>% 
                            ungroup() %>% select(-account) %>% 
                            rename(in.hand = units), 
                        by = c("algo.ID", "model.ID", "ticker"))
      } else
      {
        op$in.hand <- NA
      }
    
    op <- op %>%
      # Provisions for Missed Sell
      mutate(missed.sell = case_when(!is.na(in.hand) &&
                                       max(which(grepl("SELL", action))) > max(which(grepl("BUY", action))) &&
                                       row_number() > max(which(grepl("BUY", action))) ~ 1,
                                     TRUE ~ 0)
             , action = ifelse(missed.sell == 1, "MISSED SELL", action)
             , Type = ifelse(missed.sell == 1, zoo::na.locf(Type), Type)
             , signal = ifelse(missed.sell == 1 & is.na(signal), zoo::na.locf(signal), signal)
             , sell.price = ifelse(missed.sell == 1 & is.na(sell.price), zoo::na.locf(sell.price), sell.price)
             , stop.price = ifelse(missed.sell == 1 & is.na(stop.price), zoo::na.locf(stop.price), stop.price)
             , last.sell = case_when(missed.sell == 1 && row_number() > max(which(!!last.sell > 0)) ~
                                                                    last(last.sell[!!last.sell > 0]),
                                     TRUE ~ last.sell)
             , continue.trading = ifelse(missed.sell == 1, 1, continue.trading)
             , ROI = ifelse(missed.sell == 1, NA, ROI)
             , invest.period = ifelse(missed.sell == 1, NA, invest.period)
             ) %>%
      select(-missed.sell) %>%
      
      # Provisions for Missed Buy
      mutate(missed.buy = case_when(is.na(in.hand) && 
                                      !is.na(last(action)) && last(action) == "HOLD" &&
                                      row_number() >= max(which(grepl("BUY", action))) ~ 1,
                                    TRUE ~ 0)
             , action = ifelse(missed.buy == 1, "MISSED BUY", action)
             , capacity = ifelse(missed.buy == 1, NA, capacity) ) %>% 
      select(-missed.buy) %>%
      
      # Removing Redundant Price Points
      mutate(buy.price = case_when(continue.trading > 0 ~ buy.price)
             , sell.price = case_when(continue.trading > 0 ~ sell.price)
             , stop.price = case_when(continue.trading > 0 ~ stop.price)
             , last.sell = case_when(continue.trading > 0 ~ last.sell) ) %>%
      select(-continue.trading) %>%
      
      # Provisions for Last Day
      mutate(action = as.character(action), 
             action = case_when(row_number() == n() &
                                  is.na(action) & !is.na(buy.price) & is.na(in.hand) ~ "CAN BUY",
                                row_number() == n() &
                                  !is.na(action) & action == "HOLD" ~ "CAN SELL",
                                TRUE ~ action))
    
  }
  
  rm(buy.signal, sell.signal, stop.signal, buy.price, sell.price, stop.price, last.sell
     , cost, type, holding.days, trade.val, continue, bin, buy.bin
     , action, capacity, ROI, invest.period, continue.trading, sim3, i, clean.x, Process)
  gc()
  
  return(op)
}

