Update.Activity <- function(Last.Order = IB.Parms[["Last.Order.Time"]])
{
  source("./Functions/F.IB.Messaging.R")
  
  good.run <- FALSE
  while(!good.run)
  {
    x <- tryCatch({req.Exec.df(tws)}
                  , warning = function(w) {0}
                  , error = function(e) {0} )
    if(is.data.frame(x) | is.null(x)) {good.run <- TRUE}
  } 
  rm(good.run)
  
  rm(twsExecution, req.Exec.2, req.Exec.df, print.twsExecution, envir = .GlobalEnv)
  
  if(is.null(x)) {return(cat("\nThere were NO Recent Activity ... "))}
  
  suppressWarnings(
    x <- x %>%
          select(conId, orderId, symbol, side, time, shares, price, avgPrice) %>%
          rename(ticker = symbol, action = side) %>%
          mutate(order.ts = as.POSIXct(time, format="%Y%m%d %H:%M:%S",tz = Sys.timezone())) %>%
          # There is a 4-5 second clock diff of IB / System: Used 10 second for safe side
          filter(order.ts > Last.Order - 10) %>%
          mutate(conId = as.character(conId)
                 , NY.time = as.numeric(strftime(format(order.ts, tz = "US/Eastern"), format = "%H.%M"))
                 , IB.action = case_when(action == "BOT" ~ "BUY",
                                      action == "SLD" ~ "SELL") ) %>%
          group_by(IB.action, ticker) %>%
          summarise(order.ts = max(order.ts, na.rm = TRUE),
                    volume.executed = sum(shares),
                    price = weighted.mean(avgPrice, W = shares)) %>%
          ungroup()
  )

  if(nrow(x) == 0) 
  {
    rm(x)
    return(cat("\nThere were NO Recent Activities ... "))
  }

  # No provision for Manual Orders: Only System Generated Orders & Emergency
  x <- inner_join(IB.02.actions, x, by = c("IB.action", "ticker")) %>%
        mutate(Situation = ifelse(IB.Parms[["Emergency"]] == TRUE, "Emergency", "Normal")) %>%
        group_by(ticker, IB.action) %>% 
        arrange(ticker, IB.action, model.ID) %>%
        select(ticker, algo.ID, model.ID, Type, Situation, IB.action, 
               order.ts, price, volume, volume.executed) %>%
        mutate(vol.cum = cumsum(volume),
               vol.left = ifelse(volume.executed >= vol.cum, 0, vol.cum - volume.executed),
               vol.left = pmin(volume, vol.left),
               vol.real = volume - vol.left,
               units = ifelse(IB.action == "BUY", vol.real, -vol.real),
               account = IB.Parms[["acctCode"]] ) %>%
        select(ticker, algo.ID, model.ID, Type, Situation, IB.action, 
               order.ts, units, price, account) %>% 
        ungroup()

  # Update Targets
  y <- x %>% 
        mutate(units.changed = units) %>%
        select(ticker, algo.ID, model.ID, units.changed) %>%
        full_join(IB.01.targets, by = c("ticker", "algo.ID", "model.ID")) %>%
        mutate(units.changed = ifelse(is.na(units.changed), 0, units.changed),
               units = units - units.changed) %>%
        filter(units != 0) %>%
        select(-units.changed)

  x <- bind_rows(x, IB.04.activity) %>% ungroup() %>% arrange(desc(order.ts)) %>% distinct()
  assign("IB.04.activity", x, envir = .GlobalEnv)
  assign("IB.01.targets", y, envir = .GlobalEnv)
  IB.Parms[["Last.Order.Time"]] <- Sys.time()
  assign("IB.Parms", IB.Parms, envir = .GlobalEnv)
  
  rm(x, y, Last.Order)
  gc()
  
}
