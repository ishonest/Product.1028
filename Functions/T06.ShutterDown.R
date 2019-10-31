AF.Manual.Activity <- function()
{
  First.Order <- as.POSIXct(paste(Sys.Date(), "09:30:00"), tz = "America/New_York")
  
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
      filter(order.ts > First.Order - 10) %>%
      mutate(conId = as.character(conId)
             , NY.time = as.numeric(strftime(format(order.ts, tz = "US/Eastern"), format = "%H.%M"))
             , IB.action = case_when(action == "BOT" ~ "BUY",
                                     action == "SLD" ~ "SELL")
      ) %>%
      group_by(IB.action, ticker) %>%
      summarise(order.ts = max(order.ts, na.rm = TRUE),
                volume.executed = sum(shares),
                price = weighted.mean(avgPrice, W = shares)) %>%
      ungroup()
  )

  if(nrow(x) == 0)
  {
    rm(x)
    return(cat("\nThere were NO Activity Today ... "))
  }

  x1 <- IB.04.activity %>% filter(order.ts >= First.Order - 10) %>%
        group_by(ticker, IB.action) %>% 
        summarise(volume.accounted = abs(sum(units))) %>%
        full_join(x, by = c("ticker", "IB.action")) %>%
        mutate(volume.accounted = ifelse(is.na(volume.accounted), 0, volume.accounted),
               volume.executed = ifelse(is.na(volume.executed), 0, volume.executed),
               volume.left = volume.executed - volume.accounted
               ) %>%
        filter(volume.left > 0) %>%
        select(-c(volume.accounted, volume.executed)) %>%
        rename(volume.executed = volume.left)

  x2 <- IB.01.targets %>%
        mutate(Situation = "Manual", 
               IB.action = ifelse(units > 0, "BUY", "SELL"),
               volume = abs(units)
               ) %>%
        select(ticker, algo.ID, model.ID, Type, Situation, IB.action, volume, account) %>%
        inner_join(x1, by = c("ticker", "IB.action")) %>%
        group_by(ticker, IB.action) %>% 
        arrange(ticker, IB.action, model.ID) %>%
        mutate(vol.cum = cumsum(volume),
               vol.left = ifelse(volume.executed >= vol.cum, 0, vol.cum - volume.executed),
               vol.left = pmin(volume, vol.left),
               vol.real = volume - vol.left,
               units = ifelse(IB.action == "BUY", vol.real, -vol.real)
        ) %>%
        filter(vol.real > 0) %>%
        select(ticker, algo.ID, model.ID, Type, Situation, IB.action, 
               order.ts, units, price, account) %>% 
        ungroup()
  
  x3 <- bind_rows(x2, IB.04.activity) %>% distinct()

  assign("IB.04.activity", x3, envir = .GlobalEnv)
  rm(x, x1, x2, x3, First.Order)
  gc()

}

AF.Performance.Summary <- function()
{
  if(nrow(IB.04.activity) == 0)
  {
    return(cat("\nThere were No activities on Production Algorithms\n"))
  }
  
  x <- IB.04.activity %>%
        group_by(ticker, algo.ID, model.ID, Type, IB.action) %>%
        arrange(ticker, algo.ID, model.ID, Type, IB.action, order.ts) %>%
        mutate(ds = min(as.Date(order.ts))) %>%
        group_by(ticker, algo.ID, model.ID, Type, IB.action, ds) %>%
        summarise(price = weighted.mean(price, units, na.rm = TRUE),
                  units = sum(units, na.rm = TRUE) ) %>%
        ungroup() %>%
        data.table::setDT() %>%
        data.table::dcast(ticker + algo.ID + model.ID + Type ~ IB.action,
                          value.var = c("ds", "price", "units"), sep = "." , 
                          fun.aggregate=mean) 
  
  if(is.null(x$units.SELL) | is.null(x$price.SELL))
  {
    rm(x)
    return(cat("\nNo trades on Production Algorithms have been closed ...\n"))
  }
  
  x <- x %>%
        filter(units.BUY + units.SELL == 0) %>%
        mutate(invest = case_when(Type == "LONG" ~ units.BUY*price.BUY,
                                  Type == "SHRT" ~ -units.SELL*price.SELL),
               return = case_when(Type == "LONG" ~ -units.SELL*price.SELL,
                                  Type == "SHRT" ~ invest - (units.BUY*price.BUY - invest)),
               roi = return/invest,
               duration = abs(ds.SELL - ds.BUY),
               buy.ds = pmin(ds.BUY, ds.SELL),
               sell.ds = pmax(ds.BUY, ds.SELL)
        ) %>%
        select(ticker, algo.ID, model.ID, Type, buy.ds, sell.ds, invest, return, roi, duration) %>%
        ungroup()
  
  if(nrow(x) == 0)
  {
    rm(x)
    return(cat("\nNo trades on Production Algorithms have been closed ...\n"))
  }
  

  y <- x %>% group_by(algo.ID, Type) %>%
        summarise(Eff = max(sell.ds) - min(buy.ds), 
                  Models = n(),
                  SR.02 = sum(roi >= 1.02)/Models,
                  Duration = weighted.mean(duration, invest),
                  Invest = sum(invest),
                  Return = sum(return),
                  ROI = Return/Invest,
                  ROI.Best = max(roi),
                  ROI.Worst = min(roi)
                  ) %>%
        bind_rows(x %>% group_by(algo.ID) %>%
                    summarise(Type = "All",
                              Eff = max(sell.ds) - min(buy.ds), 
                              Models = n(),
                              SR.02 = sum(roi >= 1.02)/Models,
                              Duration = weighted.mean(duration, invest),
                              Invest = sum(invest),
                              Return = sum(return),
                              ROI = Return/Invest,
                              ROI.Best = max(roi),
                              ROI.Worst = min(roi) )
                  ) %>%
        mutate(Return = paste("$", formatC(Return, format="f", big.mark=",", digits=0)),
               Invest = paste("$", formatC(Invest, format="f", big.mark=",", digits=0)),
               ROI = paste0(formatC(100*ROI, format="f", big.mark=",", digits=1), "%"),
               ROI.Best = paste0(formatC(100*ROI.Best, format="f", big.mark=",", digits=1), "%"),
               ROI.Worst = paste0(formatC(100*ROI.Worst, format="f", big.mark=",", digits=1), "%"),
               SR.02 = paste0(formatC(100*SR.02, format="f", big.mark=",", digits=1), "%"),
               Duration = paste0(round(Duration, 2), " Days"),
               Eff = paste0(round(Eff, 0), " Days"),
               ) %>%
        arrange(algo.ID, desc(Type)) %>%
        rename("Investment" = Invest,
               "Returns" = Return, 
               "Best" = ROI.Best,
               "Worst" = ROI.Worst,
               "SR @ 2%" = SR.02,
               "Algo ID" = algo.ID,
               "Commissioned" = Eff) 
        
  
  cat("\n")
  print(knitr::kable(y, align = 'r'))
  cat("\n")
  
  rm(x, y)
  
}

IB.Shutter.Down <- function(Force.Close = TRUE)
{
  NY.Time <- as.numeric(strftime(format(Sys.time(), tz = "US/Eastern"), format = "%H.%M"))
  if(!(NY.Time > 16.00 | Force.Close))
  {return(cat("\nWarning!!! Market is Still Open. \nCannot Shutdown the System ... "))}
  rm(NY.Time, Force.Close)

  IB.Account.Status()
  IB.Cancel.Orders()
  Update.Activity()
  AF.Manual.Activity()
  AF.Performance.Summary()

  h.activity <- readRDS("./Data/Trading/02.Historical.Activity.rds") %>%
                bind_rows(IB.04.activity) %>% 
                distinct() %>% arrange(desc(order.ts))

  # h.latest <- IB.01.targets %>% filter(grepl("SELL", action)) %>%
  #             select(account, ticker, algo.ID, model.ID, units) %>%
  #             mutate(units = -units)
  
  h.latest <- h.activity %>% group_by(account, ticker, algo.ID, model.ID) %>% 
              summarise(units = sum(units)) %>%
              filter(units != 0) %>% ungroup()
  
  h.nav <- IB.00.positions %>%
            mutate(PNL = as.numeric(unrealizedPNL) + as.numeric(realizedPNL),
                   value = ifelse(position < 0, -marketValue + 2*PNL, marketValue) ) %>%
            select(accountName, symbol, position, value, currency) %>%
            rename(account = accountName, ticker = symbol) %>%
            bind_rows(data.frame(account = IB.Parms[["acctCode"]],
                                 ticker = "^CASH",
                                 value = Available.Funds,
                                 currency = "USD",
                                 stringsAsFactors = FALSE
                                 )) %>%
            mutate(ts = Sys.time(), ds = Sys.Date()) %>%
            bind_rows(readRDS("./Data/Trading/04.Historical.NAV.rds")) %>%
            group_by(account, ticker, ds) %>%
            filter(ts == max(ts)) %>%
            ungroup()
  
  h.orders <- IB.03.orders %>% 
              bind_rows(readRDS("./Data/Trading/03.Historical.Orders.rds")) %>% 
              distinct() %>% arrange(desc(order.ts))
  
  View(h.activity)
  View(h.latest)
  
  nav.perf <- h.nav %>% group_by(ds) %>% 
              summarise(value = sum(value)) %>%
              ungroup() %>% mutate(NAV = value/first(value))
  
  p <- plot_ly(x = ~ds, y = ~NAV, data = nav.perf, 
              type = 'scatter', mode = 'lines', line = list(color = '#a6a6a6', width = 0.5)) %>%
        layout(font = list(size = 12), showlegend = FALSE, 
               xaxis = list(title = NA),
               yaxis = list(title = "NAV", color = '#3f6ea6', tickformat = ".2%") )
  
  print(p)
  
  saveRDS(h.latest, "./Data/Trading/00.Latest.rds")
  saveRDS(h.activity, "./Data/Trading/02.Historical.Activity.rds")
  saveRDS(h.orders, "./Data/Trading/03.Historical.Orders.rds")
  saveRDS(h.nav, "./Data/Trading/04.Historical.NAV.rds")
  
  rm(h.activity, h.orders, h.latest, h.nav, nav.perf, p)
}

# -------------------------------------------------------------------------
IB.Shutter.Down()
rm(AF.Manual.Activity, AF.Performance.Summary)
# -------------------------------------------------------------------------

NY.Time <- as.numeric(strftime(format(Sys.time(), tz = "US/Eastern"), format = "%H.%M"))
if(NY.Time > 16.00)
{
  do.call(unlink, list(list.files(c("./Data/Simulation/", "./Data/Scores/"), full.names = TRUE)))
  unlink("./Data/Trading/01.Targets.rds")
  unlink("./Data/Summary/Latest.Prices.rds")
  twsDisconnect(tws)
  rm(list = ls())
  gc()
} else{rm(NY.Time)}
