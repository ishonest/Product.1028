# Settings: https://blog.quantinsti.com/ibpy-tutorial-implement-python-interactive-brokers-api/
# Initialization inside IB.StartDay
# -------------------------------------------------------------------------
# install.packages(c("doSNOW", "foreach", "IBrokers", "BatchGetSymbols", "TTR",
#                    "timeDate", "dplyr", "zoo", "lubridate", "tidyr", "data.table",
#                    "MASS", "profvis", "plotly", "htmlwidgets", "xlsx", "openxlsx",
#                    "knitr", "ggplot2"))

# -------------------------------------------------------------------------
# Initialization
# -------------------------------------------------------------------------
rm(list = setdiff(ls(envir = .GlobalEnv), c("IB.Parms")), envir = .GlobalEnv)
closeAllConnections()
options(scipen = 4, digits = 8, digits.secs = 0)
set.seed(1024)

library(IBrokers)
library(dplyr)
library(foreach)
library(BatchGetSymbols)
library(plotly)
library(ggplot2)
library(htmlwidgets)
library(knitr)
library(doSNOW)

# closeAllConnections()
# cl <- makeCluster(4, outfile="dopar_log.txt")
# registerDoSNOW(cl)

# -------------------------------------------------------------------------
# Start-Up Functions 
# -------------------------------------------------------------------------
IB.Account.Status <- function()
{
  AF.Connect <- function()
  {
    if(!exists("tws", envir = .GlobalEnv) || tws$clientId != IB.Parms[["clientId"]])
    {
      closeAllConnections()
      if(exists("tws", envir = .GlobalEnv)) {rm(tws, envir = .GlobalEnv)}
      tws <- list(clientId = -1)
      # This loop makes sure that the client id is what supplied
      while(tws$clientId != IB.Parms[["clientId"]] )
      {
        tws <- tryCatch({twsConnect(clientId = IB.Parms[["clientId"]])}
                        , warning = function(w) {list(clientId = -1)}
                        , error = function(e) {list(clientId = -2)} )
      }
      
      assign("tws", tws, envir = .GlobalEnv)
      cat("\nConnection was Re-established\n")
      print(tws)
    }

    setServerLogLevel(tws)
  }
  
  AF.Connect()

  cancelAccountUpdates(conn = tws)
  d <- reqAccountUpdates(conn = tws, acctCode = IB.Parms[["acctCode"]], subscribe = TRUE)

  assign("IB.00.account",
         data.frame(t(sapply(d[[1]],c)), stringsAsFactors = FALSE), 
         envir = .GlobalEnv)
  
  d2 <- d[[2]]
  IB.00.positions <- data.frame()
  if(length(d2) > 0)
  {
    for(i in 1:length(d2))
    {
      x <- bind_cols(data.frame(t(sapply(d2[[i]][1][[1]],c)), stringsAsFactors = FALSE),
                     data.frame(t(sapply(d2[[i]][2][[1]],c)), stringsAsFactors = FALSE) )
      
      IB.00.positions <- bind_rows(IB.00.positions, x)
      rm(x, i)
    }
    
    IB.00.positions <- IB.00.positions[c("accountName", "conId", "symbol", "position",
                                   "marketPrice", "marketValue", "averageCost",
                                   "unrealizedPNL", "realizedPNL",
                                   "local", "currency", "exch", "primary", "sectype",
                                   "expiry", "strike", "right",
                                   "multiplier", "combo_legs_desc", "comboleg",
                                   "include_expired", "secIdType", "secId")]
    
    cols.to.numeric <- c("strike", "right", "position", "marketPrice",
                         "marketValue", "averageCost", "unrealizedPNL")
    IB.00.positions[cols.to.numeric] <- as.numeric(as.matrix(IB.00.positions[cols.to.numeric]))
    assign("IB.00.positions", IB.00.positions, envir = .GlobalEnv)
    rm(cols.to.numeric)
  }
  IB.00.positions <- IB.00.positions %>% filter(position != 0)
  assign("IB.00.positions", IB.00.positions, envir = .GlobalEnv)
  
  if(nrow(IB.00.positions) > 0)
  {
    x <- IB.00.positions %>% ungroup() %>%
          summarise(Current.Position = sum(abs(marketValue), na.rm = TRUE),
                    Investment = sum(abs(position*averageCost), na.rm = TRUE),
                    Available.Funds = IB.Parms[["invest.max"]] - Investment
                    )
    
    assign("Available.Funds", x$Available.Funds, envir = .GlobalEnv)
    cat(paste0("\n------------------------------------------------", 
               "\n: Available Funds: $ ", formatC(x$Available.Funds, format="f", big.mark=",", digits=0),
               "\n------------------------------------------------\n"  ))
    
    if(x$Available.Funds <= 1000) {cat("\nWarning!!! Available Funds running low.\n")}
    rm(x)
    
  } else
  {
    assign("Available.Funds", IB.Parms[["invest.max"]], envir = .GlobalEnv)
    cat(paste0("\nThere have been no trades on this account yet!", 
               "\n------------------------------------------------", 
               "\n: Available Funds: $ ", formatC(IB.Parms[["invest.max"]], 
                                                  format="f", big.mark=",", digits=0),
               "\n------------------------------------------------\n"  ))
  }

  rm(d, d2, AF.Connect)
  
}

IB.System.Status <- function()
{
  source("./Functions/F.Trading.Days.R")
  NY.Time <- round(as.numeric(strftime(format(Sys.time(), tz = "US/Eastern"), format = "%H.%M")), 2)
  Next.Day <- format(NextTradingDate(), '%A, %B %d, %Y')
  Trade.Days <- TradingDates()
  rm(NextTradingDate, PrevTradingDate, TradingDates, envir = .GlobalEnv)
  
  # -------------------------------------------------------------------------
  Time.Difference <- function(to, fr)
  {
    # to = 9.3
    # fr = NY.Time
    to <- strptime(sprintf("%.2f", to), format="%H.%M")
    fr <- strptime(sprintf("%.2f", fr), format="%H.%M") 
    
    d <- difftime(to, fr, units = "hours")
    d.hours <- floor(as.numeric(d))
    d.minutes <- round((as.numeric(d)%% 1 * 60))
    
    op <- if(d.hours == 0) 
    {
      if(d.minutes == 1) {paste("1 minute")} else {paste(d.minutes, "minutes")}
    } else if(d.hours == 1)
    {
      paste("1 hour and", 
            if(d.minutes == 1) {paste("1 minute")} else {paste(d.minutes, "minutes")}
      )
    } else
    {
      paste(d.hours, "hours and", 
            if(d.minutes == 1) {paste("1 minute")} else {paste(d.minutes, "minutes")}
      )
    }
    
    rm(to, fr, d, d.hours, d.minutes)
    return(op)
  }
  
  # -------------------------------------------------------------------------
  if(!(Sys.Date() %in% Trade.Days) | NY.Time > IB.Parms[["Stop.Trading.At"]]) 
  {
    IB.Parms[["System.Live"]] <- FALSE
    cat(paste("\nNYSE & NASDAQ are closed today. Markets will reopen on", Next.Day, "... \n"))
    rm(NY.Time, Next.Day, Trade.Days)
  } else if(NY.Time < IB.Parms[["Start.Trading.At"]])
  {
    IB.Parms[["System.Live"]] <- FALSE
    cat("\nMarkets are closed now. Will reopen in", 
        Time.Difference(to = IB.Parms[["Start.Trading.At"]], fr = NY.Time), "... \n")
    rm(NY.Time, Next.Day, Trade.Days, Time.Difference)
    
  } else if(IB.Parms[["Emergency"]] == TRUE)
  {
    IB.Parms[["System.Live"]] <- FALSE
    cat("\nSystem halted due to Emergency", "... \n")
    rm(NY.Time, Next.Day, Trade.Days, Time.Difference)
    
  } else 
  {
    IB.Parms[["System.Live"]] <- TRUE
    rm(NY.Time, Next.Day, Trade.Days, Time.Difference)
    
  }
  
  assign("IB.Parms", IB.Parms, envir = .GlobalEnv)
}

IB.StartDay <- function()
{
  assign("IB.01.targets"
         , readRDS("./Data/Trading/01.Targets.rds") %>%
           filter(account == IB.Parms[["acctCode"]]) 
         , envir = .GlobalEnv)
  
  assign("IB.03.orders"
         , data.frame(stringsAsFactors = FALSE)
         , envir = .GlobalEnv)
  
  assign("IB.04.activity"
         , readRDS("./Data/Trading/02.Historical.Activity.rds") %>%
           filter(account == IB.Parms[["acctCode"]])
         , envir = .GlobalEnv)
  
  IB.Parms[["Last.Order.Time"]] <- Sys.time()
  assign("IB.Parms", IB.Parms, envir = .GlobalEnv)
  
  IB.Account.Status()
  IB.System.Status()
}

IB.Next.Run <- function(wait.seconds = 120)
{
  NY.Time <- as.numeric(strftime(format(Sys.time(), tz = "US/Eastern"), format = "%H.%M"))
  if(NY.Time < IB.Parms$Start.Trading.At)
  {
    next.run <- difftime(strptime(paste(Sys.Date(), IB.Parms$Start.Trading.At), "%Y-%m-%d %H.%M", 
                                  tz = "US/Eastern"),
                        lubridate::with_tz(Sys.time(), tzone = "US/Eastern"),
                        units = "secs" 
                        ) %>% as.numeric() %>% ceiling()
  } else
  {
    next.run <- wait.seconds
  }
  
  cat("\nCycle Complete: Next cycle will commence at", format(Sys.time() + next.run, "%X"), "...\n")
  Sys.sleep(next.run)
  rm(next.run, NY.Time)
  cat("\n--------------------------------------------------------------\n")
  IB.System.Status()
}

IB.StartDay()
rm(IB.StartDay)

# -------------------------------------------------------------------------
# Call Other Trading Functions
# -------------------------------------------------------------------------
source("./Functions/T02.Actions.R")
source("./Functions/T03.Order.R")
source("./Functions/T04.Cancel.R")
source("./Functions/T05.Activity.R")
