# AF.Open.Orders and AF.Close.Positions are embedded
IB.Emergency <- function()
{
  IB.Parms[["Emergency"]] <- TRUE
  assign("IB.Parms", IB.Parms, envir = .GlobalEnv)

  AF.Open.Orders <- function(return.df = FALSE)
  {
    Open.Orders <- function(tws)
    {
      tryCatch({.reqOpenOrders(tws)}
               , warning = function(w) 
               {
                 cat("\nWarning: There is a connection issue")
                 closeAllConnections()
                 rm(tws, envir = .GlobalEnv)
                 IB.Account.Status()
                 .reqOpenOrders(tws)
               }
               , error = function(e) 
               {
                 cat("\nError: There is a connection issue")
                 closeAllConnections()
                 rm(tws, envir = .GlobalEnv)
                 IB.Account.Status()
                 .reqOpenOrders(tws)
               }
      )
      con <- tws[[1]]
      eW  <- eWrapper()
      socketSelect(list(con), FALSE, NULL)
      curMsg <- readBin(con, character(), 1L)
      processMsg(curMsg, con, eW)
    }
    
    open <- data.frame()
    i <- 0 # Counter to extract information between 2 OPEN_ORDER_END messages
    n <- 0 # Counter to control the max messages / secton
    while(i < 5)
    {
      x <- Open.Orders(tws)
      if(!is.null(x) && !is.list(x))
      {
        #  5: .twsIncomingMSG$OPEN_ORDER
        # 53: .twsIncomingMSG$OPEN_ORDER_END
        #  3: .twsIncomingMSG$ORDER_STATUS
        
        if(x[1] == 53) {i = i + 1} else 
          if(x[1] == 5) {open <- bind_rows(open, data.frame(t(x), stringsAsFactors = FALSE))} 
        # if(x[1] == 3) {stat <- bind_rows(stat, data.frame(t(x), stringsAsFactors = FALSE))}
      }
      
      rm(x)
      n <- n + 1
      if(n %% 40 == 0) {Sys.sleep(1)}
    }
    
    rm(i, n, Open.Orders)
    
    if(nrow(open) > 0)
    {
      open <- open %>% distinct() %>%
        rename(IB.Version = X2, orderId = X3, conId = X4, symbol = X5, sectype = X6, 
               strike = X10, currency = X11, action = X13, totalQuantity = X14,
               orderType = X15, lmtPrice = X16, auxPrice = X17, tif = X18, 
               outsideRTH = X19, account = X20, algo.ID = X23, parentId = X25,
               orderStatus = X77
        ) %>%
        filter(!orderStatus %in% c("Filled", "Submitted")) %>%
        select(account, parentId, orderId, orderStatus, algo.ID, conId, symbol, sectype, 
               strike, currency, action, totalQuantity, orderType, lmtPrice, auxPrice, tif, 
               IB.Version) %>%
        mutate(orderId = as.integer(orderId)
               , parentId = as.integer(parentId)
               , totalQuantity = as.numeric(totalQuantity)
               , lmtPrice = as.numeric(lmtPrice)
               , auxPrice = as.numeric(auxPrice) )
      
    } else
    {
      cat("\nThere are NO Open Orders ... ")
      open <- data.frame(account = character(), parentId = integer(), orderId = integer()
                         , orderStatus = character()
                         , algo.ID = character(), conId = character(), symbol = character()
                         , sectype = character(), strike = character(), currency = character()
                         , action = character(), totalQuantity = numeric(), orderType = character()
                         , lmtPrice = numeric(), auxPrice = numeric(), tif = character()
                         , IB.Version = character(), stringsAsFactors = FALSE)
    }
    
    assign("IB.05.open.orders", open, envir = .GlobalEnv)
    
    if(return.df == TRUE) 
    {return(open)} else
    {rm(open)}
  }
  
  AF.Close.Positions <- function()
  {
    IB.Cancel.Orders()
    IB.Account.Status()
    
    x <- IB.00.positions %>%
          filter(position != 0) %>%
          rename(ticker = symbol, units = position) %>%
          select(accountName, conId, ticker, local, units, primary, currency) %>%
          mutate(primary = ifelse(primary == "NASDAQ", "ISLAND", primary)) %>%
          left_join(readRDS("./Data/Trading/01.Targets.rds") %>% 
                      select(algo.ID, ticker) %>% distinct()
                    , by = "ticker") %>%
          mutate(IB.action = ifelse(units > 0, "SELL", "BUY"), volume = abs(units)) %>%
          group_by(ticker, IB.action) %>%
          filter(row_number() == 1)
    
    AF.Update.Orderbook <- function(Contract, Order, order.type = "")
    {
      # order.type = "Order Placed"
      IB.03.orders <- data.frame(t(do.call(rbind, Contract)), stringsAsFactors = FALSE) %>%
                      bind_cols(data.frame(t(do.call(rbind, Order)), stringsAsFactors = FALSE)) %>%
                      select(account, parentId, orderId, conId, orderRef, symbol, sectype, strike,
                             currency, action, totalQuantity, orderType, lmtPrice, auxPrice, tif) %>%
                      rename(algo.ID = orderRef) %>%
                      mutate(parentId = as.integer(parentId)
                             , orderId = as.integer(orderId)
                             , totalQuantity = as.numeric(totalQuantity)
                             , lmtPrice = as.numeric(lmtPrice)
                             , auxPrice = as.numeric(auxPrice)
                             , order.ts = Sys.time()
                             , order.type) %>%
                      bind_rows(IB.03.orders)
      
      assign("IB.03.orders",  IB.03.orders, envir = .GlobalEnv)
      rm(IB.03.orders, order.type)
    }

    if(nrow(x) > 0)
    {
      IB.Parms[["Last.Order.Time"]] <- Sys.time()
      assign("IB.Parms", IB.Parms, envir = .GlobalEnv)
      
      for(i in 1:nrow(x))
      {
        Contract <- twsEquity(symbol = x$ticker[i], primary = x$primary[i], local = x$local[i], 
                              currency = x$currency[i], exch = "SMART")
        
        Order <- twsOrder(orderId = reqIds(tws)
                          , orderRef = x$algo.ID[i]
                          , action = x$IB.action[i]
                          , clientId = tws$clientId
                          , account = IB.Parms[["acctCode"]]
                          , totalQuantity = x$volume[i]
                          , orderType = "MKT"
                          , tif = "GTC")
        
        AF.Update.Orderbook(Contract, Order, order.type = "Order Placed")
        IBrokers::placeOrder(twsconn = tws, Contract, Order)
        rm(Contract, Order, i)
      }
      
    } else
    {
      rm(x)
      return(cat("\nAccount", IB.Parms[["acctCode"]],  "holds no positions. Verify on Workstation."))
    }
    
    rm(x, AF.Update.Orderbook)
  }

  repeat{
          AF.Close.Positions()
          cat("\nSystem will wait for 60 seconds to close all open positions ... \n")
          Sys.sleep(60)
          Update.Activity()
          AF.Open.Orders()
          if(nrow(IB.05.open.orders) == 0) {break}
        }

  View(IB.04.activity)
  cat("\nThe Emergency Procedure was completed.\n")
  
  # Deleting Plots
  unlink("./Data/Plots/libdir", recursive = TRUE)
  do.call(file.remove, list(list.files("./Data/Plots/", full.names = TRUE)))
  
  
  IB.Parms[["Emergency"]] <- FALSE
  assign("IB.Parms", IB.Parms, envir = .GlobalEnv)
  rm(AF.Open.Orders, AF.Close.Positions)
  
}

IB.Emergency()
rm(IB.Emergency)