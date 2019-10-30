IB.Order <- function()
{
  if(IB.Parms[["Emergency"]]) {return(cat("\nSystem halted due to Emergency"))}
  if(nrow(IB.02.actions) == 0) {return(cat("\nThere are NO Actions ... "))}
  
  IB.Parms[["Last.Order.Time"]] <- Sys.time()
  assign("IB.Parms", IB.Parms, envir = .GlobalEnv)
  
  x <- IB.02.actions %>%
        group_by(NY.Time, ticker, Exchange, ticker.ID, IB.action, IB.orderType, t.price, algo.ID) %>%
        summarise(volume = sum(volume, na.rm = TRUE))
  
  AF.Update.Orderbook <- function(Contract, Order, order.type = "")
  {
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
  
  for(i in 1:nrow(x))
  {
    Contract <- twsEquity(symbol = x$ticker[i], local = x$ticker[i], primary = x$Exchange[i]
                          , currency = "USD", exch = "SMART")

    Order <- twsOrder(orderId = reqIds(tws)
                      , orderRef = x$algo.ID[i]
                      , action = x$IB.action[i]
                      , clientId = tws$clientId
                      , account = IB.Parms[["acctCode"]]
                      , totalQuantity = x$volume[i]
                      , orderType = x$IB.orderType[i]
                      , lmtPrice = x$t.price[i]
                      , tif = "GTC")

    if(i %% 40 == 0) {Sys.sleep(1)} # Limitations of API: Can process only 50 orders/second
    IBrokers::placeOrder(twsconn = tws, Contract, Order)
    AF.Update.Orderbook(Contract, Order, order.type = "Order Placed")
    
    rm(i, Contract, Order)
  }
  
  rm(x, AF.Update.Orderbook)
}