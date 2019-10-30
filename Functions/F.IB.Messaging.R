# Source: https://stackoverflow.com/questions/35559742/reqexecutions-ibrokers-package

twsExecution <- function( execId = NA_character_,
                          time = NA_character_,
                          acctNumber = NA_character_,
                          exchange = NA_character_,
                          side = NA_character_,
                          shares = NA_integer_,
                          price = NA_real_,
                          permId = NA_integer_,
                          clientId = NA_integer_, ## no long type in R, but decoding process squeezes longs through ints, so may as well use int
                          orderId = NA_integer_, ## no long type in R, but decoding process squeezes longs through ints, so may as well use int
                          liquidation = NA_integer_,
                          cumQty = NA_integer_,
                          avgPrice = NA_real_,
                          orderRef = NA_character_,
                          evRule = NA_character_,
                          evMultiplier = NA_real_
                          ) 
{
  structure(
    list(execId=execId, time=time, acctNumber=acctNumber, exchange=exchange, side=side,
         shares=shares, price=price, permId=permId, clientId=clientId, orderId=orderId,
         liquidation=liquidation, cumQty=cumQty, avgPrice=avgPrice, orderRef=orderRef, 
         evRule=evRule, evMultiplier=evMultiplier),
    class='twsExecution'
  );
}; ## end twsExecution()

print.twsExecution <- function(x,...) str(unclass(x));

## replacement for reqExecutions()
req.Exec.2 <- function(twscon,reqId=0L,filter=list()) {
  
  ## validate the connection object
  if (!is.twsConnection(twscon)) stop('invalid twsConnection object.');
  if (!isConnected(twscon)) stop('peer has gone away. check your IB connection',call.=F);
  
  ## shallow validation of args
  if (!is.integer(reqId) || length(reqId) != 1L) stop('reqId must be a scalar integer.');
  if (!is.list(filter) || (is.null(names(filter)) && length(filter) > 0L)) stop('filter must be a named list.');
  
  ## send encoded request
  socketcon <- twscon[[1]]; ## extract socket connection from TWS connection object
  VERSION <- '3';
  prepareField <- function(x) if (is.null(x) || length(x) != 1L || is.na(x)) '' else as.character(x); ## empty string is accepted as unspecified
  outgoing <- c(
    .twsOutgoingMSG$REQ_EXECUTIONS,
    VERSION,
    prepareField(reqId), ## will receive this in the response along with data
    prepareField(filter$clientId), ## any client id; if invalid, will get zero results
    prepareField(filter$acctCode), ## must be a valid account code; seems to be ignored completely if invalid
    prepareField(filter$time), ## yyyymmdd HH:MM:SS
    prepareField(filter$symbol), ## must be a valid contract symbol, case-insensitive
    prepareField(filter$secType), ## STK|OPT|FUT|IND|FOP|CASH|BAG|NEWS
    prepareField(filter$exchange), ## must be a valid exchange name, case-insensitive; seems to be ignored completely if invalid
    prepareField(filter$side) ## buy|sell
  );
  writeBin(outgoing,socketcon); ## automatically appends a NUL after each vector element
  
  ## set handler method
  ## note: don't need to explicitly handle execDetailsEnd(); it provides no new data, and the below while-loop will check for and break on it
  ew <- eWrapper();
  ew$execDetails <- function(curMsg,msg,timestamp,file,...) {
    
    ## reqId and most contract and execution fields are returned in a character vector in msg
    ## build a return value by mapping the fields to their corresponding parameters of twsContract() and twsExecution()
    n <- (function() { n <- 0L; function() n <<- n+1L; })();
    version <- as.integer(msg[n()]);
    reqId <- if (version >= 7L) as.integer(msg[n()]) else -1L;
    orderId <- as.integer(msg[n()]); ## not sure why this is out-of-order with the remaining execution fields
    ## contract fields
    conId <- as.integer(msg[n()]);
    symbol <- msg[n()];
    secType <- msg[n()];
    lastTradeDateOrContractMonth <- msg[n()];
    strike <- as.double(msg[n()]);
    right <- msg[n()];
    multiplier <- ''; ##multiplier <- if (version >= 9L) msg[n()] else ''; ----- missing?
    exch <- msg[n()];
    primaryExchange <- ''; ## not returned
    currency <- msg[n()];
    localSymbol <- msg[n()];
    tradingClass <- if (version >= 10L) msg[n()] else '';
    includeExpired <- F; ## not returned
    secIdType <- ''; ## not returned
    secId <- ''; ## not returned
    comboLegsDescrip <- ''; ## not returned
    comboLegs <- ''; ## not returned
    underComp <- 0L; ## not returned
    ## execution fields
    execId <- msg[n()];
    time <- msg[n()];
    acctNumber <- msg[n()];
    exchange <- msg[n()];
    side <- msg[n()];
    shares <- as.integer(msg[n()]);
    price <- as.double(msg[n()]);
    permId <- as.integer(msg[n()]);
    clientId <- as.integer(msg[n()]);
    ## (orderId already assigned)
    liquidation <- as.integer(msg[n()]);
    cumQty <- if (version >= 6L) as.integer(msg[n()]) else 0L;
    avgPrice <- if (version >= 6L) as.double(msg[n()]) else 0;
    orderRef <- if (version >= 8L) msg[n()] else '';
    evRule <- if (version >= 9L) msg[n()] else '';
    evMultiplier <- if (version >= 9L) as.double(msg[n()]) else 0;
    
    ## build the list to return
    ## note: the twsContract() and twsExecution() functions provided with the IBrokers package as of 0.9-12 do not take all of the above fields; we'll pass what they take
    list(
      reqId=reqId,
      contract=twsContract(
        conId=conId,
        symbol=symbol,
        sectype=secType,
        exch=exch,
        primary=primaryExchange,
        expiry=lastTradeDateOrContractMonth,
        strike=strike,
        currency=currency,
        right=right,
        local=localSymbol,
        multiplier=multiplier,
        combo_legs_desc=comboLegsDescrip,
        comboleg=comboLegs,
        include_expired=includeExpired,
        secIdType=secIdType,
        secId=secId
      ),
      execution=twsExecution(
        execId=execId,
        time=time,
        acctNumber=acctNumber,
        exchange=exchange,
        side=side,
        shares=shares,
        price=price,
        permId=permId,
        clientId=clientId,
        orderId=orderId,
        liquidation=liquidation,
        cumQty=cumQty,
        avgPrice=avgPrice,
        orderRef=orderRef,
        evRule=evRule,
        evMultiplier=evMultiplier
      )
    );
    
  }; ## end execDetails()
  
  ## hack errorMessage() so we can differentiate between true errors and info messages; not the best design on the part of IB to conflate these
  body(ew$errorMessage)[[length(body(ew$errorMessage))+1L]] <- substitute(msg);
  
  ## iterate until we get the expected responses off the socket
  execList <- list();
  while (isConnected(twscon)) {
    socketSelect(list(socketcon),F,NULL);
    curMsg <- readBin(socketcon,character(),1L);
    res <- processMsg(curMsg,socketcon,eWrapper=ew,twsconn=twscon,timestamp=NULL,file='');
    ## check for error
    if (curMsg == .twsIncomingMSG$ERR_MSG) {
      ## note: the actual message was already catted inside processMsg() -> ew$errorMessage(); just abort if true error
      code <- as.integer(res[3L]);
      if (!code%in%c( ## blacklist info messages
        0   , ## "Warning: Approaching max rate of 50 messages per second (%d)"
        2103, ## "A market data farm is disconnected."
        2104, ## "A market data farm is connected."
        2105, ## "A historical data farm is disconnected."
        2106, ## "A historical data farm is connected."
        2107, ## "A historical data farm connection has become inactive but should be available upon demand."
        2108, ## "A market data farm connection has become inactive but should be available upon demand."
        2119  ## "Market data farm is connecting:%s" -- undocumented
      )) stop(paste0('request error ',code));
    }; ## end if
    ## check for data
    if (curMsg == .twsIncomingMSG$EXECUTION_DATA)
      execList[[length(execList)+1L]] <- res;
    ## check for completion
    if (curMsg == .twsIncomingMSG$EXECUTION_DATA_END) break;
  }; ## end while
  
  execList;
  
}; ## end req.Exec.2()

req.Exec.df <- function(...) {
  res <- req.Exec.2(...);
  do.call(rbind,lapply(res,function(e) do.call(data.frame,c(list(reqId=e$reqId),e$contract,e$execution,stringsAsFactors=F))));
}; ## end req.Exec.df()
