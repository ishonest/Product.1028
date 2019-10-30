IB.Actions <- function(today = Sys.Date())
{
  # today <- Sys.Date()
  if(length(unique(IB.01.targets$ticker)) == 0) 
  {return(cat("\nNo Targets Available. Check Today's Targets. \n"))}
  
  # Pull Latest Data
  if(Available.Funds <= IB.Parms$invest.min)
  {
    tickers <- unique(IB.00.positions$symbol)
  } else
  {
    tickers <- readRDS("./Data/Trading/01.Targets.rds") %>%
      filter(account == IB.Parms[["acctCode"]]) %>%
      select(ticker) %>% distinct() %>% unlist(use.names = FALSE)
  }
  
  cl <- makeCluster(4, outfile="dopar_log.txt")
  registerDoSNOW(cl)
  
  d1 <- foreach(ticker = tickers, .packages = "BatchGetSymbols"
                , .combine = bind_rows, .errorhandling = 'remove') %dopar%
        {
          # ticker = tickers[1]
          d1 <- get.clean.data(ticker, src = "yahoo", first.date = today, last.date  = today + 1)
          rm(ticker)
          return(d1)
        }
  
  stopCluster(cl)
  rm(cl)

  if(is.null(d1) || nrow(d1) == 0)
  {
    assign("IB.02.actions", data.frame(), envir = .GlobalEnv)
    rm(d1, today, tickers)
    return(cat("\nNo Data Found. Check Connection with Yahoo!!!"))
  }

  d1 <- d1 %>%
        rename(ds = ref.date, open = price.open, high = price.high, 
               low = price.low, close = price.close) %>%
        select(ticker, ds, open, high, low, close) %>%
        mutate(NY.Time = as.numeric(format(Sys.time(), tz = "US/Eastern", format = "%H.%M")))
  
  # Report Data Pull Errors
  failed <- setdiff(tickers, unique(d1$ticker))
  if(length(failed) > 0)
  {cat("\nData Pull Failed for", length(failed), "targets:", paste(failed, collapse = ", "), "\n" )}
  rm(failed, tickers)
  
  # Summary of Current Investment
  if(nrow(IB.00.positions) > 0)
  {
    x <- IB.00.positions %>% select(symbol, position, averageCost) %>%
          rename(ticker = symbol, cost = averageCost) %>%
          left_join(d1 %>% select(ticker, close), by = "ticker") %>%
          mutate(Investment = abs(position*cost), Gain = position*(close - cost),
                 Type = ifelse(position > 0, "LONG", "SHORT")) %>%
          arrange(Type, ticker)
    
    x <- x %>%
          bind_rows(data.frame(ticker = "Overall >>", Type = "",
                               Investment = sum(x$Investment, na.rm = TRUE),
                               Gain = sum(x$Gain, na.rm = TRUE),
                               stringsAsFactors = FALSE)) %>%
          mutate(Type = format(Type, justify = 'left'),
                 Symbol = format(ticker, justify = 'left'),
                 ROI = 100*Gain/Investment,
                 ROI = ifelse(ROI <= 0, 
                              paste0(formatC(ROI, format="f", digits=2), "% *"), 
                              paste0(formatC(ROI, format="f", digits=2), "%  ")),
                 Investment = paste(formatC(Investment, format="f", big.mark=",", digits=0), "US")
                 ) %>%
          select(Type, Symbol, Investment, ROI) 
    
    y <- x %>% select(Symbol, ROI) %>% rename("Last ROI  " = ROI)
    
    if(exists("IB.00.Last.Run", envir = .GlobalEnv))
    {
      x <- left_join(x, IB.00.Last.Run, by = "Symbol")
      colnames(x) <- c("", "", "Investment", "ROI  ", "Last ROI  ")
    } else
    {
      colnames(x) <- c("", "", "Investment", "ROI  ")
    }

    assign("IB.00.Last.Run", y, envir = .GlobalEnv)
    print(x, row.names = FALSE)
    cat("------------------------------------------------\n")
    rm(x, y)
  }
  
  # Eligible Orders: Based on current price only
  o1 <- inner_join(IB.01.targets, d1, by = c("ticker", "ds")) %>%
        mutate(IB.orderType = case_when(
                      # BUY
                      Type == "LONG" & units > 0 & close <= buy.price & close > stop.price ~ "LMT",
                      # SELL
                      Type == "LONG" & units < 0 & close >= sell.price ~ "LMT",
                      # STOP SELL
                      Type == "LONG" & units < 0 & close <= stop.price & NY.Time >= 10 ~ "LMT",
                      # MISSED SELL + EOD
                      Type == "LONG" & units < 0 & action == "MISSED SELL" & !is.na(last.sell) ~ "LMT",
                      # EOD SELL
                      Type == "LONG" & units < 0 & (!is.na(last.sell) & last.sell >= 0) &
                        between(NY.Time, IB.Parms[["Last.Sell.At"]], 16) ~ "MKT",
                      
                      # BUY
                      Type == "SHRT" & units < 0 & close >= buy.price & close < stop.price ~ "LMT",
                      # SELL
                      Type == "SHRT" & units > 0 & close <= sell.price ~ "LMT",
                      # STOP SELL
                      Type == "SHRT" & units > 0 & close >= stop.price & NY.Time >= 10 ~ "LMT",
                      # MISSED SELL + EOD
                      Type == "SHRT" & units > 0 & action == "MISSED SELL" & !is.na(last.sell) ~ "LMT",
                      # EOD SELL
                      Type == "SHRT" & units > 0 & (!is.na(last.sell) & last.sell >= 0) &
                        between(NY.Time, IB.Parms[["Last.Sell.At"]], 16) ~ "MKT")
        ) %>%
        filter(!is.na(IB.orderType)) %>%
        # Create IB Specifications
        mutate(t.price = case_when(IB.orderType == "LMT" ~ round(close, 2), TRUE ~ 0)
               , volume = abs(units)
               , IB.action = case_when(Type == "LONG" & grepl("BUY", action) ~ "BUY",
                                       Type == "LONG" & grepl("SELL", action) ~ "SELL",
                                       Type == "SHRT" & grepl("BUY", action) ~ "SELL",
                                       Type == "SHRT" & grepl("SELL", action) ~ "BUY"
                                       )
               ) %>%
              select(ticker, ds, Type, action, IB.action, IB.orderType, volume, t.price, 
                     algo.ID, model.ID, Exchange, ticker.ID, NY.Time)
  
  # Order within fund limits: Check if we need the cummin function
  # Buy only one 
  o2 <- o1 %>%
        mutate(action2 = ifelse(grepl("BUY", action), "BUY", "SELL")) %>%
        group_by(action2) %>% arrange(action2, Type, desc(t.price)) %>%
        mutate(Cost = if_else(action2 == "BUY", -volume*t.price, 0)
               , Available.Funds = Available.Funds + cumsum(Cost)) %>%
        filter(Available.Funds > 0 | action2 != "BUY") %>% 
        ungroup() %>% select(-c(Cost, Available.Funds, action2))
  
  assign("IB.02.actions", o2, envir = .GlobalEnv)
  rm(d1, o1, o2, today)
}

IB.Action.Plots <- function()
{
  if(nrow(IB.02.actions) == 0) return(cat("\n"))

  for(i in 1:nrow(IB.02.actions))
  {
    # i = 1
    x <- IB.02.actions[i, ]
    df <- readRDS(paste0("./Data/Simulation/", x$ticker, ".rds")) %>%
          semi_join(x, by = c("algo.ID", "model.ID")) %>%
          mutate(A.Score = ifelse(!is.na(buy.price), Score, NA)
                 , P.Score = ifelse(is.na(buy.price), Score, NA)
                 , T.Score = ifelse(!is.na(A.Score*lead(P.Score)) |
                                    !is.na(lag(A.Score)*P.Score) |
                                    !is.na(A.Score*lag(P.Score)) |
                                     is.na(lead(A.Score)*P.Score)
                                    , Score, NA)
          ) %>%
          filter(ds >= IB.Parms[["Start.Plot.From"]])

    x <- IB.02.actions[i,] %>%
        mutate(buy.price = last(df$buy.price)
               , sell.price = last(df$sell.price)
               , stop.price = last(df$stop.price)
               , model.ID = gsub("[|]", " ", model.ID)
               , model.ID = gsub(" +", " ", model.ID)
               ) %>%
        select(-c(ticker.ID)) %>%
        bind_cols(df %>% filter(!is.na(ROI)) %>%
                    summarise(N = n()
                              , ROI = case_when(N > 0 ~ paste0(round(100*mean(ROI), 1), "%"))
                              , invest.period = case_when(N > 0 ~ round(mean(invest.period), 2))))

    if(max(df$close, na.rm = TRUE) > 20)
    {
      min.y <- 10*floor(min(df$close/10, na.rm = TRUE)/1.25) # 1.25 to accomodate buy/sell prices
      max.y <- 10*ceiling(max(df$close/10, na.rm = TRUE)*1.25)
    } else
    {
      min.y <- floor(min(df$close, na.rm = TRUE)/1.25)
      max.y <- ceiling(max(df$close, na.rm = TRUE)*1.25)
    }

    t1 <- paste(x$ticker, x$algo.ID, x$model.ID, x$Type, sep = " | ")
    t2 <- ifelse(is.na(x$t.price) | x$t.price == 0,
                 paste0(x$action, " ", x$volume, " Units @ Market Price"),
                 paste0(x$action, " ", x$volume, " Units @ $", round(x$t.price, 2)))

    t3 <- ifelse(x$IB.action == "BUY",
                 paste0("Buy $", x$buy.price),
                 paste0("Sell $", x$sell.price, " | Stop Loss $", x$stop.price))

    t4 <- ifelse(x$N <= 0, "No History", 
                 paste0("History: ", x$N, " times for ", x$invest.period, " Days | ROI ", x$ROI))

    p <- plot_ly(data = df, x = ~ds) %>%
          add_trace(y = ~close, name = "Close",
                    type = 'scatter', mode = 'lines',
                    line = list(color = '#a6a6a6', width = 0.5),
                    fill = 'tozeroy', fillcolor='#f9f7f7' ) %>%
          add_trace(y = ~P.Score, name = 'Passive Score', yaxis = "y2",
                    type = 'scatter', mode = 'lines',
                    line = list(color = '#3f6ea6', dash='dot', width = 1.5) ) %>%
          add_trace(y = ~A.Score, name = 'Active Score', yaxis = "y2",
                    type = 'scatter', mode = 'lines',
                    line = list(color = '#3f6ea6', width = 1.5) ) %>%
          add_trace(y = ~T.Score, name = 'Transition Score', yaxis = "y2",
                    type = 'scatter', mode = 'lines+markers',
                    line = list(color = '#cc0000', dash='dot', width = 1) ) %>%
          add_trace(y = ~buy.price, name = 'Buy Price', type = 'scatter', mode = 'lines',
                    line = list(color = '#444444', width = 1) ) %>%
          add_trace(y = ~sell.price, name = 'Sell Price', type = 'scatter', mode = 'lines',
                    line = list(color = '#444444', width = 1) ) %>%
    
          add_annotations(text = t1, xref = 'paper', yref = 'paper', x = 0, y = 1.09,
                          font = list(size = 14, color = '#004080'), showarrow = F) %>%
          add_annotations(text = t2, xref = 'paper', yref = 'paper', x = 0, y = 1.05,
                          font = list(size = 12, color = '#004080'), showarrow = F) %>%
          add_annotations(text = paste0("New York Time: ", x$NY.Time),
                          xref = 'paper', yref = 'paper', x = 1, y = 1.09,
                          font = list(size = 12), showarrow = F) %>%
          add_annotations(text = t3, xref = 'paper', yref = 'paper', x = 1, y = 1.06,
                          font = list(size = 12, color = '#004080'), showarrow = F) %>%
          add_annotations(text = t4, xref = 'paper', yref = 'paper', x = 1, y = 1.03,
                          font = list(size = 12, color = '#004080'), showarrow = F) %>%
    
          layout( font = list(size = 12), showlegend = FALSE, hovermode = 'compare',
                  margin = list(l = 70, r = 70, b = 50, t = 50, pad = 10),
                  xaxis = list(title = NA, showgrid = FALSE),
                  yaxis = list(title = "Close ($)", color = '#a6a6a6',
                               range = c(min.y, max.y), showgrid = FALSE),
                  yaxis2 = list(title = "Score", color = '#3f6ea6', tickformat = ".1%",
                                overlaying = "y", side = "right", showgrid = TRUE) )

    fname <- paste0("./Data/Plots/", x$ticker, " ", x$algo.ID, " ", x$model.ID, ".html")

    tryCatch( {
                saveWidget(as_widget(p)
                           , title = paste0(x$ticker, ": ", x$Type, " ", x$action, " "
                                            , x$volume, " Units @ $", x$t.price)
                           , libdir = "libdir"
                           , file = file.path(normalizePath(dirname(fname)),basename(fname)))
                browseURL(fname)
              }
              , error = function(e) {cat("\nError in Saving Plot...\n")}
              , warning = function(w) {cat("\nWarning in Saving Plot...\n")}
            )

    rm(max.y, min.y, t1, t2, t3, t4, fname, p, i, df, x)

  }
}