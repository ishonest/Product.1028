# -------------------------------------------------------------------------
# Batch Clean Data Functions
# -------------------------------------------------------------------------
Get.Data.Clean <- function(d1, min.trade = 100000, min.tdays = 250, bad.jumps = 0.1, in.portfolio = FALSE)
{
  library(dplyr)
  library(zoo)
  library(timeDate)

  # NY.Time <- as.numeric(strftime(format(Sys.time(), tz = "US/Eastern"), format = "%H.%M"))
  # if(NY.Time >= 9.3) {d1 <- d1 %>% filter(ref.date < Sys.Date())}
  # rm(NY.Time)

  if(in.portfolio == TRUE)
  {
    min.trade <- 0
    min.tdays <- 0
    bad.jumps <- 0.001
  }
  rm(in.portfolio)

  source("./Functions/F.Trading.Days.R")

  d1 <- d1 %>% distinct()
  if(nrow(d1) < min.tdays)
  {
    cat(ticker, ": Data Pull from Yahoo Failed\n")
    rm(min.trade, min.tdays, bad.jumps)
    rm(NextTradingDate, PrevTradingDate, TradingDates, envir = .GlobalEnv)
    return(data.frame())
  }

  # ~100 Days of Prod + 150 Days of Dev: 250 Days of Good Data
  # Definition of Good Data: No Extreme Jumps, Non-NA/Zero Volume, $250k+ Daily Trading
  # Consistent for 250 Days

  # Volume Check
  d2x <- d1 %>% group_by(ticker) %>%
    mutate(volume = na_if(volume, 0)) %>%
    summarise(N = n(),
              last.na = suppressWarnings(max(which(is.na(volume)))),
              last.na = ifelse(is.infinite(last.na), 0, last.na),
              good.days = N - last.na
    ) %>%
    # filter(good.days >= min.tdays) %>%
    select(-c(N, good.days)) %>%
    inner_join(d1, by = "ticker") %>%
    group_by(ticker) %>%
    arrange(ticker, ref.date) %>%
    filter(row_number() > last.na) %>%
    rename(ds = ref.date, adjusted = price.adjusted,
           open = price.open, high = price.high,  low = price.low, close = price.close) %>%
    select(c(ds, ticker, volume, open, high, low, close, adjusted)) %>%
    # mutate_all(zoo::na.locf) # Replace NA with preceeding values
    mutate_at(vars(-group_cols()), zoo::na.locf) # Effective from dplyr 0.8.3

  if(nrow(d2x) < min.tdays)
  {
    cat(d1$ticker[1], ": Filtered @ Cleaning -", nrow(d2x), "days with Valid Volume\n")
    rm(d2x, min.trade, min.tdays, bad.jumps)
    rm(NextTradingDate, PrevTradingDate, TradingDates, envir = .GlobalEnv)
    return(data.frame())
  }

  # Remove data until with low value trades & sudden jumps
  d2y <- d2x %>%
    group_by(ticker) %>%
    arrange(ticker, ds) %>%
    mutate(value = adjusted*volume,
           delta = lead(adjusted)/adjusted,
           bad = case_when(delta < bad.jumps ~ TRUE,
                           delta > (1/bad.jumps) ~ TRUE,
                           value < min.trade ~ TRUE,
                           TRUE ~ FALSE)) %>%
    summarise(N = n(),
              last.bad = suppressWarnings(max(which(bad == TRUE))),
              last.bad = ifelse(is.infinite(last.bad), 0, last.bad),
              good.days = N - last.bad ) %>%
    select(-c(N, good.days))

  d1 <- inner_join(d2x, d2y, by = "ticker") %>%
    group_by(ticker) %>%
    arrange(ticker, ds) %>%
    filter(row_number() > last.bad) %>%
    select(-last.bad) %>%
    ungroup()

  if(nrow(d1) < min.tdays)
  {
    cat("Filtered @ Cleaning [[", ticker, "]]:",
        paste0(nrow(d1), " days with $", round(min.trade/1000, 2), "k in trade and"),
        paste0("price swings within [", bad.jumps*100, "%, ", 100/bad.jumps, "%]\n"))

    rm(d2x, d2y, min.trade, min.tdays, bad.jumps)
    rm(NextTradingDate, PrevTradingDate, TradingDates, envir = .GlobalEnv)
    return(data.frame())
  }

  # -------------------------------------------------------------------------
  d1 <- bind_rows(d1,  data.frame(ds = NextTradingDate(Date = max(d1$ds)),
                                  ticker = unique(d1$ticker), stringsAsFactors = FALSE)) %>%
    group_by(ticker) %>% arrange(ticker, ds) %>%
    mutate(ds.N = row_number())

  rm(d2x, d2y, min.trade, min.tdays, bad.jumps)
  rm(NextTradingDate, PrevTradingDate, TradingDates, envir = .GlobalEnv)
  gc()

  return(d1)

}

# -------------------------------------------------------------------------
# Incremental Data Pull
# -------------------------------------------------------------------------
Get.Incremental.Data <- function(stocks, first.date)
{
  all.d1 <- data.frame()

  while(length(stocks) > 0)
  {
    d1 <- foreach(ticker = stocks, .combine = bind_rows
                  , .errorhandling = 'remove', .packages = c("BatchGetSymbols", "timeDate") ) %dopar%
          {
            source("./Functions/F.Trading.Days.R")
            # ticker = stocks[1]
            d1 <- BatchGetSymbols::get.clean.data(ticker, src = "yahoo",
                                                  first.date, last.date = Sys.Date() ) %>%
                  rename(ds = ref.date, open = price.open, high = price.high, low = price.low,
                         close = price.close, adjusted = price.adjusted) %>%
                  arrange(ds) %>%
                  select(ds, ticker, volume, open, high, low, close, adjusted)

            if(max(d1$ds) != PrevTradingDate() | any(is.na(d1)))
            {
              rm(d1, ticker, NextTradingDate, PrevTradingDate, TradingDates)
              return(NULL)
            }

            d1 <- d1 %>% bind_rows(data.frame(ds = NextTradingDate(Date = max(d1$ds)),
                                              ticker, stringsAsFactors = FALSE))

            rm(ticker, NextTradingDate, PrevTradingDate, TradingDates)
            return(d1)
          }

    if(is.null(d1) || nrow(d1) == 0)
    {
      cat("\nData Pull/Clean failed for", length(stocks), "stocks\n", stocks, "...\n")
      rm(d1, stocks)
      break
    } else
    {
      all.d1 <- hist.d1 %>% na.omit() %>% select(-ds.N) %>% filter(ticker %in% unique(d1$ticker)) %>%
                bind_rows(d1) %>%
                group_by(ticker) %>% arrange(ticker, ds) %>% mutate(ds.N = row_number()) %>%
                bind_rows(all.d1) %>% distinct()
      stocks <- setdiff(stocks,  unique(d1$ticker))
      rm(d1)

      if(length(stocks) == 0)
      {
        cat("\nData Pull 100% Succcessful.\n\n")
        rm(stocks, first.date)
        break
      }

      cat("\nTrying Again for:", length(stocks), "stocks:", stocks, "...\n\n")
    }
  }

  all.d1 <- all.d1 %>% ungroup()
  return(all.d1)

}

# -------------------------------------------------------------------------
