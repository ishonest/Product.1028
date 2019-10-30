# -------------------------------------------------------------------------
# Associated Functions
# -------------------------------------------------------------------------

TradingDates <- function(year=format(Sys.Date(), "%Y"), FUN = timeDate::holidayNYSE)
{
  ## End of code that should be removed when this is added to a package
  year <- as.numeric(year)
  fun <- match.fun(FUN)
  do.call('c', lapply(year, function(y) {
    holidays <- as.Date(fun(year=y))
    all.days <- seq.Date(as.Date(paste(y, '01-01', sep='-')),
                         as.Date(paste(y, '12-31', sep='-')), by='days')
    nohol <- all.days[!(all.days %in% holidays)]
    nohol[!(format(nohol, '%w') %in% c("6", "0"))] #neither holiday nor weekend
  }))
}

PrevTradingDate <- function(Date=Sys.Date(), n=1)
{
  stopifnot(require(xts)) #remove this line when this is added to a package that Imports xts (needed for first/last)
  D <- as.Date(Date)
  y <- as.numeric(format(D, "%Y"))
  trading.days <- TradingDates(y)
  out <- trading.days[trading.days < Date]
  if (length(out) >= n) 
  {
    first(tail(sort(out), n))
  } else {
    prev.year.td <- TradingDates(y - 1)
    max.n <- length(out) + length(prev.year.td)
    if (n > max.n) stop("'n' is too large. Try something less than 252.")
    new.n <- n - length(out) # we need this many trading days from previous year
    # if it's the 1st trading day of the year, return the last trading date of
    # previous year
    first(last(TradingDates(y - 1), new.n))
  }
}

NextTradingDate <- function(Date=Sys.Date(), n=1)
{
  stopifnot(require(xts)) #remove this line when this is added to a package that Imports xts (needed for first/last)
  D <- as.Date(Date)
  y <- as.numeric(format(D, "%Y"))
  trading.days <- TradingDates(y)
  out <- trading.days[trading.days > Date]
  if (length(out) >= n) {
    last(first(out, n))
  } else {
    next.year.td <- TradingDates(y + 1)
    max.n <- length(out) + length(next.year.td)
    new.n <- n - length(out) # how many trading days we need from next year
    if (n > max.n) stop("'n' is too large. Try something less than 252.")
    # if it's the last trading day of the year, return the first trading date of
    # next year
    last(first(TradingDates(y + 1), new.n))
  }
}
