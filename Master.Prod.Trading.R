# WARNING: Start TWS Before Proceeding

# -------------------------------------------------------------------------
IB.Parms <- readRDS("./Data/Summary/System.Parameters.rds")
source("./Functions/T01.Start.R")

while(lubridate::hour(format(Sys.time(), tz = "US/Eastern")) < IB.Parms[["Stop.Trading.At"]])
{
  View(IB.01.targets)
  IB.Account.Status()
  IB.System.Status()
  
  if(IB.Parms[["System.Live"]])
  {
    IB.Actions()
    View(IB.02.actions)    
    IB.Order()
    IB.Action.Plots()
  }
  
  IB.Next.Run(wait.seconds = 120)
  
  if(IB.Parms[["System.Live"]])
  {
    IB.Cancel.Orders()
    Update.Activity()
    View(IB.03.orders)
    View(IB.04.activity)
  }
}

# -------------------------------------------------------------------------
# End of Day Process
# -------------------------------------------------------------------------
source("./Functions/T06.ShutterDown.R")

# -------------------------------------------------------------------------
# WARNING: 
# Emergency Process: All positions will be closed
# -------------------------------------------------------------------------
# # source("./Functions/T07.Emergency.R")
# -------------------------------------------------------------------------

