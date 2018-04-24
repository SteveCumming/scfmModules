defineModule(sim, list(
  name = "vegMapToStrataMap",
  description = "aassign yield tables (strata) to vegetation classes", #"insert module description here",
  keywords = NA, # c("insert key words here"),
  authors = person("First", "Last", email = "first.last@example.com", role = c("aut", "cre")),
  childModules = character(0),
  version = list(SpaDES.core = "0.1.1.9007", vegMapToStrataMap = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "vegMapToStrataMap.Rmd"),
  reqdPkgs = list(),
  parameters = rbind(
    defineParameter(".plotInitialTime", NA, NA, NA, desc = "duh"),
    defineParameter(".plotInterval", NA, NA, NA, desc = "duh"),
    defineParameter("returnInterval", NA, NA, NA, desc = "duh"),
    defineParameter("startTime", 0, NA, NA, desc = "duh")
    )
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = "vegMap", objectClass = "RasterLayer", desc = "vegetation class map")
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = "strataMap", objectClass = "RasterLayer", desc = "yield curve indexes")
  )
)

## event types
#   - type `init` is required for initialiazation

doEvent.vegMapToStrataMap = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      sim <- Init(sim)
      # schedule future event(s)
      sim <- scheduleEvent(sim, P(sim)$startTime, "vegMapToStrataMap", "stratify")
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "vegMapToStrataMap", "plot")
    },
    stratify = {
      sim <- Stratify(sim)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$returnInterval, "vegMapToStrataMap", "stratify")
    },
    plot = {
       Plot(sim$strataMap)
       scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "vegMapToStrataMap", "plot")
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}


Stratify <- function(sim){
  
  return(invisible(sim))
}

Init <- function(sim) {
 
  sim$strataMap <- raster::raster(sim$vegMap) 
  sim$strataMap[] <- sim$strataMap[] * 0
  
  return(invisible(sim))
}

