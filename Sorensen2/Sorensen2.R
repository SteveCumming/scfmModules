defineModule(sim, list(
  name = "Sorensen2",
  description ="Calculates changes in boreal caribou herd size based on Sorensen model (2008)",
  keywords = c("Rangifer tarandus caribou","population size","habitat","disturbances"),
  authors = person("Antoine", "Adde", email = "antoine.adde.1@ulaval.ca", role = c("aut", "cre")),
  childModules = character(0),
  version = list(SpaDES.core = "0.1.1", Sorensen2 = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "Sorensen2.Rmd"),
  reqdPkgs = list(),
  parameters = rbind(
    defineParameter("N0", "numeric", NA_real_, 0, 5000, "Initial herd size"),
    defineParameter(".statsInitialTime", "numeric", 0, NA_real_, NA_real_, "This describes the simulation time at which the first stat event should occur"),
    defineParameter(".statsInterval", "numeric", 1, NA_real_, NA_real_, "This describes the simulation time interval between stat events")
  ),
  inputObjects = bind_rows(
    expectsInput(objectName="disturbanceMap", objectClass = "RasterLayer", desc="state of burned or harvestd cells")
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = "SorensenStats", objectClass ="data.frame", desc = "lambda and herd size table")
  )
))


##################################
doEvent.Sorensen2 = function(sim, eventTime, eventType, debug = FALSE) {
  switch(
    eventType,
    init = {
      sim <- statsInit(sim)
      sim <- scheduleEvent(sim, P(sim)$.statsInitialTime, "Sorensen2", "stats")  #get the initial values
    },
    stats = {
      sim <- Stats(sim)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$.statsInterval, "Sorensen2", "stats")
      
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}
###################
statsInit <- function(sim){
  Lambda1<-NA
  Nt1=P(sim)$N0
  sim$SorensenStats <-data.frame(Lambda1,Nt1)
  names(sim$SorensenStats)<-c("Lambda","Nt")
  return(invisible(sim))
}

Stats <- function(sim) {
  burn <- which(mySim$disturbanceMap[] == 1)
  cut <- which(mySim$disturbanceMap[] == 2)
  adjcut <- which(mySim$disturbanceMap[] == 3)

  pBurn <- (length(burn)/sum(table(mySim$disturbanceMap[])))*100
  pCut <- (length(cut)/sum(table(mySim$disturbanceMap[])))*100
  pAdjcut <- (length(adjcut)/sum(table(mySim$disturbanceMap[])))*100

  pIND=pCut+pAdjcut

  lambda<-1.19 - (0.0032* pIND)- (0.0030 * pBurn)
  Nt<-  lambda * tail(sim$SorensenStats$Nt,n=1)
  tmpVec <- c(lambda,Nt)
  sim$SorensenStats[nrow(sim$SorensenStats) + 1, ] <- tmpVec
  return(invisible(sim))
}
