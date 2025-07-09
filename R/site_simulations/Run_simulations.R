library(medfate) # Requires v.4.8.3
library(medfateutils)

## simulation function
sim_plot<-function(pl_code, transpirationMode, rhizosphereOverlap, verbose = FALSE) {
  cli::cli_li(paste0("Simulating ", pl_code, " ", transpirationMode, " ", rhizosphereOverlap))
  pl_data <- readRDS(paste0("data-raw/site_input/",pl_code,"_data.rds"))
  
  s <- soil(pl_data$soilData)
  SpParams <- modifySpParams(pl_data$sp_params, pl_data$customParams)
  forest <- emptyforest()
  forest$treeData <- pl_data$treeData[pl_data$treeData$LAI>0,]
  if("shrubData" %in% names(pl_data)) forest$shrubData <- pl_data$shrubData
  pl_meteo <- pl_data$meteoData
  
  control <- defaultControl(transpirationMode, soilDomains = "single", rhizosphereOverlap = rhizosphereOverlap)
  if(transpirationMode=="Sureau") control$segmentedXylemVulnerability <- FALSE
  control$leafCavitationRecovery <- "rate"
  control$stemCavitationRecovery <- "rate"
  control$fracRootResistance <- 0.4
  control$verbose <- verbose

  x <- spwbInput(forest, s, SpParams, control)

  S <- spwb(x, pl_meteo, 
            elevation = pl_data$terrainData$elevation, 
            slope = pl_data$terrainData$slope, 
            aspect = pl_data$terrainData$aspect, 
            latitude = pl_data$terrainData$latitude)
  saveRDS(S, paste0("data/site_output/",pl_code,"_", tolower(transpirationMode),"_", rhizosphereOverlap,".rds"))
}

# process all sites and combinations
for(site in c("fb", "pr", "pu", "cb", "es", "ro")) {
  for(mode in c("Granier", "Sperry", "Sureau")) {
    for(rhizosphereOverlap in c("total", "partial", "none")) {
      sim_plot(site, mode, rhizosphereOverlap)
    }
  }
}
