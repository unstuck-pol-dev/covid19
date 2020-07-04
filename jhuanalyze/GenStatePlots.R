#Driver file to create current log-scale case and death plots for all states
library(here)
library(svglite)
source("code/covid19/jhuanalyze/ReadUSCovidData.R")
source("code/covid19/jhuanalyze/StateVectors.R")

autoplotToFile <- function(fileName, plotObj) {
  svglite(fileName)
  print(plotObj)
  dev.off()
}

generate_state_plots <- function(state_abbrs, state_names, zDeaths, zCases, year, month, day) {
  numPrinted <- 0
  for (stateNum in 1:length(state_abbrs)) {
    zState <- getStateCasesAndDeaths(zDeaths, zCases, state_names[stateNum], with_trends = FALSE)
    dirName <- stringi::stri_c("pages/plots/", year, "/", formatC(month, width=2, flag="0"), 
                               "/", formatC(day, width=2, flag="0"), "/States/",
                               state_abbrs[stateNum])
    dir.create(dirName, recursive = TRUE)

    stateNDate <- stringi::stri_c(state_abbrs[stateNum],
                                  "_", year, "-", formatC(month, width=2, flag="0"), "-",
                                  formatC(day, width=2, flag="0"), ".svg")
    fileName <- stringi::stri_c(dirName, "/", "CDLog_", stateNDate)
    my.plot <- autoplot(log10(zState), facets=NULL) +
      labs(x="Days since 1/20/20", y="Log(7-day average)", title=state_names[stateNum])
    autoplotToFile(fileName, my.plot)
    
    fileName <- stringi::stri_c(dirName, "/", "CDVal_", stateNDate)
    my.plot <- autoplot(zState, facets=NULL) +
      labs(x="Days since 1/20/20", y="7-day average", title=state_names[stateNum])
    autoplotToFile(fileName, my.plot)
  }
}


