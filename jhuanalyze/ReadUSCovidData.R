library(stringr)
library(tidyverse)
# Note: xts library may not be loadable when using here library
#library(xts)
library(zoo)
library(purrr)
library(here)
#library(plotrix)

# Assumes you have downloaded a version of the Johns Hopkins data in the 
# appropriate directory - see the README
dfCovidUSDeaths <- read.csv(here("data/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"))
dfCovidUSCases <- read.csv(here("data/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"))
dfWorldCovidDeaths <- read.csv(here("data/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"))

# uh, that's one way to do it
Avg7Day <- mapply(mean, dfCovidUSDeaths$X5.19.20,dfCovidUSDeaths$X5.18.20, dfCovidUSDeaths$X5.17.20, dfCovidUSDeaths$X5.16.20, dfCovidUSDeaths$X5.15.20, dfCovidUSDeaths$X5.14.20, dfCovidUSDeaths$X5.13.20)
dfCovidAverages <- cbind(Avg7Day, dfCovidUSDeaths)
zz <- dfCovidAverages[order(-dfCovidAverages$Avg7Day),]
qq <- cbind(zz$Avg7Day, zz$Combined_Key)
qq <- zz[,c(1,12)]

dfTots <- dfCovidUSDeaths %>% .[,c(11,13:ncol(.))]
dfStateTots <- dfCovidUSDeaths %>% .[,c(7,13:ncol(.))]
dfCaseTots <- dfCovidUSCases %>% .[,c(11,13:ncol(.))]
dfStateCaseTots <- dfCovidUSCases %>% .[,c(7,13:ncol(.))]
dates <- seq(as.Date("2020-01-20"), length = ncol(dfTots), by = "days")
# Note: xts library may not be loadable when using here library
# ts1 <- xts(x=as.numeric(dfTots[1234,]), order.by=dates)
# plot.ts(ts1)


# This is maybe a better way
# https://stackoverflow.com/questions/47968665/how-to-convert-data-frame-rows-iteratively-into-time-series-object-and-apply-aut
tib_deaths <- dfTots %>% pivot_longer(-Combined_Key, names_to = "day", values_to = "cumulative_deaths") %>%
  mutate(Date=as.Date(str_replace(day, "X", ""), format="%m.%d.%y")) %>%
  select(County=Combined_Key, Date, cumulative_deaths) %>%
  group_by(County) %>% nest() %>%
  mutate(zoo_obj=map(data, ~with(.x, zoo(cumulative_deaths, Date))),
         new_ones=map(data, ~with(.x, diff(zoo(cumulative_deaths, Date)))),
         rolling_avg_7day=map(data, ~with(.x, rollmeanr(diff(zoo(cumulative_deaths, Date)), k=7)))
         )

# Here it is in a function
calc_7day <- function (dfTots) {
  tib7day <- dfTots %>% pivot_longer(-Combined_Key, names_to = "day",
                                     values_to = "cumulative_deaths") %>%
    mutate(Date=as.Date(str_replace(day, "X", ""), format="%m.%d.%y")) %>%
    select(County=Combined_Key, Date, cumulative_deaths) %>%
    group_by(County) %>% nest() %>%
    mutate(zoo_obj=map(data, ~with(.x, zoo(cumulative_deaths, Date))),
           new_ones=map(data, ~with(.x, diff(zoo(cumulative_deaths, Date)))),
           rolling_avg_7day=map(data, ~with(.x, rollmeanr(diff(zoo(cumulative_deaths, Date)),
                                                          k=7))),
           trend7=map(data, ~with(.x, diff(rollmeanr(diff(zoo(cumulative_deaths, Date)),
                                                 k=7), lag=7, arithmetic=FALSE)
                                                    ))
           )
  return(tib7day)
  }


# Plot graph based on county name
plot7death <- function(tib7, county) {
  return(autoplot(filter(tib7, County==county)$rolling_avg_7day[[1]],
           color=county) +
           labs(x="Date", y="Deaths (7-day Moving Average)", title="COVID-19 Deaths"))
}


# TODO: Order tib7day by highest current 7-day value, lowest, biggest % inc/dec
# TODO: Test with recovered dataset

# Put all of the cumulative stats in the same zoo
get_merged_zoo <- function (tib, colname) {
  MergedZ <- tib[1,][[colname]][[1]]
  namevec <- c(as.vector(tib$County[[1]]))
  for(i in 2:nrow(tib)) {
    namevec <- append(namevec, as.vector(tib$County[[i]]))
    MergedZ <- merge(MergedZ, tib[i,][[colname]][[1]])
  }
  names(MergedZ) <- namevec
  return(MergedZ)
}

# Put all of the 7-day plots in the same zoo
get_merged_7day <- function (tib7) {
  return(get_merged_zoo(tib7, "rolling_avg_7day"))
}

# Put all of the cumulative stats in the same zoo
get_merged_cumulative <- function (tib) {
  return(get_merged_zoo(tib, "zoo_obj"))
}

# Put all of the 7-day plots in the same zoo
get_merged_relations <- function (tib7) {
  MergedZ <- tib7[1,]$rolling_avg_7day[[1]]
  namevec <- c(as.vector(tib7$County[[1]]))
  for(i in 2:nrow(tib7)) {
    namevec <- append(namevec, as.vector(tib7$County[[i]]))
    MergedZ <- merge(MergedZ, tib7[i,]$rolling_avg_7day[[1]])
  }
  names(MergedZ) <- namevec
  return(MergedZ)
}

# Log of cases and deaths on one plot
plotCasesAndDeaths <- function(tib7Deaths, tib7Cases, county) {
  mergedCountyStats <- 
    merge(filter(tib7Deaths, County==county)$rolling_avg_7day[[1]],
          filter(tib7Cases, County==county)$rolling_avg_7day[[1]])
  names(mergedCountyStats) <- c("Deaths", "Cases")
  return(autoplot(log10(mergedCountyStats), facets=NULL))
}

plotZCasesAndDeaths <- function(ZDeaths, ZCases, tit) {
  mergedCountyStats <- 
    merge(ZDeaths, ZCases)
  names(mergedCountyStats) <- c(names(ZDeaths), names(ZCases))
  return(autoplot(log10(mergedCountyStats), facets=NULL) +
           labs(x="Days since 1/20/20", y="Log(7-day average)", title=tit))
}


getStateCasesAndDeaths <- function(zDeaths, zCases, state, with_trends=FALSE) {
  zStateDeaths <- zoo(rowSums(zDeaths[,grep(stringi::stri_c(state, ", US"), names(zDeaths), invert=FALSE, value=FALSE)]))
  zStateCases <- zoo(rowSums(zCases[,grep(stringi::stri_c(state, ", US"), names(zCases), invert=FALSE, value=FALSE)]))
  zCombo <- merge(zStateDeaths, zStateCases)
  namesToAdd <- c(stringi::stri_c(state, " Deaths"), stringi::stri_c(state, "Cases"))
  if (with_trends) {
    zTrend7Deaths <- diff(zStateDeaths, lag=7, arithmetic=FALSE)
    zTrend7Cases <- diff(zStateCases, lag=7, arithmetic=FALSE)
    zCombo <- merge(zCombo, zTrend7Deaths, zTrend7Cases)
    namesToAdd <- append(namesToAdd, c(stringi::stri_c(state, " Deaths Trend"),
                                       stringi::stri_c(state, " Cases Trend")))
  }
  names(zCombo) <- namesToAdd
  #  plotZCasesAndDeaths(zStateDeaths, zStateCases, state)
  return(zCombo)
}


# Get the cumulative cases or deaths for a state
getStateCumulative <- function(zItems, state) {
  zState <- zoo(rowSums(zItems[,grep(stringi::stri_c(state, ", US"), names(zItems), invert=FALSE, value=FALSE)]))
  return(zState)
}

# Total deaths per million
# Total deaths per million / weighted population density
