library(stringr)
library(tidyverse)
library(xts)
library(zoo)
library(purrr)
#library(plotrix)

# Assumes you have downloaded a version of the Johns Hopkins data in the 
# appropriate directory - see the README
dfCovidUSDeaths <- read.csv("../../data/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")
dfCovidUSCases <- read.csv("../../data/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
dfWorldCovidDeaths <- read.csv("../../data/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")

# uh, that's one way to do it
Avg7Day <- mapply(mean, dfCovidUSDeaths$X5.19.20,dfCovidUSDeaths$X5.18.20, dfCovidUSDeaths$X5.17.20, dfCovidUSDeaths$X5.16.20, dfCovidUSDeaths$X5.15.20, dfCovidUSDeaths$X5.14.20, dfCovidUSDeaths$X5.13.20)
dfCovidAverages <- cbind(Avg7Day, dfCovidUSDeaths)
zz <- dfCovidAverages[order(-dfCovidAverages$Avg7Day),]
qq <- cbind(zz$Avg7Day, zz$Combined_Key)
qq <- zz[,c(1,12)]

dfTots <- dfCovidUSDeaths %>% .[,c(11,13:ncol(.))]
dfStateTots <- dfCovidUSDeaths %>% .[,c(7,13:ncol(.))]
dfCaseTots <- dfCovidUSCases %>% .[,c(11,13:ncol(.))]
dates <- seq(as.Date("2020-01-20"), length = ncol(dfTots), by = "days")
ts1 <- xts(x=as.numeric(dfTots[1234,]), order.by=dates)
plot.ts(ts1)


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
  tib7day <- dfTots %>% pivot_longer(-Combined_Key, names_to = "day", values_to = "cumulative_deaths") %>%
    mutate(Date=as.Date(str_replace(day, "X", ""), format="%m.%d.%y")) %>%
    select(County=Combined_Key, Date, cumulative_deaths) %>%
    group_by(County) %>% nest() %>%
    mutate(zoo_obj=map(data, ~with(.x, zoo(cumulative_deaths, Date))),
           new_ones=map(data, ~with(.x, diff(zoo(cumulative_deaths, Date)))),
           rolling_avg_7day=map(data, ~with(.x, rollmeanr(diff(zoo(cumulative_deaths, Date)), k=7)))
    )
  return(tib7day)
  }

tib7 <- calc_7day(dfTots)
tib7Cases <- calc_7day(dfCaseTots)

# Middlesex, MA
autoplot(tib7[1231,]$rolling_avg_7day[[1]])

# Maricopa, AZ
autoplot(tib7[109,]$rolling_avg_7day[[1]])

# New York City
autoplot(tib7["1864",]$rolling_avg_7day[[1]])

# Plot graph based on county name
plot7death <- function(tib7, county) {
  return(autoplot(filter(tib7, County==county)$rolling_avg_7day[[1]],
           color=county) +
           labs(x="Date", y="Deaths (7-day Moving Average)", title="COVID-19 Deaths"))
}


# Middlesex, MA
plot7death(tib7, "Middlesex, Massachusetts, US")
# Maricopa, AZ
plot7death(tib7, "Maricopa, Arizona, US")
# New York City
plot7death(tib7, "New York City, New York, US")

# TODO: Order tib7day by highest current 7-day value, lowest, biggest % inc/dec
# TODO: Test with recovered dataset


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

# Plot some graphs out of the merged zoo
z2 <- get_merged_relations(tib7)
z22 <- get_merged_relations(tib7Cases)

# Deaths
autoplot(z2[,grep("Am", names(z2), value=FALSE)])
autoplot(z2[,grep("Massachusetts", names(z2), value=FALSE)])
autoplot(z2[,grep("Middlesex", names(z2), value=FALSE)])
autoplot(z2[,grep("New York City", names(z2), value=FALSE)])
autoplot(z2[,grep("Middlesex|New York City", names(z2), value=FALSE)])

# Cases
autoplot(z22[,grep("Am", names(z2), value=FALSE)])
autoplot(z22[,grep("Massachusetts", names(z2), value=FALSE)])
autoplot(z22[,grep("Middlesex, Ma", names(z2), value=FALSE)])
autoplot(z22[,grep("New York City", names(z2), value=FALSE)])
autoplot(z22[,grep("Middlesex|New York City", names(z2), value=FALSE)])

# Plot sum of several graphs
# TODO: Fix axis labels
# All of MA
autoplot(zoo(rowSums(z2[,grep("Massachusetts", names(z2), value=FALSE)])))
autoplot(zoo(rowSums(z22[,grep("Massachusetts", names(z22), value=FALSE)])))

# Everything except New York
z3 <- z2[,grep("New York, US", names(z2), invert=TRUE, value=FALSE)]
# autoplot(zoo(rowSums(z3)))

# New York vs. Rest of U.S. on same plot
# z4 <- merge(z3, zoo(rowSums(z2[,grep("New York, US", names(z2), invert=FALSE, value=FALSE)])))
z3 <- zoo(rowSums(z2[,grep("New York, US", names(z2), invert=TRUE, value=FALSE)]))
z4 <- zoo(rowSums(z2[,grep("New York, US", names(z2), invert=FALSE, value=FALSE)]))
z23 <- zoo(rowSums(z22[,grep("New York, US", names(z2), invert=TRUE, value=FALSE)]))
z24 <- zoo(rowSums(z22[,grep("New York, US", names(z2), invert=FALSE, value=FALSE)]))
z5 <- merge(z3, z4, z23, z24)
names(z5) <- c("Rest of U.S. Deaths", "New York Deaths", "Rest of U.S. Cases", "New York Cases")
autoplot(z5, facets=NULL)
autoplot(log10(z5), facets=NULL) +
         labs(x="Days since 1/20/20", y="Log10(7-day average)", title="New York and Rest of U.S. Cases and Deaths")

# Top 50 counties by population
dfDeathsBigCounties = dfCovidUSDeaths[order(-dfCovidUSDeaths$Population),]
dfBigCountyTots <- dfDeathsBigCounties %>% .[,c(11,13:ncol(.))]
tibBigCounty7 <- calc_7day(dfBigCountyTots[1:50,])
zc <- get_merged_relations(tibBigCounty7)
autoplot(zc[,grep("Los", names(zc), invert=FALSE)])

# FL, GA, AL, MS, LA, TX, SC
z9 <- zoo(rowSums(z2[,grep("Florida, US", names(z2), invert=FALSE, value=FALSE)]))
z10 <- zoo(rowSums(z2[,grep("Georgia, US", names(z2), invert=FALSE, value=FALSE)]))
z11 <- zoo(rowSums(z2[,grep("Alabama, US", names(z2), invert=FALSE, value=FALSE)]))
z12 <- zoo(rowSums(z2[,grep("Mississippi, US", names(z2), invert=FALSE, value=FALSE)]))
z13 <- zoo(rowSums(z2[,grep("Louisiana, US", names(z2), invert=FALSE, value=FALSE)]))
z14 <- zoo(rowSums(z2[,grep("Texas, US", names(z2), invert=FALSE, value=FALSE)]))
z17 <- zoo(rowSums(z2[,grep("South Carolina, US", names(z2), invert=FALSE, value=FALSE)]))
z15 <- merge(z9, z10, z11, z12, z13, z14, z17, drop=FALSE)
names(z15) <- c("FL Deaths", "GA Deaths", "AL Deaths", "MS Deaths", "LA Deaths", "TX Deaths", "SC Deaths")
z16 <- merge(z15, zoo(rowSums(z15)))
names(z16) <- c("Total Deaths", names(z15))
autoplot(z16, facets=NULL)
autoplot(log10(z16), facets=NULL)


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

# FL, GA, AL, MS, LA, TX, SC

z29 <- zoo(rowSums(z22[,grep("Florida, US", names(z22), invert=FALSE, value=FALSE)]))
z30 <- zoo(rowSums(z22[,grep("Georgia, US", names(z22), invert=FALSE, value=FALSE)]))
z31 <- zoo(rowSums(z22[,grep("Alabama, US", names(z22), invert=FALSE, value=FALSE)]))
z32 <- zoo(rowSums(z22[,grep("Mississippi, US", names(z22), invert=FALSE, value=FALSE)]))
z33 <- zoo(rowSums(z22[,grep("Louisiana, US", names(z22), invert=FALSE, value=FALSE)]))
z34 <- zoo(rowSums(z22[,grep("Texas, US", names(z22), invert=FALSE, value=FALSE)]))
z37 <- zoo(rowSums(z22[,grep("South Carolina, US", names(z22), invert=FALSE, value=FALSE)]))
z35 <- merge(z29, z30, z31, z32, z33, z34, z37)
names(z35) <- c("FL Cases", "GA Cases", "AL Cases", "MS Cases", "LA Cases", "TX Cases", "SC Cases")
z36 <- merge(z35, zoo(rowSums(z35)))
names(z36) <- c("Total Cases", names(z35))
autoplot(z36, facets=NULL)
autoplot(log10(z36), facets=NULL)
plotZCasesAndDeaths(z10, z30, "Georgia")
plotZCasesAndDeaths(z15, z35, "Selected southern states")
plotZCasesAndDeaths(z16, z36, "Selected southern states")

getStateLogCasesAndDeaths <- function(zDeaths, zCases, state) {
  zStateDeaths <- zoo(rowSums(z2[,grep(stringi::stri_c(state, ", US"), names(z2), invert=FALSE, value=FALSE)]))
  zStateCases <- zoo(rowSums(z22[,grep(stringi::stri_c(state, ", US"), names(z22), invert=FALSE, value=FALSE)]))
  zCombo <- merge(zStateDeaths, zStateCases)
  names(zCombo) <- c(stringi::stri_c(state, " Deaths"), stringi::stri_c(state, "Cases"))
#  plotZCasesAndDeaths(zStateDeaths, zStateCases, state)
  return(zCombo)
}

zVA <- getStateLogCasesAndDeaths(z2, z22, "Virginia")
autoplot(log10(zVA), facets=NULL) +
  labs(x="Days since 1/20/20", y="Log(7-day average)", title="Virginia")
autoplot(zVA, facets=NULL) +
  labs(x="Days since 1/20/20", y="7-day average", title="Virginia")

# Total deaths per million
# Total deaths per million / weighted population density
