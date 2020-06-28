# Output weighted population density to CSV file

library(here)

source(here("code/covid19/density/WeightedPopDensity.R"))

write.csv(dfWeightedDensityAntweiler, here("code/covid19/density/AntweilerDensity.csv"))
