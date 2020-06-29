# Produce a CSV file of the US states similar to 
# https://en.wikipedia.org/w/index.php?title=Template:COVID-19_pandemic_data%2FUnited_States_medical_cases

library(tidyverse)
# Note: xts library may not be loadable when using here library
#library(xts)
library(zoo)
library(here)

source(here("code/covid19/jhuanalyze/StateVectors.R"))

# Assumes you have downloaded a version of the Johns Hopkins data in the 
# appropriate directory - see the README
dfCovidUSCases <- read.csv(here("data/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"))
dfCaseTots <- dfCovidUSCases %>% .[,c(7, 11,13:ncol(.))]

tib_states <- dfCaseTots %>%
  pivot_longer(cols=starts_with("X"),
               names_to = "day", values_to = "cumulative_deaths") %>%
  mutate(Date=as.Date(str_replace(day, "X", ""), format="%m.%d.%y")) %>%
  select(State=Province_State, Date, cumulative_deaths) %>%
  group_by(State, Date) %>%
  summarise(cumulative_deaths = sum(cumulative_deaths))

tib_inter <- tib_states %>%
  pivot_wider(names_from = "State", values_from = "cumulative_deaths") 

tib_final <- data.frame(tib_inter[2:nrow(tib_inter),1], apply( tib_inter[-1] , 2 , diff ),
                        check.names = FALSE) ## FFS

states <- data.frame(state_names, state_abbrs, stringsAsFactors = FALSE)


names(tib_final)[match(states[, "state_names"], names(tib_final))] = states[, "state_abbrs"]

keep_colnames <- c("Date", sort(state_abbrs))
tib_final <- tib_final[keep_colnames]

write.csv(tib_final, here("data/csse_covid_19_data/csse_covid_19_time_series/USConfirmedWikiFmt.csv"))
                      
