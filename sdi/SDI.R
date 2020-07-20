library(stringr)
library(tidyverse)
# Note: xts library may not be loadable when using here library
#library(xts)
library(zoo)
library(purrr)
library(here)
source("code/covid19/jhuanalyze/StateVectors.R")

# sh1 should contain data from Unstuck Politics user @nunnehi 's spreadsheet
# For example:
# doc1 <- gs4_get("https://docs.google.com/spreadsheets/d/1Y7rSvbVuLJMJjNUtWi9gYuxke9UZm36YLHtrgTlDjVc/edit#gid=1460461824")
# sh1 <- read_sheet(doc1, "SDI Daily")
# z1 <- piv_sdi(sh1)

piv_sdi <- function(sh1) {
  sh2 <- sh1 %>% pivot_longer(-StateCode, names_to = "day", values_to = "sdi") %>%
    mutate(Date = as.Date("2020-03-10") + as.integer(str_replace(day, "...", ""))) %>%
    mutate(State = str_replace_all(StateCode, "[0-9]+ ([A-Z][A-Z]) SDI", "\\1")) %>%
    select(State, Date, sdi) %>% group_by(State) %>% nest()
    mutate(zoo_obj=map(data, ~with(.x, zoo(sdi, Date))),
           rolling_avg_7day=map(data, ~with(.x, rollmeanr(zoo(sdi, Date), k=7))),
           trend7=map(data, ~with(.x, diff(rollmeanr(zoo(sdi, Date), k=7),
                                           lag=7, arithmetic=FALSE)
          ))
    )
  return(sh2)
}

# sh2 should be read from the same spreadsheet, but using the sheet
read_target_sdi <- function(shTarget) {
  return(shTarget %>% mutate(State=getStateAbbr(.$`State or Place`)))
}
