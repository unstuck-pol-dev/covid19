# Generate trend map of cases or deaths for U.S. by state
# Format can be uploaded to mapcharts.net
# For example:
# {
#   "groups":{
#     "#e31a1c":{
#       "div":"#box0",
#       "label":"",
#       "paths":["VA","MA","NY","CA","AL"]
#     },
#     "#4eb3d3":{
#       "div":"#box1",
#       "label":"",
#       "paths":["TX","OK","KY"]
#     },
#     "#fc4e2a":{
#       "div":"#box2",
#       "label":"",
#       "paths":["CO"]
#     },
#     "#b2abd2":{
#       "div":"#box3",
#       "label":"",
#       "paths":["KS"]
#     }
#   },
#   "title":"",
#   "hidden":[],
#   "background":"#ffffff",
#   "borders":"#000000"
# }

# Values for 7 day moving average on chart date vs. 7 days prior are compared as a ratio.
# If the ratio is > 0, the color is red. The level of red is proportional to the ratio.
# If the ration is < 0, the color is blue. The level of blue is inversely proportional to the ratio.
# The darkness of the color is relative to to the peak value in absolute numbers. 
# TODO: It should be relative to numbers divided by population

source(file="jhuanalyze/StateVectors.R")
source(file="jhuanalyze/ReadUSCovidData.R")

# https://stackoverflow.com/questions/24519794/r-max-function-ignore-na
my.max <- function(x) ifelse( !all(is.na(x)), max(x, na.rm=T), NA)

generate_trend_map <- function(state_abbrs, state_names, zDeaths, zCases, nColumn, nLogScaleMax, nStart, nStop) {
  numPrinted <- 0
  cat("{\"groups\":{")
  for (stateNum in nStart:nStop) {
    zState <- getStateLogCasesAndDeaths(zDeaths, zCases, state_names[stateNum], with_trends = TRUE)
    nMaxItems <- my.max(as.vector(zState[,nColumn]))
    trendVec <- as.vector(zState[,nColumn + 2])
    curTrend <- NA
    iUse <- length(trendVec)
    while (is.na(curTrend) && iUse > 0) {
      curTrend <- trendVec[iUse]
      iUse <- iUse - 1
    }
    colorBlack <- 256 - max(1, min(c(trunc(log10(nMaxItems)/nLogScaleMax * 256), 255)) %% 256)
    colorGreen <- colorBlack
    if(!is.na(curTrend)) {
      if (numPrinted > 0) {
        cat(",")
      }
      if (curTrend > 1) {
        colorRed <- min(c(trunc((curTrend - 1) * 256), 255)) %% 256
        colorBlue <- colorBlack
        colorRed <- min(255, colorRed + colorBlack)
      } else {
        colorBlue <- min(c(trunc((1 - curTrend) * 256), 255)) %% 256
        colorRed <- colorBlack
        colorBlue <- min(255, colorBlue + colorBlack)
      }
      cat(stringi::stri_c("\"#",
                          stringi::stri_pad(as.hexmode(colorRed), 2, pad="0"),
                          stringi::stri_pad(as.hexmode(colorGreen), 2, pad="0"),
                          stringi::stri_pad(as.hexmode(colorBlue), 2, pad="0"),
                          "\":{"))
      cat(stringi::stri_c("\"div\":\"#box", stateNum - nStart, "\",",
                          "\"label\":\"\",",
                          "\"paths\":[\"", state_abbrs[stateNum], "\"]",
                          "}"))
      numPrinted <- numPrinted + 1
    }
  }
  cat(stringi::stri_c("},\"title\":\"\",",
                      "\"hidden\":[],",
                      "\"background\":\"#ffffff\",",
                      "\"borders\":\"#000000\"",
                      "}"))
}

generate_case_trend_map <- function() {
  generate_trend_map(state_abbrs, state_names, zDeaths, zCases, 
                     nColumn=2, nLogScaleMax=6, nStart=1, nStop=length(state_abbrs))
}

generate_death_trend_map <- function() {
  generate_trend_map(state_abbrs_without_lows, state_names_without_lows, zDeaths, zCases, nColumn=1, nLogScaleMax=4, nStart=1, nStop=length(state_abbrs_without_lows))
}

