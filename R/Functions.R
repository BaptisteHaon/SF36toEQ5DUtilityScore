#'@param SF36_matrix Columns must be named as follows. The order is not important.
#'PF Physical Functionning score ranging from 0 to 1
#'RP Role limitations due to Physical health score ranging from 0 to 1
#'BP Bodily Pain score ranging from 0 to 1
#'GH General Health score ranging from 0 to 1
#'VT Vitality score ranging from 0 to 1
#'SF Social Functioning score ranging from 0 to 1
#'RE Role limitations due to Emotional problems score ranging from 0 to 1
#'MH General Mental Health score ranging from 0 to 1,
#'AGE Patient's age
#'GENDER Patient's gender coded as male=1 and female=2.
#'@export OLS_mapping
#'@export MARS_mapping
#'@export GBT_mapping

#'@return A EQ5D-5L Utility score
#'@import stats
#'@import earth
#'@import gbm

library(stats)
library(earth)
library(gbm)

OLS_mod <- readRDS("OLS.rds")
MARS_mod <- readRDS("MARS.rds")
GBT_mod <- readRDS("GBT.rds")

SF36_matrix$PF <- SF36_matrix$PF/100
SF36_matrix$RP <- SF36_matrix$RP/100
SF36_matrix$BP <- SF36_matrix$BP/100
SF36_matrix$GH <- SF36_matrix$GH/100
SF36_matrix$VT <- SF36_matrix$VT/100
SF36_matrix$SF <- SF36_matrix$SF/100
SF36_matrix$RE <- SF36_matrix$RE/100
SF36_matrix$MH <- SF36_matrix$MH/100

OLS_mapping <- function (SF36_matrix){
  stopifnot("PF score must be numeric in the range 0 to 1." = SF36_matrix$PF>=0&SF36_matrix$PF<=100)
  stopifnot("RP score must be numeric in the range 0 to 1." = SF36_matrix$RP>=0&SF36_matrix$RP<=100)
  stopifnot("BP score must be numeric in the range 0 to 1." = SF36_matrix$BP>=0&SF36_matrix$BP<=100)
  stopifnot("GH score must be numeric in the range 0 to 1." = SF36_matrix$GH>=0&SF36_matrix$GH<=100)
  stopifnot("VT score must be numeric in the range 0 to 1." = SF36_matrix$VT>=0&SF36_matrix$VT<=100)
  stopifnot("SF score must be numeric in the range 0 to 1." = SF36_matrix$SF>=0&SF36_matrix$SF<=100)
  stopifnot("RE score must be numeric in the range 0 to 1." = SF36_matrix$RE>=0&SF36_matrix$RE<=100)
  stopifnot("MH score must be numeric in the range 0 to 1." = SF36_matrix$MH>=0&SF36_matrix$MH<=100)
  stopifnot("AGE must be numeric in the range 0 to 110." = SF36_matrix$AGE>=0&SF36_matrix$AGE<=100)
  EQ5D_ut_score <- predict(OLS_mod, SF36_matrix)
  return(EQ5D_ut_score)
}

MARS_mapping <- function (SF36_matrix){
  stopifnot("PF score must be numeric in the range 0 to 1." = SF36_matrix$PF>=0&SF36_matrix$PF<=100)
  stopifnot("RP score must be numeric in the range 0 to 1." = SF36_matrix$RP>=0&SF36_matrix$RP<=100)
  stopifnot("BP score must be numeric in the range 0 to 1." = SF36_matrix$BP>=0&SF36_matrix$BP<=100)
  stopifnot("GH score must be numeric in the range 0 to 1." = SF36_matrix$GH>=0&SF36_matrix$GH<=100)
  stopifnot("VT score must be numeric in the range 0 to 1." = SF36_matrix$VT>=0&SF36_matrix$VT<=100)
  stopifnot("SF score must be numeric in the range 0 to 1." = SF36_matrix$SF>=0&SF36_matrix$SF<=100)
  stopifnot("RE score must be numeric in the range 0 to 1." = SF36_matrix$RE>=0&SF36_matrix$RE<=100)
  stopifnot("MH score must be numeric in the range 0 to 1." = SF36_matrix$MH>=0&SF36_matrix$MH<=100)
  stopifnot("AGE must be numeric in the range 0 to 110." = SF36_matrix$AGE>=0&SF36_matrix$AGE<=100)
  EQ5D_ut_score <- predict(MARS_mod, SF36_matrix)
  return(EQ5D_ut_score)
}

GBT_mapping <- function (SF36_matrix){
  stopifnot("PF score must be numeric in the range 0 to 1." = SF36_matrix$PF>=0&SF36_matrix$PF<=100)
  stopifnot("RP score must be numeric in the range 0 to 1." = SF36_matrix$RP>=0&SF36_matrix$RP<=100)
  stopifnot("BP score must be numeric in the range 0 to 1." = SF36_matrix$BP>=0&SF36_matrix$BP<=100)
  stopifnot("GH score must be numeric in the range 0 to 1." = SF36_matrix$GH>=0&SF36_matrix$GH<=100)
  stopifnot("VT score must be numeric in the range 0 to 1." = SF36_matrix$VT>=0&SF36_matrix$VT<=100)
  stopifnot("SF score must be numeric in the range 0 to 1." = SF36_matrix$SF>=0&SF36_matrix$SF<=100)
  stopifnot("RE score must be numeric in the range 0 to 1." = SF36_matrix$RE>=0&SF36_matrix$RE<=100)
  stopifnot("MH score must be numeric in the range 0 to 1." = SF36_matrix$MH>=0&SF36_matrix$MH<=100)
  stopifnot("AGE must be numeric in the range 0 to 110." = SF36_matrix$AGE>=0&SF36_matrix$AGE<=100)
  EQ5D_ut_score <- predict(GBT_mod, SF36_matrix)
  return(EQ5D_ut_score)
}
