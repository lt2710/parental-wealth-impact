## ----setup-------------------------------------------------------------------------------------------------------------------------
#set working directory
path_to_code<-rstudioapi::getActiveDocumentContext()$path
main_directory<-strsplit(path_to_code,"/[a-zA-Z0-9_-]*.R$")[[1]]
setwd(main_directory)
#Set time variables to debug date transformation
Sys.setlocale("LC_TIME", "C")
Sys.setenv(TZ="Europe/Berlin")

## ----packages, message = FALSE-----------------------------------------------------------------------------------------------------
# Load packages.
packages <- c(
  "readstata13",
  "knitr",
  "bit64",
  "tidyverse",
  "data.table",
  "reshape2"
)
packages <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x)
      library(x, character.only = TRUE)
    }
  }
)
select <- dplyr::select

########### individual data ####################

#read in data
person_2018_raw =
  read.dta13(
    "CFPS/CFPS2018/cfps2018person_202012.dta",
    convert.factors = TRUE,
    generate.factors = TRUE,
    encoding = "UTF-8"
  )

ind_2017 = ind_2017_raw %>%
  transmute(

  )

########### family data ####################

#read in data
famconf_2018_raw =
  read.dta13(
    "CFPS/CFPS2018/cfps2018famconf_202008.dta",
    convert.factors = TRUE,
    generate.factors = TRUE,
    encoding = "UTF-8"
  )

ind_2017 = ind_2017_raw %>%
  transmute(
    hhid = hhid,
    relation = a2001,
    male = a2003 == 1,
    age = 2017 - a2005,
    education_years = a2012 %>%
      recode(
        `1` = 0,
        `2` = 6,
        `3` = 9,
        `4` = 12,
        `5` = 12,
        `6` = 15,
        `7` = 16,
        `8` = 19,
        `9` = 22
      ),
    party = a2015 == 1,
    urbanhukou = ifelse(a2022 == 3,
                        a202201 == 2,
                        a2022 == 2)
  )
