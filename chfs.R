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
ind_2017_raw =
  read.dta13(
    "CHFS/CHFS2017/stata13/ind2017_20191202.dta",
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

############ read in data #############################
hh_2017_raw =
  read.dta13(
    "CHFS/CHFS2017/stata13/hh2017_20191120.dta",
    convert.factors = TRUE,
    generate.factors = TRUE,
    encoding = "UTF-8"
  )

hh_2017 = hh_2017_raw %>%
  transmute(
    hhid = hhid,
    ownership = c2002,
    # areabulild 
    c2003_1:c2003_6,
    # channel obtained
    c2006_1:c2006_6,
    # current home
    c2008b_1:c2008b_6,
    # time obtained
    c2012_1:c2012_6,
    # value
    c2016_1_imp:c2016_6_imp
  )