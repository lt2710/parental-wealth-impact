# setup-------------------------------------------------------------------------------------------------------------------------
# run fyrst_key_coding.txt first
# set working directory
path_to_code <- rstudioapi::getActiveDocumentContext()$path
main_directory <- strsplit(path_to_code, "/[a-zA-Z0-9_-]*.R$")[[1]]
setwd(main_directory)

# packages-----------------------------------------------------------------------------------------------------
# load packages.
packages <- c(
  "tidyverse",
  "data.table", 
  # for cross tab
  "gtsummary",
  # for regression output summary
  "VGAM",
  "jtools",
  "huxtable",
  "officer",
  "flextable",
  "openxlsx"
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

# filter married
load("output/fyrst.RData")
fyrst_mutated = fyrst %>% mutate(
  ownership_husb_father_cat = ownership_husb_father %>% pmin(2) %>% factor(labels = c("0","1","2 or more")),
  ownership_wife_father_cat = ownership_wife_father %>% pmin(2) %>% factor(labels = c("0","1","2 or more")),
  diploma_husb = case_when(
    education_years_husb %in% c(0:12) ~ "1 <= Senior High",
    education_years_husb %in% c(15:22) ~ "2 Tertiary & Above",
  ),
  diploma_husb_father = case_when(
    education_years_husb_father %in% c(0:9) ~ "1 <= Junior High",
    education_years_husb_father %in% c(11:22) ~ "2 Senior High & Above",
  ),
  diploma_wife = case_when(
    education_years_wife %in% c(0:12) ~ "1 <= Senior High",
    education_years_wife %in% c(15:22) ~ "2 Tertiary & Above",
  ),
  diploma_wife_father = case_when(
    education_years_wife_father %in% c(0:9) ~ "1 <= Junior High",
    education_years_wife_father %in% c(11:22) ~ "2 Senior High & Above",
  ),
)
fyrst_mutated_filtered = fyrst_mutated %>% filter()