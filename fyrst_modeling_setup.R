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
fyrst_mutated = fyrst %>% mutate()
fyrst_mutated_filtered = fyrst_mutated %>% filter()