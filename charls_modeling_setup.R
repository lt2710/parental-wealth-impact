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
  "tidyverse",
  "data.table",
  # below is for output summary
  "VGAM",
  "jtools",
  "huxtable",
  "officer",
  "flextable",
  "gtsummary"
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

# mutate and filter -------------------------------------------------------------------------------------------------
load("output/charls_merged.RData")
charls_mutated <-
  charls_merged %>%
  arrange(hhid, desc(age)) %>%
  group_by(hhid) %>%
  mutate(sibling_rank = row_number(),
         sibling_num = n()) %>%
  ungroup()

charls_filtered <-
  charls_mutated %>%
  # exclude rural population and include 10% to 90% age percentile
  # working is good indicator for clustered missing value
  filter(urban %in% c("1 Central of City/Town", "2 Urban-Rural Integration Zone"),
         age >= 23,
         age <= 50,
         !is.na(working)
  )

# sample -------------------------------------------------------------------------------------------------

charls_filtered_sampled <- charls_filtered %>%
  group_by(hhid) %>%
  sample_n(1) %>%
  ungroup()

# impute --------------------------
charls_to_impute <- charls_filtered_sampled %>% 
  mutate(homevalue = ifelse(ownership, homevalue, 0),
         marriagehomevalue = ifelse(marriagehome, marriagehomevalue, 0),
         marriagegiftvalue = ifelse(marriagegift, marriagegiftvalue, 0))
charls_filtered_sampled_imputed_object <- charls_to_impute %>%
  mice::mice(
    m = 1,
    method = "rf",
    maxit = 30,
    seed = 999
  )
charls_filtered_sampled_imputed <- mice::complete(charls_filtered_sampled_imputed_object, action = 1)

# mutate ---------------------------------
charls_filtered_sampled_imputed_mutated <- charls_filtered_sampled_imputed %>% # use charls_to_impute if no impute
  mutate(
    asset_total = (asset_home  +
                     asset_fin +
                     asset_land +
                     asset_durable_fixed), 
    asset_other = (asset_land + asset_durable_fixed),
    job = 8 - (job %>% as.numeric()),
    job_spouse = 8 - (job_spouse %>% as.numeric()),
    income_logged = log(income + 50),
    homevalue = homevalue + marriagehomevalue,
    manager_parent = manager_parent %in%
      c(
        "2 Team Leader",
        "3 Section Chief",
        "4 Director of Division",
        "5 Director-general of a bureau or Above",
        "6 Village Leader",
        "7 Township Leader",
        "8 Division Manager",
        "9 General Manager"
      ),
    job_parent = job_parent %>% fct_recode(
      `1 Private` = "1 Agricultural",
      `1 Private` = "2 Private",
      `2 Public` = "3 State Controlled Firm",
      `2 Public` = "4 Public institution",
      `2 Public` = "5 Government"
    )
  ) %>%
  mutate_at(
    c(
      "homevalue",
      "asset_fin",
      "asset_land",
      "asset_durable_fixed",
      "asset_home",
      "asset_total",
      "asset_other",
      "marriagehomevalue",
      "marriagegiftvalue"
    ),
    .funs = list(logged = ~ log(pmax(., 0) + 1))
  ) %>%
  mutate_at(
    c(
      "homevalue",
      "asset_fin",
      "asset_land",
      "asset_durable_fixed",
      "asset_home",
      "asset_total",
      "asset_other",
      "marriagehomevalue",
      "marriagegiftvalue"
    ),
    .funs = list(flag = ~ ifelse(. < 0,
                                 1,
                                 0))
  ) 
# final mutate
charls = charls_filtered_sampled_imputed_mutated %>%
  mutate(
    asset_total_quant = cut(
      asset_total,
      breaks = quantile(
        charls_filtered_sampled_imputed_mutated$asset_total,
        probs = seq(0, 1, 0.25),
        na.rm = T
      )
    )
  )
# save
save(charls, file = "output/charls.RData")