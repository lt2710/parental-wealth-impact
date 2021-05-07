## ----setup----------------------------------------------------------------------------------------
path_to_code<-rstudioapi::getActiveDocumentContext()$path
main_directory<-strsplit(path_to_code,"/[a-zA-Z0-9_-]*.R$")[[1]]
setwd(main_directory)

## ----packages, message = FALSE, warning = FALSE, echo=FALSE---------------------------------------
# Load packages.
packages <- c(
  "tidyverse",
  "VGAM",
  # below is for output summary
  "jtools",
  "huxtable",
  "officer",
  "flextable"
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

## ----filter and select final data,echo=TRUE, warning=FALSE----------------------------------------
load("output/charls_merged.RData")

## -------------------------------------------------------------------------------------------------
charls_mutated <-
  charls_merged %>%
  mutate(
    asset_fin = pmax(asset_fin, 0),
    asset_total = pmax(asset_total, 0),
    job = 8-(job%>%as.numeric()),
    job_spouse = 8-(job_spouse%>%as.numeric()),
    income_logged = log(income + 50),
    homevalue_logged = log(ifelse(ownership == FALSE,
                                  0,
                                  homevalue) + 1),
    manager_parent = manager_parent %in% 
      c("2 Team Leader",
        "3 Section Chief",
        "4 Director of Division",
        "5 Director-general of a bureau or Above",
        "6 Village Leader",
        "7 Township Leader",
        "8 Division Manager",
        "9 General Manager"),
    job_parent = job_parent %>% fct_recode(`1 Private`= "1 Agricultural",
                                           `1 Private`= "2 Private",
                                           `2 Public`= "3 State Controlled Firm",
                                           `2 Public`= "4 Public institution",
                                           `2 Public`= "5 Government")
  ) %>%
  mutate_at(
    c(
      "homevalue",
      "asset_fin",
      "asset_land",
      "asset_durable_fixed",
      "asset_home",
      "asset_total"
    ),
    .funs = list(logged = ~ log(. + 1))
  ) %>%
  mutate_at(
    c(
      "homevalue",
      "asset_fin",
      "asset_land",
      "asset_durable_fixed",
      "asset_home",
      "asset_total"
    ),
    .funs = list(flag = ~ ifelse(. <= 0,
                                 1,
                                 0))
  ) %>%
  arrange(hhid, desc(age)) %>%
  group_by(hhid) %>%
  mutate(sibling_rank = row_number(),
         sibling_num = n()) %>%
  ungroup()


## -------------------------------------------------------------------------------------------------
charls_mutated_filtered <-
  charls_mutated%>% filter(
    urban %in% c(
      "1 Central of City/Town"
    ),
    age > 25,
    married == TRUE
  )

# sample
charls <- charls_mutated_filtered %>%
  group_by(hhid) %>%
  sample_n(1) %>%
  ungroup()

####################################### education #################################
varlist <-
  paste(
    # child
    "age"
    # parent
    ,"education_years_parent"
    ,"urbanhukou_parent"
    ,"party_parent"
    ,"job_parent"
    ,"asset_total_flag"
    ,"asset_total_logged"
    ,sep = " + "
  )

formula <-
  formula(paste("education_years ~",
                varlist))

jtools::export_summs(
  lm(
    formula,
    data = charls %>% filter(male == TRUE)
  ),
  lm(
    formula,
    data = charls %>% filter(male == FALSE)
  ),
  error_pos = "same",
  model.names = c("Husband","Wife"),
  to.file = "xlsx",
  file.name = file.path("tables",paste0("charls - ",paste0(as.character(formula)[-1],collapse = " ~ "),".xlsx"))
)

####################################### income ############################
varlist <-
  paste(
    # child
    "age"
    ,"education_years"
    ,"urbanhukou"
    ,"party"
    # spouse
    , "education_years_spouse"
    # parent
    ,"education_years_parent"
    ,"job_parent"
    ,"asset_total_flag"
    ,"asset_total_logged"
    ,sep = " + "
  )

formula <-
  formula(paste("income_logged ~",
                varlist))

jtools::export_summs(
  lm(
    formula,
    data = charls %>% filter(male == TRUE)
  ),
  lm(
    formula,
    data = charls %>% filter(male == FALSE)
  ),
  error_pos = "same",
  model.names = c("Husband","Wife"),
  to.file = "xlsx",
  file.name = file.path("tables",paste0("charls - ",paste0(as.character(formula)[-1],collapse = " ~ "),".xlsx"))
)

####################################### ownership ############################
varlist <-
  paste(
    # child
    "age"
    ,"education_years"
    ,"urbanhukou"
    ,"party"
    ,"income_logged"
    # spouse
    , "education_years_spouse"
    # parent
    ,"education_years_parent"
    ,"job_parent"
    ,"asset_total_flag"
    ,"asset_total_logged"
    ,sep = " + "
  )

formula <-
  formula(paste("ownership ~",
                varlist))

jtools::export_summs(
  glm(
    formula,
    data = charls %>% filter(male == TRUE),
    family = "binomial"
  ),
  glm(
    formula,
    data = charls %>% filter(male == FALSE),
    family = "binomial"
  ),
  error_pos = "same",
  model.names = c("Husband","Wife"),
  to.file = "xlsx",
  file.name = file.path("tables",paste0("charls - ",paste0(as.character(formula)[-1],collapse = " ~ "),".xlsx"))
)

####################################### home value ############################
varlist <-
  paste(
    # child
    "age"
    ,"education_years"
    ,"urbanhukou"
    ,"party"
    ,"income_logged"
    # spouse
    , "education_years_spouse"
    # parent
    ,"education_years_parent"
    ,"job_parent"
    ,"asset_total_flag"
    ,"asset_total_logged"
    ,sep = " + "
  )

formula <-
  formula(paste("homevalue_logged ~",
                varlist))

jtools::export_summs(
  lm(
    formula,
    data = charls %>% filter(male == TRUE)
  ),
  lm(
    formula,
    data = charls %>% filter(male == FALSE)
  ),
  error_pos = "same",
  model.names = c("Husband","Wife"),
  to.file = "xlsx",
  file.name = file.path("tables",paste0("charls - ",paste0(as.character(formula)[-1],collapse = " ~ "),".xlsx"))
)