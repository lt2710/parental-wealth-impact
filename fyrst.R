## ----setup-------------------------------------------------------------------------------------------------------------------------
#set working directory
path_to_code <- rstudioapi::getActiveDocumentContext()$path
main_directory <- strsplit(path_to_code, "/[a-zA-Z0-9_-]*.R$")[[1]]
setwd(main_directory)

## ----packages, message = FALSE-----------------------------------------------------------------------------------------------------
# Load packages.
packages <- c(
  "haven",
  "tidyverse",
  "data.table",
  "VGAM",
  # below is for output summary
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

############################### read in data ###################################
path = file.path("FYRST/61896_2013fyrst.sav")
fyrst_raw = read_sav(path, encoding = "UTF-8")
# remove abnormal strings in data 
for (i in names(fyrst_raw)) {
  fyrst_raw[[i]] = fyrst_raw[[i]] %>%
    str_replace_all('[:punct:]', '') %>%
    str_replace_all(' ', '') %>%
    na_if("")
}

############################### feature engineering ##############################
# select necessary variables
fyrst = fyrst_raw %>%
  transmute(
    # respondent
    pid = pid,
    male = a1 == 1,
    age = 2013 - as.numeric(a2_y),
    party = a4a == 1,
    urbanhukou = recode(a6, `3` = "1", `4` = "1") == 2,
    # respondent father
    education_years_res_father = b404 %>%
      as.numeric() %>%
      recode(
        `1` = 0,
        `2` = 3,
        `3` = 6,
        `4` = 9,
        `5` = 11,
        `6` = 12,
        `7` = 12,
        `8` = 12,
        `9` = 15,
        `10` = 15,
        `11` = 16,
        `12` = 16,
        `13` = 19,
        `14` = 19,
        `15` = 22,
        `16` = 22,
        `17` = 16
      ),
    hukou_res_father = b405 == "2",
    party_res_father = b407 == "1",
    job_res_father = ifelse(b408 == 3,
                        ifelse(
                          b410 %in% c("1", "2", "3", "4", "5", "7"),
                          "Public",
                          ifelse(
                            b410 %in% c("8", "9"),
                            "Private",
                            ifelse(b412 %in% c("1", "2"),
                                   "Public",
                                   "Private")
                          )
                        ),
                        "Private"), 
    manager_res_father = ifelse(b408 == 3,
                                b411 > 1,
                                FALSE), 
    # mother
    education_years_res_mother = b504 %>%
      as.numeric() %>%
      recode(
        `1` = 0,
        `2` = 6,
        `3` = 9,
        `4` = 11,
        `5` = 12,
        `6` = 12,
        `7` = 12,
        `8` = 15,
        `9` = 15,
        `10` = 16,
        `11` = 16,
        `12` = 19,
        `13` = 19,
        `14` = 22,
        `15` = 22,
        `16` = 16
      ),
    hukou_res_mother = b505 == "2",
    party_res_mother = b507 == "1",
    # respondent cont'd
    education_years = b504 %>%
      as.numeric() %>%
      recode(
        `1` = 0,
        `2` = 3,
        `3` = 6,
        `4` = 9,
        `5` = 11,
        `6` = 12,
        `7` = 12,
        `8` = 12,
        `9` = 15,
        `10` = 15,
        `11` = 16,
        `12` = 16,
        `13` = 19,
        `14` = 19,
        `15` = 22,
        `16` = 22,
        `17` = 16
      ),
    job = ifelse(d2 == 3,
                 ifelse(
                   d12 %in% c("1", "2", "3", "4", "5", "7"),
                   "Public",
                   ifelse(
                     d12 %in% c("8", "9"),
                     "Private",
                     ifelse(d13 %in% c("1", "2"),
                            "Public",
                            "Private")
                   )
                 ),
                 "Private"), 
    manager = ifelse(d2 == 3,
                     d8>1,
                     FALSE), 
    income = as.numeric(d21),
    married = e1 == "3",
    # spouse 
    party_spouse = e12a == "1",
    education_years_spouse = e1303 %>%
      as.numeric() %>%
      recode(
        `1` = 0,
        `2` = 3,
        `3` = 6,
        `4` = 9,
        `5` = 11,
        `6` = 12,
        `7` = 12,
        `8` = 12,
        `9` = 15,
        `10` = 15,
        `11` = 16,
        `12` = 16,
        `13` = 19,
        `14` = 19,
        `15` = 22,
        `16` = 22,
        `17` = 16
      ),
    job_spouse = ifelse(e1404 == 3,
                 ifelse(
                   e1406 %in% c("1", "2", "3", "4", "5", "7"),
                   "Public",
                   ifelse(
                     e1406 %in% c("8", "9"),
                     "Private",
                     ifelse(e1407 %in% c("1", "2"),
                            "Public",
                            "Private")
                   )
                 ),
                 "Private"), 
    manager_spouse = ifelse(e1404 == 3,
                     e1408>1,
                     FALSE), 
    # spouse father 
    education_years_sps_father = e1703 %>%
      as.numeric() %>%
      recode(
        `1` = 0,
        `2` = 3,
        `3` = 6,
        `4` = 9,
        `5` = 11,
        `6` = 12,
        `7` = 12,
        `8` = 12,
        `9` = 15,
        `10` = 15,
        `11` = 16,
        `12` = 16,
        `13` = 19,
        `14` = 19,
        `15` = 22,
        `16` = 22,
        `17` = 16
      ),
    hukou_sps_father = e1701 == "2",
    party_sps_father = e1704 == "1",
    job_sps_father = ifelse(e1705 == 3,
                            ifelse(
                              e1707 %in% c("1", "2", "3", "4", "5", "7"),
                              "Public",
                              ifelse(
                                e1707 %in% c("8", "9"),
                                "Private",
                                ifelse(e1708 %in% c("1", "2"),
                                       "Public",
                                       "Private")
                              )
                            ),
                            "Private"), 
    manager_sps_father = ifelse(e1705 == 3,
                                e1709>1,
                                FALSE),
    # spouse mother 
    education_years_sps_mother = e1803 %>%
      as.numeric() %>%
      recode(
        `1` = 0,
        `2` = 3,
        `3` = 6,
        `4` = 9,
        `5` = 11,
        `6` = 12,
        `7` = 12,
        `8` = 12,
        `9` = 15,
        `10` = 15,
        `11` = 16,
        `12` = 16,
        `13` = 19,
        `14` = 19,
        `15` = 22,
        `16` = 22,
        `17` = 16
      ),
    hukou_sps_mother = e1801 == "2",
    party_sps_mother = e1804 == "1",
    # household
    hh_income = as.numeric(k2),
    hh_income_logged = log(hh_income+1),
    ownership = k11%in%c("3"),
    homevalue = ifelse(as.numeric(k16)>1000,
                              as.numeric(k16)/10000,
                              as.numeric(k16)),
    homevalue_logged = log(homevalue+1),
    ownership_res_parent = k21 == "1",
    ownership_sps_parent = k22 == "1"
  )

# sort variable names
other_col = c(
  names(fyrst)[str_detect(names(fyrst),"res_father")],
  names(fyrst)[str_detect(names(fyrst),"res_mother")],
  names(fyrst)[str_detect(names(fyrst),"res_parent")],
  "married",
  names(fyrst)[str_detect(names(fyrst),"spouse")],
  names(fyrst)[str_detect(names(fyrst),"sps_father")],
  names(fyrst)[str_detect(names(fyrst),"sps_mother")],
  names(fyrst)[str_detect(names(fyrst),"sps_parent")]
)
respondent_col = setdiff(names(fyrst),other_col)
fyrst = fyrst %>%
  select(all_of(respondent_col), all_of(other_col))

# model ownership
fyrst = fyrst %>%
  filter(age >= 25,
         married == TRUE)

####################################### education #################################
varlist <-
  paste(
    # child
    "age"
    # parent
    ,"hukou_res_father"
    ,"party_res_father"
    ,"education_years_res_father"
    ,"job_res_father"
    ,"ownership_res_parent"
    ,sep = " + "
  )

formula <-
  formula(paste("education_years ~",
                varlist))

jtools::export_summs(
  lm(
    formula,
    data = fyrst %>% filter(male == TRUE)
  ),
  lm(formula,
     data = fyrst %>% filter(male == FALSE)
  ),
  error_pos = "same",
  model.names = c("Husband","Wife"),
  to.file = "xlsx",
  file.name = file.path("tables",paste0("fyrst - ",paste0(as.character(formula)[-1],collapse = " ~ "),".xlsx"))
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
    ,"education_years_res_father"
    ,"job_res_father"
    ,"ownership_res_parent"
    ,sep = " + "
  )

formula <-
  formula(paste("hh_income_logged ~",
                varlist))

jtools::export_summs(
  lm(
    formula,
    data = fyrst %>% filter(male == TRUE)
  ),
  lm(
    formula,
    data = fyrst %>% filter(male == FALSE)
  ),
  error_pos = "same",
  model.names = c("Husband","Wife"),
  to.file = "xlsx",
  file.name = file.path("tables",paste0("fyrst - ",paste0(as.character(formula)[-1],collapse = " ~ "),".xlsx"))
)

####################################### ownership ############################
varlist <-
  paste(
    # child
    "age"
    ,"education_years"
    ,"urbanhukou"
    ,"party"
    ,"hh_income_logged"
    # spouse
    , "education_years_spouse"
    # parent
    ,"education_years_res_father"
    ,"job_res_father"
    ,"ownership_res_parent"
    ,sep = " + "
  )

formula <-
  formula(paste("ownership ~",
                varlist))

jtools::export_summs(
  glm(
    formula,
    data = fyrst %>% filter(male == TRUE),
    family = "binomial"
  ),
  glm(
    formula,
    data = fyrst %>% filter(male == FALSE),
    family = "binomial"
  ),
  error_pos = "same",
  model.names = c("Husband","Wife"),
  to.file = "xlsx",
  file.name = file.path("tables",paste0("fyrst - ",paste0(as.character(formula)[-1],collapse = " ~ "),".xlsx"))
)

####################################### home value ############################
varlist <-
  paste(
    # child
    "age"
    ,"education_years"
    ,"urbanhukou"
    ,"party"
    ,"hh_income_logged"
    # spouse
    , "education_years_spouse"
    # parent
    ,"education_years_res_father"
    ,"job_res_father"
    ,"ownership_res_parent"
    ,sep = " + "
  )

formula <-
  formula(paste("homevalue_logged ~",
                varlist))

jtools::export_summs(
  lm(
    formula,
    data = fyrst %>% filter(male == TRUE)
  ),
  lm(
    formula,
    data = fyrst %>% filter(male == FALSE)
  ),
  error_pos = "same",
  model.names = c("Husband","Wife"),
  to.file = "xlsx",
  file.name = file.path("tables",paste0("fyrst - ",paste0(as.character(formula)[-1],collapse = " ~ "),".xlsx"))
)
