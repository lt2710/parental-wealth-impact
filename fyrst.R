# setup-------------------------------------------------------------------------------------------------------------------------
# run fyrst_key_coding.txt first
# set working directory
path_to_code <- rstudioapi::getActiveDocumentContext()$path
main_directory <- strsplit(path_to_code, "/[a-zA-Z0-9_-]*.R$")[[1]]
setwd(main_directory)

# packages-----------------------------------------------------------------------------------------------------
# load packages.
packages <- c(
  "haven",
  "tidyverse",
  "data.table"
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

# read in data -----------------------------
fyrst_path = file.path("FYRST/61896_2013fyrst.sav")
fyrst_raw = read_sav(fyrst_path, encoding = "UTF-8")
# remove abnormal strings in data 
for (i in names(fyrst_raw)) {
  fyrst_raw[[i]] = fyrst_raw[[i]] %>%
    str_replace_all('[:punct:]', '') %>%
    str_replace_all(' ', '') %>%
    na_if("")
}

# data preparation ----------------------------------
# select necessary variables
fyrst_raw_mutated = fyrst_raw %>%
  transmute(
    # respondent
    pid = as.numeric(pid),
    male_res = a1 == 1,
    age_res = 2013 - as.numeric(a2_y),
    party_res = a4a == 1,
    urbanhukou_res = recode(a6, `3` = "1", `4` = "1") == 2,
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
    urbanhukou_res_father = b405 == "2",
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
    # education_years_res_mother = b504 %>%
    #   as.numeric() %>%
    #   recode(
    #     `1` = 0,
    #     `2` = 6,
    #     `3` = 9,
    #     `4` = 11,
    #     `5` = 12,
    #     `6` = 12,
    #     `7` = 12,
    #     `8` = 15,
    #     `9` = 15,
    #     `10` = 16,
    #     `11` = 16,
    #     `12` = 19,
    #     `13` = 19,
    #     `14` = 22,
    #     `15` = 22,
    #     `16` = 16
    #   ),
    # hukou_res_mother = b505 == "2",
    # party_res_mother = b507 == "1",
    # respondent cont'd
    education_years_res = c1 %>%
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
    job_res = ifelse(d2 == 3,
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
    manager_res = ifelse(d2 == 3,
                     d8>1,
                     FALSE),
    married_res = e1 == "3", 
    # spouse 
    age_sps = 2013 - as.numeric(e9_y),
    urbanhukou_sps = recode(e10, `3` = "1", `4` = "1") == 2,
    party_sps = e12a == "1",
    education_years_sps = e1303 %>%
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
    job_sps = ifelse(e1404 == 3,
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
    manager_sps = ifelse(e1404 == 3,
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
    urbanhukou_sps_father = e1701 == "2",
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
    # # spouse mother 
    # education_years_sps_mother = e1803 %>%
    #   as.numeric() %>%
    #   recode(
    #     `1` = 0,
    #     `2` = 3,
    #     `3` = 6,
    #     `4` = 9,
    #     `5` = 11,
    #     `6` = 12,
    #     `7` = 12,
    #     `8` = 12,
    #     `9` = 15,
    #     `10` = 15,
    #     `11` = 16,
    #     `12` = 16,
    #     `13` = 19,
    #     `14` = 19,
    #     `15` = 22,
    #     `16` = 22,
    #     `17` = 16
    #   ),
    # hukou_sps_mother = e1801 == "2",
    # party_sps_mother = e1804 == "1",
    # household
    hh_income = as.numeric(k2),
    hh_income_logged = log(hh_income+1),
    ownership_vehicle = k5 == 1,
    areabuild = as.numeric(k1001),
    ownership =  k11%in%c("1","2","3","4"),
    year_obtained_before_marriage = as.numeric(e7_y) - as.numeric(k1401),
    homevalue = ifelse(ownership,
                       ifelse(as.numeric(k16)>1000,
                              as.numeric(k16)/10000,
                              as.numeric(k16)),
                       0),
    homevalue_logged = log(homevalue+1),
    secondhome = (as.numeric(k2001) + as.numeric(ownership)) >= 2,
    homevalue_other = ifelse(as.numeric(k2002)>1000,
                             as.numeric(k2002)/10000,
                             as.numeric(k2002)),
    homevalue_total = homevalue + homevalue_other,
    homevalue_total_logged = log(homevalue_total+1),
    # called father but actually parent
    ownership_res_father = k21 == "2",
    ownership_sps_father = k22 == "2"
  )

# key school education
fyrst_edu = 
  fyrst_raw %>%
  select(pid,
         c4a1:c4a10,
         c4g1:c4g10) %>%
  mutate(pid = as.numeric(pid),
         key_seq = "")
for (i in 1:10){
  var_diploma = paste0("c4a", i)
  var_key = paste0("c4g", i)
  fyrst_edu = fyrst_edu %>%
    mutate({{var_diploma}} := ifelse(!!as.name(var_diploma) %in% list_pm_diploma, "pm", 
                                     ifelse(!!as.name(var_diploma) %in% list_md_diploma, "md", 
                                            ifelse(!!as.name(var_diploma) %in% list_hi_diploma, "hi",
                                                   ifelse(!!as.name(var_diploma) %in% list_clg_diploma, "clg",
                                                          NA
                                                          )
                                                    )
                                            )
                                     ) %>% replace_na(""),
    {{var_key}} := ifelse(!!as.name(var_diploma) %in% c("pm","md","hi"),
                          !!as.name(var_key) %in% list_key_cpsr,
                          !!as.name(var_key) %in% list_key_high
                          ) %>% replace_na(""),
    key_seq = str_c(key_seq,",",as.character(!!as.name(var_diploma)),"-",as.character(!!as.name(var_key)))
    )
}
fyrst_edu = 
  fyrst_edu %>%
  mutate(
    pid = as.numeric(pid),
    key_pm_res = ifelse(
      str_detect(key_seq, "pm-TRUE"),
      TRUE,
      ifelse(str_detect(key_seq, "pm-FALSE"), FALSE,
             NA)
    ),
    key_md_res = ifelse(
      str_detect(key_seq, "md-TRUE"),
      TRUE,
      ifelse(str_detect(key_seq, "md-FALSE"), FALSE, NA)
    ),
    key_hi_res = ifelse(
      str_detect(key_seq, "hi-TRUE"),
      TRUE,
      ifelse(str_detect(key_seq, "hi-FALSE"), FALSE, NA)
    ),
    key_clg_res = ifelse(
      str_detect(key_seq, "clg-TRUE"),
      TRUE,
      ifelse(str_detect(key_seq, "clg-FALSE"), FALSE, NA)
    )
  ) %>% 
  select(pid,
         key_pm_res,
         key_md_res,
         key_hi_res,
         key_clg_res)

fyrst_to_impute <-
  left_join(fyrst_raw_mutated, fyrst_edu, on = "pid") %>%
  mutate(
    key_pm_res = ifelse(!is.na(key_pm_res),
                        key_pm_res,
                        ifelse(is.na(key_pm_res) &
                                 education_years_res < 6,
                               "dropout",
                               NA)) %>% as.factor(), 
    key_md_res = ifelse(!is.na(key_md_res),
                        key_md_res,
                        ifelse(is.na(key_md_res) &
                                 education_years_res <= 6,
                               "dropout",
                               NA)) %>% as.factor(), 
    key_hi_res = ifelse(!is.na(key_hi_res),
                        key_hi_res,
                        ifelse(is.na(key_hi_res) &
                                 education_years_res <= 9,
                               "dropout",
                               ifelse(education_years_res == 11,
                                      "FALSE",
                                      NA))) %>% as.factor(),
    key_clg_res = ifelse(!is.na(key_clg_res),
                         key_clg_res,
                         ifelse(is.na(key_clg_res) &
                                  education_years_res <= 12,
                                "dropout",
                                NA)) %>% as.factor()
  )

# perform imputation
fyrst_object = fyrst_to_impute %>%
  mice::mice(
    m = 5,
    method = "rf",
    maxit = 5,
    seed = 999
  )
fyrst = fyrst_object %>%
  mice::complete(action = 1) %>%
  mutate_at(c("key_pm_res",
              "key_md_res",
              "key_hi_res",
              "key_clg_res"),
            ~na_if(.,"dropout")%>%as.logical())

# some additional variables for husband/wife matching ------------------------------------ 

for (var in c("age","education_years","urbanhukou","party","job","manager","ownership")){
  if (var!= "ownership"){
    varname_husb = paste0(var,"_husb")
    fyrst[[varname_husb]] = NA
    varname_wife = paste0(var,"_wife")
    fyrst[[varname_wife]] = NA
  }
  if (var!= "age"){
    varname_husb_father = paste0(var,"_husb_father")
    fyrst[[varname_husb_father]] = NA
    varname_wife_father = paste0(var,"_wife_father")
    fyrst[[varname_wife_father]] = NA
  }
  for (i in 1:nrow(fyrst)){
    if (var!= "ownership"){
      fyrst[[varname_husb]][i] = ifelse(fyrst[["male_res"]][i] == TRUE, 
                                        fyrst[[paste0(var,"_res")]][i],
                                        fyrst[[paste0(var,"_sps")]][i])
      fyrst[[varname_wife]][i] = ifelse(fyrst[["male_res"]][i] == TRUE, 
                                        fyrst[[paste0(var,"_sps")]][i],
                                        fyrst[[paste0(var,"_res")]][i])
    }
    if (var!= "age"){
      fyrst[[varname_husb_father]][i] = ifelse(fyrst[["male_res"]][i] == TRUE, 
                                               fyrst[[paste0(var,"_res_father")]][i],
                                               fyrst[[paste0(var,"_sps_father")]][i])
      fyrst[[varname_wife_father]][i] = ifelse(fyrst[["male_res"]][i] == TRUE, 
                                               fyrst[[paste0(var,"_sps_father")]][i],
                                               fyrst[[paste0(var,"_sps_father")]][i])
    }
  }
}

# sort variable names
main_col = c(
  "pid",
  names(fyrst)[str_detect(names(fyrst),"res")],
  "married_res",
  names(fyrst)[str_detect(names(fyrst),"res_father")],
  names(fyrst)[str_detect(names(fyrst),"sps")],
  names(fyrst)[str_detect(names(fyrst),"sps_father")],
  names(fyrst)[str_detect(names(fyrst),"husb")],
  names(fyrst)[str_detect(names(fyrst),"husb_father")],
  names(fyrst)[str_detect(names(fyrst),"wife")],
  names(fyrst)[str_detect(names(fyrst),"husb_father")]
)
other_col = setdiff(names(fyrst),main_col)
fyrst = fyrst %>%
  select(all_of(main_col), all_of(other_col))

save(fyrst, file = "output/fyrst.RData")