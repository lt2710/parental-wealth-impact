# education -------------------------------
# continuation rate
key_vars = c("key_pm_res","key_md_res","key_hi_res","key_clg_res")
# loop through the variables
for (i in 1:3){ 
  tbl_cross(row = key_vars[i],
            col = key_vars[i+1],
            percent = "row",
            missing = "ifany",
            data = fyrst %>% filter(!is.na(!!as.name(key_vars[i])),
                                    !!as.name(key_vars[i])!="dropout"
            )) %>%
    add_p()%>%
    as_tibble()%>%
    write.csv(file.path("output",paste0("continuation_from_",key_vars[i],".csv")))
}
# models
predictors <-
  paste(
    # child
    "age_res"
    ,"male_res"
    # parent
    ,"urbanhukou_res_father"
    ,"party_res_father"
    ,"education_years_res_father"
    ,"job_res_father"
    ,"ownership_res_father"
    ,sep = " + "
  )

formula <-
  formula(paste("education_years_res ~",
                predictors))
formula2 <-
  formula(paste("key_md_res == 'TRUE'~ key_pm_res +",
                predictors))
formula3 <-
  formula(paste("key_hi_res == 'TRUE'~ key_md_res +",
                predictors))
formula4 <-
  formula(paste("key_clg_res == 'TRUE'~ key_hi_res +",
                predictors))
formula5 <-
  formula(paste("education_years_res >=9 ~ key_pm_res +",
                predictors))
formula6 <-
  formula(paste("education_years_res >=12 ~ key_md_res +",
                predictors))
formula7 <-
  formula(paste("education_years_res >=15 ~ key_hi_res +",
                predictors))

jtools::export_summs(
  lm(
    formula,
    data = fyrst
  ),
  glm(formula2,
      data = fyrst %>% 
        filter(key_pm_res != "dropout") %>% 
        mutate(key_md_res = recode(key_md_res, dropout = "FALSE")),
      family = "binomial"
  ),
  glm(formula6,
      data = fyrst %>% 
        filter(education_years_res>=9) %>% 
        mutate(key_md_res = recode(key_md_res, dropout = "FALSE"),
               key_hi_res = recode(key_hi_res, dropout = "FALSE")),
      family = "binomial"
  ),
  glm(formula3,
      data = fyrst %>% 
        filter(key_md_res != "dropout") %>% 
        mutate(key_hi_res = recode(key_hi_res, dropout = "FALSE")),
      family = "binomial"
  ),
  glm(formula7,
      data = fyrst %>% 
        filter(education_years_res>=12) %>% 
        mutate(key_hi_res = recode(key_hi_res, dropout = "FALSE"),
               key_clg_res = recode(key_clg_res, dropout = "FALSE")),
      family = "binomial"
  ),
  glm(formula4,
      data = fyrst %>% 
        filter(key_hi_res != "dropout") %>% 
        mutate(key_clg_res = recode(key_clg_res, dropout = "FALSE")),
      family = "binomial"
  ),
  error_pos = "same",
  to.file = "xlsx",
  file.name = file.path("output",paste0("fyrst_education.xlsx"))
)
