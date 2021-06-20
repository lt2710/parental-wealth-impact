# income ----------------------------------
predictors <-
  paste(
    "age_husb"
    ,"urbanhukou_husb"
    #,"urbanhukou_wife"
    ,"education_years_husb"
    #,"education_years_wife"
    ,"job_husb"
    #,"job_wife"
    ,"party_husb"
    #,"party_wife"
    ,"education_years_husb_father"
    ,"education_years_wife_father"
    #,"job_husb_father"
    #,"job_wife_father"
    ,"ownership_husb_father"
    ,"ownership_wife_father"
    ,sep = " + "
  )

formula <-
  formula(paste("hh_income_logged ~",
                predictors))

jtools::export_summs(
  lm(
    formula,
    data = fyrst_married
  ),
  error_pos = "same",
  model.names = c("HusbandWife"),
  to.file = "xlsx",
  file.name = file.path("tables",paste0("fyrst - ",str_remove_all(paste0(as.character(formula)[2],collapse = " ~ ")," "),".xlsx"))
)

# wealth ---------------------------------------------
predictors <-
  paste(
    "age_husb"
    ,"urbanhukou_husb"
    #,"urbanhukou_wife"
    ,"education_years_husb"
    #,"education_years_wife"
    ,"job_husb"
    #,"job_wife"
    ,"party_husb"
    #,"party_wife"
    ,"hh_income_logged"
    ,"education_years_husb_father"
    ,"education_years_wife_father"
    #,"job_husb_father"
    #,"job_wife_father"
    ,"ownership_husb_father"
    ,"ownership_wife_father"
    ,sep = " + "
  )

formula <-
  formula(paste("ownership_vehicle ~",
                predictors))
formula2 <-
  formula(paste("ownership ~",
                predictors))
formula3 <-
  formula(paste("secondhome ~",
                predictors))
formula4 <-
  formula(paste("homevalue_total_logged ~",
                predictors))

jtools::export_summs(
  glm(
    formula,
    data = fyrst_married,
    family = "binomial"
  ),
  glm(
    formula2,
    data = fyrst_married,
    family = "binomial"
  ),
  glm(
    formula3,
    data = fyrst_married %>% filter(ownership == TRUE),
    family = "binomial"
  ),
  lm(
    formula4,
    data = fyrst_married %>% filter(ownership == TRUE)
  ),
  error_pos = "same",
  model.names = c(
    "Vehicle Ownership",
    "Home Ownership",
    "Having Second Home",
    "Total Home Asset"
  ), 
  to.file = "xlsx",
  file.name = file.path("tables",paste0("fyrst - ",str_remove_all(paste0(as.character(formula)[2],collapse = " ~ ")," "),".xlsx"))
)

