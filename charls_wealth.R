# income -----------------------------------------
varlist <-
  paste(
    # child
    "age"
    ,"urbanhukou"
    ,"education_years"
    ,"party"
    ,"income_logged"
    # spouse
    #, "education_years_spouse"
    # parent
    ,"education_years_parent"
    #,"job_parent"
    ,"asset_total_logged"
    ,sep = " + "
  )

formula <-
  formula(paste("income_logged ~",
                varlist))

jtools::export_summs(
  lm(
    formula,
    data = charls_married %>% filter(male == TRUE)
  ),
  lm(
    formula,
    data = charls_married %>% filter(male == FALSE)
  ),
  error_pos = "same",
  model.names = c("Husband","Wife"),
  to.file = "xlsx",
  file.name = file.path("tables",paste0("charls - ",str_remove_all(paste0(as.character(formula)[2],collapse = " ~ ")," "),".xlsx"))
)

####################################### home ############################
varlist <-
  paste(
    # child
    "age"
    ,"urbanhukou"
    ,"education_years"
    ,"party"
    ,"income_logged"
    # spouse
    #, "education_years_spouse"
    # parent
    ,"education_years_parent"
    #,"job_parent"
    ,"asset_total_logged"
    ,sep = " + "
  )

formula <-
  formula(paste("ownership ~",
                varlist))
formula2 <-
  formula(paste("homevalue_logged ~",
                varlist))

jtools::export_summs(
  glm(
    formula,
    data = charls_married %>% filter(male == TRUE),
    family = "binomial"
  ),
  glm(
    formula,
    data = charls_married %>% filter(male == FALSE),
    family = "binomial"
  ),
  lm(
    formula2,
    data = charls_married %>% filter(male == TRUE)
  ),
  lm(
    formula2,
    data = charls_married %>% filter(male == FALSE)
  ),
  error_pos = "same",
  model.names = c("Ownership Husband","Ownership Wife","Home Value Husband","Home Value Wife"),
  to.file = "xlsx",
  file.name = file.path("tables",paste0("charls - ",str_remove_all(paste0(as.character(formula)[2],collapse = " ~ ")," "),".xlsx"))
)
