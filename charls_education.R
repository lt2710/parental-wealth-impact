# education ---------------------------------- 
transmute(
  charls,
  education_diploma = case_when(
    education_years %in% c(0:9) ~ "ms&below",
    education_years %in% 12 ~ "high",
    education_years %in% c(15:21) ~ "bachelor&above",
  )
) %>% table()

charls = 
  charls %>% 
  mutate(asset_total_high = asset_total > median(charls$asset_total, na.rm = T),
         asset_total_quant = cut(asset_total, 
                                 breaks = quantile(charls$asset_total, probs = seq(0, 1, by = 0.25), na.rm = T)
                                 )
         )

varlist <-
  paste(
    # child
    "age"
    ,"male"
    # parent
    ,"urbanhukou_parent"
    ,"education_years_parent"
    ,"party_parent"
    ,"job_parent"
    ,"asset_total_quant"
    ,sep = " + "
  )

formula <-
  formula(paste("education_years ~",
                varlist))
formula2 <-
  formula(paste("(education_years>=9) ~",
                varlist))
formula3 <-
  formula(paste("(education_years>=12) ~",
                varlist))
formula4 <-
  formula(paste("(education_years>=15) ~",
                varlist))

jtools::export_summs(
  lm(
    formula,
    data = charls %>% filter()
  ),  
  glm(formula2,
      data = charls %>% 
        filter(education_years >=6
        ),
      family = "binomial"
  ),
  glm(formula3,
      data = charls %>% 
        filter(education_years >=9
        ),
      family = "binomial"
  ),
  glm(formula4,
      data = charls %>% 
        filter(education_years >=12
        ),
      family = "binomial"
  ),
  error_pos = "same",
  to.file = "xlsx",
  file.name = file.path("output",paste0("charls_education.xlsx"))
)