# formats --------------------------
format_statistic = list(
  all_continuous() ~ c("{mean} ({sd})",
                       "{min},{median},{max}")
  ,
  all_categorical() ~ c("{n} / {N} ({p}%)")
  ,
  all_dichotomous() ~ c("{n} ({p}%)")
)
format_type = list(
  where(is.numeric) ~ "continuous2",
  where(is.character) ~ "categorical"
)

# charls --------------
# variables
vars = 
  c("age",
    "male",
    "married",
    "education_years",
    "urbanhukou",
    "party",
    "job_public",
    "income",
    "income_logged",
    "ownership",
    "homevalue",
    "homevalue_logged",
    "urbanhukou_parent",
    "party_parent",
    "education_years_parent",
    "job_parent",
    "asset_total",
    "asset_total_logged",
    "marriagehome")
charls_for_summary = charls %>%
  mutate(job_public = job == 7) %>%
  select(all_of(vars)) %>%
  mutate_at(
    c("asset_total_logged", "asset_total", "education_years"),
    as.numeric
  )
# make table
cbind(
  gtsummary::tbl_summary(
    charls_for_summary,
    type = format_type,
    statistic = format_statistic,
    missing = "no"
  ) %>% as.tibble(),
  gtsummary::tbl_summary(
    charls_for_summary %>% filter(male, married),
    type = format_type,
    statistic = format_statistic,
    missing = "no"
  ) %>% as.tibble()
) %>% write.csv(file.path("output",paste0("charls_summary.csv")))

# fyrst all --------------
# variables
vars = 
  c("age_res",
    "male_res",
    "married_res",
    "education_years_res",
    "urbanhukou_res",
    "party_res",
    "job_res",
    "hh_income",
    "hh_income_logged",
    "ownership",
    "homevalue",
    "homevalue_logged",
    "secondhome",
    "homevalue_total",
    "homevalue_total_logged",
    "ownership_vehicle",
    "urbanhukou_res_father",
    "party_res_father",
    "education_years_res_father",
    "job_res_father",
    "ownership_res_father")
fyrst_for_summary = fyrst %>%
  select(all_of(vars))
gtsummary::tbl_summary(
  fyrst_for_summary,
  type = format_type,
  statistic = format_statistic,
  missing = "no"
) %>% 
  as.tibble() %>% 
  write.csv(file.path("output", paste0("fyrst_summary_all.csv")))

# fyrst married --------------
# variables
vars = 
  c("age_husb", 
    "education_years_husb",
    "urbanhukou_husb",
    "party_husb",
    "job_husb", 
    "hh_income",
    "hh_income_logged",
    "ownership",
    "homevalue",
    "homevalue_logged",
    "secondhome",
    "homevalue_total",
    "homevalue_total_logged",
    "ownership_vehicle",
    "urbanhukou_husb_father",
    "party_husb_father",
    "education_years_husb_father",
    "job_husb_father",
    "ownership_husb_father")
fyrst_for_summary = fyrst_married %>%
  select(all_of(vars))
gtsummary::tbl_summary(
  fyrst_for_summary,
  type = format_type,
  statistic = format_statistic,
  missing = "no"
) %>% 
  as.tibble() %>% 
  write.csv(file.path("output", paste0("fyrst_summary_married.csv")))