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
    "asset_fin",
    "asset_fin_logged",
    "asset_home",
    "asset_home_logged",
    "asset_other",
    "asset_other_logged",
    "marriagehome",
    "marriagehomevalue",
    "marriagehomevalue_logged",
    "marriagegift",
    "marriagegiftvalue",
    "marriagegiftvalue_logged",
    "tier_parent")
charls_for_summary = charls %>%
  filter(male, married) %>%
  mutate(job_public = job == 7) %>%
  select(all_of(vars)) %>%
  mutate_at(
    c(
      "asset_total",
      "asset_total_logged",
      "asset_fin",
      "asset_fin_logged",
      "asset_home",
      "asset_home_logged",
      "asset_other",
      "asset_other_logged",
      "education_years"
    ),
    as.numeric
  )
# make table
cbind(
  gtsummary::tbl_summary(
    charls_for_summary %>% filter(tier_parent%in%c("2nd tier","3rd tier")),
    type = format_type,
    statistic = format_statistic,
    missing = "no"
  ) %>% as.tibble(),
  gtsummary::tbl_summary(
    charls_for_summary %>% filter(tier_parent%in%c("4th tier")),
    type = format_type,
    statistic = format_statistic,
    missing = "no"
  ) %>% as.tibble()
) %>% write.csv(file.path("output",paste0("charls_summary.csv")))

# fyrst married --------------
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
    "homevalue_other",
    "homevalue_other_logged",
    "homevalue_total",
    "homevalue_total_logged",
    "ownership_vehicle",
    "subj_status",
    "consumption",
    "consumption_logged",
    "urbanhukou_res_father",
    "party_res_father",
    "education_years_res_father",
    "job_res_father",
    "ownership_res_father")
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