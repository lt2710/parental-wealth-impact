# number of homes -----------------------------
joint_home = rbind(
  fyrst_married %>%
    transmute(
      tier = "1. Shanghai",
      homevalue = homevalue,
      num_home_cat = ownership_husb_father_cat
    ),
  charls_for_plot %>%
    select(tier,
           homevalue,
           num_home_cat)
)

joint_home %>% 
  filter(homevalue>0) %>%
  group_by(tier) %>%
  summarise(n(),
            mean(homevalue),
            sd(homevalue),
            quantile(homevalue, 0.25),
            quantile(homevalue, 0.5),
            quantile(homevalue, 0.75)
            ) %>%
  as_tibble()%>%
  write.csv(file.path("output",paste0("joint_homevalue.csv")))

# num of homes on homevalue-----------------------------
joint_homevalue = rbind(
  joint_home %>%
    filter(homevalue>0),
  charls_plot %>%
    select(tier,
           homevalue,
           num_home_cat)
)
# plot
ggplot(aes(x = num_home_cat,
           y = homevalue,
           fill = tier),
       data = joint_homevalue) +
  geom_boxplot() +
  xlab("Number of Parental Homes") +
  ylab("Total Value of Homes (in CNY 10k)") +
  scale_y_continuous(limits = c(0, 180)) +
  theme_classic() +
  theme(legend.position = "none") +
  facet_wrap(~tier)

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

# wealth by tier ---------------------------------------------------------------
# make data
plot_data = charls_married %>%
  filter(ownership, tier_parent!= "1st tier") %>%
  mutate(tier_parent = recode(tier_parent,
                              "2nd tier" = "2 Large and Medium Cities",
                              "3rd tier" = "2 Large and Medium Cities",
                              "4th tier" = "3 Small Cities")) %>%
  group_by(tier_parent) %>%
  summarise(num = n(), 
            "Median Home Wealth" = median(homevalue*10000, na.rm = T),
            "Median Parental Net Wealth" = median(asset_total, na.rm = T)) %>%
  add_row(tier_parent= "1 Shanghai",
          num = nrow(fyrst_married),
          "Median Home Wealth" = median(fyrst_married$homevalue_total*10000, na.rm = T) * 1.67,
          "Median Parental Net Wealth" =  NA) %>%
  gather(key = "type", value = "metric", - num, -tier_parent)
# plot
ggplot(aes(x = tier_parent,
           y = metric,
           group = type,
           color = type),
       data = plot_data) +
  geom_point() + 
  geom_line()  +
  xlab("") +
  ylim(0, 1800000) +
  ylab("(in CNY)") +
  labs(color = "") +
  theme_classic()