fyrst_married = fyrst_mutated_filtered %>%
  filter(married_res == TRUE) 
plot_data = fyrst_married %>%
  mutate(
    ownership_husb_father = ownership_husb_father %>% pmin(2) %>% as.character(),
    ownership_wife_father = ownership_wife_father %>% pmin(2) %>% as.character(),
    diploma_husb = case_when(
      education_years_husb %in% c(0:12) ~ "1 <= Senior High",
      education_years_husb %in% c(15:22) ~ "2 Tertiary & Above",
    ),
    diploma_husb_father = case_when(
      education_years_husb_father %in% c(0:9) ~ "1 <= Junior High",
      education_years_husb_father %in% c(11:22) ~ "2 Senior High & Above",
    ),
    diploma_wife = case_when(
      education_years_wife %in% c(0:12) ~ "1 <= Senior High",
      education_years_wife %in% c(15:22) ~ "2 Tertiary & Above",
    ),
    diploma_wife_father = case_when(
      education_years_wife_father %in% c(0:9) ~ "1 <= Junior High",
      education_years_wife_father %in% c(11:22) ~ "2 Senior High & Above",
    ),
    income_quant = cut(
      hh_income,
      breaks = quantile(
        fyrst_married$hh_income,
        probs = seq(0, 1, 0.25),
        na.rm = T
      )
    )
    ,
    income_high = hh_income > quantile(fyrst_married$hh_income,
                                       probs = 0.8,
                                       na.rm = T)
  )
# ownership------------------------------------------------------------
plot_data %>%
  tbl_cross(row = "ownership_husb_father",
            col = "ownership",
            percent = "row",
            missing = "ifany") %>%
  add_p()%>%
  as_tibble()%>%
  write.csv(file.path("output",paste0("fyrst_ownership.csv")))

# marriage homogamy -----------------------------------------------------------
plot_data %>%
  tbl_cross(row = "diploma_husb",
            col = "diploma_wife",
            percent = "cell",
            missing = "ifany") %>%
  add_p()%>%
  as_tibble()%>%
  write.csv(file.path("output",paste0("fyrst_homogamy_edu.csv")))

plot_data %>%
  tbl_cross(row = "diploma_husb_father",
            col = "diploma_wife_father",
            percent = "cell",
            missing = "ifany") %>%
  add_p()%>%
  as_tibble()%>%
  write.csv(file.path("output",paste0("fyrst_homogamy_edu_parent.csv")))

select(
  plot_data,
  ownership_husb_father,
  ownership_wife_father
) %>%
  tbl_cross(row = "ownership_husb_father",
            col = "ownership_wife_father",
            percent = "cell",
            missing = "ifany") %>%
  add_p()%>%
  as_tibble()%>%
  write.csv(file.path("output",paste0("fyrst_homogamy_home.csv")))

# home value ------------------------------------------------------------
# make data
plot_data_small = plot_data %>%
  filter(!is.na(homevalue)) %>%
  group_by(ownership_husb_father, income_quant) %>%
  summarise(num = n(),
            metric = median(homevalue, na.rm = T)) %>%
  drop_na()
# plot
ggplot(aes(x = income_quant,
           y = metric,
           group = ownership_husb_father,
           color = ownership_husb_father),
       data = plot_data_small) +
  geom_point() + 
  geom_line()  +
  xlab("Income Level") +
  ylab("Median Home Wealth") +
  labs(color = "Number of Parental Homes") +
  theme_classic()

# second home ------------------------------------------------
plot_data %>%
  filter(ownership) %>%
  tbl_cross(row = "ownership_husb_father",
            col = "secondhome",
            percent = "row",
            missing = "ifany") %>%
  add_p()%>%
  as_tibble()%>%
  write.csv(file.path("output",paste0("fyrst_secondhome.csv")))

# modeling ----------------------------------
predictors <-
  c(
    "age_husb"
    ,"urbanhukou_husb"
    ,"education_years_husb"
    ,"job_husb"
    ,"party_husb"
    ,"hh_income_logged"
    ,"education_years_husb_father"
    ,"ownership_husb_father"
  )

formula0 <-
  formula(paste("subj_status ~",
                paste(predictors, collapse = "+")))
formula1 <-
  formula(paste("consumption_logged ~",
                paste(predictors, collapse = "+")))
formula2 <-
  formula(paste("ownership_vehicle ~",
                paste(predictors, collapse = "+")))
formula3 <-
  formula(paste("ownership ~",
                paste(predictors, collapse = "+")))
formula4 <-
  formula(paste("homevalue_logged ~",
                paste(predictors, collapse = "+"))) 
formula5 <-
  formula(paste("secondhome ~",
                paste(predictors, collapse = "+"))) 
formula5_5 <-
  formula(paste("homevalue_other_logged ~",
                paste(predictors, collapse = "+"))) 
formula6 <-
  formula(paste("homevalue_total_logged ~",
                paste(predictors, collapse = "+"))) 
formula7 <-
  formula(paste("homevalue_total_logged ~ homevalue_other_logged +",
                paste(predictors, collapse = "+"))) 

jtools::export_summs(
  lm(
    formula0,
    data = fyrst_married
  ),
  lm(
    formula1,
    data = fyrst_married
  ),
  glm(
    formula2,
    data = fyrst_married,
    family = "binomial"
  ),
  glm(
    formula3,
    data = fyrst_married,
    family = "binomial"
  ),
  lm(
    formula4,
    data = fyrst_married %>% filter(ownership)
  ),
  glm(
    formula5,
    data = fyrst_married %>% filter(ownership),
    family = "binomial"
  ),
  lm(
    formula5_5,
    data = fyrst_married %>% filter(secondhome)
  ),
  lm(
    formula6,
    data = fyrst_married %>% filter(ownership)
  ),
  lm(
    formula7,
    data = fyrst_married %>% filter(ownership)
  ),
  error_pos = "same",
  to.file = "xlsx",
  file.name = file.path("output",paste0("fyrst_wealth.xlsx"))
)

# vglm(formula5,
#      tobit(Lower  = 1),
#      data = fyrst_married) %>%
#   summary()