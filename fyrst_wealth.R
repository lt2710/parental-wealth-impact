fyrst_married = fyrst_mutated_filtered %>%
  filter(married_res == TRUE) 
fyrst_married = fyrst_married %>%
  mutate(
    diploma_husb = case_when(
      education_years_husb %in% c(0:12) ~ "1 <= Senior High",
      education_years_husb %in% c(15:22) ~ "2 Tertiary & Above",
    ),
    diploma_wife = case_when(
      education_years_wife %in% c(0:12) ~ "1 <= Senior High",
      education_years_wife %in% c(15:22) ~ "2 Tertiary & Above",
    ),
    income_quant = cut(
      hh_income,
      breaks = quantile(
        fyrst_married$hh_income,
        probs = seq(0, 1, 0.2),
        na.rm = T
      )
    )
    ,
    income_high = hh_income > quantile(fyrst_married$hh_income,
                                       probs = 0.8,
                                       na.rm = T)
  )
# ownership------------------------------------------------------------
fyrst_married %>%
  tbl_cross(row = "ownership_husb_father",
            col = "ownership",
            percent = "row",
            missing = "ifany") %>%
  add_p()%>%
  as_tibble()%>%
  write.csv(file.path("output",paste0("fyrst_ownership.csv")))

# marriage homogamy -----------------------------------------------------------
fyrst_married %>%
  tbl_cross(row = "diploma_husb",
            col = "diploma_wife",
            percent = "cell",
            missing = "ifany") %>%
  add_p()%>%
  as_tibble()%>%
  write.csv(file.path("output",paste0("fyrst_homogamy.csv")))

select(
  fyrst_married,
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
plot_data = fyrst_married %>%
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
       data = plot_data) +
  geom_point() + 
  geom_line()  +
  xlab("Income Level") +
  ylab("Median Home Wealth") +
  labs(color = "Parental Home Ownership") +
  theme_classic()

# second home ------------------------------------------------
fyrst_married %>%
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

formula <-
  formula(paste("hh_income_logged ~",
                paste(predictors %>% setdiff("hh_income_logged"), collapse = "+")))
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
formula6 <-
  formula(paste("homevalue_total_logged ~",
                paste(predictors, collapse = "+"))) 
formula7 <-
  formula(paste("homevalue_total_logged ~",
                paste(predictors %>% c("secondhome"), collapse = "+"))) 

jtools::export_summs(
  # lm(
  #   formula,
  #   data = fyrst_married
  # ),
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