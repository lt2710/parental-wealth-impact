fyrst_married = fyrst_mutated_filtered %>%
  filter(married_res == TRUE) 
plot_data = fyrst_married %>%
  mutate(
    income_quant = cut(
      hh_income,
      breaks = quantile(
        fyrst_married$hh_income,
        probs = seq(0, 1, 0.25),
        na.rm = T
      )
    )
  )
# ownership------------------------------------------------------------
plot_data %>%
  tbl_cross(row = "ownership_husb_father_cat",
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
cor(plot_data$education_years_husb,
    plot_data$education_years_wife,
    method = "spearman")

plot_data %>%
  tbl_cross(row = "diploma_husb_father",
            col = "diploma_wife_father",
            percent = "cell",
            missing = "ifany") %>%
  add_p()%>%
  as_tibble()%>%
  write.csv(file.path("output",paste0("fyrst_homogamy_edu_parent.csv")))
cor(plot_data$education_years_husb_father,
    plot_data$education_years_wife_father,
    method = "spearman")

plot_data %>%
  tbl_cross(row = "ownership_husb_father_cat",
            col = "ownership_wife_father_cat",
            percent = "cell",
            missing = "ifany") %>%
  add_p()%>%
  as_tibble()%>%
  write.csv(file.path("output",paste0("fyrst_homogamy_home.csv")))
cor(plot_data$ownership_husb_father,
    plot_data$ownership_wife_father,
    method = "spearman")

# second home ------------------------------------------------
plot_data %>%
  filter(ownership) %>%
  tbl_cross(row = "ownership_husb_father_cat",
            col = "secondhome",
            percent = "row",
            missing = "ifany") %>%
  add_p()%>%
  as_tibble()%>%
  write.csv(file.path("output",paste0("fyrst_secondhome.csv")))

# plot
plot_data = fyrst_married %>%
  filter(ownership) %>%
  transmute(
    secondhome = secondhome,
    `Owner-Occupied Home` = homevalue,
    `Additional Home` = homevalue_other
  ) %>%
  gather(key = "varname",
         value = "value",-secondhome) %>%
  group_by(secondhome, varname) %>%
  summarise(mean_value = mean(value)) %>%
  ungroup()

ggplot(aes(x = secondhome,
           y = mean_value,
           fill = varname),
       data = plot_data) +
  geom_bar(position="stack", 
           stat="identity",
           color = "black") +
  xlab("Having Second/Additional Home") +
  ylab("Total Value of Homes (Group Average. In CNY 10k)") +
  labs(fill = "Asset Type") +
  theme_classic()

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
  formula(paste("homevalue_total_logged ~ secondhome +",
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

library(lavaan)
library(semPlot)
# model 1 - canonical mimic model
model1 <- paste(
  # measurement model
  "living_standard =~ ownership_vehicle + ownership + subj_status",
  paste("living_standard ~",
        paste(predictors, collapse = "+")),
  sep = " \n"
)

# run lavaan
fit1 <- lavaan::sem(model1, data=fyrst_married)
summary(fit1)
jtools::export_summs(
  fit1,
  error_pos = "same",
  to.file = "xlsx",
  file.name = file.path("output",paste0("fyrst_wealth_sem.xlsx"))
  )
semPlot::semPaths(fit1, 
                  what = "path", 
                  whatLabels = "est", 
                  style = "OpenMx",
                  layout = "tree",
                  intercepts = F,
                  residuals = F, 
                  thresholds = F)