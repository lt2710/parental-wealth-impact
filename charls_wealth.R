charls_married <- filter(charls
                         ,married
                         ,male
                         ) 
charls_married = charls_married %>%
  mutate(
    diploma = case_when(
      education_years %in% c(0:12) ~ "1 up to high",
      education_years %in% c(15:22) ~ "2 tertiary & above",
    ),
    diploma_spouse = case_when(
      education_years_spouse %in% c(0:12) ~ "1 up to high",
      education_years_spouse %in% c(15:22) ~ "2 tertiary & above",
    ),
    income_cat = case_when(
      income %in% c(0:25000) ~ "1",
      income %in% c(0:40000) ~ "2",
      income %in% c(75000) ~ "3",
      income %in% c(125000:500000) ~ "4",
    )
    ,
    income_high = income >= 75000
    ,
    asset_total_high = asset_total > quantile(
      charls_married$asset_total,
      probs = 0.75,
      na.rm = T
    )
    ,
    # public sector manager job
    job_public = job == 7
  )
# eda 
# ownership ---------------------------------------------------------------
charls_married %>%
  tbl_cross(row = "asset_total_quant",
            col = "ownership",
            percent = "row",
            missing = "ifany") %>%
  add_p()%>%
  as_tibble()%>%
  write.csv(file.path("output",paste0("charls_ownership.csv")))
# marriage homogamy ------------------------------------------------
charls_married %>%
  tbl_cross(row = "diploma",
            col = "diploma_spouse",
            percent = "cell",
            missing = "ifany") %>%
  add_p()%>%
  as_tibble()%>%
  write.csv(file.path("output",paste0("charls_homogamy.csv")))

# home value ---------------------------------------------------------------
# make data
plot_data = charls_married %>%
  filter(ownership) %>% 
  group_by(asset_total_quant, income_cat) %>%
  summarise(num = n(),
            metric = median(homevalue, na.rm = T)) %>%
  drop_na()
# plot
ggplot(aes(x = income_cat,
           y = metric,
           group = asset_total_quant,
           color = asset_total_quant),
       data = plot_data) +
  geom_point() + 
  geom_line()  +
  xlab("Income Level") +
  ylab("Median Home Wealth") +
  labs(color = "Parental Net Worth") +
  theme_classic()

# marriage home ------------------------------------------------
charls %>% 
  filter(married) %>%
  tbl_cross(row = "male",
            col = "marriagehome",
            percent = "row",
            missing = "ifany") %>%
  add_p()%>%
  as_tibble()%>%
  write.csv(file.path("output",paste0("charls_marriagehome.csv")))

# modeling ------------------------------------------------------
predictors <-
  c(
    # child
    "age"
    ,"urbanhukou"
    ,"education_years"
    ,"job_public"
    ,"party"
    ,"income_logged"
    # parent
    ,"education_years_parent"
    ,"asset_total_logged"
  )

formula1 <-
  formula(paste("income_logged ~",
                paste(predictors %>% setdiff("income_logged"),collapse = "+")))
formula2 <-
  formula(paste("marriagehome == TRUE ~",
                paste(predictors,collapse = "+")))
formula3 <-
  formula(paste("ownership ~",
                paste(predictors,collapse = "+")))
formula4 <-
  formula(paste("homevalue_logged ~ ",
                paste(predictors,collapse = "+")))
formula5 <-
  formula(paste("homevalue_logged ~ marriagehome + ",
                paste(predictors,collapse = "+")))

jtools::export_summs(
  glm(
    formula2,
    data = charls_married,
    family = "binomial"
  ), 
  glm(
    formula3,
    data = charls_married,
    family = "binomial"
  ), 
  lm(
    formula4,
    data = charls_married %>% filter(ownership)
  ),
  lm(
    formula5,
    data = charls_married %>% filter(ownership)
  ),
  error_pos = "same", 
  to.file = "xlsx",
  file.name = file.path("output",paste0("charls_wealth.xlsx"))
)

# vglm(formula5,
#      tobit(Lower  = 1),
#      data = charls_married %>% filter(ownership)) %>%
#   summary()