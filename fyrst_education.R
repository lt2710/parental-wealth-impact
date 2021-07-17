# eda ------------------------------- 
# make data
plot_data = fyrst_mutated_filtered %>%
  mutate(
    ownership_res_father = ownership_res_father %>% pmin(2) %>% as.character(),
    diploma_res = case_when(
      education_years_res %in% c(0:6) ~ "1 <= Primary",
      education_years_res %in% 9 ~ "3 Junior High",
      education_years_res %in% 12 ~ "4 Senior High",
      education_years_res %in% 15 ~ "5 Some College",
      education_years_res %in% 16 ~ "6 Bachelors",
      education_years_res %in% c(19:22) ~ "7 Graduate School",
    ),
    diploma_parent = case_when(
      education_years_res_father %in% c(0:6) ~ "1 <= Primary",
      education_years_res_father %in% 9 ~ "3 Junior High",
      education_years_res_father %in% 12 ~ "4 Senior High",
      education_years_res_father %in% c(15:22) ~ "5 >= Bachelor"
    )
  )
# father wealth on child education
ggplot(aes(x = diploma_parent,
           y = metric,
           group = ownership_res_father,
           color = ownership_res_father),
       data = plot_data %>%
         group_by(ownership_res_father, diploma_parent) %>%
         summarise(num = n(),
                   metric = mean(education_years_res, na.rm = T)) %>%
         drop_na()) +
  geom_point() + 
  geom_line()  +
  xlab("Father's Education") +
  ylab("Average Years of Schooling") +
  labs(color = "Father is Homeowner") +
  theme_classic()

# key school 
# plot
ggplot(
  aes(
    x = diploma_res,
    y = metric,
    group = key_clg_res,
    color = key_clg_res
  ),
  data = rbind(
    plot_data %>%
      filter(education_years_res >= 15, education_years_res<= 19) %>%
      group_by(key_clg_res, diploma_res) %>%
      summarise(num = n(),
                metric = mean(hh_income, na.rm = T)) %>%
      drop_na()
  )
) +
  geom_point() + 
  geom_line()  +
  xlab("Education Level") +
  ylab("Average Household Income") +
  labs(color = "Degree from Key-Point Institution") +
  theme_classic()

# continuation rate
# calculate dropout
fyrst_continuation = fyrst_mutated_filtered %>%
  mutate(
    key_pm_res = ifelse(!is.na(key_pm_res),
                        key_pm_res,
                        ifelse(is.na(key_md_res),
                               "dropout",
                               NA)),
    key_md_res = ifelse(!is.na(key_md_res),
                        key_md_res,
                        ifelse(is.na(key_hi_res),
                               "dropout",
                               NA)),
    key_hi_res = ifelse(!is.na(key_hi_res),
                        key_hi_res,
                        ifelse(is.na(key_clg_res),
                               "dropout",
                               NA)),
    key_clg_res = ifelse(!is.na(key_clg_res),
                         key_clg_res,
                         "dropout")
    )
# loop through the variables
key_vars = c("key_pm_res","key_md_res","key_hi_res","key_clg_res")
for (i in 1:3){ 
  table = tbl_cross(row = key_vars[i],
            col = key_vars[i+1],
            percent = "row",
            missing = "ifany",
            data = fyrst_continuation %>%
              filter(!is.na(!!as.name(key_vars[i])),!!as.name(key_vars[i]) !=
                       "dropout"
            ))
  print(table)
  table %>%
    add_p()%>%
    as_tibble()%>%
    write.csv(file.path("output",paste0("continuation_from_",key_vars[i],".csv")))
}
# models
predictors <-
  paste(
    # child
    "age_res"
    ,"male_res"
    # parent
    ,"urbanhukou_res_father"
    ,"education_years_res_father"
    ,"party_res_father"
    ,"job_res_father"
    ,"ownership_res_father"
    ,sep = " + "
  )

formula1 <-
  formula(paste("education_years_res ~",
                predictors))
formula2 <-
  formula(paste("key_pm_res == 'TRUE'~ ",
                predictors))
formula3 <-
  formula(paste("key_md_res == 'TRUE'~ key_pm_res +",
                predictors))
formula4 <-
  formula(paste("key_hi_res == 'TRUE'~ key_md_res +",
                predictors))
formula5 <-
  formula(paste("education_years_res >=15 ~ ",
                predictors))
formula6 <-
  formula(paste("education_years_res >=15 ~ key_hi_res +",
                predictors))
formula7 <-
  formula(paste("key_clg_res == 'TRUE'~ key_hi_res +",
                predictors))

jtools::export_summs(
  lm(
    formula1,
    data = fyrst_mutated_filtered
  ),
  glm(formula5,
      data = fyrst_mutated_filtered,
      family = "binomial"
  ),
  glm(formula6,
      data = fyrst_mutated_filtered,
      family = "binomial"
  ),
  glm(formula7,
      data = fyrst_mutated_filtered,
      family = "binomial"
  ), 
  glm(formula4,
      data = fyrst_mutated_filtered,
      family = "binomial"
  ), 
  glm(formula3,
      data = fyrst_mutated_filtered,
      family = "binomial"
  ), 
  glm(formula2,
      data = fyrst_mutated_filtered,
      family = "binomial"
  ), 
  error_pos = "same",
  to.file = "xlsx",
  file.name = file.path("output",paste0("fyrst_education.xlsx"))
)