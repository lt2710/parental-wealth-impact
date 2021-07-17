## ----setup-------------------------------------------------------------------------------------------------------------------------
#set working directory
path_to_code<-rstudioapi::getActiveDocumentContext()$path
main_directory<-strsplit(path_to_code,"/[a-zA-Z0-9_-]*.R$")[[1]]
setwd(main_directory)
#Set time variables to debug date transformation
Sys.setlocale("LC_TIME", "C")
Sys.setenv(TZ="Europe/Berlin")

## ----packages, message = FALSE-----------------------------------------------------------------------------------------------------
# Load packages.
packages <- c(
  "tidyverse",
  "data.table",
  # below is for output summary
  "VGAM",
  "jtools",
  "huxtable",
  "officer",
  "flextable",
  "gtsummary"
)
packages <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x)
      library(x, character.only = TRUE)
    }
  }
)
select <- dplyr::select
# eda ---------------------------------- 
load("output/charls.RData")

charls_married <- filter(charls
                         ,married
                         ,male
                         ,tier_parent%in%c("2nd tier","3rd tier")
                         #,tier_parent%in%c("4th tier")
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
      income %in% c(75000:75000) ~ "3",
      income %in% c(125000:500000) ~ "4",
    )
    ,
    # public sector manager job
    job_public = job == 7
  )
# parental wealth ---------------------------------------------------------------
# make data
plot_data = charls_married %>%
  group_by(asset_total_quant) %>%
  summarise(num = n(),
            median_asset_fin = median(asset_fin, na.rm = T),
            median_asset_other = median(asset_other, na.rm = T),
            median_asset_home = median(asset_home, na.rm = T),
            median_asset_total = median(asset_total, na.rm = T)) %>%
  gather(key = "type", value = "metric", - num, -asset_total_quant) %>%
  drop_na()
# plot
ggplot(aes(x = asset_total_quant,
           y = metric,
           group = type,
           color = type),
       data = plot_data) +
  geom_point() + 
  geom_line()  +
  xlab("Parental Net Worth Quantile") +
  ylab("Median of Wealth Components (CNY)") +
  labs(color = "") +
  theme_classic()
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

# make data
plot_data = charls_married %>%
  filter(ownership) %>% 
  group_by(marriagehome, income_cat) %>%
  summarise(num = n(),
            metric = median(homevalue, na.rm = T)) %>%
  drop_na()
# plot
ggplot(aes(x = income_cat,
           y = metric,
           group = marriagehome,
           color = marriagehome),
       data = plot_data) +
  geom_point() + 
  geom_line()  +
  xlab("Income Level") +
  ylab("Median Home Wealth") +
  labs(color = "Gifted Home Upon Marriage") +
  theme_classic()

# make data
plot_data = charls_married %>%
  filter(marriagehome) %>% 
  group_by(asset_total_quant) %>%
  summarise(num = n(),
            median_homevalue = median(homevalue, na.rm = T),
            median_marriagehomevalue = median(marriagehomevalue, na.rm = T),
            marriagegiftvalue = median(marriagegiftvalue, na.rm = T)) %>%
  gather(key = "type", value = "metric", - num, -asset_total_quant) %>%
  drop_na()
# plot
ggplot(aes(x = asset_total_quant,
           y = metric,
           group = type,
           color = type),
       data = plot_data) +
  geom_point() + 
  geom_line()  +
  xlab("Parental Net Worth Level") +
  ylab("Median Home Wealth") +
  labs(color = "") +
  theme_classic()

# marriage gift ------------------------------------------------
charls %>% 
  filter(married) %>%
  tbl_cross(row = "male",
            col = "marriagegift",
            percent = "row",
            missing = "ifany") %>%
  add_p()%>%
  as_tibble()%>%
  write.csv(file.path("output",paste0("charls_marriagegift.csv")))

# make data
plot_data = charls_married %>%
  filter(ownership) %>% 
  group_by(marriagegift, income_cat) %>%
  summarise(num = n(),
            metric = median(homevalue, na.rm = T)) %>%
  drop_na()
# plot
ggplot(aes(x = income_cat,
           y = metric,
           group = marriagegift,
           color = marriagegift),
       data = plot_data) +
  geom_point() + 
  geom_line()  +
  xlab("Income Level") +
  ylab("Median Home Wealth") +
  labs(color = "Other Marriage Gifts") +
  theme_classic()

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
  formula(paste("marriagegift == TRUE ~",
                paste(predictors,collapse = "+")))
formula1_2 <-
  formula(paste("marriagegiftvalue_logged ~",
                paste(predictors,collapse = "+")))
formula2 <-
  formula(paste("marriagehome == TRUE ~",
                paste(predictors,collapse = "+")))
formula2_2 <-
  formula(paste("marriagehomevalue_logged ~",
                paste(predictors,collapse = "+")))
formula3 <-
  formula(paste("ownership ~",
                paste(predictors,collapse = "+")))
formula4 <-
  formula(paste("homevalue_logged ~ ",
                paste(predictors,collapse = "+")))
formula5 <-
  formula(paste("homevalue_logged ~ asset_fin_logged + asset_fin_flag + asset_home_logged + asset_other_logged +",
                paste(predictors %>% 
                        setdiff(c("asset_total_logged",
                                  "asset_total_logged:tier_4th")),
                      collapse = "+")))
formula6 <-
  formula(paste("homevalue_logged ~ marriagegift +  marriagehome +",
                paste(predictors,collapse = "+")))
formula7 <-
  formula(paste("homevalue_logged ~ marriagegiftvalue_logged + marriagehomevalue_logged + ",
                paste(predictors,collapse = "+")))

jtools::export_summs(
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
  glm(
    formula1,
    data = charls_married,
    family = "binomial"
  ), 
  lm(
    formula1_2,
    data = charls_married %>% filter(marriagegift)
  ),
  glm(
    formula2,
    data = charls_married,
    family = "binomial"
  ), 
  lm(
    formula2_2,
    data = charls_married %>% filter(marriagehome)
  ),
  lm(
    formula6,
    data = charls_married %>% filter(ownership)
  ),
  lm(
    formula7,
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