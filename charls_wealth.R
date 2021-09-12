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
charls = charls %>%
  mutate(
    num_home_cat = num_home%>%pmin(2)%>%factor(labels = c("0","1","2 or more")),
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
    ),
    # public sector manager job
    job_public = job == 7
  )
charls_married <- filter(charls
                         ,married
                         ,male
                         ,tier_parent%in%c("2nd tier","3rd tier")
                         #,tier_parent%in%c("4th tier")
                         )
charls_married = charls_married %>%
  mutate(
    asset_total_quant = cut(
      asset_total,
      breaks = quantile(
        charls_married$asset_total,
        probs = seq(0, 1, 0.25)
      ),
      labels = c("0-25%","25-50%","50-75%","75-100%"),
      include.lowest=TRUE
    )
  )
# ownership ---------------------------------------------------------------
charls_married %>%
  tbl_cross(row = "asset_total_quant",
            col = "ownership",
            percent = "row",
            missing = "ifany") %>%
  add_p()%>%
  as_tibble()%>%
  write.csv(file.path("output",paste0("charls_ownership.csv")))

charls_married %>%
  tbl_cross(row = "num_home_cat",
            col = "ownership",
            percent = "row",
            missing = "ifany") %>%
  add_p()%>%
  as_tibble()%>%
  write.csv(file.path("output",paste0("charls_ownership1.csv")))
# marriage homogamy ------------------------------------------------
charls_married %>%
  tbl_cross(row = "diploma",
            col = "diploma_spouse",
            percent = "cell",
            missing = "ifany") %>%
  add_p()%>%
  as_tibble()%>%
  write.csv(file.path("output",paste0("charls_homogamy.csv")))
cor(charls_married$education_years,
    charls_married$education_years_spouse,
    method = "spearman")

# home value ---------------------------------------------------------------
charls_for_plot = charls %>%
  filter(tier_parent != "1st tier",
         married,
         male) %>%
  mutate(tier = ifelse(
    tier_parent %in% c("2nd tier", "3rd tier"),
    "2. Other Large/Medium Cities",
    "3. Small Cities"
  ))
charls_plot = charls_for_plot %>%
  filter(homevalue_logged > 0,
         asset_total_logged > 0)
# net worth
ggplot(
  aes(x = asset_total_logged,
      y = homevalue_logged,
      color = tier),
  data = charls_plot
  ) +
  geom_jitter(alpha = 0.5,
              width = 0.25,
              height = 0.25) +
  geom_smooth(method = 'lm', se = FALSE) +
  xlab("Parental Net Worth (Logged)") +
  ylab("Total Value of Homes (Logged)") +
  labs(color = "City Size") +
  theme_classic()

# marriage home ------------------------------------------------
charls %>% 
  filter(married
         ,tier_parent%in%c("2nd tier","3rd tier")) %>%
  tbl_cross(row = "male",
            col = "marriagehome",
            percent = "row",
            missing = "ifany") %>%
  add_p()%>%
  as_tibble()%>%
  write.csv(file.path("output",paste0("charls_marriagehome.csv")))

# marriage gift ------------------------------------------------
charls %>% 
  filter(married
         ,tier_parent%in%c("2nd tier","3rd tier")) %>%
  tbl_cross(row = "male",
            col = "marriagegift",
            percent = "row",
            missing = "ifany") %>%
  add_p()%>%
  as_tibble()%>%
  write.csv(file.path("output",paste0("charls_marriagegift.csv")))

# plot
ggplot(aes(x = marriagehome,
           y = homevalue,
           fill = marriagegift),
       data = charls_married %>% filter(ownership)) +
  geom_boxplot(outlier.shape = NA) +  
  xlab("Receiving A Home as Gift") +
  ylab("Total Value of Homes (in CNY 10k)") +
  labs(fill = "Receiving Other Material Gifts") +
  scale_y_continuous(limits = c(0, 200)) +
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
formula3a <-
  formula(paste("ownership ~ asset_fin_logged + asset_fin_flag + asset_home_logged + asset_home_flag + asset_other_logged +",
                paste(predictors %>% 
                        setdiff("asset_total_logged"),
                      collapse = "+")))
formula4 <-
  formula(paste("homevalue_logged ~ ",
                paste(predictors,collapse = "+")))
formula5 <-
  formula(paste("homevalue_logged ~ asset_fin_logged + asset_fin_flag + asset_home_logged + asset_home_flag + asset_other_logged +",
                paste(predictors %>% 
                        setdiff("asset_total_logged"),
                      collapse = "+")))
formula6 <-
  formula(paste("homevalue_logged ~ marriagegift +  marriagehome +",
                paste(predictors,collapse = "+")))
formula7 <-
  formula(paste("homevalue_logged ~ marriagegiftvalue_flag + marriagegiftvalue_logged + marriagehomevalue_flag + marriagehomevalue_logged + ",
                paste(predictors,collapse = "+")))

jtools::export_summs(
  glm(
    formula3,
    data = charls_married,
    family = "binomial"
  ), 
  glm(
    formula3a,
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