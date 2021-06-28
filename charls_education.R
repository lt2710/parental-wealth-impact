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
# make data
plot_data = charls %>%
  mutate(
    diploma_parent = case_when(
      education_years_parent %in% c(0:5) ~ "1 < Primary",
      education_years_parent %in% 6 ~ "2 Primary",
      education_years_parent %in% 9 ~ "3 Junior High",
      education_years_parent %in% c(12:22) ~ "4 Senior High >"
    )
  ) %>%
  group_by(asset_total_quant, diploma_parent) %>%
  summarise(num = n(),
            metric = mean(education_years, na.rm = T)) %>%
  drop_na()
# plot
ggplot(aes(x = diploma_parent,
           y = metric,
           group = asset_total_quant,
           color = asset_total_quant),
       data = plot_data) +
  geom_point() + 
  geom_line()  +
  xlab("Father's Education") +
  ylab("Average Years of Schooling") +
  labs(color = "Parental Net Worth") +
  theme_classic()

# modeling -----------------------------
# make models
varlist <-
  paste(
    # child
    "age"
    ,"male"
    # parent
    ,"urbanhukou_parent"
    ,"education_years_parent"
    ,"party_parent"
    ,"job_parent"
    ,"asset_total_logged"
    ,sep = " + "
  )

model_list =
  list(
    lm(formula(paste("education_years ~",
                     varlist)),
       data = charls)
    ,glm(
      formula(paste("(education_years>=15) ~",
                    varlist)),
      data = charls %>%
        filter(education_years >= 12),
      family = "binomial"
    )
    ,glm(formula(paste("(education_years>=12) ~ ",
                       varlist)),
         data = charls %>%
           filter(education_years >= 9),
         family = "binomial")
    ,glm(formula(paste("(education_years>=9) ~ ",
                       varlist)),
         data = charls %>%
           filter(education_years >= 6),
         family = "binomial")
  )

# tabular summary
jtools::export_summs(
  model_list,
  error_pos = "same",
  to.file = "xlsx",
  file.name = file.path("output",paste0("charls_education.xlsx"))
)

# plot summary
# list_coef = c(
#   "25~50%" = "asset_total_quant(1.04e+05,3.09e+05]"
#   ,"50%~75%" = "asset_total_quant(3.09e+05,7.42e+05]"
#   ,"75>%" = "asset_total_quant(7.42e+05,2.14e+07]"
# )
# list_modelnames = c(
#   "M2 (Adjusted)"
#   ,"M1 (Bivariate)"
# )
# 
# for (i in c(2,4,6)){
#   plot = jtools::plot_summs(
#     list(model_list[[i]],model_list[[i-1]])
#     ,coefs = list_coef
#     ,legend.title = ""
#     ,ci_level = 0.95 
#     ,point.shape = FALSE
#     ,model.names = list_modelnames
#   ) +
#     xlab("Offspring Years of Education") +
#     ylab("Parental Net Worth Quantile") +
#     theme_classic()
#   print(plot)
# }