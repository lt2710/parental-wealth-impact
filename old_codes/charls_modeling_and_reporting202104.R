## ----setup----------------------------------------------------------------------------------------
path_to_code<-rstudioapi::getActiveDocumentContext()$path
main_directory<-strsplit(path_to_code,"/[a-zA-Z0-9_-]*.Rmd$")[[1]]
setwd(main_directory)
#Set up default knitr chunk options
library("knitr")
knitr::opts_chunk$set(
  echo = TRUE,
  eval = TRUE,
  message = FALSE,
  warning = FALSE,
  # fig.height = 9,
  # fig.width = 15,
  # fig.align = "center", 
  cache = TRUE,
  cache.lazy = FALSE
)
options(htmltools.dir.version = FALSE)


## ----packages, message = FALSE, warning = FALSE, echo=FALSE---------------------------------------
# Load packages.
packages <- c(
  "knitr",
  "tidyverse",
  "finalfit",
  "MissMech",
  "mice",
  "qwraps2",
  "jtools",
  "VGAM"
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


## ----filter and select final data,echo=TRUE, warning=FALSE----------------------------------------
load("output/merged.RData")
dta <- merged 


## -------------------------------------------------------------------------------------------------
dta_mutated <-
  dta %>%
  mutate(
    asset_fin = pmax(asset_fin, 0),
    asset_total = pmax(asset_total, 0),
    income_logged = log(income + 50),
    # ownership = (ownership=="1 Yes")%>%as.numeric(),
    marriagehome = (marriagehome %>%replace_na("2 No") == "1 Yes"),
    job_child_manager = (job_child %>% replace_na("5 Agricultural, Forestry, Husbandry and Fishery Producer") == "1 Manager") 
  ) %>%
  mutate_at(
    c(
      "homevalue",
      "asset_fin",
      "asset_land",
      "asset_durable_fixed",
      "asset_home",
      "asset_total"
    ),
    .funs = list(logged = ~ log(. + 1))
  ) %>%
  mutate_at(
    c(
      "homevalue",
      "asset_fin",
      "asset_land",
      "asset_durable_fixed",
      "asset_home",
      "asset_total"
    ),
    .funs = list(flag = ~ ifelse(. <= 0,
                                 1,
                                 0))
  )


## -------------------------------------------------------------------------------------------------
dta_mutated_filtered <-
  dta_mutated%>% filter(
    urban_child %in% c(
      "1 Main City Zone",
      "2 Combination Zone Between Urban and Rural Areas",
      "3 The Town Center"
      #"4 ZhenXiang Area"
    ),
    !independence %in% c(
      "1 This Household, And Economicly Dependent",
      "2 This Household, But Economicly Independent"
    ),
    age_child > 18,
    #age_child < 55,
    marriage == "2 Married"
  )


## -------------------------------------------------------------------------------------------------
missing_table <- dta_mutated_filtered %>% missing_glimpse ()
missing_table


## -------------------------------------------------------------------------------------------------
dta_independence <-
  dta_mutated_filtered %>%
  mutate(same_city = ifelse(independence %in% c("6 Other"),
                                      "other",
                                      "close"),
         job_parent = job_parent %>% fct_recode(`1 Private`="1 Agricultural",
                                                `1 Private`="2 Private",
                                                `2 Public`="3 State Controlled Firm",
                                                `2 Public`="4 Public institution",
                                                `2 Public`="5 Government"))


## -------------------------------------------------------------------------------------------------
var.summary <-
  c(
    #child,
    
    "age_child",
    # "sex_child",
    "numchildren",
    "hukou_child",
    # "party_child",
    "education_years_child",
    "job_child_manager",
    "income_logged",
    "marriagehome",
    "ownership",
    "homevalue_logged",
    
    #spouse
    
    "education_years_spouse",
    "hukou_spouse",
    
    #parent family
    
    "siblings",
    "sibling_rank",
    "education_years_parent",
    # "job_parent",
    # "hukou_parent",
    # "party_parent",
    # "asset_total_flag", 
    "asset_total_logged",
    # "urban_parent",
    
    #control
    "urban_child"
  )

options(qwraps2_markup = "markdown") 
data1 <- 
  dta_independence %>% filter(sex_child == "1 Male") %>% select(one_of(var.summary))
table1 <-
  summary_table(data1)
data2 <- 
  dta_independence %>% filter(sex_child == "2 Female") %>% select(one_of(var.summary))
table2 <-
  summary_table(data2)
cbind(summary_table(dta_independence %>% select(one_of(var.summary))),
      table1,
      table2) 


## -------------------------------------------------------------------------------------------------
data_never_married <-
 dta_mutated %>% filter(
    urban_child %in% c(
      "1 Main City Zone",
      "2 Combination Zone Between Urban and Rural Areas",
      "3 The Town Center"
      #"4 ZhenXiang Area"
    ),
    marriage == "1 Never married"
  )

table(data_never_married$sex_child,
      data_never_married$ownership)

table(data_never_married$sex_child,
      data_never_married$ownership)%>%chisq.test()


## -------------------------------------------------------------------------------------------------
string.u <-
  paste(
##control
    #"urban_child",
##child,
    "age_child",
    # "sex_child",
    # "numchildren",
    # "hukou_child",
    # "party_child",
    "education_years_child",
    "job_child_manager",
    "income_logged",
##parent family
    # "urban_parent",
    # "same_city",
    "siblings",
    "sibling_rank",
    # "education_years_parent",
    # "job_parent",
    # "hukou_parent",
    # "party_parent",
    # "asset_total_flag",
    "asset_total_logged",
    sep = " + "
  )

formula.independence.ownership <-
  formula(paste("(ownership) ~",
                string.u)) 

export_summs(
  glm(
    formula.independence.ownership,
    data = data_never_married %>% filter(sex_child=="1 Male"),
    family = binomial
  ),
    glm(
    formula.independence.ownership,
    data = data_never_married %>% filter(sex_child=="2 Female"),
    family = binomial
  ),
  error_pos = "same"
  ,to.file = "xlsx",
  file.name = "output/1-never-married.xlsx"
)


## -------------------------------------------------------------------------------------------------
table(dta_independence$sex_child,
      dta_independence$ownership)

table(dta_independence$sex_child,
      dta_independence$ownership)%>%chisq.test()

t.test(homevalue ~ sex_child, data = dta_independence%>%filter(homevalue>0))


## -------------------------------------------------------------------------------------------------
cor(dta_independence$homevalue[dta_independence$sex_child == "1 Male"],
    dta_independence$asset_total[dta_independence$sex_child == "1 Male"],
    use = "complete.obs")

cor(dta_independence$homevalue[dta_independence$sex_child == "2 Female"],
    dta_independence$asset_total[dta_independence$sex_child == "2 Female"],
    use = "complete.obs")


## -------------------------------------------------------------------------------------------------
table(dta_independence$marriagehome,
      dta_independence$ownership) 

table(dta_independence$marriagehome,
      dta_independence$ownership) %>%chisq.test()

t.test(homevalue ~ marriagehome, data = dta_independence%>%filter(homevalue>0))


## -------------------------------------------------------------------------------------------------
table(dta_independence$marriagehome,
      dta_independence$sex_child)

table(dta_independence$marriagehome,
      dta_independence$sex_child)%>%chisq.test()


## -------------------------------------------------------------------------------------------------
string.a <-
  paste(
##control
    #"urban_child",
##child,
    "age_child",
    # "sex_child",
    "numchildren",
    "hukou_child",
    # "party_child",
    "education_years_child",
    "job_child_manager",
    "income_logged",
##spouse
    "education_years_spouse",
    "hukou_spouse",
##parent family
    # "urban_parent",
    # "same_city",
    "siblings",
    "sibling_rank",
    "education_years_parent",
    # "job_parent",
    # "hukou_parent",
    # "party_parent",
    # "asset_total_flag",
    "asset_total_logged",
    sep = " + "
  )
formula.independence.marriagehome <-
  formula(paste("marriagehome ~",
                string.a))
 
export_summs(
  glm(
    formula.independence.marriagehome,
    data = dta_independence%>%filter(sex_child=="1 Male"),
    family = binomial
  ),
  error_pos = "same"
  ,to.file = "xlsx",
  file.name = "output/2-married-gift-home.xlsx"
)


## -------------------------------------------------------------------------------------------------
cor(dta_independence$homevalue[dta_independence$sex_child == "1 Male" &
                                 dta_independence$marriagehome == FALSE],
    dta_independence$asset_total[dta_independence$sex_child == "1 Male" &
                                   dta_independence$marriagehome == FALSE],
    use = "complete.obs")

cor(dta_independence$homevalue[dta_independence$sex_child == "2 Female" &
                                 dta_independence$marriagehome == FALSE],
    dta_independence$asset_total[dta_independence$sex_child == "2 Female" &
                                   dta_independence$marriagehome == FALSE],
    use = "complete.obs")


## -------------------------------------------------------------------------------------------------
string.early <-
  paste(
##control
    #"urban_child",
##child,
    "age_child",
    # "sex_child",
    # "numchildren",
    # "hukou_child",
    # "party_child",
    # "education_years_child",
    # "job_child_manager",
    # "income_logged",
##spouse
    # "education_years_spouse",
    # "hukou_spouse",
##parent family
    # "urban_parent",
    # "same_city",
    "siblings",
    "sibling_rank",
    "education_years_parent",
    # "job_parent",
    # "hukou_parent",
    # "party_parent",
    # "asset_total_flag",
    "asset_total_logged",
    sep = " + "
  )

string.late <-
  paste(
##control
    #"urban_child",
##child,
    "age_child",
    # "sex_child",
    "numchildren",
    "hukou_child",
    # "party_child",
    "education_years_child",
    "job_child_manager",
    "income_logged",
##spouse
    "education_years_spouse",
    "hukou_spouse",
##parent family
    # "urban_parent",
    # "same_city",
    "siblings",
    "sibling_rank",
    "education_years_parent",
    # "job_parent",
    # "hukou_parent",
    # "party_parent",
    # "asset_total_flag",
    "asset_total_logged",
    sep = " + "
  )

formula.independence.ownership.early <-
  formula(paste("(ownership) ~",
                string.early))
formula.independence.ownership.late <-
  formula(paste("(ownership) ~",
                string.late))

formula.independence.homevalue.early <-
  formula(paste("homevalue_logged ~",
                string.early))
formula.independence.homevalue.late <-
  formula(paste("(homevalue_logged) ~",
                string.late))


## -------------------------------------------------------------------------------------------------
export_summs(
  glm(
    formula.independence.ownership.early,
    data = dta_independence %>% filter(sex_child=="1 Male"),
    family = binomial
  ),  
  glm(
    formula.independence.ownership.late,
    data = dta_independence %>% filter(sex_child=="1 Male"),
    family = binomial
  ),
  glm(
    formula.independence.ownership.early,
    data = dta_independence %>%filter(sex_child=="2 Female"),
    family = binomial
  ),
  glm(
    formula.independence.ownership.late,
    data = dta_independence %>%filter(sex_child=="2 Female"),
    family = binomial
  ),
  error_pos = "same"
  ,to.file = "xlsx",
  file.name = "output/3-married-ownership.xlsx"
)


## -------------------------------------------------------------------------------------------------
vglm(formula.independence.homevalue.early,
     tobit(Lower  = 1),
     data = dta_independence %>%filter(sex_child=="1 Male")) %>%
  summary()
vglm(formula.independence.homevalue.late,
     tobit(Lower  = 1),
     data = dta_independence %>%filter(sex_child=="1 Male")) %>%
  summary()

vglm(formula.independence.homevalue.early,
     tobit(Lower  = 1),
     data = dta_independence %>%filter(sex_child=="2 Female")) %>%
  summary()

vglm(formula.independence.homevalue.late,
     tobit(Lower  = 1),
     data = dta_independence %>%filter(sex_child=="2 Female")) %>%
  summary()


## -------------------------------------------------------------------------------------------------
export_summs(
  lm(
    formula.independence.homevalue.early,
    data = dta_independence %>%
      filter(sex_child=="1 Male",
             ownership ==1)
  ),
  lm(
    formula.independence.homevalue.late,
    data = dta_independence %>%
      filter(sex_child=="1 Male",
             ownership ==1)
  ),
  lm(
    formula.independence.homevalue.early,
    data = dta_independence %>%
      filter(sex_child=="2 Female",
             ownership ==1)
  ),
  lm(
    formula.independence.homevalue.late,
    data = dta_independence %>%
      filter(sex_child=="2 Female",
             ownership ==1)
  ),
  error_pos = "same",
  model.names = c("close", "other")
)

