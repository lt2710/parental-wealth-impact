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
  "readstata13",
  "knitr",
  "bit64",
  "tidyverse",
  "data.table"
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


## ----import 2018 demographics------------------------------------------------------------------------------------------------------
#read in data
demographic_raw_2018 <-
  read.dta13(
    "CHARLS2018/Demographic_Background.dta",
    convert.factors = TRUE,
    generate.factors = TRUE
  )

## ----------------------------------------------------------------------------------------------------------------------------------
demographic_2018 <-
  demographic_raw_2018 %>%
  #select necessary variables
  select(
    id = ID,
    hhid = householdID,
    cid = communityID,
    sex = ba000_w2_3,
    birthyear = ba004_w3_1,
    last_urban = bb000_w3_2,
    urban_change = bb001_w3,
    new_urban = bb001_w3_2
  ) %>%
  #generate age at 2018
  mutate(age = 2018 - birthyear) %>%
  select(-birthyear) %>%
  #generate accurate address
  mutate(
    urban = ifelse(
      urban_change == "1 Address during Last Survey",
      last_urban %>% as.character(),
      ifelse(
        urban_change == "2 Other Place",
        new_urban %>% as.character(),
        NA
      )
    ) %>%
      recode(`4 Special Zone` = "1 Central of City/Town")%>% 
      as.factor()
  ) %>%
  select(-last_urban, 
         -urban_change, 
         -new_urban)

demographic_2018 %>% head()


## ----import 2014 demographics------------------------------------------------------------------------------------------------------
#read in data
demographic_raw_2014 <-
  read.dta13(
    "CHARLS2014/Demographic_Background.dta",
    convert.factors = TRUE,
    generate.factors = TRUE
  )
#put in age
residence_2014 <-
  read.dta13(
    "CHARLS2014/Residence.dta",
    convert.factors = TRUE,
    generate.factors = TRUE
  ) %>%
  select(id = ID,
         birthyear = rbirthyear)

## ----------------------------------------------------------------------------------------------------------------------------------
# hukou
hukou_2018 <- demographic_raw_2018 %>%
  select(
    id = ID,
    hukou_last = zbc004,
    hukou_correct = bc001_w3_1,
    hukou_last_correct = bc001_w3_2,
    hukou_change = bc002_w3,
    hukou_new = bc002_w3_1
  ) %>%
  mutate(
    hukou = ifelse(
      is.na(hukou_new),
      ifelse(
        is.na(hukou_last_correct),
        hukou_last %>% as.character(),
        hukou_last_correct %>% as.character()
      ),
      hukou_new %>% as.character()
    ),
    hukou = hukou %>%
      fct_recode(
        `1 Agricultural Hukou` = "4 Do not have Hukou",
        `1 Agricultural Hukou` = "4 Do Not Have Hukou",
        `1 Agricultural Hukou` = "3 Unified Residence Hukou"
      )
  )%>%
  select(id,
         hukou)
hukou_2018 %>% head()

## ----------------------------------------------------------------------------------------------------------------------------------
#restrucutre party column
party_2018 <- demographic_raw_2018 %>%
  select(id = ID,
         party = bg004_w4) %>%
  mutate(party = party %>%
           as.numeric () %>%
           recode (2,1)%>%
           factor(
             levels = c(1, 2),
             labels = c("1 Non Party Member","2 Party Member")
           ))
party_2018 %>% head()

## ----import education--------------------------------------------------------------------------------------------------------------
#education 2018
education_raw_2018 <-
  read.dta13(
    "CHARLS2018/Demographic_Background.dta",
    convert.factors = TRUE,
    generate.factors = TRUE
  )
education_2018 <- education_raw_2018 %>%
  select(id = ID,
         education18 = bd001_w2_4) %>%
  mutate(education18 = education18 %>%
           fct_recode(NULL = "12 No Change"))
#education 2015
education_raw_2015 <-
  read.dta13(
    "CHARLS2015/Demographic_Background.dta",
    convert.factors = TRUE,
    generate.factors = TRUE
  )
education_2015 <- education_raw_2015 %>%
  select(id = ID,
         education15 = bd001_w2_4)
#education 2013
education_raw_2013 <-
  read.dta13(
    "CHARLS2013/Demographic_Background.dta",
    convert.factors = TRUE,
    generate.factors = TRUE
  )
education_2013 <- education_raw_2013 %>%
  select(id = ID,
         education11 = zbd001,
         education13 = bd001)

## ----------------------------------------------------------------------------------------------------------------------------------
#merge all data and replace in order
education_merged <-
  education_2018 %>%
  left_join(education_2015, by = "id") %>%
  left_join(education_2013, by = "id") %>%
  mutate_if(is.factor, as.numeric) %>%
  transmute(id = id,
            education = ifelse(
              is.na(education18),
              ifelse(is.na(education15),
                     ifelse(is.na(education13),
                            education11,education13
                            ),
                     education15),
              education18
            ))
#recode lavel
education_merged  <-
  education_merged %>%
  transmute(
    id = id,
    education_years = recode(education, 0, 3, 4, 6, 9, 12, 12, 15, 16, 18, 21)
  )
education_merged %>% head()


## ----occupation--------------------------------------------------------------------------------------------------------------------
#import data
work_raw_2014 <-
  read.dta13(
    "CHARLS2014/Work_History.dta",
    convert.factors = TRUE,
    generate.factors = TRUE
  )


## ----------------------------------------------------------------------------------------------------------------------------------
#subset variables
career_age <- 40
work_2014 <- work_raw_2014 %>%
  select(
    id = ID,
    hhid = householdID,
    f_2_1_0_:f_2_1_18_,
    #Start year for job 1-18
    f_3_0_:f_3_18_,
    #Job category
    f104_0_:f104_18_,
    #Job category detailed
    f105_0_:f105_14_,
    #detail firm
  ) %>%
  left_join(residence_2014, by = "id")
#transform into long format
work_long <- work_2014 %>%
  transmute(
    id = id,
    `0` = birthyear + career_age - f_2_1_0_,
    `1` = birthyear + career_age - f_2_1_1_,
    `2` = birthyear + career_age - f_2_1_2_,
    `3` = birthyear + career_age - f_2_1_3_,
    `4` = birthyear + career_age - f_2_1_4_,
    `5` = birthyear + career_age - f_2_1_5_,
    `6` = birthyear + career_age - f_2_1_6_,
    `7` = birthyear + career_age - f_2_1_7_,
    `8` = birthyear + career_age - f_2_1_8_,
    `9` = birthyear + career_age - f_2_1_9_,
    `10` = birthyear + career_age - f_2_1_10_,
    `11` = birthyear + career_age - f_2_1_11_,
    `12` = birthyear + career_age - f_2_1_12_,
    `13` = birthyear + career_age - f_2_1_13_,
    `14` = birthyear + career_age - f_2_1_14_,
    `15` = birthyear + career_age - f_2_1_15_,
    `16` = birthyear + career_age - f_2_1_16_,
    `17` = birthyear + career_age - f_2_1_17_,
    `18` = birthyear + career_age - f_2_1_18_,
  )
#for a given row, output the column whose time is right before the ideal year
work_var <- work_long %>%
  gather(column, value,-id,) %>%
  group_by(id) %>%
  filter(value > 0|is.na(value))%>%
  filter(rank(value) == 1) %>%
  select(-value) %>%
  arrange(id)
#display
work_var %>% head()


## ----------------------------------------------------------------------------------------------------------------------------------
#join var into hukou
work_merged <- full_join(work_2014,
                  work_var,
                  by = "id") %>%
  mutate_if(is.factor,as.character)%>%
  mutate(job = NULL,
         column = column %>% replace_na("1"))
#extract hukou status in the value
for (i in 1:nrow(work_merged)) {
  varname <- work_merged$column[i]
  f_3_varname_ <- paste("f_3_", varname, "_", sep = "")
  f104_varname_ <- paste("f104_", varname, "_", sep = "")
  f105_varname_ <- paste("f105_", varname, "_", sep = "")
  work_merged$job[i] <-
    ifelse(
      work_merged[[f_3_varname_]][i] %in% c("2 Agricultural Employment", "3 Non-agricultural Employment"),
      ifelse(work_merged[[f104_varname_]][i] %in% c("4 Firm"),
             work_merged[[f105_varname_]][i],
             work_merged[[f104_varname_]][i]),
      work_merged[[f_3_varname_]][i]
    )
}
#keep relevant variables and recode job
work_merged <- work_merged %>%
  select(id, job) %>%
  mutate(
    job = job %>%
      recode(
        `1 Own Agricultural Production and Business` = "1 Agricultural",
        `2 Agricultural Employment` = "1 Agricultural",
        `6 Farmer` = "1 Agricultural",
        `8 Rural Collective Economic Organization` = "1 Agricultural",
        `4 Non-agricultural self-employment` = "2 Private",
        `3 Non-agricultural Employment` = "2 Private",
        `3 NGO` = "2 Private",
        `5 Individual Firm` = "2 Private",
        `7 Individual Household` = "2 Private",
        `9 Other` = "2 Private",
        `3 Private Controlled Firm` = "2 Private",
        `4 Hongkong/Macao/Taiwan Controlled Firm` = "2 Private",
        `5 Foreign Controlled Firm` = "2 Private",
        `2 Collective Controlled Firm` = "2 Private",
        `6 Other` = "2 Private",
        `1 State Controlled Firm` = "3 State Controlled Firm",
        `2 Public Institution` = "4 Public institution",
        `6 Army` = "4 Public institution",
        `1 Government` = "5 Government",
        `5 Unpaid Household Business Help` = NULL
      )%>%
      as.factor()
  )
work_merged %>% head()


## ----------------------------------------------------------------------------------------------------------------------------------
psu <- read.dta13("CHARLS2013/PSU.dta",
                  fromEncoding = "GB2312",
                  convert.factors = FALSE)
#city name missalinius
psu$city[psu$city == "??????"] <- "?????????"
psu$city[psu$city == "?????????"] <- "????????????"
psu$city[psu$city == "??????"] <- "?????????"
#a new tier variable
psu$tier <- NA
psu$tier[psu$city %in% c(
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????"
)] <- "1st tier"
psu$tier[psu$city %in% c(
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "????????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "????????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "???????????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????"
)] <- "2nd tier"
psu$tier[psu$city %in% c(
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "???????????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "????????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "????????????",
  "?????????",
  "?????????",
  "?????????",
  "????????????????????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "?????????",
  "????????????",
  "?????????",
  "?????????",
  "?????????",
  "???????????????"
)] <- "3rd tier"
psu$tier<-
  psu$tier%>%replace_na("4th tier")

psu <- psu %>%
  transmute(
    cid = communityID,
    province = province %>% as.factor(),
    city = city %>% as.factor(),
    tier = tier %>% as.factor()
  )

psu %>% head()


## ----Merge all 2018----------------------------------------------------------------------------------------------------------------
parent_ind <-
  demographic_2018 %>%
  left_join(hukou_2018, by = "id") %>%
  left_join(party_2018, by = "id") %>%
  left_join(education_merged, by = "id") %>%
  left_join(work_merged, by = "id") %>% 
  left_join(psu, by = "cid")
#select householders
parent_ind_hh <-
  parent_ind %>% 
  group_by(hhid) %>%
  arrange(sex) %>%
  filter(row_number() == 1)

parent_ind_hh %>% head()

## ----ind income--------------------------------------------------------------------------------------------------------------------
ind_income_raw_2018 <-
  read.dta13(
    "CHARLS2018/Individual_Income.dta",
    convert.factors = TRUE,
    generate.factors = TRUE
  )
ind_income_2018 <- ind_income_raw_2018 %>%
  select(
    id = ID,
    #Asset
    hhid = householdID,
    hc001,
    hc002_max:hc002_min,
    #cash on hand
    hc003_w4,
    hc004_w4_max,hc004_w4_min,
    #Bank deposit,
    hc005,
    hc006_max,
    hc006_min,
    #gov bond,
    hc008,
    hc009_max,
    hc009_min,
    #stock
    hc013,
    hc014_max,
    hc014_min,
    #funds
    hc018,
    hc019_max,
    hc019_min,
    #other portfolio
    hc017_w4,
    hc017_w4_max,
    hc017_w4_min,
    #portfollio under other's name
    hc022,
    #HPF
    hc028,
    hc029_max,
    hc029_min,
    #work unit funding
    hc031,
    hc032_max,
    hc032_min,
    #unpaid salary
    hc034,
    hc035_max,
    hc035_min,
    #Debt
    hc040_w3,
    #debt collectable
    hc041_w3_max,
    hc041_w3_min,
    hd001,
    #unpaid private debt
    hd002_max,
    hd002_min,
    #payable private debt,
    hd003,
    #credit card payable
    hd004_max,
    hd004_min,
    hd004_w3,
    #other debt payable
    hd004_w3_1_max,
    hd004_w3_1_min
  )


## ----------------------------------------------------------------------------------------------------------------------------------
#replace all NA into 0
ind_income_2018[is.na(ind_income_2018)] <- 0
#construct wealth measure
hh_fin_2018 <- ind_income_2018 %>%
  mutate(
    asset = (hc001 * 2 + hc002_max + hc002_min) / 2 +
      (hc003_w4 * 2 + hc004_w4_max + hc004_w4_min) / 
      2 +
      (hc005 * 2 + hc006_max + hc006_min) /
      2 +
      (hc008 * 2 + hc009_max + hc009_min) /
      2 +
      (hc013 * 2 + hc014_max + hc014_min) /
      2 +
      (hc017_w4 * 2 + hc017_w4_max + hc017_w4_min) /
      2 +
      hc022 +
      (hc018 * 2 + hc019_max + hc019_min) /
      2 +
      (hc028 * 2 + hc029_max + hc029_min) /
      2 +
      (hc031 * 2 + hc032_max + hc032_min) /
      2 +
      (hc034 * 2 + hc035_max + hc035_min) /
      2 +
      (hc040_w3 * 2 + hc041_w3_max + hc041_w3_min) /
      2,
    debt =
      (hd001 * 2 + hd002_max + hd002_min) /
      2 +
      (hd003 * 2 + hd004_max + hd004_min) /
      2 +
      (hd004_w3 * 2 + hd004_w3_1_max + hd004_w3_1_min) /
      2,
    ind_fin = asset - debt
  ) %>% 
  group_by(hhid) %>% 
  summarise(asset_fin = sum(ind_fin, na.rm = TRUE))

hh_fin_2018 %>% head()


## ----household income--------------------------------------------------------------------------------------------------------------
hh_income_raw_2018 <- read.dta13(
  "CHARLS2018/Household_Income.dta",
  convert.factors = TRUE,
  generate.factors = TRUE
)
hh_income_raw_2015 <- read.dta13(
  "CHARLS2015/Household_Income.dta",
  convert.factors = TRUE,
  generate.factors = TRUE
)


## ----durable and fixed-------------------------------------------------------------------------------------------------------------
hh_durable_fixed_2018 <- hh_income_raw_2018 %>% select(
  hhid = householdID,
  ha057_1_:ha057_4_,
  #lands
  ha066_w4_1:ha066_w4_7,
  #durables
  ha065_1:ha065_18,
  #fixed asset for agriculture
  ha067_w4,
  #fixed asset for business
  ha068_1#other fixed asset
)
#recode all NA to 0
hh_durable_fixed_2018[is.na(hh_durable_fixed_2018)] <- 0
#compute land and durables
hh_durable_fixed_2018 <- hh_durable_fixed_2018 %>% mutate(
  asset_land =
    (ha057_1_ +
       ha057_1_ +
       ha057_2_ +
       ha057_3_ +
       ha057_4_) * 20,
  asset_durable_fixed =
    ha065_1 +
    ha065_2 +
    ha065_3 +
    ha065_4 +
    ha065_5 +
    ha065_6 +
    ha065_7 +
    ha065_8 +
    ha065_9 +
    ha065_10 +
    ha065_11 +
    ha065_12 +
    ha065_13 +
    ha065_14 +
    ha065_15 +
    ha065_16 +
    ha065_17 +
    ha065_18 +
    ha066_w4_1 +
    ha066_w4_2 +
    ha066_w4_3 +
    ha066_w4_4 +
    ha066_w4_5 +
    ha066_w4_6 +
    ha066_w4_7 +
    ha067_w4 +
    ha068_1
)
hh_durable_fixed_2018 <- hh_durable_fixed_2018 %>% select(hhid,
                                          asset_land,
                                          asset_durable_fixed)

hh_durable_fixed_2018 %>% head()


## ----home characteristics 2013-----------------------------------------------------------------------------------------------------
hh_income_raw_2013 <-
  read.dta13(
    "CHARLS2013/Household_Income.dta",
    convert.factors = TRUE,
    generate.factors = TRUE
  )
hh_house_2013 <- hh_income_raw_2013 %>%
  select(hhid = householdID,
         buildarea = ha001_w2)


## ----house-------------------------------------------------------------------------------------------------------------------------
hh_house_raw_2018 <-
  read.dta13(
    "CHARLS2018/Housing.dta",
    convert.factors = TRUE,
    generate.factors = TRUE
  )
hh_house_2018 <- hh_house_raw_2018 %>% 
  select(
    hhid = householdID,
    #last wave
    #area 
    zsize_1_:zsize_7_,
    #property value
    zvaluelasttime_1_:zvaluelasttime_7_,
    #this time
    #value/unit 
    ha004_w4_1_:ha004_w4_7_,
    #total value update
    ha004_w4_1_1_:ha004_w4_1_7_,
    #unit value update
    ha004_w4_2_1_:ha004_w4_2_5_,
    #unit value estimate
    ha004_w4_1__max:ha004_w4_6__max,
    ha004_w4_1__min:ha004_w4_6__min,
    #new respondent or new house of old respondent
    #area
    ha035_w4_1_:ha035_w4_11_,
    #value/unit
    ha036_w4_1_:ha036_w4_11_,
    #total value update
    ha036_w4_1_1_:ha036_w4_1_11_,
    #unit value update
    ha036_w4_2_1_:ha036_w4_2_7_,
    #unit value estimat
    ha036_w4_1__max:ha036_w4_2__max,
    ha036_w4_1__min:ha036_w4_2__min
    ) %>%
  #treat total value above 1000k as incorretly coded value. cap value at 500. 
  mutate_at(vars(matches("ha004_w4_1_[[:digit:]]_")), function(x){return(ifelse(x>=1000,x/10000,ifelse(x>500,500,x)))}) %>%
  mutate_at(vars(matches("ha036_w4_1_[[:digit:]]_")), function(x){return(ifelse(x>=1000,x/10000,ifelse(x>500,500,x)))}) %>%
  #treat unit value above 20k as incorretly coded value 
  mutate_at(vars(matches("ha004_w4_2_[[:digit:]]_")), function(x){return(ifelse(x>50,x/1000,x))}) %>%
  mutate_at(vars(matches("ha036_w4_2_[[:digit:]]_")), function(x){return(ifelse(x>50,x/1000,x))})
# use coalescne to impute missing min/max
for (i in 1:6){
  var_max <- paste0("ha004_w4_",i,"__max")
  var_min <- paste0("ha004_w4_",i,"__min")
  hh_house_2018 <- hh_house_2018 %>%
    mutate({{var_max}} := coalesce(!!as.name(var_max), !!as.name(var_min)),
           {{var_min}} := coalesce(!!as.name(var_min), !!as.name(var_max))
    )
}
for (i in 1:2){
  var_max <- paste0("ha036_w4_",i,"__max")
  var_min <- paste0("ha036_w4_",i,"__min")
  hh_house_2018 <- hh_house_2018 %>%
    mutate({{var_max}} := coalesce(!!as.name(var_max), !!as.name(var_min)),
           {{var_min}} := coalesce(!!as.name(var_min), !!as.name(var_max))
    )
}
# assign some fake columns to make loops 
vars_to_create <- c(
  paste0("ha004_w4_2_",c(1:7),"_"),
  paste0("ha004_w4_",c(1:7),"__max"),
  paste0("ha004_w4_",c(1:7),"__min"),
  paste0("ha036_w4_2_",c(1:11),"_"),
  paste0("ha036_w4_",c(1:11),"__max"),
  paste0("ha036_w4_",c(1:11),"__min")
)
for(x in vars_to_create){
  if(is.null(hh_house_2018[[x]])) {
    hh_house_2018 <- hh_house_2018%>%
      mutate({{x}} := NA)
  }
}
# calculate housing value
# old record value
for (i in 1:7){
  var_present_value = paste0("old_property_", i)
  hh_house_2018 <- hh_house_2018 %>%
    mutate({{var_present_value}} := ifelse(#whether has new value
                                           !is.na(!!as.name(paste0("ha004_w4_",i,"_"))),
                                           #has new value
                                           coalesce(# concrete value
                                             as.numeric(!!as.name(paste0("ha004_w4_1_",i,"_"))),
                                             # unit value * area
                                             as.numeric(!!as.name(paste0("ha004_w4_2_",i,"_")))*as.numeric(!!as.name(paste0("zsize_",i,"_")))/10,
                                             # unit value min/max * area
                                             #average min/max
                                             ((as.numeric(!!as.name(paste0("ha004_w4_",i,"__min"))) + as.numeric(!!as.name(paste0("ha004_w4_",i,"__max"))))/2)*
                                               #size
                                               as.numeric(!!as.name(paste0("zsize_",i,"_")))/10000,
                                           ),
                                           #use old value
                                           as.numeric(!!as.name(paste0("zvaluelasttime_",i,"_")))
                                           #measured in 10k
                                           ) %>%
             replace_na(0)
           )
}
# new property
for (i in 1:11){
  var_present_value = paste0("new_property_", i)
  hh_house_2018 <- hh_house_2018 %>%
    mutate({{var_present_value}} := ifelse(#whether has new value
      !is.na(!!as.name(paste0("ha036_w4_",i,"_"))),
      #has new value
      coalesce(# concrete value
        as.numeric(!!as.name(paste0("ha036_w4_1_",i,"_"))),
        # unit value * area
        as.numeric(!!as.name(paste0("ha036_w4_2_",i,"_")))*as.numeric(!!as.name(paste0("ha035_w4_",i,"_")))/10,
        # unit value min/max * area
        #average min/max
        ((as.numeric(!!as.name(paste0("ha036_w4_",i,"__min"))) + as.numeric(!!as.name(paste0("ha036_w4_",i,"__max"))))/2)*
          #size
          as.numeric(!!as.name(paste0("ha035_w4_",i,"_")))/10000,
      ),
      #use old value
      NA
      #measured in 10k
    ) %>%
      replace_na(0)
    )
}

# sum rowwise columns

hh_house_2018 = hh_house_2018 %>%
  mutate(asset_home = select(., old_property_1:new_property_11) %>% rowSums() * 10000 )
hh_homevalue = hh_house_2018 %>%
  select(hhid,
         asset_home)

## ----------------------------------------------------------------------------------------------------------------------------------
parent_hh <-
  hh_fin_2018 %>%
  full_join(hh_durable_fixed_2018, by = "hhid") %>%
  full_join(hh_homevalue, by = "hhid") %>% 
  mutate(asset_total = asset_home%>%replace_na(0)  +
           asset_fin%>%replace_na(0) +
           asset_land%>%replace_na(0) +
           asset_durable_fixed%>%replace_na(0))
parent_hh %>% head()


## ----------------------------------------------------------------------------------------------------------------------------------
parent<- parent_ind_hh %>%
  left_join(parent_hh, by ="hhid")

parent %>% head()


## ----import child data, echo=FALSE, warning=FALSE----------------------------------------------------------------------------------
child_raw_2018 <-
  read.dta13("CHARLS2018/Family_Information.dta",
             convert.factors = TRUE,
             generate.factors = TRUE)
#Child data
child_2018_wide <- child_raw_2018 %>% select(
  hhid = householdID,
  xchildgender_1_:xchildgender_16_,
  xchildbirth_1_:xchildbirth_16_, 
  #education
  zchildedu_1_:zchildedu_14_,
  cb052_w3_1_:cb052_w3_15_,
  #independence
  cb053_1_:cb053_15_,
  #urban if independence = other
  cb054_1_:cb054_14_,
  #hukou
  cb055_1_:cb055_15_,
  #party
  cb063_w3_2_1_:cb063_w3_2_15_,
  #working or not
  cb070_w4_1_:cb070_w4_15_,
  #occupation
  cb071_1_:cb071_15_,
  #marriage
  cb063_1_:cb063_15_,
  #spouse education
  cb091_w4_1_:cb091_w4_15_,
  #spouse working or not
  cb093_w4_1_:cb093_w4_15_,
  #num of children
  cb065_1_:cb065_15_,
  #income
  cb069_1_:cb069_15_,
  #ownership
  cb071_w3_1_:cb071_w3_15_,
  #homevalue
  cb072_w3_1_:cb072_w3_15_
)
#transform into child-id based
var_child_prefix <- child_2018_wide %>%
  names()%>%
  setdiff("hhid")%>%
  str_replace_all("[:digit:]+_$","")%>%
  unique()
child_2018_long <- select(child_2018_wide, hhid)
for (prefix in var_child_prefix){
  #output variable name
  varname <- str_sub(prefix,1,-2)
  #set of current variable names
  vars_child <- c(paste0(prefix,c(1:16),"_"))
  #make null columns if necessary
  for (x in vars_child){
    if(is.null(child_2018_wide[[x]])) {
      child_2018_wide <- child_2018_wide%>%
        mutate({{x}} := NA)
    }
  }
  #transform data
  child_data <- child_2018_wide %>% 
    data.table::melt(id.vars = "hhid",
         measure.vars = vars_child)%>%
    transmute(hhid = hhid,
              childid = str_extract(variable,"_[:digit:]+_$")%>%str_extract("[:digit:]+"),
              {{varname}} := value)
  if(prefix == var_child_prefix[1]){
    key = "hhid"
  } else {
    key = c("hhid","childid")
  }
  child_2018_long <- left_join(child_2018_long, child_data, by = key)
}

child_2018_long <- child_2018_long%>%
  filter_at(str_sub(var_child_prefix,1,-2),any_vars(!is.na(.)))

## ----------------------------------------------------------------------------------------------------------------------------------
child_2018 <-
  child_2018_long %>%
  transmute(hhid = as.numeric(hhid),
            childid = as.numeric(childid),
            sex = xchildgender%>%as.factor(),
            age = 2018 - xchildbirth,
            education_years = coalesce(cb052_w3,zchildedu)%>%
              str_extract("[:digit:]+")%>%
              as.numeric()%>%
              recode(0, 3, 4, 6, 9, 12, 12, 15, 16, 18, 21)%>%
              na_if(997)%>%
              na_if(999),
            independence = cb053%>%
              na_if("997 Don't Know")%>%
              na_if("999 Refuse to Answer")%>%
              as.factor(),
            urban = ifelse(cb053 %in% 
                             c("1 Livng with Respondent",
                               "2 Livng with Respondent but Financially Independent",
                               "3 Living in the Same or Nearby Courtyard House(Apartment) with Respondent",
                               "4 Another Household In Your Permanent Address's Village/Neighorhood"),
                           "same as R",
                           cb054%>%as.character()
                           )%>%
              na_if("997 Don't Know")%>%
              na_if("999 Refuse to Answer")%>%
              recode(`4 Special Area` = "1 Center Area of City or Town")%>%
              as.factor(),
            hukou = cb055 %>%
            recode(
              `3 Unified Residency Hukou` = "1 Agriculture Hukou",
              `4 Do not Have Hukou` = "1 Agriculture Hukou"
            ) %>%
              na_if("997 Don't Know")%>%
              na_if("999 Refuse to Answer")%>%
              as.factor(),
            party = cb063_w3_2%>%
              na_if("997 Don't Know")%>%
              na_if("999 Refuse to Answer")%>%
              recode(`1 Yes` = "1 Non Party Member",
                     `2 No` = "2 Party Member")%>%
              as.factor(),
            working = cb070_w4%>%
              na_if("997 Don't Know")%>%
              na_if("999 Refuse to Answer")%>%
              as.factor(),
            job = cb071%>%as.factor(),
            married = cb063%>% recode(
              `1 Married with Spouse Present` = "1 Married",
              `2 Married but not Living with Spouse Temporarily for Reasons Such as Work` = "1 Married",
              `3 Separated` = "1 Married",
              `4 Divorced` = "2 Not Married",
              `5 Widowed` = "2 Not Married",
              `6 Never Married` = "2 Not Married") %>%
              na_if("997 Don't Know")%>%
              na_if("999 Refuse to Answer")%>%
              as.factor(),
            education_years_spouse = cb091_w4%>%
              str_extract("[:digit:]+")%>%
              as.numeric()%>%
              recode(0, 3, 4, 6, 9, 12, 12, 15, 16, 18, 21)%>%
              na_if(997)%>%
              na_if(999),
            job_spouse = cb093_w4%>%as.factor(),
            numchildren = cb065,
            income = cb069 %>%
              str_extract("[:digit:]+")%>%
              as.numeric()%>%
              recode(
                0,
                1000,
                2500,
                7500,
                15000,
                25000,
                40000,
                75000,
                125000,
                175000,
                250000,
                500000
              )%>%
              na_if(997)%>%
              na_if(999),
            homevalue = (ifelse(cb072_w3>=500,cb072_w3/10000,ifelse(cb072_w3>1000,1000,cb072_w3)))
            ) 

## ----------------------------------------------------------------------------------------------------------------------------------
transfer_raw_2018 <-
  read.dta13("CHARLS2018/Family_Transfer.dta")
#gift home upon marriage
marriage_home <- transfer_raw_2018%>%
  select(hhid = householdID,
         ce069_w2_1_1_:ce069_w2_1_15_) %>%
  gather(column, value, -hhid, ) %>%
  mutate(childid = column %>%
           str_extract("_[:digit:]+_$")%>%
           str_extract("[:digit:]+") %>%
           as.numeric()) %>%
  transmute(hhid = hhid %>% as.numeric(), 
            childid = childid %>% as.numeric(),
            marriagehome = value %>% 
              recode(`1 Yes` = "2 Yes",
                     `2 No` = "1 No") %>%
              as.factor())

#gift home value
marriage_homevalue <- transfer_raw_2018%>%
  select(hhid = householdID,
         ce070_w2_1_1_:ce070_w2_1_9_) %>%
  gather(column, value, -hhid, ) %>%
  mutate(childid = column %>%
           str_extract("_[:digit:]+_$")%>%
           str_extract("[:digit:]+") %>%
           as.numeric()) %>%
  mutate(value = ifelse(
    value > 1000,
    value / 1000,
    value
  ))%>%
  transmute(hhid = hhid %>% as.numeric(), 
            childid = childid %>% as.numeric(),
            marriagehomevalue = value)


## ----------------------------------------------------------------------------------------------------------------------------------
child <-
  child_2018 %>%
  left_join(marriage_home, by = c("hhid","childid")) %>%
  left_join(marriage_homevalue, by = c("hhid","childid")) 

child %>% head()


## ----------------------------------------------------------------------------------------------------------------------------------
#merge and resolve name conflict
merged <-
  child %>%
  ungroup()%>%
  left_join(parent %>% 
              ungroup%>% 
              mutate(hhid = hhid %>%as.numeric()), by = c("hhid")) %>%
  rename_at(vars(contains(".x")),
            function(child_var) {
              return(child_var %>%
                       str_sub(end = -3) %>%
                       paste("_child", sep = ""))
            }) %>%
  rename_at(vars(contains(".y")),
            function(parent_var) {
              return(parent_var %>%
                       str_sub(end = -3) %>%
                       paste("_parent", sep = ""))
            }) %>%
  mutate(urban_child = 
           ifelse(urban_child == "same as R",
                  urban_parent %>% as.character(),
                  urban_child %>% as.character()
                  ) %>%
           recode(`1 Center Area of City or Town` = "1 Central of City/Town",
                  `2 Urban-suburban-integration Area`= "2 Urban-Rural Integration Zone",
                  `3 Rural Area`="3 Rural") %>%
           as.factor()
         ) %>%
  mutate_at(c("childid",
              "hhid",
              "cid",
              "id"),
            as.numeric)

merged %>% head()


## ----------------------------------------------------------------------------------------------------------------------------------
summary(merged)


## ----------------------------------------------------------------------------------------------------------------------------------
save(merged, file = "output/merged.RData")

