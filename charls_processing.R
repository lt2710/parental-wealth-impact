## ----setup-------------------------------------------------------------------------------------------------------------------------
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
  "tidyverse"
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


## ----import 2015 demographics------------------------------------------------------------------------------------------------------
#read in data
demographic_raw_2015 <-
  read.dta13(
    "CHARLS2015/Demographic_Background.dta",
    convert.factors = TRUE,
    generate.factors = TRUE
  ) 

## ----------------------------------------------------------------------------------------------------------------------------------
demographic_2015 <-
  demographic_raw_2015 %>%
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
  #generate age at 2015
  mutate(age = 2015 - birthyear) %>%
  select(-birthyear) %>%
  #generate accurate address
  mutate(
    urban = ifelse(
      urban_change == "1 ZLocation",
      last_urban %>% as.character(),
      ifelse(
        urban_change == "2 Other Province/City/County Township Villiage/Neiborhood	",
        new_urban %>% as.character(),
        NA
      )
    ) %>% as.factor()
  ) %>%
  select(-last_urban, 
         -urban_change, 
         -new_urban)

demographic_2015 %>% head()


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
#read hukou data
hukou_2014 <- demographic_raw_2014 %>% select(
  id = ID,
  hhid = householdID,
  hukoutype,#original type
  hk002_1_1_:hk002_1_6_,#year of change
  hk004_1_:hk004_6_#new type
) %>%
  left_join(residence_2014, by = "id")


## ----------------------------------------------------------------------------------------------------------------------------------
#set ideal year to be when the respondent is 40yrs old
career_age <- 40
#calculate the gap between years of change and ideal year
hukou_long <- hukou_2014 %>%
  transmute(
    id = id,
    hk004_1_ = birthyear + career_age - hk002_1_1_,
    hk004_2_ = birthyear + career_age - hk002_1_2_,
    hk004_3_ = birthyear + career_age - hk002_1_3_,
    hk004_4_ = birthyear + career_age - hk002_1_4_,
    hk004_5_ = birthyear + career_age - hk002_1_5_,
    hk004_6_ = birthyear + career_age - hk002_1_6_,
    hukoutype = birthyear + career_age - birthyear
  )
#for a given row, output the column whose time is right before the ideal year
hukou_var <- hukou_long %>%
  gather(column, value,-id,) %>%
  group_by(id) %>%
  filter(value > 0|is.na(value))%>%
  filter(rank(value) == 1) %>%
  select(-value) %>%
  arrange(id)
#display
hukou_var %>% head()


## ----------------------------------------------------------------------------------------------------------------------------------
#join var into hukou
hukou_2014 <- full_join(hukou_2014,
                        hukou_var,
                        by = "id") %>%
  mutate(hukou = NULL,
         column = column %>% replace_na("hukoutype"))
#extract hukou status in the value
for (i in 1:nrow(hukou_2014)) {
  varname <- hukou_2014$column[i]
  hukou_2014$hukou[i] <- hukou_2014[[varname]][i]
}
#keep relevant variables
hukou_2014 <- hukou_2014 %>%
  select(id, hukou) %>% 
  mutate(hukou = hukou %>%
           fct_recode(`1 Agricultural Hukou`="4 None",
                      `1 Agricultural Hukou`="3 Unified Residence Hukou"))

hukou_2014 %>% head()


## ----------------------------------------------------------------------------------------------------------------------------------
#restrucutre party column
party_2014 <- demographic_raw_2014 %>%
  select(id = ID,
         party = p022s1) %>%
  mutate(party = party %>%
           as.numeric () %>%
           replace_na(0) %>%
           factor(
             levels = c(0, 1),
             labels = c("0 non party member", "1 party member")
           ))
party_2014 %>% head()


## ----import education--------------------------------------------------------------------------------------------------------------
#education 2015
education_raw_2015 <-
  read.dta13(
    "CHARLS2015/Demographic_Background.dta",
    convert.factors = TRUE,
    generate.factors = TRUE
  )
education_2015 <- education_raw_2015 %>%
  select(id = ID,
         education15 = bd001_w2_4) %>%
  mutate(education15 = education15 %>%
           fct_recode(NULL = "12 No Change"))
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
  education_2015 %>%
  left_join(education_2013, by = "id") %>%
  mutate_if(is.factor, as.numeric) %>%
  transmute(id = id,
            education = ifelse(
              is.na(education15),
              ifelse(is.na(education13),
                     education11,
                     education13),
              education15
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
psu$city[psu$city == "北京"] <- "北京市"
psu$city[psu$city == "哈尔滨"] <- "哈尔滨市"
psu$city[psu$city == "天津"] <- "天津市"
#a new tier variable
psu$tier <- NA
psu$tier[psu$city %in% c(
  "上海市",
  "北京市",
  "广州市",
  "深圳市",
  "重庆市",
  "天津市",
  "苏州市",
  "成都市",
  "武汉市",
  "杭州市",
  "南京市",
  "西安市",
  "长沙市",
  "沈阳市",
  "青岛市",
  "郑州市",
  "大连市",
  "东莞市",
  "宁波市"
)] <- "1st tier"
psu$tier[psu$city %in% c(
  "厦门市",
  "福州市",
  "无锡市",
  "合肥市",
  "昆明市",
  "哈尔滨市",
  "济南市",
  "佛山市",
  "长春市",
  "温州市",
  "石家庄市",
  "南宁市",
  "常州市",
  "泉州市",
  "南昌市",
  "贵阳市",
  "太原市",
  "烟台市",
  "嘉兴市",
  "南通市",
  "金华市",
  "珠海市",
  "惠州市",
  "徐州市",
  "海口市",
  "乌鲁木齐市",
  "绍兴市",
  "中山市",
  "台州市",
  "兰州市"
)] <- "2nd tier"
psu$tier[psu$city %in% c(
  "潍坊市",
  "保定市",
  "镇江市",
  "扬州市",
  "桂林市",
  "唐山市",
  "三亚市",
  "湖州市",
  "呼和浩特市",
  "廊坊市",
  "洛阳市",
  "威海市",
  "盐城市",
  "临沂市",
  "江门市",
  "汕头市",
  "泰州市",
  "漳州市",
  "邯郸市",
  "济宁市",
  "芜湖市",
  "淄博市",
  "银川市",
  "柳州市",
  "绵阳市",
  "湛江市",
  "鞍山市",
  "赣州市",
  "大庆市",
  "宜昌市",
  "包头市",
  "咸阳市",
  "秦皇岛市",
  "株洲市",
  "莆田市",
  "吉林市",
  "淮安市",
  "肇庆市",
  "宁德市",
  "衡阳市",
  "南平市",
  "连云港市",
  "丹东市",
  "丽江市",
  "揭阳市",
  "延边朝鲜族自治州",
  "舟山市",
  "九江市",
  "龙岩市",
  "沧州市",
  "抚顺市",
  "襄阳市",
  "上饶市",
  "营口市",
  "三明市",
  "蚌埠市",
  "丽水市",
  "岳阳市",
  "清远市",
  "荆州市",
  "泰安市",
  "衢州市",
  "盘锦市",
  "东营市",
  "南阳市",
  "马鞍山市",
  "南充市",
  "西宁市",
  "孝感市",
  "齐齐哈尔市"
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


## ----Merge all 2014----------------------------------------------------------------------------------------------------------------
parent_ind <-
  demographic_2015 %>%
  left_join(hukou_2014, by = "id") %>%
  left_join(party_2014, by = "id") %>%
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
ind_income_raw_2015 <-
  read.dta13(
    "CHARLS2015/Individual_Income.dta",
    convert.factors = TRUE,
    generate.factors = TRUE
  )
ind_income_2015 <- ind_income_raw_2015 %>%
  select(
    id = ID,
    #Asset
    hhid = householdID,
    hc001,
    #cash on hand
    hc002_bracket_max:hc002_bracket_min,
    hc005,
    #Bank deposit,
    hc006_bracket_max,
    hc006_bracket_min,
    hc008,
    #gov bond,
    hc009_bracket_max,
    hc009_bracket_min,
    hc013,
    #stock
    hc014_bracket_max,
    hc014_bracket_min,
    hc018,
    #funds
    hc019_bracket_max,
    hc019_bracket_min,
    hc028,
    #HPF
    hc029_bracket_max,
    hc029_bracket_min,
    hc031,
    #work unit funding
    hc032_bracket_max,
    hc032_bracket_min,
    hc034,
    #unpaid salary
    hc035_bracket_max,
    hc035_bracket_min,
    #Debt
    hc040_w3,
    #debt collectable
    hc041_w3_bracket_max,
    hc041_w3_bracket_min,
    hd001,
    #unpaid private debt
    hd002_bracket_max,
    hd002_bracket_min,
    hc037,
    #payable private debt,
    hc038_w3_bracket_max,
    hc038_w3_bracket_min,
    hd003,
    #credit card payable
    hd004_bracket_max,
    hd004_bracket_min,
    hd004_w3,
    #other debt payable
    hd004_w3_1_bracket_max,
    hd004_w3_1_bracket_min
  )


## ----------------------------------------------------------------------------------------------------------------------------------
#replace all NA into 0
ind_income_2015[is.na(ind_income_2015)] <- 0
#construct wealth measure
hh_fin_2015 <- ind_income_2015 %>%
  mutate(
    asset = (hc001 * 2 + hc002_bracket_max + hc002_bracket_min) / 2 +
      (hc005 * 2 + hc006_bracket_max + hc006_bracket_min) /
      2 +
      (hc008 * 2 + hc009_bracket_max + hc009_bracket_min) /
      2 +
      (hc013 * 2 + hc014_bracket_max + hc014_bracket_min) /
      2 +
      (hc018 * 2 + hc019_bracket_max + hc019_bracket_min) /
      2 +
      (hc028 * 2 + hc029_bracket_max + hc029_bracket_min) /
      2 +
      (hc031 * 2 + hc032_bracket_max + hc032_bracket_min) /
      2 +
      (hc034 * 2 + hc035_bracket_max + hc035_bracket_min) /
      2 +
      (hc040_w3 * 2 + hc041_w3_bracket_max + hc041_w3_bracket_min) /
      2,
    debt =
      (hd001 * 2 + hd002_bracket_max + hd002_bracket_min) /
      2 +
      (hc037 * 2 + hc038_w3_bracket_max + hc038_w3_bracket_min) /
      2 +
      (hd003 * 2 + hd004_bracket_max + hd004_bracket_min) /
      2 +
      (hd004_w3 * 2 + hd004_w3_1_bracket_max + hd004_w3_1_bracket_min) /
      2,
    ind_fin = asset - debt
  ) %>% 
  group_by(hhid) %>% 
  summarise(asset_fin = sum(ind_fin, na.rm = TRUE))

hh_fin_2015 %>% head()


## ----household income--------------------------------------------------------------------------------------------------------------
hh_income_raw_2015 <- read.dta13(
  "CHARLS2015/Household_Income.dta",
  convert.factors = TRUE,
  generate.factors = TRUE
)


## ----durable and fixed-------------------------------------------------------------------------------------------------------------
hh_durable_fixed_2015 <- hh_income_raw_2015 %>% select(
  hhid = householdID,
  ha057_1_:ha057_4_,
  #lands
  ha065_1_1_:ha065_1_19_,
  #durables
  ha066_1_1_:ha066_1_6_,
  #fixed asset for agriculture
  ha067,
  #fixed asset for business
  ha068_1#other fixed asset
)
#recode all NA to 0
hh_durable_fixed_2015[is.na(hh_durable_fixed_2015)] <- 0
#compute land and durables
hh_durable_fixed_2015 <- hh_durable_fixed_2015 %>% mutate(
  asset_land =
    (ha057_1_ +
       ha057_1_ +
       ha057_2_ +
       ha057_3_ +
       ha057_4_) * 20,
  asset_durable_fixed =
    ha065_1_1_ +
    ha065_1_2_ +
    ha065_1_3_ +
    ha065_1_4_ +
    ha065_1_5_ +
    ha065_1_6_ +
    ha065_1_7_ +
    ha065_1_8_ +
    ha065_1_9_ +
    ha065_1_10_ +
    ha065_1_11_ +
    ha065_1_12_ +
    ha065_1_13_ +
    ha065_1_14_ +
    ha065_1_15_ +
    ha065_1_16_ +
    ha065_1_17_ +
    ha065_1_18_ +
    ha065_1_19_ +
    ha066_1_1_ +
    ha066_1_2_ +
    ha066_1_3_ +
    ha066_1_4_ +
    ha066_1_5_ +
    ha066_1_6_ +
    ha067 +
    ha068_1
)
hh_durable_fixed_2015 <- hh_durable_fixed_2015 %>% select(hhid,
                                          asset_land,
                                          asset_durable_fixed)

hh_durable_fixed_2015 %>% head()


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
hh_house_2015 <- hh_income_raw_2015 %>% select(
  hhid = householdID,
  samehouse = ha000_w2_1,
  ownership = ha007,
  valueinprice = ha011_1,
  valueinunit = ha011_2,
  bracketlow = ha012_bracket_min,
  brackethigh = ha012_bracket_max,
) %>%
  mutate(
    ownership = ownership %>%
      fct_recode(`1 Owned by Household Member` = "2 Partly Owned by Household Member"),
    valueinprice = ifelse(valueinprice > 1000,
                          valueinprice / 10000,
                          valueinprice) * 10000,
    valueinunit = ifelse(valueinunit > 500,
                         valueinunit / 1000,
                         valueinunit) * 1000,
    bracketlow = ifelse(is.na(bracketlow) & is.na(brackethigh)==FALSE,
                        brackethigh,
                        bracketlow),
    brackethigh = ifelse(is.na(brackethigh) & is.na(bracketlow)==FALSE,
                        bracketlow,
                        brackethigh),
  ) %>%
  left_join(hh_house_2013, by = "hhid")


## ----create home value-------------------------------------------------------------------------------------------------------------
hh_primaryhome_2015 <- hh_house_2015 %>%
  transmute(
    hhid = hhid,
    homevalue = ifelse(
      ownership == "3 None of Household Member",
      0,
      ifelse(
        is.na(valueinprice),
        ifelse(
          is.na(valueinunit) &
            is.na(valueinunit) == FALSE & samehouse == "1 Yes",
          (bracketlow + brackethigh) / 2,
          valueinunit * buildarea
        ),
        valueinprice
      )
    )
  )
hh_primaryhome_2015 %>% head()


## ----------------------------------------------------------------------------------------------------------------------------------
hh_otherhome_2015 <- hh_income_raw_2015 %>%
  select(hhid = householdID,
         ha045_1_1_:ha045_1_3_,
         ha045_2_1_:ha045_2_3_,
         ha051_1_:ha051_3_)
hh_otherhome_2015[is.na(hh_otherhome_2015)] <- 0
hh_otherhome_2015 <-
  hh_otherhome_2015 %>%
  transmute(
    hhid = hhid,
    otherhomevalue15 = (
      ha045_1_1_ * 10000 +
        ha045_1_2_ * 10000 +
        ha045_1_3_ * 10000 +
        ha045_2_1_ * ha051_1_ * 1000 +
        ha045_2_2_ * ha051_2_ * 1000 +
        ha045_2_3_ * ha051_3_ * 1000
    ) %>% 
      pmin(500000) %>%
      replace_na(0)
  )
hh_otherhome_2015 %>% head()


## ----------------------------------------------------------------------------------------------------------------------------------
hh_otherhome_2013 <- hh_income_raw_2013 %>%
  select(hhid = householdID,
         ha034_1_1_:ha034_1_4_,
         ha034_2_1_:ha034_2_4_,
         ha051_1_:ha051_4_)
hh_otherhome_2013[is.na(hh_otherhome_2013)] <- 0
hh_otherhome_2013 <-
  hh_otherhome_2013 %>%
  transmute(
    hhid = hhid,
    otherhomevalue13 = (
      ha034_1_1_ * 10000 +
        ha034_1_2_ * 10000 +
        ha034_1_3_ * 10000 +
        ha034_1_4_ * 10000 +
        ha034_2_1_ * ha051_1_ * 1000 +
        ha034_2_2_ * ha051_2_ * 1000 +
        ha034_2_3_ * ha051_3_ * 1000 +
        ha034_2_3_ * ha051_3_ * 1000
    ) %>% 
      pmin(500000) %>%
      replace_na(0)
  )
hh_otherhome_2013 %>% head()


## ----------------------------------------------------------------------------------------------------------------------------------
hh_income_raw_2011 <-
  read.dta13(
    "CHARLS2011/household_income.dta",
    convert.factors = TRUE,
    generate.factors = TRUE
  )
hh_otherhome_2011 <- hh_income_raw_2011 %>%
  select(hhid = householdID,
         ha034_1_1_:ha034_1_3_,
         ha034_2_1_:ha034_2_3_,
         ha051_1_:ha051_3_) %>%
  mutate(hhid = hhid %>% paste("0", sep = ""))#a bug of hhid 2011
hh_otherhome_2011[is.na(hh_otherhome_2011)] <- 0
hh_otherhome_2011 <-
  hh_otherhome_2011 %>%
  transmute(
    hhid = hhid,
    otherhomevalue11 = (
      ha034_1_1_ * 10000 +
        ha034_1_2_ * 10000 +
        ha034_1_3_ * 10000 +
        ha034_2_1_ * ha051_1_ * 1000 +
        ha034_2_2_ * ha051_2_ * 1000 +
        ha034_2_3_ * ha051_3_ * 1000
    ) %>% 
      pmin(500000) %>%
      replace_na(0)
  )
hh_otherhome_2011 %>% head()


## ----------------------------------------------------------------------------------------------------------------------------------
hh_homevalue <-
  hh_primaryhome_2015 %>%
  full_join(hh_otherhome_2015, by = "hhid") %>%
  full_join(hh_otherhome_2013, by = "hhid") %>%
  full_join(hh_otherhome_2011, by = "hhid") %>%
  mutate_at(c("otherhomevalue11",
              "otherhomevalue13",
              "otherhomevalue15"),
            function(x) {
              replace_na(x, 0)
            }) %>%
  transmute(
    hhid = hhid,
    asset_home = homevalue +
      otherhomevalue11 +
      otherhomevalue13 +
      otherhomevalue15
  )
hh_homevalue %>% head()


## ----------------------------------------------------------------------------------------------------------------------------------
parent_hh <-
  hh_fin_2015 %>%
  full_join(hh_durable_fixed_2015, by = "hhid") %>%
  full_join(hh_homevalue, by = "hhid") %>% 
  mutate(asset_total = asset_home %>% replace_na(0) +
           asset_fin +
           asset_land +
           asset_durable_fixed)
parent_hh %>% head()


## ----------------------------------------------------------------------------------------------------------------------------------
parent<- parent_ind_hh %>%
  left_join(parent_hh, by ="hhid")

parent %>% head()


## ----import child data, echo=FALSE, warning=FALSE----------------------------------------------------------------------------------
child_raw_2015 <-
  read.dta13("CHARLS2015/Child.dta",
             convert.factors = TRUE,
             generate.factors = TRUE)
#Child data
child_2015 <- child_raw_2015 %>% select(
  id = ID,
  hhid = householdID,
  childid = childID,
  sex = gender,
  age = age,
  independence = cb053,
  education = cb052_w3,
  hukou = cb055,
  hukou_prior = cb055_w2_1,
  marriage = cb063,
  party = cb063_w3_2,
  numchildren = cb065,
  income = cb069,
  ownership = cb071_w3,
  homevalue = cb072_w3,
  urban = cb054
) %>%
  arrange(hhid, desc(age)) %>%
  group_by(hhid) %>%
  mutate(sibling_rank = row_number(),
         siblings = n(),
         have_other_sibling = (length(unique(na.omit(sex)))%>%pmax(1)-1)) %>%
  ungroup()

child_2015 %>% head()


## ----------------------------------------------------------------------------------------------------------------------------------
child_2015 <-
  child_2015 %>%
  mutate(
    numchildren = ifelse(marriage == "6 Never Married",
                         0,
                         numchildren),
    marriage = marriage %>% fct_recode(
      `2 Married` = "3 Separated",
      `2 Married` = "4 Divorced",
      `1 Never married` = "6 Never Married",
      `1 Never married` = "7 Cohabitated",
      `2 Married` = "1 Married With Spouse At Present",
      `2 Married` = "2 Married But Not Living With Spouse Temporarily",
      `2 Married` = "5 Widowed"
    ),
    party = party %>% fct_recode(`2 Not in ccp` = "2 No",
                                 `1 In ccp` = "1 Yes"),
    hukou = ifelse(
      hukou == "3 Unified Residency Hukou",
      hukou_prior %>% as.character(),
      hukou %>% as.character()
    ) %>%
      recode(
        `4 Does Not Have Hukou` = "1 Agricultural Hukou",
        `3 Does Not Have Hukou` = "1 Agricultural Hukou"
      ) %>%
      as.factor(),
    education_years = education %>%
      as.numeric() %>%
      recode(0, 3, 4, 6, 9, 12, 12, 15, 16, 18, 21), 
    income = income %>%
      as.numeric() %>%
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
      ),
    homevalue = ifelse(ownership == "2 No",
                       0,
                       homevalue %>% 
                         pmin(1000) %>%
                         pmax(0)) *10000
  ) %>%
  select(-hukou_prior,
         -education)


child_2015  %>% head()


## ----------------------------------------------------------------------------------------------------------------------------------
spouse_long <-
  demographic_raw_2014 %>%
  select(hhid = householdID,
         c036_1_:c036_18_,
         c037a1_1_:c037a1_18_) %>% 
  gather(column, value,-hhid,) %>%
  mutate(childid = column %>%
           str_extract("[[:punct:]][[:digit:]]+[[:punct:]]")%>%
           str_extract("[[:digit:]]+") %>%
           as.numeric()) %>%
  group_by(hhid)

spouse_education <-
  spouse_long %>%
  filter(column %>%
           str_detect("c036"),
         is.na(value)==FALSE) %>%
  mutate(education_years_spouse =
           value %>% 
           str_extract("[[:digit:]]+") %>%
           as.numeric() %>%
           recode(0, 3, 4, 6, 9, 12, 12, 15, 16, 18, 21, NULL)
         ) %>%
  select(hhid,
         childid,
         education_years_spouse)

spouse_education %>% head()


## ----------------------------------------------------------------------------------------------------------------------------------
spouse_hukou <-
  spouse_long %>%
  filter(column %>%
           str_detect("c037"),
         is.na(value) == FALSE) %>%
  mutate(hukou_spouse =
           value %>%
           recode("3 Unified Residence Hukou" = "1 Agricultural Hukou")) %>%
  select(hhid,
         childid,
         hukou_spouse)

spouse_hukou %>% head()


## ----Occupation--------------------------------------------------------------------------------------------------------------------
child_raw_2013 <-
  read.dta13("CHARLS2013/Child.dta",
             convert.factors = TRUE,
             generate.factors = TRUE)
child_2013 <-
  child_raw_2013 %>%
  select(hhid = householdID,
         childid = childID,
         job = cb071)%>%
  mutate(job = job %>% 
           fct_rev()) 
child_2013 %>% head()


## ----------------------------------------------------------------------------------------------------------------------------------
#buy marriage house
transfer_raw_2015 <-
  read.dta13("CHARLS2015/Family_Transfer.dta")
transfer_2015 <- transfer_raw_2015 %>%
  select(hhid = householdID,
         ce069_w2_1_1_:ce069_w2_1_15_,
         ce070_w2_1_1_:ce070_w2_1_9_)


transfer_long <- transfer_2015 %>%
  gather(column, value, -hhid, ) %>%
  mutate(childid = column %>%
           str_sub(12, 12) %>%
           as.numeric()) %>%
  group_by(hhid)

marriagehome <-
  transfer_long %>%
  filter(column %>%
           str_detect("ce069"),
         is.na(value) == FALSE) %>%
  mutate(marriagehome = value) %>%
  select(hhid,
         childid,
         marriagehome
         )

marriagehomevalue <-
  transfer_long %>%
  filter(column %>%
           str_detect("ce070"),
         is.na(value) == FALSE) %>%
  select(hhid,
         childid,
         marriagehomevalue = value) %>%
  mutate(marriagehomevalue =
           marriagehomevalue %>%
           as.numeric()) %>%
  mutate(
    marriagehomevalue =
      ifelse(
        marriagehomevalue > 1000,
        marriagehomevalue / 1000,
        marriagehomevalue
      )
  )

child_marriagehome <-
  marriagehome %>%
  left_join(marriagehomevalue,
            by = c("hhid",
                   "childid"))
child_marriagehome$marriagehome <-
  child_marriagehome$marriagehome %>% as.factor()

child_marriagehome %>% head()


## ----------------------------------------------------------------------------------------------------------------------------------
child_ind <-
  child_2015 %>%
  left_join(spouse_education, by = c("hhid","childid")) %>%
  left_join(spouse_hukou, by = c("hhid","childid")) %>%
  left_join(child_2013, by = c("hhid","childid")) %>%
  left_join(child_marriagehome, by = c("hhid","childid")) %>%
  distinct()

child_ind %>% head()


## ----------------------------------------------------------------------------------------------------------------------------------
#merge and resolve name conflict
merged <-
  child_ind %>%
  left_join(parent, by = c("hhid")) %>%
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
           ifelse(independence %in% 
                    c("1 This Household, And Economicly Dependent",
                      "2 This Household, But Economicly Independent",
                      "3 The Same Or Adjacent Dwelling/Courtyard With You",
                      "4 Another Household In Your Permanent Address's Village/Neighorhood"),
                  urban_parent%>%as.character(),
                  urban_child%>%as.character()
                  ) %>% 
           recode("2 Combination Zone Between Urban And Rural Areas" = "2 Combination Zone Between Urban and Rural Areas")%>% 
           as.factor(),
         hukou_spouse = hukou_spouse %>%
           as.factor()
         ) %>%
  mutate_at(c("id_child",
              "hhid",
              "id_parent",
              "cid"),
            as.numeric)

merged %>% head()


## ----------------------------------------------------------------------------------------------------------------------------------
summary(merged)


## ----------------------------------------------------------------------------------------------------------------------------------
save(merged, file = "output/merged.RData")

