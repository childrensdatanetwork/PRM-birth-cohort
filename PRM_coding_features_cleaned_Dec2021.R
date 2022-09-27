library("readstata13")
library("ggplot2")
library("magrittr")
library("dplyr")
library("knitr")
library("kableExtra")

####FUNCTIONS TO CODE PRIs###
#code if gender of child is female or missing
code_child_female <- function(data)
{
  #1: male, 2: female, 9: missing
  data <- data %>% 
            rowwise() %>%
              mutate(PRI_CHILD_FEMALE_OR_MISS = ifelse(sex == 2 | sex == 9, 1, 0))
  
  data
}

#code mom age, continuous and in categories
code_mom_age <- function(data)
{
  data$PRI_MOM_AGE <- data$mage
  data$PRI_MOM_AGE[data$PRI_MOM_AGE == 99] <- 0
  
  data <- data %>% 
            rowwise() %>%
              mutate(PRI_MOM_AGE_17UNDER = ifelse(PRI_MOM_AGE <= 17, 1, 0), 
                        PRI_MOM_AGE_18_19 = ifelse(PRI_MOM_AGE >= 18 & PRI_MOM_AGE <= 19, 1, 0), 
                        PRI_MOM_AGE_20_24 = ifelse(PRI_MOM_AGE >= 20 & PRI_MOM_AGE <= 24, 1, 0), 
                        PRI_MOM_AGE_25_29 = ifelse(PRI_MOM_AGE >= 25 & PRI_MOM_AGE <= 29, 1, 0),
                        PRI_MOM_AGE_30_34 = ifelse(PRI_MOM_AGE >= 30 & PRI_MOM_AGE <= 34, 1, 0),
                        PRI_MOM_AGE_35_39 = ifelse(PRI_MOM_AGE >= 35 & PRI_MOM_AGE <= 39, 1, 0),
                        PRI_MOM_AGE_40PLUS = ifelse(PRI_MOM_AGE >= 40, 1, 0),
                        PRI_MOM_AGE_MISS = ifelse(PRI_MOM_AGE == 0, 1, 0))
  data
  
}

#code if paternity cannot be established
code_paternity_missing <- function(data)
{
  data <- data %>% 
    rowwise() %>%
    mutate(PRI_DAD_PATERNITY_MISSING = ifelse(is.na(fbthdate) | (fage == 99 & feduc == 9 & fracecodem == 9), 1, 0))
  data
}

#code dad age, continuous and in categories
code_dad_age <- function(data)
{
  data$PRI_DAD_AGE <- data$fage
  data$PRI_DAD_AGE[data$PRI_DAD_AGE == 99] <- 0
  
  data <- data %>% 
            rowwise() %>%
              mutate(PRI_DAD_AGE_17UNDER = ifelse(PRI_DAD_AGE <= 17, 1, 0), 
                 PRI_DAD_AGE_18_19 = ifelse(PRI_DAD_AGE >= 18 & PRI_DAD_AGE <= 19, 1, 0), 
                 PRI_DAD_AGE_20_24 = ifelse(PRI_DAD_AGE >= 20 & PRI_DAD_AGE <= 24, 1, 0), 
                 PRI_DAD_AGE_25_29 = ifelse(PRI_DAD_AGE >= 25 & PRI_DAD_AGE <= 29, 1, 0),
                 PRI_DAD_AGE_30_34 = ifelse(PRI_DAD_AGE >= 30 & PRI_DAD_AGE <= 34, 1, 0),
                 PRI_DAD_AGE_35_39 = ifelse(PRI_DAD_AGE >= 35 & PRI_DAD_AGE <= 39, 1, 0),
                 PRI_DAD_AGE_40PLUS = ifelse(PRI_DAD_AGE >= 40, 1, 0),
                 PRI_DAD_AGE_MISS = ifelse(PRI_DAD_AGE == 0, 1, 0))
  data
}

#code mom educ
code_mom_educ <- function(data)
{
  data <- data %>% 
            rowwise() %>%
              mutate(PRI_MOM_EDUC_BA_OR_HIGHER = ifelse(meduc == 6 | meduc == 7 | meduc == 8, 1, 0), 
                 PRI_MOM_EDUC_COLLEGE = ifelse(meduc == 4 | meduc == 5, 1, 0), 
                 PRI_MOM_EDUC_HSORGED = ifelse(meduc == 3, 1, 0), 
                 PRI_MOM_EDUC_LESS_HS = ifelse(meduc < 3, 1, 0), 
                 PRI_MOM_EDUC_MISS = ifelse(meduc == 9 | meduc == 0, 1, 0))
  
  data
}

#code dad educ
code_dad_educ <- function(data)
{
  data <- data %>% 
            rowwise() %>%
              mutate(PRI_DAD_EDUC_BA_OR_HIGHER = ifelse(feduc == 6 | feduc == 7 | feduc == 8, 1, 0), 
                 PRI_DAD_EDUC_COLLEGE = ifelse(feduc == 4 | feduc == 5, 1, 0), 
                 PRI_DAD_EDUC_HSORGED = ifelse(feduc == 3, 1, 0), 
                 PRI_DAD_EDUC_LESS_HS = ifelse(feduc < 3, 1, 0), 
                 PRI_DAD_EDUC_MISS = ifelse(feduc == 9 | feduc == 0, 1, 0))
  
  data
}

#code mom hispanic (CHECK this is the field)
code_mom_hispanic_origin <- function(data)
{
  data <- data %>% 
            rowwise() %>%
              mutate(PRI_MOM_RACE_HISPANIC = ifelse(msporig == 2 | msporig == 3 | msporig == 4 | msporig == 5 | msporig == 6 | msporig == 8, 1, 0), 
                 PRI_MOM_RACE_HISPANIC_MISS = ifelse(msporig == 9, 1, 0))
  data
}

#code mom race PRIs
code_mom_race <- function(data)
{
  data <- data %>% 
            rowwise() %>%
              mutate(PRI_MOM_RACE_WHITE = ifelse(mracecodem == 1, 1, 0)) #| mracecode1 == 10 | mracecode2 == 10 | mracecode3 == 10, 1, 0))
  
  data <- data %>% 
            rowwise() %>%
              mutate(PRI_MOM_RACE_BLACK_AA = ifelse(mracecodem == 2, 1, 0)) # | mracecode1 == 20 | mracecode2 == 20 | mracecode3 == 20, 1, 0))
  
  data <- data %>% 
            rowwise() %>%
              mutate(PRI_MOM_RACE_NATIVE_AM = ifelse(mracecodem == 3, 1, 0)) # | mracecode1 == 30 | mracecode2 == 30 | mracecode3 == 30, 1, 0))
  
  data <- data %>% 
            rowwise() %>%
              mutate(PRI_MOM_RACE_ASIAN = ifelse(mracecodem == 4 | mracecodem == 5, 1, 0)) # | (mracecode1 >= 40 & mracecode1 <= 49) | (mracecode2 >= 40 & mracecode2 <= 49) | (mracecode3 >= 40 & mracecode3 <= 49), 1, 0))
  
  data <- data %>% 
            rowwise() %>%
              mutate(PRI_MOM_RACE_OTHER = ifelse(mracecodem == 6, 1, 0)) # | (mracecode1 >= 51 & mracecode1 <= 59) | (mracecode2 >= 51 & mracecode2 <= 59) | (mracecode3 >= 51 & mracecode3 <= 59), 1, 0))
  
  data <- data %>% 
            rowwise() %>%
              mutate(PRI_MOM_RACE_MISS = ifelse(PRI_MOM_RACE_WHITE == 0 & PRI_MOM_RACE_BLACK_AA == 0 & PRI_MOM_RACE_NATIVE_AM == 0 & PRI_MOM_RACE_ASIAN == 0 & PRI_MOM_RACE_OTHER == 0, 1, 0))
  
  data  
}

#code dad hispanic origin
code_dad_hispanic_origin <- function(data)
{
  data <- data %>% 
            rowwise() %>%
              mutate(PRI_DAD_RACE_HISPANIC = ifelse(fsporig == 2 | fsporig == 3 | fsporig == 4 | fsporig == 5 | fsporig == 6 | fsporig == 8, 1, 0), 
                 PRI_DAD_RACE_HISPANIC_MISS = ifelse(fsporig == 9, 1, 0))
  data
}

#code dad race PRIs
code_dad_race <- function(data)
{
  data <- data %>% 
            rowwise() %>%
              mutate(PRI_DAD_RACE_WHITE = ifelse(fracecodem == 1, 1, 0)) # | fracecode1 == 10 | fracecode2 == 10 | fracecode3 == 10, 1, 0))
  
  data <- data %>% 
            rowwise() %>%
              mutate(PRI_DAD_RACE_BLACK_AA = ifelse(fracecodem == 2, 1, 0)) # | fracecode1 == 20 | fracecode2 == 20 | fracecode3 == 20, 1, 0))
  
  data <- data %>% 
            rowwise() %>%
              mutate(PRI_DAD_RACE_NATIVE_AM = ifelse(fracecodem == 3, 1, 0)) # | fracecode1 == 30 | fracecode2 == 30 | fracecode3 == 30, 1, 0))
  
  data <- data %>% 
            rowwise() %>%
              mutate(PRI_DAD_RACE_ASIAN = ifelse(fracecodem == 4 | fracecodem == 5, 1, 0)) # | (fracecode1 >= 40 & fracecode1 <= 49) | (fracecode2 >= 40 & fracecode2 <= 49) | (fracecode3 >= 40 & fracecode3 <= 49), 1, 0))
  
  data <- data %>% 
            rowwise() %>%
              mutate(PRI_DAD_RACE_OTHER = ifelse(fracecodem == 6, 1, 0)) # | (fracecode1 >= 51 & fracecode1 <= 59) | (fracecode2 >= 51 & fracecode2 <= 59) | (fracecode3 >= 51 & fracecode3 <= 59), 1, 0))
  
  data <- data %>% 
            rowwise() %>%
              mutate(PRI_DAD_RACE_MISS = ifelse(PRI_DAD_RACE_WHITE == 0 & PRI_DAD_RACE_BLACK_AA == 0 & PRI_DAD_RACE_NATIVE_AM == 0 & PRI_DAD_RACE_ASIAN == 0 & PRI_DAD_RACE_OTHER == 0, 1, 0))
  
  data 
}

#code prenatal visits
code_prenatal_visits <- function(data)
{
  data <- data %>% 
            rowwise() %>%
              mutate(PRI_MOM_PRENATAL_VISIT_YES = ifelse(!is.na(precare) & precare > 0, 1, 0),
                        PRI_MOM_PRENATAL_VISIT_MISS = ifelse(is.na(precare) | precare == "-", 1, 0))
  data
}

#code got wic food
code_mom_wic_food <- function(data)
{
  data <- data %>% 
            rowwise() %>%
              mutate(PRI_MOM_WIC_YES = ifelse(mfood == "Y", 1, 0), 
                        PRI_MOM_WIC_MISS = ifelse(is.na(mfood) | mfood == "U", 1, 0))
  
  data
}

code_mom_firstborn <- function(data)
{
  data <- data %>% 
            rowwise() %>%
              mutate(PRI_MOM_FIRSTBORN = ifelse((prevlbl == 0 & prevlbd == 0) | ((is.na(prevlbl) | prevlbl == 99) & prevlbd == 0) | (prevlbl == 0 & (is.na(prevlbd) | prevlbd == 99)), 1, 0))
  
  data <- data %>% 
            rowwise() %>%
              mutate(PRI_MOM_FIRSTBORN_MISSING = ifelse((is.na(prevlbl) | prevlbl == 99) & (is.na(prevlbd) | prevlbd == 99), 1, 0))
  
  data
}

code_live_births <- function(data)
{
  data <- data %>% 
            rowwise() %>%
              mutate(PRI_MOM_PR_LIVE_BIRTHS_0 = ifelse(prevlbl == 0, 1, 0),
                        PRI_MOM_PR_LIVE_BIRTHS_1 = ifelse(prevlbl == 1, 1, 0), 
                        PRI_MOM_PR_LIVE_BIRTHS_2 = ifelse(prevlbl == 2, 1, 0), 
                        PRI_MOM_PR_LIVE_BIRTHS_3 = ifelse(prevlbl == 3, 1, 0),
                        PRI_MOM_PR_LIVE_BIRTHS_4PLUS = ifelse(prevlbl >= 4, 1, 0))
  
  data <- data %>% 
            rowwise() %>%
              mutate(PRI_MOM_PR_LIVE_BIRTHS_COUNT = ifelse(!is.na(prevlbl) & prevlbl != 99, prevlbl, 0))
  
  data <- data %>% 
            rowwise() %>%
              mutate(PRI_MOM_PR_LIVE_BIRTHS_MISS = ifelse(is.na(prevlbl) | prevlbl == 99, 1, 0))
  
  data
}

code_dead_births <- function(data)
{
  data <- data %>% 
            rowwise() %>%
              mutate(PRI_MOM_PR_DEAD_BIRTHS_0 = ifelse(prevlbd == 0, 1, 0),
                        PRI_MOM_PR_DEAD_BIRTHS_1 = ifelse(prevlbd == 1, 1, 0), 
                        PRI_MOM_PR_DEAD_BIRTHS_2 = ifelse(prevlbd == 2, 1, 0), 
                        PRI_MOM_PR_DEAD_BIRTHS_3 = ifelse(prevlbd == 3, 1, 0),
                        PRI_MOM_PR_DEAD_BIRTHS_4PLUS = ifelse(prevlbd >= 4, 1, 0))
  
  data <- data %>% 
            rowwise() %>%
              mutate(PRI_MOM_PR_DEAD_BIRTHS_COUNT = ifelse(!is.na(prevlbd) & prevlbd != 99, prevlbd, 0))
  
  data <- data %>% 
            rowwise() %>%
              mutate(PRI_MOM_PR_DEAD_BIRTHS_MISS = ifelse(is.na(prevlbd) | prevlbd == 99, 1, 0))
  
  data
}

code_mom_smoke <- function(data)
{
  data <- data %>% 
            rowwise() %>%
              mutate(PRI_MOM_SMKD_3MTH_PRIOR = ifelse(!is.na(cig2tri) & cig2tri > 0 & cig2tri != 99, 1, 0), 
                        PRI_MOM_SMKD_3MTH_PRIOR_MISS = ifelse(is.na(cig2tri) | cig2tri == 99, 1, 0))
  
  data <- data %>% 
            rowwise() %>%
              mutate(PRI_MOM_SMKD_DURING_PREG = ifelse(!is.na(cigbefp) & cigbefp > 0 & cigbefp != 99, 1, 0), 
                        PRI_MOM_SMKD_DURING_PREG_MISS = ifelse(is.na(cigbefp) | cigbefp == 99, 1, 0))
  
  data
}

code_mom_payment_method <- function(data)
{
  data <- data %>% 
            rowwise() %>%
              mutate(PRI_MOM_PAY_MEDICAID = ifelse(paymsopc == 2, 1, 0), 
                        PRI_MOM_PAY_PRIVATE = ifelse(paymsopc == 7, 1, 0), 
                        PRI_MOM_PAY_SELF = ifelse(paymsopc == 9, 1, 0),
                        PRI_MOM_PAY_OTHER = ifelse(!paymsopc %in% c(2, 7, 9) & !is.na(paymsopc) & paymsopc != 99, 1, 0), 
                        PRI_MOM_PAY_MISS = ifelse(paymsopc == 99, 1, 0))
  
  data
}

#pre-pregnancy diabetes: 09, gestational diabetes: 31, pre-pregnancy hypertension: 03, 
#gestational hypertension: 01, previos pre-term birth: 23, previous poor pregnancy outcomes: 36
#vaginal bleeding: ?, pregnancy resulted from infertility treatment: 40-41
#previous cesarean: prev_c_sec
code_mom_risk_pregnancy <- function(data)
{
  data <- data %>% 
            rowwise() %>%
              mutate(PRI_MED_PREG_RF_YES = 
                   ifelse(all(!c("00", "09", "31", "03", "01", "23", "36", "40", "41") %in% 
                                 strsplit(probl_1, "(?<=.{2})", perl = TRUE)[[1]]) &
                                  prev_c_sec != 1, 0, 1))
  
  data
}

#infections
#gonorrhea: 43, syphilis: 46, herpes: 16, chlamydia: 42, hepatitis b: 18, hepatitis c: 45
code_mom_infections <- function(data)
{
  data <- data %>% 
            rowwise() %>%
              mutate(PRI_MED_PREG_INFECTION_YES = 
                   ifelse(all(!c("43", "46", "16", "42", "18", "45") %in% 
                                strsplit(probl_1, "(?<=.{2})", perl = TRUE)[[1]]), 0, 1))
  
  data
}

#cervical cerclage: 24, tocolysis: 28, external cephalic version failed: 38
code_mom_obstetrics <- function(data)
{
  data <- data %>% 
            rowwise() %>%
              mutate(PRI_MED_OBSTETRICS = 
                   ifelse(all(!c("24", "28", "38") %in% 
                                strsplit(probl_1, "(?<=.{2})", perl = TRUE)[[1]]), 0, 1))
  
  data
}

#onset of labor
#premature rupture: 10, precipitous labor: 07, prolonged labor: 08
code_mom_onset_labor <- function(data)
{
  data <- data %>% 
            rowwise() %>%
              mutate(PRI_MED_ONSET_LBR = 
                   ifelse(all(!c("10", "07", "08") %in% 
                                strsplit(probl_2, "(?<=.{2})", perl = TRUE)[[1]]), 0, 1))
  
  data
}

#characteristics of delivery
#induction: 11, augmentation: 12, non-vertex: 32, steroids: 33, antibiotics: 34, chramnionitis: 35, 
#meconium: 19, fetal intoler: 36, anesthesia: 37
code_mom_char_delivery <- function(data)
{
  data <- data %>% 
            rowwise() %>%
              mutate(PRI_MED_CHR_LBR_COUNT = 
                   length(which(c("11", "12", "32", "33", "34", "35", "19", "36", "37") %in% 
                                strsplit(probl_2, "(?<=.{2})", perl = TRUE)[[1]])))
  
  data
}

#method of delivery: cesarean
#if 01, 11, 21, 31, 02, 12, 22, 32
code_mom_method_delivery <- function(data)
{
  data <- data %>% 
            rowwise() %>%
              mutate(PRI_MED_DEL_MTHD_ROUTE_CESAREAN = ifelse(mdel %in% c(1, 11, 21, 31, 2, 12, 22, 32), 1, 0))
  
  data
}

#fetal presentation:
#cephalic: 2, breech: 3, other: 4, unknown: 9
code_mom_fetal_presentation <- function(data)
{
  data <- data %>% 
            rowwise() %>%
              mutate(PRI_MED_DEL_MTHD_FETAL_BREECH = ifelse(fetlbth == 3, 1, 0), 
                        PRI_MED_DEL_MTHD_FETAL_CEPHALIC = ifelse(fetlbth == 2, 1, 0), 
                        PRI_MED_DEL_MTHD_FETAL_OTHER = ifelse(fetlbth == 4, 1, 0), 
                        PRI_MED_DEL_MTHD_FETAL_UNKNOWN = ifelse(fetlbth == 9, 1, 0))
  
  data
}

#morbidity conditions
#transfusion: 24, prenineal: 40, rupture uterus: 41, unplaneed hysterectomy: 42, 
#intense care: 43, unplaned operation: 44 
code_mom_morbidity <- function(data)
{
  data <- data %>% 
            rowwise() %>%
              mutate(PRI_MED_MTRNL_MORBIDITY_COUNT = 
                   length(which(c("24", "40", "41", "42", "43", "44") %in% 
                                  strsplit(probl_2, "(?<=.{2})", perl = TRUE)[[1]])))
  
  data  
}

#child birth weight
code_child_birthweight <- function(data)
{
  data$PRI_CHILD_BIRTH_WT <- data$bthweight
  
  data <- data %>% 
            rowwise() %>% 
              mutate(PRI_CHILD_BIRTH_WT_LOW = ifelse(PRI_CHILD_BIRTH_WT < 2500, 1, 0))
  
  data <- data %>% 
            rowwise() %>%
              mutate(PRI_CHILD_BIRTH_WT_MISS = ifelse(is.na(bthweight) | bthweight == 9999, 1, 0)) #validate this
  
  data
}

#preterm birth
code_preterm_birth <- function(data)
{
  data <- data %>% 
            rowwise() %>% 
              mutate(PRI_CHILD_PRETERM_BIRTH = ifelse(obstegest >= 33 & obstegest <= 37, 1, 0), 
                        PRI_CHILD_VERY_PRETERM_BIRTH = ifelse(obstegest <= 32, 1, 0), 
                        PRI_CHILD_VERY_PRETERM_BIRTH_MIS = ifelse(is.na(obstegest), 1, 0)) #validate this
  
  data
}

#apgar score
code_apgar_score <- function(data)
{
  data <- data %>% 
            rowwise() %>% 
              mutate(PRI_CHILD_APGAR_SCORE_5MIN_LOW = ifelse(ascore5m >= 0 & ascore5m <= 3, 1, 0), 
                        PRI_CHILD_APGAR_SCORE_5MIN_MODER = ifelse(ascore5m >= 4 & ascore5m <= 6, 1, 0), 
                        PRI_CHILD_APGAR_SCORE_5MIN_REASS = ifelse(ascore5m >= 7 & ascore5m <= 10, 1, 0), 
                        PRI_CHILD_APGAR_SCORE_5MIN_MISS = ifelse(is.na(ascore5m) | ascore5m == 99, 1, 0)) #validate this logic in particular
  
  data
  
}

#code abnormal conditions of child
#assisted ventilation: 71, assisted ventilation gt6: 85, nicu: 73, surfactant therapy: 86, 
#antibiotics: 87, seizures: 70, significant birth injury: ?
code_abnormal_conditions <- function(data)
{
  data <- data %>% 
            rowwise() %>% 
              mutate(PRI_CHILD_ABNRML_COND_COUNT = 
                   length(which(c("71", "85", "73", "86", "87", "70") %in% 
                                  strsplit(probl_3, "(?<=.{2})", perl = TRUE)[[1]])))
  
  data  
}

#code congenital anomalies of child
#anencephaly: 01, mening: 02, cyanotic cong heart disease: 76, diaphrag hernya: 77, omphalocele: 78
#gasthrochisis: 79, limd reduction: 80, cleft palate: 28, cleft lip: 29, down syndrome conf: 57, 
#down syndrome pending: 81, suspected chrom disorder conf: 82, suspected chrom disorder pending: 83
#hyposdapias: 35 
code_congenital_anomalies <- function(data)
{
  data <- data %>% 
            rowwise() %>% 
              mutate(PRI_CHILD_CONG_ANOM = 
                   length(which(c("01", "02", "76", "77", "78", "79", "80", "28", "29", "57", "81", "82", "83", "35") %in% 
                                  strsplit(probl_3, "(?<=.{2})", perl = TRUE)[[1]])))
  
  data 
}


#MAIN CODE#
#"birth_cps_linked_file" is a child level data set that contains both birth variables and CPS variables
setwd("working directory")
df <- read.dta13("birth_cps_linked_file")

#remove noisy data
birth_id_counts <- df %>% group_by(bid) %>% summarise(n = n())
df <- df[!df$bid %in% birth_id_counts$bid[birth_id_counts$n > 1],]

#now code PRIs
# some of the functions won't run since the df may not have all the variables that are needed. But no need to panic. Just run all the functions.
df <- code_abnormal_conditions(df)
df <- code_apgar_score(df)
df <- code_child_birthweight(df)
df <- code_child_female(df)
df <- code_congenital_anomalies(df)
df <- code_dad_age(df)
df <- code_dad_educ(df)
df <- code_dad_hispanic_origin(df)
df <- code_dad_race(df)
df <- code_dead_births(df)
df <- code_live_births(df)
df <- code_mom_age(df)
df <- code_mom_char_delivery(df)
df <- code_mom_educ(df)
df <- code_mom_fetal_presentation(df)
df <- code_mom_firstborn(df)
df <- code_mom_hispanic_origin(df)
df <- code_mom_infections(df)
df <- code_mom_method_delivery(df)
df <- code_mom_morbidity(df)
df <- code_mom_obstetrics(df)
df <- code_mom_onset_labor(df)
df <- code_mom_payment_method(df)
df <- code_mom_race(df)

df <- code_mom_risk_pregnancy(df)

df <- code_mom_smoke(df)
df <- code_mom_wic_food(df) 
df <- code_paternity_missing(df)
df <- code_prenatal_visits(df)
df <- code_preterm_birth(df)
df$PRI_CHILD_VERY_PRETERM_BIRTH[is.na(df$PRI_CHILD_VERY_PRETERM_BIRTH)] <- 0
df$PRI_CHILD_PRETERM_BIRTH[is.na(df$PRI_CHILD_PRETERM_BIRTH)] <- 0


#save rds
setwd("working directory")
saveRDS(df, file = "birth_placements_pris.rds")



#now label with placement in, e.g., 3 years
setwd("working directory")
df <- readRDS("birth_placements_pris.rds")


df <- df %>% 
  rowwise() %>% 
  mutate(YEAR_FIRST_PLACEMENT = ifelse(!is.na(PE_S_DT), 
                                       difftime(PE_S_DT, as.Date(BIRTH_DT, format = "%Y%m%d"), units = "days") / 365, NA))

#code particular years: 1 to 6
df <- mutate(df, 
                           PRO_PLACED_1YEARS = ifelse(!is.na(YEAR_FIRST_PLACEMENT) & YEAR_FIRST_PLACEMENT <= 1, 1, 0),
                           PRO_PLACED_2YEARS = ifelse(!is.na(YEAR_FIRST_PLACEMENT) & YEAR_FIRST_PLACEMENT <= 2, 1, 0),
                           PRO_PLACED_3YEARS = ifelse(!is.na(YEAR_FIRST_PLACEMENT) & YEAR_FIRST_PLACEMENT <= 3, 1, 0),
                           PRO_PLACED_4YEARS = ifelse(!is.na(YEAR_FIRST_PLACEMENT) & YEAR_FIRST_PLACEMENT <= 4, 1, 0),
                           PRO_PLACED_5YEARS = ifelse(!is.na(YEAR_FIRST_PLACEMENT) & YEAR_FIRST_PLACEMENT <= 5, 1, 0),
                           PRO_PLACED_6YEARS = ifelse(!is.na(YEAR_FIRST_PLACEMENT) & YEAR_FIRST_PLACEMENT <= 6, 1, 0))

#save data labeled
setwd("working directory")
saveRDS(df, file = "birth_placements_pris_labeled.rds")

write.csv(df, "birth_placements_pris_labeled.csv", row.names = FALSE)

