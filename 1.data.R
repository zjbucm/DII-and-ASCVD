library(devtools)
devtools::install_github('yikeshu0611/nhshelp',force=TRUE)

nhshelp::install_nhanesR('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')

# ——————————————————————————————————————————————————————————————

setwd('I:\\NHANES\\Research\\DII and ASCVD\\DII and ASCVD 2022.11.03')

library(nhanesR)
library(reshape2)
library(do)
library(dplyr)
library(openxlsx)
library(survey)

nhs_tsv('demo',years = 1999:2018)

dii_demo <- db_demo(years = 1999:2018,
                    ageyr = 'age',
                    sex = 'sex',
                    eth1 = 'eht',
                    edu = 'edu',
                    marital = 'Marital status',
                    annual_family_income = 'Annual family income',
                    Year = T)
dii_demo <- dii_demo[dii_demo$age>=20,]


dietyear <- nhs_tsv('tot')
dietyear1 <- nhs_read(dietyear,'wtdrd1','wtdr4yr')
dietyear2 <- drop_col(dietyear1,'wtdrd1','wtdrd1.y','wtdrd1.x.x','wtdrd1.y.y')
d <- inner_join(dii_demo,dietyear2,'seqn')
d$nhs_wt <- ifelse(d$Year.x %in% c('1999-2000','2001-2002'),2/10*d$wtdr4yr,
                   1/10*d$wtdrd1.x)
d <- drop_col(d,'wtdr4yr','wtdrd1.x')

dii_dii <- read.xlsx('dii.xlsx')

nhs_tsv('bmx|bmx_b') #find
bmi <- nhs_tsv('bmx|bmx_b')
dii_bmi <- nhs_read(bmi,'bmxbmi')
dii_bmi <- drop_col(dii_bmi,'Year')

dii_ins <- read.xlsx('insurance.xlsx')

poverty <- nhs_tsv('DEMO')
dii_poverty <- nhs_read(poverty,'INDFMPIR')
dii_poverty <- drop_col(dii_poverty,'Year','sdmvpsu','sdmvstra')

d_MET <- dex_PhysicalActivity(
  years=1999:2018,
  all.5 = FALSE,
  walk_bicycle = TRUE,
  Tasks.HomeYard = TRUE,
  Muscle.strength = TRUE,
  WorkActivity = TRUE,
  RecreationalActivity = TRUE,
  activity = TRUE,
  time = TRUE,
  MET = TRUE,
  week = TRUE,
  direction = c("m", "v", "no"),
  total_time=TRUE,
  total_MET=TRUE,
  component = FALSE,
  Year = TRUE,
  join = "left")

head(d_MET)
tail(d_MET)
unique(d_MET$PA_total_MET)

dii_met <- drop_col(d_MET,'walk_bicycle','Tasks.HomeYard','Muscle.strength','work.activity','recreational.activity','PA_total_time','Year')
dii_met$MET <- dii_met$PA_total_MET
dii_met <- drop_col(dii_met,'PA_total_MET')

ASCVD <- diag_ASCVD(years = 1999:2018)
smq <- diag_smoke(years = 1999:2018)

angina <- diag_angina(years = 1999:2018)
heart_attack <- diag_heart.attack(years = 1999:2018)
stroke<- diag_stroke(years = 1999:2018)
CHD<- diag_coronary.heart.disease(years = 1999:2018)

DM <- diag_DM(years = 1999:2018)
hypertension <- diag_Hypertension(years = 1999:2018)
hyperlipidemia <- diag_Hyperlipidemia(years = 1999:2018)

d1 <- d
D <- d
D <- inner_join(D,ASCVD,'seqn')
D = drop_row(D, is.na(D$ASCVD))
D <- inner_join(D,dii_dii,'seqn')
D = drop_row(D, is.na(D$dii)) 
D = drop_row(D, is.na(D$nhs_wt))
D1 <- D
d <- D

D <- inner_join(D,smq,'seqn') # smq
D <- inner_join(D,dii_bmi,'seqn') # BMI
D <- inner_join(D,dii_poverty,'seqn') # BMI
D <- inner_join(D,dii_ins,'seqn') # BMI

D <- inner_join(D,angina,'seqn')
D <- inner_join(D,heart_attack,'seqn')
D <- inner_join(D,stroke,'seqn')
D <- inner_join(D,CHD,'seqn')

D <- inner_join(D,DM,'seqn') 
D <- inner_join(D,hyperlipidemia,'seqn') 
D <- inner_join(D,hypertension,'seqn') 
D <- inner_join(D,dii_met,'seqn')
D <- drop_col(D,'Year.x','Year.y')

d <- D 
    
D$hard = paste0(D$heart.attack, '~', D$stroke)
# Recode(D$hard)
D$hard <- Recode(D$hard,
                          "no~no::0", 
                          "yes~no::1", 
                          "no~yes::1", 
                          "yes~yes::1", 
                          "NA~no::NA", 
                          "NA~yes::1", 
                          "NA~NA::NA", 
                          "no~NA::NA", 
                          "yes~NA::1",
                          to.numeric = TRUE)

nrow(D)
d <- D  
D2 <- D 


D|> write.xlsx('all data.xlsx')


d$ASCVD <- Recode(d$ASCVD,
                  "no::0", 
                  "yes::1",
                  to.numeric = TRUE)
d$ASCVD <- factor(d$ASCVD,
                  levels = c('0','1'))

d$hard <- Recode(d$hard,
                          "no::0", 
                          "yes::1",
                          to.numeric = TRUE)
d$hard <- factor(d$hard,
                      levels = c('0','1'))

d$angina <- Recode(d$angina,
	"no::0", 
	"yes::1", 
	to.numeric = TRUE)
d$angina <- factor(d$angina,levels = c('0','1'))

d$heart.attack <- Recode(d$heart.attack,
                   "no::0", 
                   "yes::1", 
                   to.numeric = TRUE)
d$heart.attack <- factor(d$heart.attack,levels = c('0','1'))

d$stroke <- Recode(d$stroke,
                   "no::0", 
                   "yes::1", 
                   to.numeric = TRUE)
d$stroke <- factor(d$stroke,levels = c('0','1'))

d$CHD <- Recode(d$coronary.heart.disease,
                         "no::0", 
                         "yes::1", 
                         to.numeric = TRUE)
d$CHD <- factor(d$CHD,levels = c('0','1'))

d <- drop_col(d,'coronary.heart.disease')

D <- d

quantile(d$dii, c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1))
bu_x <- d$dii
d$dii10[bu('(  , -1.1382949]')] <- '1'
d$dii10[bu('(-1.1382949, -0.1440404]')] <- '2'
d$dii10[bu('(-0.1440404,0.5829917]')] <- '3'
d$dii10[bu('(0.5829917,1.1959413]')] <- '4'
d$dii10[bu('(1.1959413,1.7526220]')] <- '5'
d$dii10[bu('(1.7526220,2.2553322]')] <- '6'
d$dii10[bu('(2.2553322, 2.7159579]')] <- '7'
d$dii10[bu('(2.7159579,3.1938060]')] <- '8'
d$dii10[bu('(3.1938060,3.7337556]')] <- '9'
d$dii10[bu('(3.7337556, )')] <- '10'
d$dii10 <- factor(d$dii10,
                  levels = c('1', '2','3', '4','5','6','7','8','9','10'))

bu_x <- d$age
d$age3[bu('[20  , 45)')] <- '20-44'
d$age3[bu('[45  , 65)')] <- '45-64'
d$age3[bu('[65 ,   )')] <- '>=65'
d$age3 <- factor(d$age3,
                 levels=c('20-44','45-64','>=65'))

d$sex <- factor(d$sex,
                levels = c('Female','Male'))

d$eht <- Recode(d$eht,
                "Non-Hispanic White::white", 
                "Non-Hispanic Black::black", 
                "Other Race - Including Multi-Racial::other", 
                "Mexican American::mexican", 
                "Other Hispanic::other")
d$eht <- factor(d$eht,
                levels = c('white','black','mexican','other'))

bu_x <- d$bmxbmi
d$BMI[bu('(    , 18.5)')] <- 'Low weight'
d$BMI[bu('[18.5, 25)')]   <- 'Normal weight'
d$BMI[bu('[25  , 30)')]   <- 'Overweight'
d$BMI[bu('[30  ,   )')]   <- 'Obesity'
d$BMI <- factor(d$BMI,
                levels = c('Normal weight','Low weight','Overweight','Obesity'))


d$smoke <- factor(d$smoke,
                  levels = c('never','former','now'))

bu_x <- d$INDFMPIR
d$INDFMPIR[bu('(    , 1]')] <- '<=1'
d$INDFMPIR[bu('(1, )')]   <- '>1'

d$poverty <- factor(d$INDFMPIR,
                    levels = c('<=1', '>1'))
d <- drop_col(d,'INDFMPIR','Annualfamilyincome')

unique(d$edu)
d$edu <- Recode(d$edu,
                "College Graduate or above::College Graduate or above", 
                "9-11th Grade (Includes 12th grade with no diploma)::< High school", 
                "High School Grad/GED or Equivalent::High school graduate", 
                "Some College or AA degree::Some college", 
                "Less Than 9th Grade::< High school", 
                "High school graduate/GED or equivalent::High school graduate", 
                "Some college or AA degree::Some college", 
                "College graduate or above::College Graduate or above", 
                "9-11th grade (Includes 12th grade with no diploma)::< High school", 
                "Less than 9th grade::< High school")

d$edu <- factor(d$edu,levels=c('< High school','High school graduate','Some college','College Graduate or above'))

d$insurance <- factor(d$insurance,
                      levels = c('Yes','No'))

d$hl <- factor(d$Hyperlipidemia,
               levels = c('no','yes'))
     d <- drop_col(d,'Hyperlipidemia')

d$Hypertension <- factor(d$Hypertension,
                         levels = c('no','yes'))

d$DM <- factor(d$DM,
               levels = c('no','DM','IFG','IGT'))

nhs <- svy_design(d)

D <- d  
D3 <- d 
D|> write.xlsx('data.xlsx')



