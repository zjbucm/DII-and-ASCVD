
library(nhanesR)
library(reshape2)
library(do)
library(dplyr)
library(openxlsx)
library(survey)
library(rms)

nhs <- svy_design(d)

table1.gv <- svy_tableone(design = nhs,
                          gv = c('age3','sex','eht','BMI','edu','smoke','poverty','insurance',
                                 'hl','Hypertension','DM',
                                 'CHD','heart.attack','angina','stroke'),
                          by = 'ASCVD',
                          g_nSQper = T,
                          total = T) 
table1.gv

table1.cv <- svy_tableone(design = nhs,
                          cv = c('age','dii','bmxbmi','MET'),
                          by = 'ASCVD',
                          c_meanSQse = T,
                          total = T)
table1.cv

table1.gv |> write.xlsx('table1.gv.xlsx')
table1.cv |> write.xlsx('table1.cv.xlsx')

model1 <- svyglm(ASCVD~dii10,
                 family = quasibinomial,
                 design = nhs) |>
  reg_table()
model1

model2 <- svyglm(ASCVD~dii10+age+sex+eht+edu+smoke+poverty+insurance,
                 family = quasibinomial,
                 design = nhs) |>
  reg_table()

model3 <- svyglm(ASCVD~dii10+age+sex+eht+BMI+edu+smoke+poverty+insurance+hl+Hypertension+DM,
                 family = quasibinomial,
                 design = nhs) |>
  reg_table()

model3|> write.xlsx('model3.xlsx')
model2|> write.xlsx('model2.xlsx')
model1|> write.xlsx('model1.xlsx')

f0 <- svyglm(ASCVD ~ rcs(dii)+age+sex+eht+BMI+edu+smoke+poverty+insurance+hl+Hypertension+DM,
             family = quasibinomial,
             design = nhs)
optimal_nKnots(f0)

f10 <- svyglm(ASCVD ~ rcs(dii,10)+age+sex+eht+BMI+edu+smoke+poverty+insurance+hl+Hypertension+DM,
              family = quasibinomial,
              design = nhs)
r10 <- RCS(f10)
ggplot(r10)

Changepoints <- getChangepoints(r10)

f0 <- svyglm(CHD ~ rcs(dii)+age+sex+eht+BMI+edu+smoke+poverty+insurance+hl+Hypertension+DM,
             family = quasibinomial,
             design = nhs)
optimal_nKnots(f0) 

fCHD <- svyglm(CHD ~ rcs(dii,10)+age+sex+eht+BMI+edu+smoke+poverty+insurance+hl+Hypertension+DM,
               family = quasibinomial,
               design = nhs)
rCHD <- RCS(fCHD)
ggplot(rCHD)

Changepoints <- getChangepoints(rCHD)

rCHDsex <- RCS(fCHD, by = 'sex')
ggplot(rCHDsex)

Changepoints <- getChangepoints(rCHDsex)

bu_x <- d$dii
d$dii3[bu('( , -2]')] <- 'a'
d$dii3[bu('(-2, 3)')] <- 'b'
d$dii3[bu('[3  , )')]   <- 'c'

nhs <- svy_design(data = d)

svyglm(ASCVD~dii+age+sex+eht+BMI+edu+smoke+poverty+insurance+hl+Hypertension+DM,
       family = quasibinomial,
       design = subset(nhs,dii3=='a')) |>
  reg_table()
#2
svyglm(ASCVD~dii+age+sex+eht+BMI+edu+smoke+poverty+insurance+hl+Hypertension+DM,
       family = quasibinomial,
       design = subset(nhs,dii3=='b')) |>
  reg_table()
#3
svyglm(ASCVD~dii+age+sex+eht+BMI+edu+smoke+poverty+insurance+hl+Hypertension+DM,
       family = quasibinomial,
       design = subset(nhs,dii3=='c')) |>
  reg_table()

table(d$dii3)

svyglm(ASCVD~dii+age+eht+BMI+edu+smoke+poverty+insurance+hl+Hypertension+DM,
       family = quasibinomial,
       design = subset(nhs, sex=='Female')) |>
  reg_table()

svyglm(ASCVD~dii+age+eht+BMI+edu+smoke+poverty+insurance+hl+Hypertension+DM,
       family = quasibinomial,
       design = subset(nhs, sex=='Male')) |>
  reg_table()

bu_x <- d$dii
d$dii3[bu('( , 1]')] <- 'a'
d$dii3[bu('(1, 3)')] <- 'b'
d$dii3[bu('[3  , )')]   <- 'c'

nhs <- svy_design(data = d)
svyglm(hard~dii+age+sex+eht+BMI+edu+smoke+poverty+insurance+hl+Hypertension+DM,
       family = quasibinomial,
       design = subset(nhs,dii3=='a')) |>
  reg_table()
svyglm(hard~dii+age+sex+eht+BMI+edu+smoke+poverty+insurance+hl+Hypertension+DM,
       family = quasibinomial,
       design = subset(nhs,dii3=='b')) |>
  reg_table()
svyglm(hard~dii+age+sex+eht+BMI+edu+smoke+poverty+insurance+hl+Hypertension+DM,
       family = quasibinomial,
       design = subset(nhs,dii3=='c')) |>
  reg_table()

table(d$dii3)

svyglm(hard~dii+age+eht+BMI+edu+smoke+poverty+insurance+hl+Hypertension+DM,
       family = quasibinomial,
       design = subset(nhs, sex=='Female')) |>
  reg_table()

svyglm(hard~dii+age+eht+BMI+edu+smoke+poverty+insurance+hl+Hypertension+DM,
       family = quasibinomial,
       design = subset(nhs, sex=='Male')) |>
  reg_table()

svyglm(CHD~dii+age+sex+eht+BMI+edu+smoke+poverty+insurance+hl+Hypertension+DM,
       family = quasibinomial,
       design = nhs) |>
  reg_table()

svyglm(CHD~dii+age+eht+BMI+edu+smoke+poverty+insurance+hl+Hypertension+DM,
       family = quasibinomial,
       design = subset(nhs, sex=='Female')) |>
  reg_table()

svyglm(CHD~dii+age+eht+BMI+edu+smoke+poverty+insurance+hl+Hypertension+DM,
       family = quasibinomial,
       design = subset(nhs, sex=='Male')) |>
  reg_table()

svyglm(angina~dii+age+sex+eht+BMI+edu+smoke+poverty+insurance+hl+Hypertension+DM,
       family = quasibinomial,
       design = nhs) |>
  reg_table()

svyglm(angina~dii+age+eht+BMI+edu+smoke+poverty+insurance+hl+Hypertension+DM,
       family = quasibinomial,
       design = subset(nhs, sex=='Female')) |>
  reg_table()

svyglm(angina~dii+age+eht+BMI+edu+smoke+poverty+insurance+hl+Hypertension+DM,
       family = quasibinomial,
       design = subset(nhs, sex=='Male')) |>
  reg_table()

svyglm(heart.attack~dii+age+sex+eht+BMI+edu+smoke+poverty+insurance+hl+Hypertension+DM,
       family = quasibinomial,
       design = nhs) |>
  reg_table()

svyglm(heart.attack~dii+age+eht+BMI+edu+smoke+poverty+insurance+hl+Hypertension+DM,
       family = quasibinomial,
       design = subset(nhs, sex=='Female')) |>
  reg_table()

bu_x <- d$dii
d$diix[bu('(    , -1.5)')] <- 'a'
d$diix[bu('[-1.5 , 0)')]   <- 'b'
d$diix[bu('[0, 2.5)')] <- 'c'
d$diix[bu('[2.5 , )')]   <- 'd'

nhs <- svy_design(data = d)

svyglm(heart.attack~dii+age+eht+BMI+edu+smoke+poverty+insurance+hl+Hypertension+DM,
       family = quasibinomial,
       design = subset(nhs, sex=='Male' & diix =='a')) |>
  reg_table()
svyglm(heart.attack~dii+age+eht+BMI+edu+smoke+poverty+insurance+hl+Hypertension+DM,
       family = quasibinomial,
       design = subset(nhs, sex=='Male' & diix=='b')) |>
  reg_table()
svyglm(heart.attack~dii+age+eht+BMI+edu+smoke+poverty+insurance+hl+Hypertension+DM,
       family = quasibinomial,
       design = subset(nhs, sex=='Male' & diix =='c')) |>
  reg_table()
svyglm(heart.attack~dii+age+eht+BMI+edu+smoke+poverty+insurance+hl+Hypertension+DM,
       family = quasibinomial,
       design = subset(nhs, sex=='Male' & diix=='d')) |>
  reg_table()
bu_x <- d$dii
d$dii3[bu('( ,-1.5)')] <- 'a'
d$dii3[bu('[-1.5 ,3)')]   <- 'b'
d$dii3[bu('[3  , )')]   <- 'c'

nhs <- svy_design(data = d)
svyglm(stroke~dii+age+sex+eht+BMI+edu+smoke+poverty+insurance+hl+Hypertension+DM,
       family = quasibinomial,
       design = subset(nhs,dii3=='a')) |>
  reg_table()
svyglm(stroke~dii+age+sex+eht+BMI+edu+smoke+poverty+insurance+hl+Hypertension+DM,
       family = quasibinomial,
       design = subset(nhs,dii3=='b')) |>
  reg_table()
svyglm(stroke~dii+age+sex+eht+BMI+edu+smoke+poverty+insurance+hl+Hypertension+DM,
       family = quasibinomial,
       design = subset(nhs,dii3=='c')) |>
  reg_table()
table(d$dii3)

bu_x <- d$dii
d$diix[bu('(, 2.5)')] <- 'a'
d$diix[bu('[2.5 , )')]   <- 'b'

nhs <- svy_design(data = d)
svyglm(stroke~dii+age+eht+BMI+edu+smoke+poverty+insurance+hl+Hypertension+DM,
       family = quasibinomial,
       design = subset(nhs, sex=='Female' & diix =='a')) |>
  reg_table()
svyglm(stroke~dii+age+eht+BMI+edu+smoke+poverty+insurance+hl+Hypertension+DM,
       family = quasibinomial,
       design = subset(nhs, sex=='Female' & diix=='b')) |>
  reg_table()

bu_x <- d$dii
d$diix[bu('( ,-2)')] <- 'a'
d$diix[bu('[-2,3.5)')]   <- 'b'
d$diix[bu('[3.5 , )')]   <- 'c'

nhs <- svy_design(data = d)

svyglm(stroke~dii+age+eht+BMI+edu+smoke+poverty+insurance+hl+Hypertension+DM,
       family = quasibinomial,
       design = subset(nhs, sex=='Male' & diix =='a')) |>
  reg_table()
svyglm(stroke~dii+age+eht+BMI+edu+smoke+poverty+insurance+hl+Hypertension+DM,
       family = quasibinomial,
       design = subset(nhs, sex=='Male' & diix=='b')) |>
  reg_table()
svyglm(stroke~dii+age+eht+BMI+edu+smoke+poverty+insurance+hl+Hypertension+DM,
       family = quasibinomial,
       design = subset(nhs, sex=='Male' & diix=='c')) |>
  reg_table()

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
                  levels = c('2', '1','3', '4','5','6','7','8','9','10'))

nhs <- svy_design(d)

model1 <- svyglm(ASCVD~dii10,
                 family = quasibinomial,
                 design = nhs) |>
  reg_table()

model2 <- svyglm(ASCVD~dii10+age+sex+eht+edu+smoke+poverty+insurance,
                 family = quasibinomial,
                 design = nhs) |>
  reg_table()

model3 <- svyglm(ASCVD~dii10+age+sex+eht+BMI+edu+smoke+poverty+insurance+hl+Hypertension+DM,
                 family = quasibinomial,
                 design = nhs) |>
  reg_table()

model3|> write.xlsx('model3.xlsx')
model2|> write.xlsx('model2.xlsx')
model1|> write.xlsx('model1.xlsx')















