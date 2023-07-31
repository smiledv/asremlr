library(asreml)
library(readxl)

ped <- read_excel("CCFAR Pedigree until 2020.xlsx")
ainv <- ainverse(ped)

pelt <- read_excel("CCFAR Live grading.xlsx", 
                                 sheet = "Sheet2")
elisa <- read_excel("CCFAR ELISA (until 2021) and CIEP (2018 and 2019).xlsx", 
                   sheet = "Sheet2")
harvest <- read_excel("FEtraits.xlsx", 
                      sheet = "Sheet2")
pcv <- read_excel("CompleteUnivariate CCFAR_MILAD12.03_PCV.xlsx", 
                    sheet = "Sheet2")

data <- merge(pelt, elisa, by=c("Animal","Dam","Sire","Year","Color","Sex"), all = T)
data <- merge(data, harvest, by=c("Animal","Dam","Year","Color","Sex"), all = T)
data <- merge(data, pcv, by=c("Animal","Dam","Sire","Year","Color","Sex"), all = T)

data <- data[!duplicated(data[c("Animal","Dam","Sire","Year","Color","Sex","qELISA_NS","Blood sample Age.x")]),]
data <- data[!duplicated(data[c("Animal","Dam","Sire","Year","Color","Sex","Live_grade")]),]
data <- data[!duplicated(data[c("Animal","Dam","Sire","Year","Color","Sex","HW")]),]
data <- data[!duplicated(data[c("Animal","Dam","Sire","Year","Color","Sex","HL")]),]
data <- data[!duplicated(data[c("Animal","Dam","Sire","Year","Color","Sex","PCV")]),]

mink <- data

mink$sqrt_elisa <- sqrt(mink$qELISA_NS)* (-1)
mink$sqrt_elisa <- as.numeric(mink$sqrt_elisa)
mink$HW <- as.numeric(mink$HW)
mink$HL <- as.numeric(mink$HL)
mink$Age <- as.numeric(mink$Age)
mink$Live_grade <- as.numeric(mink$Live_grade)
mink$PCV <- as.numeric(mink$PCV)
mink$blood_sample_age <- as.numeric(mink$`Blood sample Age.x`)

mink$Sex <- as.factor(mink$Sex)
mink$Color <- as.factor(mink$Color)
mink$Year <- as.factor(mink$Year)
mink$CEIP <- as.factor(mink$CEIP)
mink$Dam <- as.factor(mink$Dam)
mink$Penv <- as.factor(mink$Animal)
mink$Animal <- as.factor(mink$Animal)

nlevels(mink$Animal)
nlevels(mink$Sex)
nlevels(mink$Color)
nlevels(mink$Year)
nlevels(mink$Dam)
nlevels(mink$ComLit)
nlevels(mink$CEIP)
nlevels(mink$Penv)

#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
###Aleutex univariate model
mink <- elisa <- read_excel("CCFAR ELISA (until 2021) and CIEP (2018 and 2019).xlsx", 
                            sheet = "Sheet2")
mink$sqrt_elisa <- sqrt(mink$qELISA_NS)* (-1)
mink$sqrt_elisa <- as.numeric(mink$sqrt_elisa)
mink$blood_sample_age <- as.numeric(mink$`Blood sample Age`)

mink$Sex <- as.factor(mink$Sex)
mink$Color <- as.factor(mink$Color)
mink$Year <- as.factor(mink$Year)
mink$CEIP <- as.factor(mink$CEIP)
mink$Dam <- as.factor(mink$Dam)
mink$Penv <- as.factor(mink$Animal)
mink$Animal <- as.factor(mink$Animal)


model1 <- asreml(fixed = sqrt_elisa ~ 1  + CEIP 
                 , random = ~ vm(Animal,ainv)  + vm(Dam,ainv) + Penv
                 , data = mink
                 , residual = ~ id(units)
                 , na.action.Y = "omit"
                 , na.action.X = "omit")

wald(model1)
summary(model1)$varcomp
vpredict(model1, Pv ~ V1)
vpredict(model1, Gv ~ V2)
vpredict(model1, Dv ~ V3)
vpredict(model1, Rv ~ V4)

vpredict(model1, PHV ~ (V1 + V2 + V3))
vpredict(model1, hA ~ V2/(V1 + V2 + V3 + V4))


write.csv(model1$coefficients$random, file = "AleuTeX_EBV.csv")
#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
###PCV univariate model
mink <- read_excel("CompleteUnivariate CCFAR_MILAD12.03_PCV.xlsx", 
                  sheet = "Sheet2")

mink$PCV <- as.numeric(mink$PCV)
mink$blood_sample_age <- as.numeric(mink$`Blood sample Age`)
mink$Animal <- as.factor(mink$Animal)
mink$Sex <- as.factor(mink$Sex)
mink$Color <- as.factor(mink$Color)
mink$Year <- as.factor(mink$Year)
mink$Dam <- as.factor(mink$Dam)
mink$Penv <- as.factor(mink$Animal)


model1 <- asreml(fixed = PCV ~ 1 + blood_sample_age + Year + Color
                 , random = ~ vm(Animal,ainv) 
                 , data = mink
                 , residual = ~ id(units)
                 , na.action.Y = "omit"
                 , na.action.X = "omit")

wald(model1)
summary(model1)$varcomp
vpredict(model1, Gv ~ V1)
vpredict(model1, Rv ~ V2)
vpredict(model1, PHV ~ (V1 + V2))
vpredict(model1, hA ~ V1/(V1 + V2))
write.csv(model1$coefficients$random, file = "PCV_EBV.csv")
#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
###Harvest weight univariate model
mink <- harvest
mink$HW <- as.numeric(mink$HW)
mink$HL <- as.numeric(mink$HL)
mink$Age <- as.numeric(mink$Age)
mink$Dam <- as.factor(mink$Dam)
mink$Animal <- as.factor(mink$Animal)
mink$Sex <- as.factor(mink$Sex)
mink$Color <- as.factor(mink$Color)
mink$Year <- as.factor(mink$Year)

mink <- mink[!is.na(mink$Harvest_weight),]
mink <- mink[!is.na(mink$Harvest_age),]
mink <- mink[!is.na(mink$Sex),]
mink <- mink[!is.na(mink$Year),]
mink <- mink[!is.na(mink$Color),]



model1 <- asreml(fixed = HW ~ 1  + Sex + Year 
                 , random = ~ vm(Animal,ainv) + vm(Dam,ainv)
                 , data = mink
                 , residual = ~ id(units)
                 , na.action.Y = "omit"
                 , na.action.X = "omit")

wald(model1)
summary(model1)$varcomp
vpredict(model1, Gv ~ V1)
vpredict(model1, Rv ~ V3)
vpredict(model1, Dv ~ V2)
vpredict(model1, hA ~ V1/(V1 + V2 + V3 ))
write.csv(model1$coefficients$random, file = "HW_EBV.csv")
#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
###Harvest length univariate model
mink <- mink[!is.na(mink$Harvest_length),]
mink <- mink[!is.na(mink$Harvest_age),]
mink <- mink[!is.na(mink$Sex),]
mink <- mink[!is.na(mink$Year),]
mink <- mink[!is.na(mink$Color),]
mink <- mink[!is.na(mink$Dam),]

model2 <- asreml(fixed = HL ~ 1 + Sex 
                 , random = ~ vm(Animal,ainv) 
                 , data = mink
                 , residual = ~ id(units)
                 , na.action.Y = "omit"
                 , na.action.X = "omit")


wald(model2)
summary(model2)$varcomp
vpredict(model2, Gv ~ V1)
vpredict(model2, Rv ~ V2)
vpredict(model2, PHV ~ (V1 + V2))
vpredict(model2, hA ~ V1/(V1 + V2))
write.csv(model2$coefficients$random, file = "HL_EBV.csv")
#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
###live pelt quality model
mink <- pelt
mink$Dam <- as.factor(mink$Dam)
mink$Penv <- as.factor(mink$Animal)
mink$Animal <- as.factor(mink$Animal)
mink$Sex <- as.factor(mink$Sex)
mink$Color <- as.factor(mink$Color)
mink$Year <- as.factor(mink$Year)

mink <- mink[!is.na(mink$Live_grade),]
mink <- mink[!is.na(mink$Sex),]
mink <- mink[!is.na(mink$Year),]
mink <- mink[!is.na(mink$Color),]

model1 <- asreml(fixed = Live_grade ~ 1 + Year + Color  
                 , random = ~ vm(Animal,ainv) 
                 , data = mink
                 , residual = ~ id(units)
                 , na.action.Y = "omit"
                 , na.action.X = "omit")


wald(model1)
summary(model1)$varcomp
vpredict(model1, Gv ~ V1)
vpredict(model1, Gv ~ V2)
vpredict(model1, PHV ~ (V1 + V2))
vpredict(model1, hA ~ V1/(V1 + V2))
write.csv(model1$coefficients$random, file = "LPQ_EBV.csv")
#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
###AleUtex & harvest weight bivariate model

mink <- mink[!is.na(mink$sqrt_elisa),]
mink <- mink[!is.na(mink$HW),]

model_cor <- asreml(cbind(sqrt_elisa, HW) ~ trait + at(trait,1):CEIP + at(trait,2):Sex + at(trait,2):Year, 
                    random=  ~ us(trait):vm(Animal, ainv) + us(trait):vm(Dam, ainv),
                    residual = ~ units:us(trait),
                    data = mink,
                    na.action.Y = "omit",
                    na.action.X = "omit",
                    maxiter = 20)

summary(model_cor)$varcomp

#PHENOTYPIC CORRELATION
vpredict(model_cor, pc ~ (V2+V5+V9)/ sqrt((V1+V4+V8)*(V3+V6+V10)))

#GENETIC CORRELATION
vpredict(model_cor, gc ~V2 / sqrt((V1 * V3)))

#HERITABILITY
vpredict(model_cor, h2 ~V1 / (V1 + V4 + V8 ))
vpredict(model_cor, h2 ~V3 / (V3 + V6 + V10))

#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
###AleUtex & harvest length bivariate model

mink <- mink[!is.na(mink$sqrt_elisa),]
mink <- mink[!is.na(mink$HL),]

model_cor <- asreml(cbind(sqrt_elisa, HL) ~ trait + at(trait,1):CEIP + at(trait,2):Sex, 
                    random=  ~ us(trait,init=c(0.1,0,0.1)):vm(Animal, ainv) + at(trait,1):vm(Dam, ainv),
                    residual = ~ units:us(trait,init=c(0.1,0,0.1)),
                    data = mink,
                    na.action.Y = "omit",
                    na.action.X = "omit",
                    maxiter = 20)

summary(model_cor)$varcomp

#PHENOTYPIC CORRELATION
vpredict(model_cor,pc ~ (V3 +V7)/ sqrt((V1 + V2 + V6 )*(V4+V8)))

#GENETIC CORRELATION
vpredict(model_cor, gc~V3 / sqrt((V2*V4)))

#HERITABILITY
vpredict(model_cor, h2 ~V2 / (V1 + V2 + V6 ))
vpredict(model_cor, h2 ~V4 / (V4 + V8 ))

#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
###AleUtex & pelt quality model

mink <- mink[!is.na(mink$sqrt_elisa),]
mink <- mink[!is.na(mink$Live_grade),]

model_cor <- asreml(cbind(sqrt_elisa, Live_grade) ~ trait + at(trait,1):CEIP + at(trait,2):Color + at(trait,2):Year,
                    random=  ~ us(trait,init=c(0.1,0,0.1)):vm(Animal, ainv) + at(trait,1):vm(Dam, ainv),
                    data = mink,
                    na.action.Y = "omit",
                    na.action.X = "omit",
                    maxiter = 20)

summary(model_cor)$varcomp

#PHENOTYPIC CORRELATION
vpredict(model_cor,pc ~ (V3 +V7)/ sqrt((V1 + V2 + V6 )*(V4+V8)))

#GENETIC CORRELATION
vpredict(model_cor, gc~V3 / sqrt((V2*V4)))

#HERITABILITY
vpredict(model_cor, h2 ~V2 / (V1 + V2 + V6 ))
vpredict(model_cor, h2 ~V4 / (V4 + V8 ))

#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
###Harvest weight & length

mink <- mink[!is.na(mink$HW),]

model_cor <- asreml(cbind(HW, HL) ~ trait +Sex + at(trait,1):Year,
                    random=  ~ us(trait,init=c(0.1,0,0.1)):vm(Animal, ainv) + at(trait,1):vm(Dam, ainv),
                    data = mink,
                    na.action.Y = "omit",
                    na.action.X = "omit",
                    maxiter = 20)

summary(model_cor)$varcomp

#PHENOTYPIC CORRELATION
vpredict(model_cor,pc ~ (V3 +V7)/ sqrt((V1 + V2 + V6 )*(V4+V8)))

#GENETIC CORRELATION
vpredict(model_cor, gc~V3 / sqrt((V2*V4)))

#HERITABILITY
vpredict(model_cor, h2 ~V2 / (V1 + V2 + V6 ))
vpredict(model_cor, h2 ~V4 / (V4 + V8 ))

#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
###Harvest weight & pelt quality

mink <- mink[!is.na(mink$HW),]
mink <- mink[!is.na(mink$Live_grade),]

model_cor <- asreml(cbind(HW, Live_grade) ~ trait + at(trait,2):Color + at(trait,2):Year +  at(trait,1):Sex ,
                    random=  ~ us(trait,init=c(0.1,0,0.1)):vm(Animal, ainv) + at(trait,1):vm(Dam, ainv),
                    residual = ~ units:us(trait,init=c(0.1,0.5,0.1)),
                    data = mink,
                    na.action.Y = "omit",
                    na.action.X = "omit",
                    maxiter = 20)

summary(model_cor)$varcomp

#PHENOTYPIC CORRELATION
vpredict(model_cor,pc ~ (V3 +V7)/ sqrt((V1 + V2 + V6 )*(V4+V8)))

#GENETIC CORRELATION
vpredict(model_cor, gc~V3 / sqrt((V2*V4)))

#HERITABILITY
vpredict(model_cor, h2 ~V2 / (V1 + V2 + V6 ))
vpredict(model_cor, h2 ~V4 / (V4 + V8 ))

#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
###Harvest length & pelt quality

mink <- mink[!is.na(mink$HL),]
mink <- mink[!is.na(mink$Live_grade),]

model_cor <- asreml(cbind(HL, Live_grade) ~ trait +  at(trait,1):Sex + at(trait,2):Color + at(trait,2):Year,
                    random=  ~ us(trait,init=c(0.1,0,0.1)):vm(Animal, ainv),
                    data = mink,
                    na.action.Y = "omit",
                    na.action.X = "omit",
                    maxiter = 20)

summary(model_cor)$varcomp

#PHENOTYPIC CORRELATION
vpredict(model_cor,pc ~ (V2 +V6)/ sqrt(( V1 + V5 )*(V3+V7)))

#GENETIC CORRELATION
vpredict(model_cor, gc~V2 / sqrt((V1*V3)))

#HERITABILITY
vpredict(model_cor, h2 ~V1 / (V1 + V5 ))
vpredict(model_cor, h2 ~V3 / (V3 + V7 ))

#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
###Aleutex & PCV


mink <- mink[!is.na(mink$sqrt_elisa),]
mink <- mink[!is.na(mink$PCV),]
mink <- mink[!is.na(mink$blood_sample_age),]

model_cor <- asreml(cbind(sqrt_elisa, PCV) ~ trait + at(trait,1):CEIP + at(trait,2):blood_sample_age + at(trait,2):Year + at(trait,2):Color, 
                    random=  ~ us(trait,init=c(0.1,0,0.1)):vm(Animal, ainv) + at(trait,1):vm(Dam, ainv) + at(trait,1):Penv,
                    residual = ~ units:us(trait,init=c(0.1,0,0.1)),
                    data = mink,
                    na.action.Y = "omit",
                    na.action.X = "omit",
                    maxiter = 20)

summary(model_cor)$varcomp

#PHENOTYPIC CORRELATION
vpredict(model_cor,pc ~ (V4 +V8)/ sqrt((V1 + V2 + V3 + V7 )*(V5 + V9)))

#GENETIC CORRELATION
vpredict(model_cor, gc~V4 / sqrt((V3*V5)))

#HERITABILITY
vpredict(model_cor, h2 ~V3 / (V1 + V2 + V3 + V7 ))
vpredict(model_cor, h2 ~V5 / (V5 + V9))

#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
###Harvest weight & PCV
pcv <- read_excel("CompleteUnivariate CCFAR_MILAD12.03_PCV.xlsx", 
                  sheet = "Sheet2")
harvest <- read_excel("FEtraits.xlsx", 
                      sheet = "Sheet2")
mink <- merge(pcv,harvest, by=c("Animal","Dam","Sex","Year","Color"))
mink <- mink[!is.na(mink$HW),]
mink <- mink[!is.na(mink$PCV),]


model_cor <- asreml(cbind(HW, PCV) ~ trait + Year+ at(trait,1):Sex  + at(trait,2):Color+ at(trait,2):blood_sample_age,
                    random=  ~ us(trait,init=c(0.1,0,0.1)):vm(Animal, ainv) + at(trait,1):vm(Dam, ainv),
                    data = mink,
                    residual = ~ units:us(trait,init=c(0.1,0,0.1)),
                    na.action.Y = "omit",
                    na.action.X = "omit",
                    maxiter = 20)

summary(model_cor)$varcomp

#PHENOTYPIC CORRELATION
vpredict(model_cor,pc ~ (V3 +V7)/ sqrt((V1 + V2 + V6 )*(V4+V8)))

#GENETIC CORRELATION
vpredict(model_cor, gc~V3 / sqrt((V2*V4)))

#HERITABILITY
vpredict(model_cor, h2 ~V2 / (V1 + V2 + V6 ))
vpredict(model_cor, h2 ~V4 / (V4 + V8 ))


#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
###Harvest length & pelt quality

mink <- mink[!is.na(mink$Harvest_length),]
mink <- mink[!is.na(mink$PCV),]
mink <- mink[!is.na(mink$blood_sample_age),]

model_cor <- asreml(cbind(HL, PCV) ~ trait +  at(trait,1): Sex + at(trait,2):Color+ at(trait,2):Year+ at(trait,2):blood_sample_age,
                    random=  ~ us(trait,init=c(0.1,0,0.1)):vm(Animal, ainv),
                    data = mink,
                    na.action.Y = "omit",
                    na.action.X = "omit",
                    maxiter = 20)

summary(model_cor)$varcomp

#PHENOTYPIC CORRELATION
vpredict(model_cor,pc ~ (V2 +V6)/ sqrt(( V1 + V5 )*(V3+V7)))

#GENETIC CORRELATION
vpredict(model_cor, gc~V2 / sqrt((V1*V3)))

#HERITABILITY
vpredict(model_cor, h2 ~V1 / (V1 + V5 ))
vpredict(model_cor, h2 ~V3 / (V3 + V7 ))

#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
###pelt quality & pcv

mink <- mink[!is.na(mink$PCV),]
mink <- mink[!is.na(mink$blood_sample_age),]
mink <- mink[!is.na(mink$Live_grade),]
mink <- mink[!is.na(mink$Color),]
mink <- mink[!is.na(mink$Year),]

model_cor <- asreml(cbind(Live_grade, PCV) ~ trait + Color+ Year+ at(trait,2):blood_sample_age,
                    random=  ~ us(trait):vm(Animal, ainv),
                    residual = ~ units:us(trait),
                    data = mink,
                    na.action.Y = "omit",
                    na.action.X = "omit",
                    maxiter = 20)

summary(model_cor)$varcomp

#PHENOTYPIC CORRELATION
vpredict(model_cor,pc ~ (V2 +V6)/ sqrt(( V1 + V5 )*(V3+V7)))

#GENETIC CORRELATION
vpredict(model_cor, gc~V2 / sqrt((V1*V3)))

#HERITABILITY
vpredict(model_cor, h2 ~V1 / (V1 + V5 ))
vpredict(model_cor, h2 ~V3 / (V3 + V7 ))
