################################################################################
#######THESIS CHP 1: STREAM-ASSOC AMPHIB OCCU MODELS AND eDNA TRANSPORT#########
#############################KELSEY MARSHALL 2021###############################

#####READ IN PACKAGES#####
library('unmarked')
library(AICcmodavg)
library('MuMIn')
library(sampler)
library(lattice)
library(dplyr)
library(ggplot2)
library(tibble)
library(Rfast)
library(aod)
library(ResourceSelection)
library(drc)
library(nlme)
library(devtools)
library(aomisc)
library(ggthemes)

#####READ IN AND FORMAT DATA#####

setwd("C:/RFiles")
Data2019 <- read.csv("JDSF_2019.csv")
colnames(Data2019)[1] <- "Site"
Data2019WS <- Data2019 %>%
  filter(WS != "BRW", WS !="Jug", WS != "RG", WS !="Hare", Class != "NA")

Data2019DZRM <- Data2019 %>%
  filter(DDens !=0)
Data2019AZRM <- Data2019 %>%
  filter(ADens !=0)

Data2020 <- read.csv("AllSpHab2020.csv")
colnames(Data2020)[1] <- "Site"
Data2020 <- Data2020 %>%
  filter(WS != "BRW", WS !="Jug", WS !="Hare")

summary(Data2020)

Source20 <- Data2020 %>%
  filter(Source == 1)
Source20DZRM <- Source20 %>%
  filter(DDens != 0, DDens !="NA")
Source20AZRM <- Source20 %>%
  filter(ADens != 0, ADens !="NA")

JDSF1920 <- read.csv("JDSF1920.csv")
colnames(JDSF1920)[2] <- "Site"
colnames(JDSF1920)[1] <- "Year"
JDSF1920 <- JDSF1920 %>%
  filter(CTFMod != 0)

WRCOV <- read.csv("WRCOV.csv")
WRCOV <- WRCOV %>%
  filter(Sqavg !="NA", Sqavg !=0, WRcov !=0)
colnames(WRCOV)[1] <- "SP"

WRCOV_ASTR <- WRCOV %>%
  filter(SP=="ASTR")
WRCOV_DITE <- WRCOV %>%
  filter(SP=="DITE")

JDSFRepeat <- read.csv("RepeatSites.csv")
colnames(JDSFRepeat)[1] <- "Site"

JDSF1920AZRM <- JDSF1920 %>%
  filter(ADens !=0)
JDSF1920DZRM <- JDSF1920 %>%
  filter(DDens !=0)

colnames(TransPt)[1] <- "number"
DITEBOXDATA <- TransPt %>%
  filter(TransectDist ==110)
DITEBOXDATA <- read.csv("DITEBOX.csv")
colnames(DITEBOXDATA)[1] <- "Species"

BinnedSource <- read.csv("BinDistTrans.csv")
Binned2Sp <- read.csv("Binned2sp.csv")
colnames(Binned2Sp)[1] <- "Species"
Bin100TwoSp <- Binned2Sp %>%
  filter (Bin == 100)
TransPt <- read.csv("TransectPts.csv")

colnames(TransPt)[1] <- "Transect"

Yr2Yr <- read.csv("Yr2Yr.csv")
colnames(Yr2Yr)[1] <- "Year"
Yr2YrD <- Yr2Yr %>%
  filter(Sp == "DITE")
Yr2YrA <- Yr2Yr %>%
  filter(Sp == "ASTR")

#Import and format 2020 site habitat data
ADHAB <- read.csv("C:/2020CalFireR/ASTRDITEHAB.csv")
ADHAB <- ADHAB %>% select(1:10, 12, 14:17, 19:24, 26:29, 34, 39:42, 45)
colnames(ADHAB)[1] <- "Site"
colnames(ADHAB)[14] <- "Watershed"
colnames(ADHAB)[23] <- "CWD"
colnames(ADHAB)[24] <- "SRTypeNum"
colnames(ADHAB)[16] <- "nFilters"
colnames(ADHAB)[31] <- "CGap"
ADHAB <- arrange(ADHAB, order(SITENUM))

HABC2 <- ADHAB %>%
  filter(CTFMod != "NA") %>%
  filter(Class != "1") %>%
  select(1, 12, 14, 17:21, 23:25, 27:29)
HABAllC2 <- ADHAB %>%
  filter(Class != "1") %>%
  filter(CatchArea != "NA") %>%
  filter(SRTypeNum != "NA") %>%
  select(1, 12, 14, 17:21, 23:25, 27:29)

HABC2Sites <- ADHAB %>%
  filter(CTFMod != "NA") %>%
  filter(Class != "1") %>%
  select(1)
HABAllC2Sites <- ADHAB %>%
  filter(Class != "1") %>%
  filter(CatchArea != "NA") %>%
  filter(SRTypeNum != "NA") %>%
  select(1)




#####CREATE OCCU MOD LIST, SELECT BEST MODEL, GET OCCU RATE 2019#####
y <- Data2019[,14:16]
site12Covs <- Data2019[,62:70]
ASTRC12 <- unmarkedFrameOccu(y = y, siteCovs = site12Covs)
summary(ASTRC12)

ASTRC12@siteCovs$LS <- scale(ASTRC12@siteCovs$LS)
ASTRC12@siteCovs$Grad <- scale(ASTRC12@siteCovs$Grad)
ASTRC12@siteCovs$THP5Yr <- scale(ASTRC12@siteCovs$THP5Yr)
ASTRC12@siteCovs$CatchArea <- scale(ASTRC12@siteCovs$CatchArea)
ASTRC12@siteCovs$CGap <- scale(ASTRC12@siteCovs$CGap)
ASTRC12@siteCovs$FLCC <- scale(ASTRC12@siteCovs$FLCC)
ASTRC12@siteCovs$Z <- scale(ASTRC12@siteCovs$Z)
ASTRC12@siteCovs$SRTypeNum <- scale(ASTRC12@siteCovs$SRTypeNum)

fmA <- occu(formula=~1~1, data=ASTRC12)
fmA
ocASTR <- backTransform(fmA, type='state')
dtASTR <- backTransform(fmA, type='det')
ocASTR
dtASTR
confint(ocASTR) 
confint(dtASTR) 

fmA1 <- occu(formula=~1
             ~ FLCC  + CGap + CatchArea,
             data=ASTRC12)
fmA2 <- occu(formula=~1
             ~ CatchArea + SRTypeNum,
             data=ASTRC12)
fmA3 <- occu(formula=~1
             ~ FLCC + CGap + THP5Yr,
             data=ASTRC12)
fmA4 <- occu(formula=~1
             ~ FLCC + CatchArea + THP5Yr,
             data=ASTRC12)
fmA5 <- occu(formula=~ 1
             ~ FLCC  + CGap + CatchArea + SRTypeNum + THP5Yr,
             data=ASTRC12)
fmA6 <- occu(formula=~ LS
             ~ 1,
             data=ASTRC12)
fmA7 <- occu(formula=~ Grad
             ~ 1,
             data=ASTRC12)
fmA8 <- occu(formula=~ Z
             ~ 1,
             data=ASTRC12)

fmA10 <- occu(formula=~ LS + Grad + Z
              ~ 1,
              data=ASTRC12)
fmA11 <- occu(formula=~ LS
              ~ FLCC  + CGap + CatchArea,
              data=ASTRC12)
fmA12 <- occu(formula=~ LS
              ~ CatchArea + SRTypeNum,
              data=ASTRC12)
fmA13 <- occu(formula=~ LS
              ~ FLCC + CGap + THP5Yr,
              data=ASTRC12)
fmA14 <- occu(formula=~ LS
              ~ FLCC + CatchArea + THP5Yr,
              data=ASTRC12)
fmA15 <- occu(formula=~ LS
              ~ FLCC  + CGap + CatchArea + SRTypeNum + THP5Yr,
              data=ASTRC12)
fmA16 <- occu(formula=~ Grad
              ~ FLCC  + CGap + CatchArea,
              data=ASTRC12)
fmA17 <- occu(formula=~ Grad
              ~ CatchArea + THP5Yr,
              data=ASTRC12)
fmA18 <- occu(formula=~ Grad
              ~ FLCC + CGap + THP5Yr,
              data=ASTRC12)
fmA19 <- occu(formula=~ Grad
              ~ FLCC + CatchArea + THP5Yr,
              data=ASTRC12)
fmA20 <- occu(formula=~ Grad
              ~ FLCC  + CGap + CatchArea + SRTypeNum + THP5Yr,
              data=ASTRC12)
fmA21 <- occu(formula=~ Z
              ~ FLCC  + CGap + CatchArea,
              data=ASTRC12)
fmA22 <- occu(formula=~ Z
              ~ CatchArea + SRTypeNum,
              data=ASTRC12)
fmA23 <- occu(formula=~ Z
              ~ FLCC + CGap + THP5Yr,
              data=ASTRC12)
fmA24 <- occu(formula=~ Z
              ~ FLCC + CatchArea + THP5Yr,
              data=ASTRC12)
fmA25 <- occu(formula=~ Z
              ~ FLCC  + CGap + CatchArea + SRTypeNum + THP5Yr,
              data=ASTRC12)
fmA26 <- occu(formula=~ LS + Grad + Z
              ~ FLCC  + CGap + CatchArea,
              data=ASTRC12)
fmA27 <- occu(formula=~ LS + Grad + Z
              ~ CatchArea + SRTypeNum,
              data=ASTRC12)
fmA28 <- occu(formula=~ LS + Grad + Z
              ~ FLCC + CGap + THP5Yr,
              data=ASTRC12)
fmA29 <- occu(formula=~ LS + Grad + Z
              ~ FLCC + CatchArea + THP5Yr,
              data=ASTRC12)
fmA30 <- occu(formula=~ LS + Grad + Z
              ~ FLCC  + CGap + CatchArea + SRTypeNum + THP5Yr,
              data=ASTRC12)
fmA31 <- occu(formula=~1
              ~ FLCC,
              data=ASTRC12)
fmA32 <- occu(formula=~Z
              ~ FLCC,
              data=ASTRC12)
fmA33 <- occu(formula=~LS
              ~ FLCC,
              data=ASTRC12)
fmA34 <- occu(formula=~Grad
              ~ FLCC,
              data=ASTRC12)
fmA35 <- occu(formula=~Z + LS + Grad
              ~ FLCC,
              data=ASTRC12)
FitOccA12 <- fitList('psi(.)p(.)' = fmA,
                     'psi(FLCC  + CGap + CatchArea) p(.)' = fmA1,
                     'psi(CatchArea + SRTypeNum) p(.)' = fmA2,
                     'psi(FLCC + CGap + THP5Yr) p(.)' = fmA3,
                     'psi(FLCC + CatchArea + THP5Yr) p(.)' = fmA4,
                     'psi(FLCC  + CGap + CatchArea + SRTypeNum + THP5Yr) p()' = fmA5,
                     'psi(.) p(LS)' = fmA6,
                     'psi(.) p(Grad)' = fmA7,
                     'psi(.) p(Z)' = fmA8,
                     'psi(.) p(LS + Grad + Z)' = fmA10,
                     'psi(FLCC + CGap + CatchArea) p(LS)' = fmA11,
                     'psi(CatchArea + SRTypeNum) p(LS)' = fmA12,
                     'psi(FLCC + CGap + THP5Yr) p(LS)' = fmA13,
                     'psi(FLCC + CatchArea + THP5Yr) p(LS)' = fmA14,
                     'psi(FLCC  + CGap + CatchArea + SRTypeNum + THP5Yr) p(LS)' = fmA15,
                     'psi(FLCC  + CGap + CatchArea) p(Grad)' = fmA16,
                     'psi(CatchArea + SRTypeNum) p(Grad)' = fmA17,
                     'psi(FLCC + CGap + THP5Yr) p(Grad)' = fmA18,
                     'psi(FLCC + CatchArea + THP5Yr) p(Grad)' = fmA19,
                     'psi(FLCC  + CGap + CatchArea + SRTypeNum + THP5Yr) p(Grad)' = fmA20,
                     'psi(FLCC  + CGap + CatchArea) p(Z)' = fmA21,
                     'psi(CatchArea + SRTypeNum) p(Z)' = fmA22,
                     'psi(FLCC + CGap + THP5Yr) p(Z)' = fmA23,
                     'psi(FLCC + CatchArea + THP5Yr) p(Z)' = fmA24,
                     'psi(FLCC  + CGap + CatchArea + SRTypeNum + THP5Yr) p(Z)' = fmA25,
                     'psi(FLCC  + CGap + CatchArea) p(LS + Grad + Z)' = fmA26,
                     'psi(CatchArea + SRTypeNum) p(LS + Grad + Z)' = fmA27,
                     'psi(FLCC + CGap + THP5Yr) p(LS + Grad + Z)' = fmA28,
                     'psi(FLCC + CatchArea + THP5Yr) p(LS + Grad + Z)' = fmA29,
                     'psi(FLCC  + CGap + CatchArea + SRTypeNum + THP5Yr) p(LS + Grad + Z)' = fmA30,
                     'psi(FLCC) p(.)' = fmA31,
                     'psi(FLCC) p(Z)' = fmA32,
                     'psi(FLCC) p(LS)' = fmA33,
                     'psi(FLCC) p(Grad)' = fmA34,
                     'psi(FLCC) p(Z + LS + Grad)' = fmA35)


models <- c("fmA", "fmA1", "fmA2", "fmA3", "fmA4", "fmA5", "fmA6", "fmA7", "fmA8", "fmA10","fmA11", "fmA12", "fmA13", "fmA14", "fmA15", "fmA16", "fmA17","fmA18", "fmA19", "fmA20", "fmA21", "fmA22", "fmA23", "fmA24", "fmA25", "fmA26", "fmA27", "fmA28", "fmA29", "fmA30", "fmA31", "fmA32", "fmA33", "fmA34", "fmA35")
AICcs <- c(AICc(fmA),AICc(fmA1),AICc(fmA2), AICc(fmA3), AICc(fmA4), AICc(fmA5), AICc(fmA6), AICc(fmA7), AICc(fmA8), AICc(fmA10), AICc(fmA11), AICc(fmA12), AICc(fmA13), AICc(fmA14), AICc(fmA15), AICc(fmA16), AICc(fmA17), AICc(fmA18), AICc(fmA19), AICc(fmA20), AICc(fmA21), AICc(fmA22), AICc(fmA23), AICc(fmA24), AICc(fmA25), AICc(fmA26), AICc(fmA27), AICc(fmA28), AICc(fmA29), AICc(fmA30), AICc(fmA31), AICc(fmA32), AICc(fmA33), AICc(fmA34), AICc(fmA35))
AICctable <- data.frame(models, AICcs)
AICcsort <- AICctable[order(AICcs),]
weights <- data.frame(Weights(AICcsort$AICcs))
AICcweights <- data.frame(AICcsort, weights)
AICcweights

mb.gof.test(fmA30, nsim = 1000, plot.hist = TRUE,
            report = NULL, parallel = TRUE,
            cex.axis = 1, cex.lab = 1, cex.main = 1,
            lwd = 1)

models <- c("fmA", "fmA1", "fmA2", "fmA3", "fmA4", "fmA5", "fmA6", "fmA7", "fmA8", "fmA10","fmA11", "fmA12", "fmA13", "fmA14", "fmA15", "fmA16", "fmA17","fmA18", "fmA19", "fmA20", "fmA21", "fmA22", "fmA23", "fmA24", "fmA25", "fmA26", "fmA27", "fmA28", "fmA29", "fmA30", "fmA31", "fmA32", "fmA33", "fmA34", "fmA35")
QAICcs <- c(QAICc(fmA, chat = 2.06),QAICc(fmA1, chat = 2.06),QAICc(fmA2, chat = 2.06), QAICc(fmA3, chat = 2.06), QAICc(fmA4, chat = 2.06), QAICc(fmA5, chat = 2.06), QAICc(fmA6, chat = 2.06), QAICc(fmA7, chat = 2.06), QAICc(fmA8, chat = 2.06), QAICc(fmA10, chat = 2.06), QAICc(fmA11, chat = 2.06), QAICc(fmA12, chat = 2.06), QAICc(fmA13, chat = 2.06), QAICc(fmA14, chat = 2.06), QAICc(fmA15, chat = 2.06), QAICc(fmA16, chat = 2.06), QAICc(fmA17, chat = 2.06), QAICc(fmA18, chat = 2.06), QAICc(fmA19, chat = 2.06), QAICc(fmA20, chat = 2.06), QAICc(fmA21, chat = 2.06), QAICc(fmA22, chat = 2.06), QAICc(fmA23, chat = 2.06), QAICc(fmA24, chat = 2.06), QAICc(fmA25, chat = 2.06), QAICc(fmA26, chat = 2.06), QAICc(fmA27, chat = 2.06), QAICc(fmA28, chat = 2.06), QAICc(fmA29, chat = 2.06), QAICc(fmA30, chat = 2.06), QAICc(fmA31, chat = 2.06), QAICc(fmA32, chat = 2.06), QAICc(fmA33, chat = 2.06), QAICc(fmA34, chat = 2.06), QAICc(fmA35, chat = 2.06))
QAICctable <- data.frame(models, QAICcs)
QAICcsort <- QAICctable[order(QAICcs),]
weights <- data.frame(Weights(QAICcsort$QAICcs))
QAICcweights <- data.frame(QAICcsort, weights)
QAICcweights

##Occupancy accounting for imperfect detection

AICbestA12 <- occu(formula = ~ Z
                   ~ FLCC,
                   data = ASTRC12)

re <- unmarked::ranef(AICbestA12)
EBUP <- bup(re, stat="mean")
CI <- confint(re, level=0.9)
rbind(PAO = c(Estimate = sum(EBUP), colSums(CI)) / 103)

ASTR12Pred20 <- read.csv("OccMod2020.csv")
ASTR12Pred20 <- ASTR12Pred20 %>%
  filter(Class !="NA")

occuASTRC12 <- occu(formula = ~ Z
                    ~ FLCC + CGap + THP5Yr + CatchArea,
                    data = ASTRC12,
                    linkPsi = "logit",
                    method = "BFGS",
                    se = TRUE,
                    control = list(maxit = 10000),
                    engine = "C")

occuA12Pred <- predict(occuASTRC12,
                       type = "state",
                       newdata = ASTR12Pred20,
                       na.rm = TRUE,
                       inf.rm = TRUE)

##run global model

global <- lm(ASTR ~ InitMass_cent + InitMass2 + Substrate + Shade,
             data = frog)
par(mfrow = c(2, 2))
plot(global)

modavgPred(cand.set = OccA12, newdata = ASTR12Pred20, parm.type = "psi")
modavgPred(cand.set = OccA12, newdata = Data2019, parm.type = "psi")


###Individual predictions to avg


occuASTRC12 <- occu(formula = ~ Z
                    ~ FLCC + CGap + CatchArea,
                    data = ASTRC12,
                    linkPsi = "logit",
                    method = "BFGS",
                    se = TRUE,
                    control = list(maxit = 100000),
                    engine = "C")
occuASTRC12

#####OCC/DET rates

AICbestA1 <- occu(formula = ~ 1
                  ~ CatchArea + FLCC + CGap,
                  data = ASTRC1)
re <- ranef(AICbestA1)
EBUP <- bup(re, stat="mean")
CI <- confint(re, level=0.9)
rbind(PAO = c(Estimate = sum(EBUP), colSums(CI)) / 62)

fmA1 <- occu(formula=~1~1, data=ASTRC1)
fmA1
ocASTR <- backTransform(fmA1, type='state')
dtASTR <- backTransform(fmA1, type='det')
ocASTR
dtASTR
confint(ocASTR) 
confint(dtASTR)

DHHab19C2 <- cbind(DH2ASTR, C2Cov)
DHHab19C1 <- cbind(DH1ASTR, C1Cov)
SFNC2 <- DHHab19C2 %>%
  select(1:4,6:20) %>%
  filter(WS = "SFN")
SFNC1 <- DHHab19C1 %>%
  select(1:4,6:19)
filter(WS = "SFN")

#####ChiSq GOF
?mb.chisq

mb.chisq(fm2A1, print.table= TRUE, nsim= 1000)

mb.gof.test(fm2A1, nsim = 1000, plot.hist = TRUE, report = NULL,
            parallel = TRUE, ncores, cex.axis = 1, cex.lab = 1,
            cex.main = 1, lwd = 1)

mb.chisq(fm3A2, print.table= TRUE, nsim= 1000)

mb.gof.test(fm3A2, nsim = 1000, plot.hist = TRUE, report = NULL,
            parallel = TRUE, ncores, cex.axis = 1, cex.lab = 1,
            cex.main = 1, lwd = 1)


#####Load, QAQC, format data for 2020 Sites##############
ADH20 <- read.csv("C:/2020CalFireR/ASTRDH0915.csv")
ASTRDH20 <- ADH20 %>%
  filter(CTFMod !="NA") %>%
  select(1,7,13,19,23)
ADHAB20 <- ADHAB %>%
  filter(CTFMod !="NA") %>%
  select(1,14,15,17:21,23:24,27,31:32)
colnames(ADHAB20)[8] <- "Grad"  
colnames(ADHAB20)[12] <- "CGap"
ASTRDHC120 <- ASTRDH20 %>%
  filter(Class== 1)
ADHABC120 <- ADHAB20 %>%
  filter(Class==1)
CovarC120 <- ADHABC120 %>%
  select(4:10,12)

#####Single season occupancy model 2020#######################

y <- ASTRDHC120[,2:4]
siteCovs <- CovarC120
ASTRC120 <- unmarkedFrameOccu(y = y, siteCovs = siteCovs)

ASTRC120@siteCovs$Temp <- scale(ASTRC120@siteCovs$Temp)
ASTRC120@siteCovs$Grad <- scale(ASTRC120@siteCovs$Grad)
ASTRC120@siteCovs$CatchArea <- scale(ASTRC120@siteCovs$CatchArea)
ASTRC120@siteCovs$CGap <- scale(ASTRC120@siteCovs$CGap)
ASTRC120@siteCovs$FLCC <- scale(ASTRC120@siteCovs$FLCC)
ASTRC120@siteCovs$CWD <- scale(ASTRC120@siteCovs$CWD)
ASTRC120@siteCovs$SRTypeNum <- scale(ASTRC120@siteCovs$SRTypeNum)

fmA120 <- occu(formula=~1~1, data=ASTRC120)
fmA120
ocASTR <- backTransform(fmA1, type='state')
dtASTR <- backTransform(fmA1, type='det')
ocASTR
dtASTR
confint(ocASTR) 
confint(dtASTR) 

fm1A120 <- occu(formula=~1
                ~ CatchArea + SRTypeNum + FLCC + CGap,
                data=ASTRC120)
fm2A120 <- occu(formula=~1
                ~ FLCC + CGap + CatchArea,
                data=ASTRC120)
fm3A120 <- occu(formula=~1
                ~ CatchArea + SRTypeNum,
                data=ASTRC120)
fm4A120 <- occu(formula=~1
                ~ CGap + FLCC + SRTypeNum,
                data=ASTRC120)
fm5A120 <- occu(formula=~ CatchArea
                ~ 1,
                data=ASTRC120)
fm6A120 <- occu(formula=~ Grad
                ~ 1,
                data=ASTRC120)
fm7A120 <- occu(formula=~ Grad + CatchArea
                ~ 1,
                data=ASTRC120)
fm8A120 <- occu(formula=~ CatchArea + CWD
                ~ 1,
                data=ASTRC120)
fm9A120 <- occu(formula=~ CatchArea
                ~ CatchArea + SRTypeNum + FLCC + CGap,
                data=ASTRC120)
fm10A120 <- occu(formula=~ CatchArea
                 ~ FLCC + CGap + CatchArea,
                 data=ASTRC120)
fm11A120 <- occu(formula=~ CatchArea
                 ~ CatchArea + SRTypeNum,
                 data=ASTRC120)
fm12A120 <- occu(formula=~ CatchArea
                 ~ CGap + FLCC + SRTypeNum,
                 data=ASTRC120)
fm13A120 <- occu(formula=~ Grad
                 ~ CatchArea + SRTypeNum + FLCC + CGap,
                 data=ASTRC120)
fm14A120 <- occu(formula=~ Grad
                 ~ FLCC + CGap + CatchArea,
                 data=ASTRC120)
fm15A120 <- occu(formula=~ Grad
                 ~ CatchArea + SRTypeNum,
                 data=ASTRC120)
fm16A120 <- occu(formula=~ Grad
                 ~ CGap + FLCC + SRTypeNum,
                 data=ASTRC120)
fm17A120 <- occu(formula=~ Grad + CatchArea
                 ~ CatchArea + SRTypeNum + FLCC + CGap,
                 data=ASTRC120)
fm18A120 <- occu(formula=~ Grad + CatchArea
                 ~ FLCC + CGap + CatchArea,
                 data=ASTRC120)
fm19A120 <- occu(formula=~ Grad + CatchArea
                 ~ CatchArea + SRTypeNum,
                 data=ASTRC120)
fm20A120 <- occu(formula=~ Grad + CatchArea
                 ~ CGap + FLCC + SRTypeNum,
                 data=ASTRC120)
fm21A120 <- occu(formula=~ CatchArea + CWD
                 ~ CatchArea + SRTypeNum + FLCC + CGap,
                 data=ASTRC120)
fm22A120 <- occu(formula=~ CatchArea + CWD
                 ~ FLCC + CGap + CatchArea,
                 data=ASTRC120)
fm23A120 <- occu(formula=~ CatchArea + CWD
                 ~ CatchArea + SRTypeNum,
                 data=ASTRC120)
fm24A120 <- occu(formula=~ CatchArea + CWD
                 ~ CGap + FLCC + SRTypeNum,
                 data=ASTRC120)

fitOccA120 <- fitList('psi(.)p(.)' = fmA120,
                      'psi(CatchArea + SRTypeNum + FLCC + CGap) p(.)' = fm1A120,
                      'psi(FLCC + CGap + CatchArea) p(.)' = fm2A120,
                      'psi(CatchArea + SRTypeNum) p(.)' = fm3A120,
                      'psi(CGap + FLCC + SRTypeNum) p(.)' = fm4A120,
                      
                      'psi(.) p(CatchArea)' = fm5A120,
                      'psi(.) p(Grad)' = fm6A120,
                      'psi(.) p(Grad + CatchArea)' = fm7A120,
                      'psi(.) p(CatchArea + CWD)' = fm8A120,
                      
                      'psi(CatchArea + SRTypeNum + FLCC + CGap) p(CatchArea)' = fm9A120,
                      'psi(FLCC + CGap + CatchArea) p(CatchArea)' = fm10A120,
                      'psi(CatchArea + SRTypeNum) p(CatchArea)' = fm11A120,
                      'psi(CGap + FLCC + SRTypeNum) p(CatchArea)' = fm12A120,
                      
                      'psi(CatchArea + SRTypeNum + FLCC + CGap) p(Grad)' = fm13A120,
                      'psi(FLCC + CGap + CatchArea) p(Grad)' = fm14A120,
                      'psi(CatchArea + SRTypeNum) p(Grad)' = fm15A120,
                      'psi(CGap + FLCC + SRTypeNum) p(Grad)' = fm16A120,
                      
                      'psi(CatchArea + SRTypeNum + FLCC + CGap) p(Grad + CatchArea)' = fm17A120,
                      'psi(FLCC + CGap + CatchArea) p(Grad + CatchArea)' = fm18A120,
                      'psi(CatchArea + SRTypeNum) p(Grad + CatchArea)' = fm19A120,
                      'psi(CGap + FLCC + SRTypeNum) p(Grad + CatchArea)' = fm20A120,
                      
                      'psi(CatchArea + SRTypeNum + FLCC + CGap) p(CatchArea + CWD)' = fm21A120,
                      'psi(FLCC + CGap + CatchArea) p(CatchArea + CWD)' = fm22A120,
                      'psi(CatchArea + SRTypeNum) p(CatchArea + CWD)' = fm23A120,
                      'psi(CGap + FLCC + SRTypeNum) p(CatchArea + CWD)' = fm24A120)


modSel(fitOccA120)

library('MuMIn')
models <- c("fmA120", "fm1A120", "fm2A120", "fm3A120", "fm4A120", "fm5A120", "fm6A120", "fm7A120", "fm8A120", "fm9A120", "fm10A120","fm11A120", "fm12A120", "fm13A120", "fm14A120", "fm15A120", "fm16A120", "fm17A120","fm18A120", "fm19A120", "fm20A120", "fm21A120", "fm22A120", "fm23A120", "fm24A120")
AICcs <- c(AICc(fmA120),AICc(fm1A120),AICc(fm2A120), AICc(fm3A120), AICc(fm4A120), AICc(fm5A120), AICc(fm6A120), AICc(fm7A120), AICc(fm8A120), AICc(fm9A120), AICc(fm10A120), AICc(fm11A120), AICc(fm12A120), AICc(fm13A120), AICc(fm14A120), AICc(fm15A120), AICc(fm16A120), AICc(fm17A120), AICc(fm18A120), AICc(fm19A120), AICc(fm20A120), AICc(fm21A120), AICc(fm22A120), AICc(fm23A120), AICc(fm24A120))
AICctable <- data.frame(models, AICcs)
AICcsort <- AICctable[order(AICcs),]
weights <- data.frame(Weights(AICcsort$AICcs))
AICcweights <- data.frame(AICcsort, weights)
AICcweights


#####OccMod Validation#####
#test predictions of 2019 occ mod on 2020 results
OccVal <- read.csv("PredQAIC.csv")
colnames(OccVal)[1] <- "Occ20"

Occ1 <- OccVal %>%
  filter(Occ20 == 1)

Occ0 <- OccVal %>%
  filter(Occ20 == 0)

t.test(Occ1$Weight, Occ0$Weight,
       alternative = c("greater"),
       mu = 0, paired = FALSE, var.equal = FALSE,
       conf.level = 0.95)

t.test(OccVal$Weight, OccVal$Occ20,
       alternative = c("less"),
       mu = 0, paired = TRUE, var.equal = FALSE,
       conf.level = 0.95)



C1APred <- read.csv("C:/2020CalFireR/C1_CTFMod_PredOcc.csv")
C2APred <- read.csv("C:/2020CalFireR/C2_CTFMod_PredOcc.csv")
ASTRDH20 <- read.csv("C:/2020CalFireR/ASTRDH0915.csv")
DHA1Obs <- ASTRDH20 %>%
  select(1, 21:24) %>%
  filter(CTFMod != "NA") %>%
  filter(Class == 1)
C1PredDH <- cbind(C1APred, DHA1Obs)
C1Pred <- select(C1PredDH, 3)
C1Occ <- select(C1PredDH, 8)

fm2A1 <- occu(formula=~1
              ~ FLCC + CGap + CatchArea,
              data=ASTRC1)


#####Model fitting for SS2 C1 Sites

fitstats <- function(a,b, 
                     method = "nonparboot") {
  
  observed <- SiteOcc
  expected <- Predicted
  
  chisq <- sum((observed - expected)^2 / expected,
               na.rm = TRUE)
  
  freeTuke <- sum((sqrt(observed) - sqrt(expected))^2, 
                  na.rm = TRUE)
  
  out <- c(Chisq = chisq,
           freemanTukey = freeTuke)
  
  return(out)
  
}

SiteOcc <- DHA1Obs %>%
  select(SiteOcc)
Predicted <- C1APred %>%
  select(Predicted)
pb <- parboot(fm2A1,
              fitstats,
              nsim = 1000,
              report = TRUE,
              method = "nonparboot")
par(mfrow = c(3,1))
plot(pb,
     main = "",
     xlab = c("SSE", "Chisq", "FT"))



#####eDNA TRANSPORT#####
#####Examining Variation#####
#####PCRWellReplicates#####

ggplot(data = WRCOV_DITE) +
  geom_histogram(mapping = aes(x= logWRcov))

ggplot(data = WRCOV_ASTR) +
  geom_histogram(mapping = aes(x= logWRcov))

WRCOV_ASTR$logAWRcov <-  log2(WRCOV_ASTR$WRcov)

AWRCOV1 <- glm(logAWRcov ~ 1, data = WRCOV_ASTR)
AWRCOV2 <- glm(logAWRcov ~ Sqavg, data = WRCOV_ASTR)
AWRCOV3 <- glm(logAWRcov ~ as.factor(Inh), data = WRCOV_ASTR)
AWRCOV4 <- glm(logAWRcov ~ Sqavg + as.factor(Inh), data = WRCOV_ASTR)
models <- c("AWRCOV1", "AWRCOV2", "AWRCOV3", "AWRCOV4")
AICcs <- c(AICc(AWRCOV1), AICc(AWRCOV2), AICc(AWRCOV3), AICc(AWRCOV4))
AICctable <- data.frame(models, AICcs)
AICcsort <- AICctable[order(AICcs),]
weights <- data.frame(Weights(AICcsort$AICcs))
AICcweights <- data.frame(AICcsort, weights)
AICcweights
hoslem.test(WRCOV_ASTR$logWRcov, fitted(AWRCOV4))
AWRCOV2

ggplot(data = WRCOV_ASTR) +
  theme_classic() +
  geom_point(mapping = aes(x = Sqavg, y = (logWRcov))) +
  geom_line(mapping = aes(x = 5, y= logWRcov), color=2) +
  geom_abline(intercept = .184, slope= -.005, linetype= "dashed") +
  labs(y= "Log-transformed coefficient of Variation of Well Replicates", x= "Estimated Coastal Tailed Frog Starting Copy Number")

WRCOV_DITE$logDWRcov <-  log2(WRCOV_DITE$WRcov)

DWRCOV1 <- glm(logDWRcov ~ 1, data = WRCOV_DITE)
DWRCOV2 <- glm(logDWRcov ~ Sqavg, data = WRCOV_DITE)
DWRCOV3 <- glm(logDWRcov ~ as.factor(Inh), data = WRCOV_DITE)
DWRCOV4 <- glm(logDWRcov ~ Sqavg + as.factor(Inh), data = WRCOV_DITE)
models <- c("DWRCOV1", "DWRCOV2", "DWRCOV3", "DWRCOV4")
AICcs <- c(AICc(DWRCOV1), AICc(DWRCOV2), AICc(DWRCOV3), AICc(DWRCOV4))
AICctable <- data.frame(models, AICcs)
AICcsort <- AICctable[order(AICcs),]
weights <- data.frame(Weights(AICcsort$AICcs))
AICcweights <- data.frame(AICcsort, weights)
AICcweights
hoslem.test(WRCOV_DITE$logWRcov, fitted(DWRCOV4))
DWRCOV2

ggplot(data = WRCOV_DITE) +
  theme_classic() +
  geom_point(mapping = aes(x = Sqavg, y = (logWRcov))) +
  geom_line(mapping = aes(x = 25, y= logWRcov), color=2) +
  geom_abline(intercept = .114, slope= -.0003, linetype= "dashed") +
  labs(y= "Log-transformed coefficient of Variation of Well Replicates", x= "Estimated Coastal Giant Salamander Starting Copy Number")

#####SiteReplicates#####
Data2019DFCOV <- Data2019 %>%
  filter(DFCOV !=0)
cor.test(Data2019DFCOV$LS, Data2019DFCOV$Grad)
ggplot(data = Data2019DFCOV) +
  geom_histogram(mapping = aes(x= DFCOV)) +
  labs(x= "DFCOV")

DFCOV1 <- glm(DFCOV ~ 1, data = Data2019DFCOV)
DFCOV2 <- glm(DFCOV ~ LS, data = Data2019DFCOV)
DFCOV3 <- glm(DFCOV ~ DSQavg, data = Data2019DFCOV)
DFCOV4 <- glm(DFCOV ~ Z, data = Data2019DFCOV)
DFCOV5 <- glm(DFCOV ~ Grad, data = Data2019DFCOV)
DFCOV6 <- glm(DFCOV ~ LS + Grad, data = Data2019DFCOV)
DFCOV7 <- glm(DFCOV ~ DSQavg + Z, data = Data2019DFCOV)
DFCOV8 <- glm(DFCOV ~ LS + DSQavg, data = Data2019DFCOV)
DFCOV9 <- glm(DFCOV ~ Grad + DSQavg, data = Data2019DFCOV)
DFCOV10 <- glm(DFCOV ~ LS + Grad + DSQavg + Z, data = Data2019DFCOV)
models <- c("DFCOV1", "DFCOV2", "DFCOV3", "DFCOV4", "DFCOV5", "DFCOV6", "DFCOV7", "DFCOV8", "DFCOV9", "DFCOV10")
AICcs <- c(AICc(DFCOV1), AICc(DFCOV2), AICc(DFCOV3), AICc(DFCOV4), AICc(DFCOV5), AICc(DFCOV6), AICc(DFCOV7), AICc(DFCOV8), AICc(DFCOV9), AICc(DFCOV10))
AICctable <- data.frame(models, AICcs)
AICcsort <- AICctable[order(AICcs),]
weights <- data.frame(Weights(AICcsort$AICcs))
AICcweights <- data.frame(AICcsort, weights)
AICcweights
hoslem.test(Data2019DFCOV$DFCOV, fitted(DFCOV10))
DFCOV5

ggplot(data = Data2019DFCOV) +
  theme_classic() +
  geom_point(mapping = aes(x = Grad, y = (DFCOV))) +
  geom_smooth(method='lm', mapping = aes(x = Grad, y = (DFCOV))) +
  labs(y= "DFCOV", x= "Grad")

Data2019AFCOV <- Data2019 %>%
  filter(ASQavg !=0, AFCOV !="NA")

ggplot(data = Data2019AFCOV) +
  geom_histogram(mapping = aes(x= AFCOV)) +
  labs(x= "AFCOV")

AFCOV1 <- glm(AFCOV ~ 1, data = Data2019AFCOV)
AFCOV2 <- glm(AFCOV ~ LS, data = Data2019AFCOV)
AFCOV3 <- glm(AFCOV ~ ASQavg, data = Data2019AFCOV)
AFCOV4 <- glm(AFCOV ~ Z, data = Data2019AFCOV)
AFCOV5 <- glm(AFCOV ~ Grad, data = Data2019AFCOV)
AFCOV6 <- glm(AFCOV ~ LS + Grad, data = Data2019AFCOV)
AFCOV7 <- glm(AFCOV ~ ASQavg + Z, data = Data2019AFCOV)
AFCOV8 <- glm(AFCOV ~ LS + ASQavg, data = Data2019AFCOV)
AFCOV9 <- glm(AFCOV ~ Grad + ASQavg, data = Data2019AFCOV)
AFCOV10 <- glm(AFCOV ~ LS + Grad + ASQavg + Z, data = Data2019AFCOV)
models <- c("AFCOV1", "AFCOV2", "AFCOV3", "AFCOV4", "AFCOV5", "AFCOV6", "AFCOV7", "AFCOV8", "AFCOV9", "AFCOV10")
AICcs <- c(AICc(AFCOV1), AICc(AFCOV2), AICc(AFCOV3), AICc(AFCOV4), AICc(AFCOV5), AICc(AFCOV6), AICc(AFCOV7), AICc(AFCOV8), AICc(AFCOV9), AICc(AFCOV10))
AICctable <- data.frame(models, AICcs)
AICcsort <- AICctable[order(AICcs),]
weights <- data.frame(Weights(AICcsort$AICcs))
AICcweights <- data.frame(AICcsort, weights)
AICcweights
hoslem.test(Data2019AFCOV$AFCOV, fitted(AFCOV10))
AFCOV8
AFCOV3

##########Yr2Yr#############
ggplot(data = JDSF1920) +
  geom_boxplot(mapping = aes(y= LS, x= as.factor(Year))) +
  labs(y= "Discharge (L/s)", x= "Year")
ggplot(data = JDSF1920) +
  geom_histogram(mapping = aes(x= LS)) +
  labs(x= "Discharge")
ggplot(data = Data2019) +
  geom_histogram(mapping = aes(x= LS)) +
  labs(x= "Discharge")
ggplot(data = Data2020) +
  geom_histogram(mapping = aes(x= LS)) +
  labs(x= "Discharge")
wilcox.test(LS~Year, alternative = "less", data = JDSF1920)
wilcox.test(LS~Year, alternative = "two.sided", data = JDSF1920)
wilcox.test(LS~Year, alternative = "greater", data = JDSF1920)


#####DITE coastal giant salamander

wilcox.test(DDens~Year, alternative = "two.sided", data = JDSF1920)
wilcox.test(DDens~Year, alternative = "less", data = JDSF1920)
wilcox.test(DDens~Year, alternative = "greater", data = JDSF1920)

wilcox.test(DTot~Year, alternative = "two.sided", data = JDSF1920)
wilcox.test(DTot~Year, alternative = "less", data = JDSF1920)
wilcox.test(DTot~Year, alternative = "greater", data = JDSF1920)

wilcox.test(JDSFRepeat$DDens19, JDSFRepeat$DDens20, alternative = "two.sided", paired = TRUE)
wilcox.test(JDSFRepeat$DDens19, JDSFRepeat$DDens20, alternative = "less", paired = TRUE)
wilcox.test(JDSFRepeat$DDens19, JDSFRepeat$DDens20, alternative = "greater", paired = TRUE)

wilcox.test(JDSFRepeat$DTot19, JDSFRepeat$DTot20, alternative = "two.sided", paired = TRUE)
wilcox.test(JDSFRepeat$DTot19, JDSFRepeat$DTot20, alternative = "less", paired = TRUE)
wilcox.test(JDSFRepeat$DTot19, JDSFRepeat$DTot20, alternative = "greater", paired = TRUE)

ggplot(data = JDSF1920) +
  geom_boxplot(mapping = aes(y= DDens, x= as.factor(Year))) +
  labs(y= "Coastal Giant Salamander eDNA Density", x= "Year")
ggplot(data = JDSF1920) +
  geom_boxplot(mapping = aes(y= DTot, x= as.factor(Year))) +
  labs(y= "Coastal Giant Salamander eDNA Abundance", x= "Year")

#####ASTR coastal tialed frog
wilcox.test(ADens~Year, alternative = "less", data = JDSF1920AZRM)
wilcox.test(ADens~Year, alternative = "two.sided", data = JDSF1920AZRM)
wilcox.test(ADens~Year, alternative = "greater", data = JDSF1920AZRM)

wilcox.test(ATot~Year, alternative = "less", data = JDSF1920AZRM)
wilcox.test(ATot~Year, alternative = "two.sided", data = JDSF1920AZRM)
wilcox.test(ATot~Year, alternative = "greater", data = JDSF1920AZRM)

wilcox.test(DRA~Year, alternative = "less", data = JDSF1920AZRM)
wilcox.test(DRA~Year, alternative = "two.sided", data = JDSF1920AZRM)
wilcox.test(DRA~Year, alternative = "greater", data = JDSF1920AZRM)

wilcox.test(JDSFRepeat$ADens19, JDSFRepeat$ADens20, alternative = "two.sided", paired = TRUE)
wilcox.test(JDSFRepeat$ADens19, JDSFRepeat$ADens20, alternative = "less", paired = TRUE)
wilcox.test(JDSFRepeat$ADens19, JDSFRepeat$ADens20, alternative = "greater", paired = TRUE)

wilcox.test(JDSFRepeat$ATot19, JDSFRepeat$ATot20, alternative = "two.sided", paired = TRUE)
wilcox.test(JDSFRepeat$ATot19, JDSFRepeat$ATot20, alternative = "less", paired = TRUE)
wilcox.test(JDSFRepeat$ATot19, JDSFRepeat$ATot20, alternative = "greater", paired = TRUE)

wilcox.test(JDSFRepeat$DRA19, JDSFRepeat$DRA20, alternative = "two.sided", paired = TRUE)
wilcox.test(JDSFRepeat$DRA19, JDSFRepeat$DRA20, alternative = "less", paired = TRUE)
wilcox.test(JDSFRepeat$DRA19, JDSFRepeat$DRA20, alternative = "greater", paired = TRUE)

ggplot(data = JDSF1920AZRM) +
  geom_boxplot(mapping = aes(y= ADens, x= as.factor(Year))) +
  labs(y= "Coastal Tailed Frog eDNA Density", x= "Year")
ggplot(data = JDSF1920AZRM) +
  geom_boxplot(mapping = aes(y= ATot, x= as.factor(Year))) +
  labs(y= "Coastal Tailed Frog eDNA Abundance", x= "Year")
ggplot(data = JDSF1920AZRM) +
  geom_boxplot(mapping = aes(y= DRA, x= as.factor(Year),)) +
  labs(y= "Coastal Tailed Frog eDNA Detection Rate", x= "Year")

Yr2YrA <- Yr2Yr %>%
  filter(Sp == "ASTR")
ggplot(data = Yr2YrA) +
  geom_boxplot(mapping = aes(y= Conc, x= Set, fill= as.factor(Year))) +
  theme_classic() +
  scale_fill_grey(start=1.0, end=0.5) +
  theme(legend.title = element_blank()) +
  labs(y= "Coastal tailed frog eDNA concentration (copies/L)", x= "Dataset")
ggplot(data = Yr2YrA) +
  geom_boxplot(mapping = aes(y= FAR, x= Set, fill= as.factor(Year))) +
  theme_classic() +
  scale_fill_grey(start=1.0, end=0.5) +
  theme(legend.title = element_blank()) +
  labs(y= "Coastal tailed frog flow-corrected eDNA rate (copies/sec)", x= "Dataset")
Yr2YrAZRM <- Yr2YrA %>%
  filter(FAR != 0)
ggplot(data = Yr2YrAZRM) +
  geom_boxplot(mapping = aes(y= DRA, x= Set, fill= as.factor(Year))) +
  theme(legend.title = element_blank()) +
  labs(y= "Coastal tailed frog eDNA detection rate", x= "Dataset")

Yr2YrD <- Yr2Yr %>%
  filter(Sp == "DITE")
ggplot(data = Yr2YrD) +
  theme_classic() + 
  geom_boxplot(mapping = aes(y= Conc, x= Set, fill= as.factor(Year))) +
  scale_fill_grey(start=1.0, end=0.5) +
  theme(legend.title = element_blank()) +
  labs(y= "Coastal giant salamander eDNA concentration (copies/L)", x= "Dataset")
ggplot(data = Yr2YrD) +
  theme_classic() + 
  geom_boxplot(mapping = aes(y= FAR, x= Set, fill= as.factor(Year))) +
  scale_fill_grey(start=1.0, end=0.5) +
  theme(legend.title = element_blank()) +
  labs(y= "Coastal giant salamander flow-corrected eDNA quantity (copies/sec)", x= "Dataset")

#####Examining source of eDNA 2020#####
#####DITE
colnames(Source20)[40] <- "C"

data(Source20)
model <- drm(C ~ DistSmallTrib, fct = DRC.expoDecay(),
             data = Source20)
summary(model)
plot(model, log="")

colnames(Source20)[24] <- "A"

data(Source20)
modelA <- drm(A ~ DistSmallTrib, fct = DRC.expoDecay(),
              data = Source20)
summary(modelA)
plot(modelA, log="")


labels(model, y= "Flow-adjusted coastal giant salamander eDNA rate", x= "Distance from headwater")

Source20DZRM$DTotLog10 <- log10(Source20DZRM$DTot)

summary(elmDTotTDTrans)

wilcox.test(DTot~Class, alternative = "less", data = Source20)
wilcox.test(DTot~Class, alternative = "two.sided", data = Source20)
wilcox.test(DTot~Class, alternative = "greater", data = Source20)

ggplot(data = Source20) +
  geom_point(mapping = aes(x = DistSmallTrib, y = (DDens))) +
  labs(y= "DITE eDNA Density", x= "Distance Upstream to Small Tributary", title= "2020 Transects")
ggplot(data = Source20) +
  geom_point(mapping = aes(x = DistSmallTrib, y = (DTot))) +
  theme_classic() +
  labs(y= "Flow-adjusted coastal giant salamander eDNA rate (copies/sec)", x= "Distance Upstream to Small Tributary")

Bin100TwoSp <- Binned2Sp %>%
  filter (Bin == 100)



###ASTR###

wilcox.test(ATot~Class, alternative = "less", data = Source20AZRM)
wilcox.test(ATot~Class, alternative = "two.sided", data = Source20AZRM)
wilcox.test(ATot~Class, alternative = "greater", data = Source20AZRM)

ggplot(data = Source20) +
  geom_point(mapping = aes(x = DistSmallTrib, y = ADens)) +
  labs(y= "ASTR eDNA Density", x= "Distance Upstream to Small Tributary", title= "2020 Transects")
ggplot(data = Source20) +
  geom_point(mapping = aes(x = DistSmallTrib, y = ATot)) +
  theme_classic() +
  labs(y= "Flow-adjusted coastal tailed frog eDNA rate (copies/sec)", x= "Distance Upstream to Small Tributary")

######ScaleofTransport######
#####SourceTrans20

ggplot(data = BinnedSource) +
  geom_bar(mapping = aes(x= Bin)) +
  theme_classic() +
  scale_fill_grey(start=1.0, end=0.5) +
  labs(x= "Point Distances")

ggplot(data = B100Source) +
  geom_boxplot(mapping = aes(y=RDIFASTR, x= )) +
  theme_classic() +
  scale_fill_grey(start=1.0, end=0.5) +
  labs(x= "Point Distances", y= "Proportion eDNA of US Point")

B100Source <- BinnedSource %>%
  filter(Bin == 100)

t.test(B100Source$RDIFDITE, y=B100Source$one,
       alternative = c("less"),
       mu = 0, paired = FALSE, var.equal = FALSE,
       conf.level = 0.95)

B100Sless <- BinnedSource %>%
  filter(Bin == 100, RDIFDITE < 1)

t.test(B100Sless$RDIFDITE, y=(B100Sless$one/2),
       alternative = c("less"),
       mu = 0, paired = FALSE, var.equal = FALSE,
       conf.level = 0.95)

DITEBOXDATA <- read.csv("DITEBOX.csv")
colnames(DITEBOXDATA)[1] <- "Species"
DITEBOXDATA <- DITEBOXDATA %>%
  filter(Distance != "NA")

ggplot(data = DITEBOXDATA) +
  theme_classic() + 
  geom_boxplot(mapping = aes(y= ProportioneDNA, x= Species)) +
  scale_fill_grey(start=1.0, end=0.5) +
  geom_point(mapping = aes(y= ProportioneDNA, x= Species)) +
  theme(legend.title = element_blank()) +
  labs(y= "Proportion of DNA from confluence at site 100 m downstream", x= "")

DITEDROPOUT <- DITEBOXDATA %>%
  filter(DITEProp != 1.71)
str(DITEDROPOUT)
sd(DITEDROPOUT$DITEProp)


ggplot(data = TransPt) +
  geom_smooth(se = FALSE, mapping = aes(x = TransectDist, y = DITE, group= Transect, color=Transect)) +
  geom_point(mapping = aes(x = TransectDist, y = DITE, group= Transect, color=Transect)) +
  theme_classic() +
  theme(legend.position= "none") +
  labs(y= "Flow-adjusted coastal giant salamander eDNA rate (copies/sec)", x= "Transect position (m)")
ggplot(data = TransPt) +
  geom_smooth(mapping = aes(x = TransectDist, y = ASTR, group= Transect, color=Transect)) +
  geom_point(mapping = aes(x = TransectDist, y = ASTR, group= Transect, color=Transect)) +
  theme_classic() +
  theme(legend.position= "none") +
  labs(y= "Flow-adjusted coastal tailed frog eDNA rate (copies/sec)", x= "Transect position (m)")


###DITE###

wilcox.test(DDens~Class, alternative = "less", data = Data2019DZRM)
wilcox.test(DDens~Class, alternative = "two.sided", data = Data2019DZRM)
wilcox.test(DDens~Class, alternative = "greater", data = Data2019DZRM)

wilcox.test(DTot~Class, alternative = "less", data = Data2019DZRM)
wilcox.test(DTot~Class, alternative = "two.sided", data = Data2019DZRM)
wilcox.test(DTot~Class, alternative = "greater", data = Data2019DZRM)

ggplot(data = Data2019) +
  geom_point(mapping = aes(x = DistSmallTrib, y = (DDens))) +
  xlim(0,1500) +
  labs(y= "DITE eDNA Density", x= "Distance Upstream to Small Tributary", title= "2019 Occupancy Modeling Sites")
ggplot(data = Data2019) +
  geom_point(mapping = aes(x = DistSmallTrib, y = (DTot))) +
  xlim(0,1500) +
  labs(y= "DITE eDNA Abundance", x= "Distance Upstream to Small Tributary", title= "2019 Occupancy Modeling Sites")

###ASTR###

wilcox.test(ADens~Class, alternative = "less", data = Data2019AZRM)

wilcox.test(ADens~Class, alternative = "two.sided", data = Data2019AZRM)
wilcox.test(ADens~Class, alternative = "greater", data = Data2019AZRM)
wilcox.test(ATot~Class, alternative = "less", data = Data2019AZRM)
wilcox.test(ATot~Class, alternative = "two.sided", data = Data2019AZRM)

wilcox.test(ATot~Class, alternative = "greater", data = Data2019AZRM)

wilcox.test(ADens~Class, alternative = "less", data = Data2019AZRM)
wilcox.test(ATot~Class, alternative = "greater", data = Data2019AZRM)

ggplot(data = Data2019AZRM) +
  geom_point(mapping = aes(x = DistSmallTrib, y = ADens)) +
  xlim(0,1500) +
  labs(y= "ASTR eDNA Density", x= "Distance Upstream to Small Tributary", title= "2019 Occupancy Modeling Sites")
ggplot(data = Data2019AZRM) +
  geom_point(mapping = aes(x = DistSmallTrib, y = ATot)) +
  xlim(0,1500) +
  labs(y= "ASTR eDNA Abundance", x= "Distance Upstream to Small Tributary", title= "2019 Occupancy Modeling Sites")

#########Site Transport GLMs###########

Data2019$ATot1 <- Data2019$ATot + 1
Data2019ANRM <- Data2019 %>%
  filter(ASQavg !="NA", ASQavg !=0)
ggplot(data = Data2019AZRM) +
  geom_histogram(mapping = aes(x= ATot)) +
  labs(x= "ATot")
FCA1
FCA1 <- glm(ATot1 ~ 1, family= Gamma (link="log"), data = Data2019ANRM)
FCA2 <- glm(ATot1 ~ FLCC + CGap, family= Gamma (link="log"), data = Data2019ANRM)
FCA3 <- glm(ATot1 ~ SRTypeNum, family= Gamma (link="log"), data = Data2019ANRM)
FCA4 <- glm(ATot1 ~ CatchArea, family= Gamma (link="log"), data = Data2019ANRM)
FCA5 <- glm(ATot1 ~ THP5Yr, family= Gamma (link="log"), data = Data2019ANRM)
FCA6 <- glm(ATot1 ~ FLCC + CGap + CatchArea, family= Gamma (link="log"), data = Data2019ANRM)
FCA7 <- glm(ATot1 ~ CatchArea + SRTypeNum, family= Gamma (link="log"), data = Data2019ANRM)
FCA8 <- glm(ATot1 ~ FLCC + CGap + SRTypeNum, family= Gamma (link="log"), data = Data2019ANRM)
FCA9 <- glm(ATot1 ~ FLCC + CatchArea + THP5Yr, family= Gamma (link="log"), data = Data2019ANRM)
FCA10 <- glm(ATot1 ~ FLCC + CGap + CatchArea + SRTypeNum + THP5Yr, family= Gamma (link="log"), data = Data2019ANRM)


models <- c("FCA1", "FCA2", "FCA3", "FCA4", "FCA5", "FCA6", "FCA7", "FCA8", "FCA9", "FCA10")
AICcs <- c(AICc(FCA1), AICc(FCA2), AICc(FCA3), AICc(FCA4), AICc(FCA5), AICc(FCA6), AICc(FCA7), AICc(FCA8), AICc(FCA9), AICc(FCA10))
AICctable <- data.frame(models, AICcs)
AICcsort <- AICctable[order(AICcs),]
weights <- data.frame(Weights(AICcsort$AICcs))
AICcweights <- data.frame(AICcsort, weights)
AICcweights
hoslem.test(Data2019AFCOV$AFCOV, fitted(xxx))

Data2019$DTot1 <- Data2019$DTot + 1
Data2019DNRM <- Data2019 %>%
  filter(ASQavg !="NA")

FCD1 <- glm(DTot1 ~ 1, family= Gamma (link="log"), data = Data2019DNRM)
FCD2 <- glm(DTot1 ~ FLCC + CGap, family= Gamma (link="log"), data = Data2019DNRM)
FCD3 <- glm(DTot1 ~ SRTypeNum, family= Gamma (link="log"), data = Data2019DNRM)
FCD4 <- glm(DTot1 ~ CatchArea, family= Gamma (link="log"), data = Data2019DNRM)
FCD5 <- glm(DTot1 ~ THP5Yr, family= Gamma (link="log"), data = Data2019DNRM)
FCD6 <- glm(DTot1 ~ FLCC + CGap + CatchArea, family= Gamma (link="log"), data = Data2019DNRM)
FCD7 <- glm(DTot1 ~ CatchArea + SRTypeNum, family= Gamma (link="log"), data = Data2019DNRM)
FCD8 <- glm(DTot1 ~ FLCC + CGap + SRTypeNum, family= Gamma (link="log"), data = Data2019DNRM)
FCD9 <- glm(DTot1 ~ FLCC + CatchArea + THP5Yr, family= Gamma (link="log"), data = Data2019DNRM)
FCD10 <- glm(DTot1 ~ FLCC + CGap + CatchArea + SRTypeNum + THP5Yr, family= Gamma (link="log"), data = Data2019DNRM)
models <- c("FCD1", "FCD2", "FCD3", "FCD4", "FCD5", "FCD6", "FCD7", "FCD8", "FCD9", "FCD10")
AICcs <- c(AICc(FCD1), AICc(FCD2), AICc(FCD3), AICc(FCD4), AICc(FCD5), AICc(FCD6), AICc(FCD7), AICc(FCD8), AICc(FCD9), AICc(FCD10))
AICctable <- data.frame(models, AICcs)
AICcsort <- AICctable[order(AICcs),]
weights <- data.frame(Weights(AICcsort$AICcs))
AICcweights <- data.frame(AICcsort, weights)
AICcweights

FCD7
FCD4

####Correlation

D19Num <- read.csv("Data2019Num.csv")
D19Num <- D19Num %>%
  filter(Easting !="NA")
str(D19Num)
cor(D19Num)


#####Figures#####
ggplot(data = Data2019) +
  theme_classic() +
  geom_point(mapping = aes(x = FLCC, y = ASTR)) +
  labs(y= "Coastal tailed frog occupancy", x= "FLCC")


ggplot(data = WRCOV_ASTR) +
  theme_classic() +
  geom_point(mapping = aes(x = Sqavg, y = (logAWRcov))) +
  geom_vline(mapping = aes(xintercept = 5)) +
  geom_abline(intercept = -1.10577, slope= -.07481, linetype= "dashed") +
  labs(y= "Log-transformed coefficient of variation of well replicates", x= "Estimated coastal tailed frog starting copy number")
ggplot(data = WRCOV_DITE) +
  theme_classic() +
  geom_point(mapping = aes(x = Sqavg, y = (logDWRcov))) +
  geom_line(mapping = aes(x = (25), y= logWRcov)) +
  geom_abline(intercept = -2.14491, slope= -.00615, linetype= "dashed") +
  labs(y= "Log-transformed coefficient of variation of well replicates", x= "Estimated coastal giant salamander starting copy number")



ggplot(data = Data2019DFCOV) +
  theme_classic() +
  geom_point(mapping = aes(x = Grad, y = (DFCOV))) +
  geom_smooth(se = FALSE, method='lm', mapping = aes(x = Grad, y = (DFCOV)), color = 1) +
  labs(y= "Coefficient of variation of coastal giant salamander eDNA", x= "Stream gradient (%)")
ggplot(data = Data2019AFCOV) +
  theme_classic() +
  geom_point(mapping = aes(x = ASQavg, y = (AFCOV))) +
  geom_smooth(se = FALSE, method='lm', mapping = aes(x = ASQavg, y = (AFCOV)), color = 1) +
  labs(y= "Coefficient of variation of coastal tailed frog eDNA", x= "Coastal tailed frog eDNA starting quantity (#copies)")



ggplot(data = JDSF1920) +
  theme_classic() +
  scale_fill_grey(start=1.0, end=0.5) +
  geom_boxplot(mapping = aes(y= LS, x= as.factor(Year))) +
  labs(y= "Discharge (L/s)", x= "Year")

ggplot(data = Yr2YrA) +
  geom_boxplot(mapping = aes(y= Conc, x= Set, fill= as.factor(Year))) +
  theme_classic() +
  scale_fill_grey(start=1.0, end=0.5) +
  theme(legend.title = element_blank()) +
  labs(y= "Coastal tailed frog eDNA concentration (copies/L)", x= "Dataset")
ggplot(data = Yr2YrA) +
  geom_boxplot(mapping = aes(y= FAR, x= Set, fill= as.factor(Year))) +
  theme_classic() +
  scale_fill_grey(start=1.0, end=0.5) +
  theme(legend.title = element_blank()) +
  labs(y= "Coastal tailed frog flow-corrected eDNA rate (copies/sec)", x= "Dataset")

ggplot(data = Yr2YrD) +
  theme_classic() + 
  geom_boxplot(mapping = aes(y= Conc, x= Set, fill= as.factor(Year))) +
  scale_fill_grey(start=1.0, end=0.5) +
  theme(legend.title = element_blank()) +
  labs(y= "Coastal giant salamander eDNA concentration (copies/L)", x= "Dataset")
ggplot(data = Yr2YrD) +
  theme_classic() + 
  geom_boxplot(mapping = aes(y= FAR, x= Set, fill= as.factor(Year))) +
  scale_fill_grey(start=1.0, end=0.5) +
  theme(legend.title = element_blank()) +
  labs(y= "Coastal giant salamander flow-corrected eDNA quantity (copies/sec)", x= "Dataset")



ggplot(data = Source20) +
  geom_point(mapping = aes(x = DistSmallTrib, y = DTot)) +
  theme_classic() +
  labs(y= "Flow-adjusted coastal giant salamander eDNA rate (copies/sec)", x= "Distance Upstream to Small Tributary")
ggplot(data = Source20) +
  geom_point(mapping = aes(x = DistSmallTrib, y = ATot)) +
  theme_classic() +
  labs(y= "Flow-adjusted coastal tailed frog eDNA rate (copies/sec)", x= "Distance Upstream to Small Tributary")



ggplot(data = Bin100TwoSp) +
  theme_classic() + 
  geom_boxplot(mapping = aes(y= PropDiff, x= Species)) +
  scale_fill_grey(start=1.0, end=0.5) +
  theme(legend.title = element_blank()) +
  labs(y= "Proportion of US eDNA at DS site 110m DS (copies/sec)", x= "Species")

ggplot(data = TransPt) +
  geom_smooth(se = FALSE, mapping = aes(x = TransectDist, y = DITE, group= Transect, color=Transect)) +
  geom_point(mapping = aes(x = TransectDist, y = DITE, group= Transect, shape=Transect)) +
  theme_classic() +
  theme(legend.position= "none") +
  labs(y= "Flow-adjusted coastal giant salamander eDNA rate (copies/sec)", x= "Transect position (m)")
ggplot(data = TransPt) +
  geom_smooth(mapping = aes(x = TransectDist, y = ASTR, group= Transect, color=Transect)) +
  geom_point(mapping = aes(x = TransectDist, y = ASTR, group= Transect, shape=Transect)) +
  theme_classic() +
  theme(legend.position= "none") +
  labs(y= "Flow-adjusted coastal tailed frog eDNA rate (copies/sec)", x= "Transect position (m)")

geom_point(mapping = aes(x = TransectDist, y = DITEProp, group= Transect, color=Transect)) +
  t

ggplot(data = TransPt) +
  geom_smooth(se = FALSE, mapping = aes(x = TransectDist, y = DITE, group= Transect, color=Transect)) +
  geom_point(mapping = aes(x = TransectDist, y = DITE, group= Transect, color=Transect)) +
  geom_errorbar(mapping = aes(x = TransectDist, y = DITE, group= Transect, color=Transect, ymin=DITE-DSE, ymax=DITE+DSE, width=.1)) +
  theme_classic() +
  
  theme(legend.position= "none") +
  labs(y= "Proportion of US coastal giant salamander eDNA rate (copies/sec)", x= "Transect position (m)")

#ggplot(data = TransPt) +
#  geom_smooth(se = FALSE, mapping = aes(x = TransectDist, y = DITE)) +
#  geom_point(mapping = aes(x = TransectDist, y = DITE)) +
#  geom_errorbar(mapping = aes(x = TransectDist, y = DITE, ymin=DITE-DSE, ymax=DITE+DSE, width=.1)) +
#  theme_classic() +
#  facet_wrap(~Transect) +
#  strip.text.x = theme_blank() +
#  theme(legend.position= "none") +
#  labs(y= "Proportion of US coastal giant salamander eDNA rate (copies/sec)", x= "Transect position (m)")


ggplot(data = TransPt) +
  geom_smooth(mapping = aes(x = TransectDist, y = ASTR, group= Transect, color=Transect)) +
  geom_point(mapping = aes(x = TransectDist, y = ASTR, group= Transect, color=Transect)) +
  theme_classic() +
  geom_errorbar(mapping = aes(x = TransectDist, y = ASTR, group= Transect, color=Transect, ymin=ASTR-ASE, ymax=ASTR+ASE, width=.1)) +
  theme(legend.position= "none") +
  labs(y= "Proportion of US coastal tailed frog eDNA rate (copies/sec)", x= "Transect position (m)")



