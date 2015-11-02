
# Info about all the experimental design packages in R can be found here: 
# http://cran.r-project.org/web/views/ExperimentalDesign.html

install.packages("support.CEs")
install.packages("xtable")
install.packages("sm")
install.packages("lmtest")
install.packages("mlogit")
install.packages("ggplot2")
library(support.CEs)
library(survival)
library(stats)
library(xtable)
library(sm)
library(lmtest)
library(mlogit)
library(ggplot2)

# installs and loads support.CEs package. Dependencies are DoE.base, MASS, simex.
# Also, DoE.base depends on vcd. Make sure all these ducks are in a row. 


des2v2 <- rotation.design(attribute.names = list(Environmental_Quality = c("2", "3", "4", "5"),  
                                                 Farm_Conversion = c(".02", ".04", ".08",".16"), 
                                                 Annual_10Yr_Tax = c("10", "20", "40","80")),
                          nalternatives = 2,
                          nblocks = 2,
                          row.renames = FALSE,
                          seed = 987)

# I recoded the Environmental_Quality variable to be numerical and continuous on a scale of 1 to 5. 
# This allows the design matrix to be made with the common base. Using environmental quality as a categorical variable did not 
# allow me to enter a common base option that was separate from the existing categories. 

# ques2v2 <- questionnaire(des2v2, common=c(Environmental_Quality = "1", Farm_Conversion = "0", Annual_10Yr_Tax = "0"), quote=FALSE)

desmat <- make.design.matrix(choice.experiment.design=des2v2, 
                             optout=FALSE,
                             continuous.attributes=c("Environmental_Quality", "Farm_Conversion","Annual_10Yr_Tax"),
                             common=c(Environmental_Quality="1", Farm_Conversion="0", Annual_10Yr_Tax="0"), 
                             binary=FALSE)

cevars <- read.csv('\\\\daryl\\users\\nenelow\\My Documents\\Puget Sound Institute\\Survey Data\\141111CESurveyNumericResponses.csv');

cedset <- make.dataset(respondent.dataset=cevars, design.matrix=desmat, 
                       choice.indicators=c("Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8"))

#Interaction Terms

# Taking the natural log of quality variable to incorporate diminishing returns to
# increased quality
cedset$lnqual <- log(cedset$Environmental_Quality)

# Conversion Level Dummies
rows <- nrow(cedset)
cedset$farmverylow <- 0
cedset$farmlow <- 0
cedset$farmlowmed <- 0
cedset$farmhimed <- 0
cedset$farmhi <- 0

cedset$enqverylow <-0
cedset$enqlow <- 0
cedset$enqlowmed <- 0
cedset$enqhimed <- 0
cedset$enqhi <- 0

for (i in 1:rows)
  if (cedset$Farm_Conversion[i]== 0.00){
    cedset$farmverylow[i] = 1 } else 
      if (cedset$Farm_Conversion[i] == 0.02) {
        cedset$farmlow[i] = 1 } else
          if (cedset$Farm_Conversion[i] == 0.04) {
            cedset$farmlowmed[i] = 1 } else
              if (cedset$Farm_Conversion[i] == 0.08) {
                cedset$farmhimed[i] = 1 } else
                  if (cedset$Farm_Conversion[i] == 0.16) {
                    cedset$farmhi[i] = 1 }

for (i in 1:rows)
  if (cedset$Environmental_Quality[i] == 1) {
    cedset$enqverylow[i] = 1 } else
      if (cedset$Environmental_Quality[i] == 2) {
        cedset$enqlow[i] = 1 } else
          if (cedset$Environmental_Quality[i] == 3) {
            cedset$enqlowmed[i] = 1 } else
              if (cedset$Environmental_Quality[i] == 4) {
                cedset$enqhimed[i] = 1 } else
                  if (cedset$Environmental_Quality[i] == 5) {
                    cedset$enqhi[i] = 1 }

# ASC for Status Quo

cedset$ascsq <- 0
numsqs <-0

for (i in 1:rows)
  if (cedset$ASC[i] == 0) {
    cedset$ascsq[i] = 1
    } else cedset$ascsq[i] = 0

# Status Quo Interactions
cedset$sqinc <- cedset$ascsq*cedset$inc
cedset$sqedu <- cedset$ascsq*cedset$edu
cedset$sqact <- cedset$ascsq*cedset$activitydummy
cedset$sqtrips <- cedset$ascsq*cedset$Numrectrips

# Specification 1: Continuous variables, with same interactions as Specification 1,
# except in levels of quality instead of logs. 
rescont <- clogit(RES~Environmental_Quality+Farm_Conversion+Annual_10Yr_Tax+ascsq+
                    sqinc+sqedu+sqact+sqtrips+
                    strata(STR), data=cedset, weights=WT)
summary(rescont)
wtpcont <- mwtp(rescont, monetary.variables=c("Annual_10Yr_Tax"), 
                nonmonetary.variables=c("Environmental_Quality", "Farm_Conversion"), nreplications=1000)
wtpcont
gofcont <- gofm(rescont)
gofcont

# Specification 2: Natural log of quality, levels of conversion;
# Interaction between quality and conversion, quality and income, conversion and income,
# Quality and education, quality and activities chosen, quality and number of trips chosen
reslog <- clogit(RES~lnqual+Farm_Conversion+Annual_10Yr_Tax+ascsq+
                sqinc+sqedu+sqact+sqtrips+strata(STR), 
                data=cedset, weights=WT)

summary(reslog)
wtplog <- mwtp(reslog, monetary.variables=c("Annual_10Yr_Tax"), 
               nonmonetary.variables=c("lnqual", "Farm_Conversion"), nreplications=1000)
wtplog
goflog <- gofm(reslog)
goflog

# Specification 3: Levels of each non-monetary variable separated into binary dummies, 
# with the common base level as the omitted dummy. Interactions between high environmental quality 
# and number of trips taken, income, and downstream activities. 
# Also, some interactions included between specific levels of quality and conversion. 
reslev <- clogit(RES~enqlow+enqlowmed+enqhimed+enqhi+
                farmlowmed+farmhimed+farmhi+
                sqinc+sqedu+sqact+sqtrips+
                Annual_10Yr_Tax+
                strata(STR), data=cedset, weights=WT)
summary(reslev)
wtplev <- mwtp(reslev, monetary.variables=c("Annual_10Yr_Tax"), 
               nonmonetary.variables=c("enqlowmed", "enqhimed", "enqhi", 
                                       "farmlow", "farmlowmed", "farmhimed", "farmhi"), 
               nreplications=1000)

wtplev
goflev <- gofm(reslev)
goflev

# Likelihood Ratio Test of Model Goodness of Fit
llr <- lrtest(rescont,reslog,reslev)

# Specification 4: use glm from stats package. Note: it has a very low goodness of fit, hence will not use. 
wts <- as.numeric(cedset$WT)
lince <- glm(RES~lnqual+Farm_Conversion+Annual_10Yr_Tax+qualrec+convrec+qualinc+convinc, data=cedset, weights=wts)
wtplin <- mwtp(lince,monetary.variables=c("Annual_10Yr_Tax"), nonmonetary.variables=c("Environmental_Quality", "Farm_Conversion"))

# Now the graphics.
# Histograms for pennies
wat <- hist(cevars$pennieswater, breaks=20, col="blue", main="Relative Importance of Water Quality")
saf <- hist(cevars$penniessafety, breaks=20, col="pink", main="Relative Importance of Safety for Swimming and Fishing")
fis <- hist(cevars$penniesfish, breaks=20, col="lightblue", main="Relative Importance of Fish Habitat")
veg <- hist(cevars$penniesveg, breaks=20, col="green", main="Relative Importance of Vegetation")
bir <- hist(cevars$penniesbird, breaks=20, col="orange", main="Relative Importance of Bird Habitat")

# Kernel density plots for marginal WTPs for all specifications
margslog <- wtplog$mwtps
hlq <- hist(margslog[,1], breaks=100, col="lightblue", main="WTP for Additional Water Quality (Natural Logs)")
dlq <- density(margslog[,1])
plot(dlq, main="WTP for Additional Water Quality (in Natural Logs)")
dcq <- density(margslog[,2])
plot(dcq, main="WTP for Additional Farmland Conversion")

margscont <- wtpcont$mwtps
dq <- density(margscont[,1])
plot(dq, main="Marginal WTP for Additional Environmental Quality")
dc <- density(margscont[,2])
plot(dc, main="Marginal WTP for Additional Farmland Conversion")

margslev <- wtplev$mwtps
dqpoor <- density(margslev[,1])
plot(dqpoor, main="WTP for Poor Environmental Quality")
dqfair <- density(margslev[,2])
plot(dqfair, main="WTP for Fair Environmental Quality")
dqgood <- density(margslev[,3])
plot(dqgood, main="WTP for Good Environmental Quality")
dqvgood <- density(margslev[,4])
plot(dqvgood, main="WTP for Very Good Environmental Quality")
dcfour <- density(margslev[,5])
plot(dcfour, main="WTP for 4% Farmland Conversion")
dceight <- density(margslev[,6])
plot(dceight, main="WTP for 8% Farmland Conversion")
dcsixteen <- density(margslev[,7])
plot(dcsixteen, main="WTP for 16% Farmland Conversion")

# Comparison
mgslevfr <- as.data.frame(margslev)
attach(mgslevfr)

# Trying to get it into shape for an mlogit with random parameters
cedtoml <- mlogit.data(cedset, shape="long", choice="RES", alt.var="ALT", id.var="ID")
# Now for the confusing part. 
ml <- mlogit(RES~lnqual+Farm_Conversion+Annual_10Yr_Tax, cedtoml, na.action=na.omit, alt.subset=c(1,2,3))
# Gives error message: 
# Error in if (abs(x - oldx) < ftol) { : 
# missing value where TRUE/FALSE needed
# Yet: 
x <- is.na(cedtoml)
length(x[x==TRUE])
# Will show you that there are no missing values in the entire dataset! 
# And, the following nonsensical formula works: 
# ml <- mlogit(lnqual~Farm_Conversion+Annual_10Yr_Tax | 0, data=cedtoml)
# So, there is something wrong with the RES variable. But it is a logical variable with no missing values. 
class(cedtoml$RES) 
# [1] "logical"
str(cedtoml)
# Will give you the structure of the cedtoml object - what data type it is and all of its variables

# A funny work-around. Create a numeric out of the logical variable.
cedtoml$choice <- as.numeric(cedtoml$RES)

# Odds and Ends
# Interaction terms with specific levels - omitted from regressions. 
cedset$hihi <- cedset$enqhi*cedset$farmhi
cedset$hihimed <- cedset$enqhi*cedset$farmhimed
cedset$lowlow <- cedset$enqlow*cedset$farmlow
cedset$lowlowmed <- cedset$enqlow*cedset$farmlowmed

cedset$hitrips <- (cedset$enqhi)*cedset$Numrectrips
cedset$hiinc <- (cedset$enqhi)*cedset$income
cedset$hiedu <- (cedset$enqhi)*cedset$edu
cedset$hiact <- (cedset$enqhi)*cedset$activitydummy

cedset$farmhitrips <- (cedset$farmhi)*cedset$Numrectrips
cedset$farmhiinc <- (cedset$farmhi)*cedset$income
cedset$farmhiedu <- (cedset$farmhi)*cedset$edu
cedset$farmhiact <- (cedset$farmhi)*cedset$activitydummy

cedset$himedtrips <- (cedset$enqhimed)*cedset$Numrectrips
cedset$himedinc <- (cedset$enqhimed)*cedset$income
cedset$himededu <- (cedset$enqhimed)*cedset$edu
cedset$himedact <- (cedset$enqhimed)*cedset$activitydummy

cedset$qualgen <- cedset$lnqual*cedset$gender
cedset$qualgenn <-cedset$Environmental_Quality*cedset$gender
cedset$convgen <-cedset$Farm_Conversion*cedset$gender
 
# Does a higher level of conversion increase WTP for quality? 
cedset$qualconv <- cedset$lnqual*cedset$Farm_Conversion
cedset$qualconvn <- cedset$Environmental_Quality*cedset$Farm_Conversion

# Does higher income increase WTP for quality? 
cedset$qualinc <-cedset$lnqual*cedset$income
cedset$qualincn <- cedset$Environmental_Quality*cedset$income

# Does higher income increase WTP/WTA for conversion?
cedset$convinc <-cedset$Farm_Conversion*cedset$income

# Does having been to Puget Sound increase WTP/WTA for quality and conversion?
cedset$qualrec <-cedset$lnqual*cedset$Recdummy
cedset$convrec <-cedset$Farm_Conversion*cedset$Recdummy

# Does having more education increase WTP for quality? Result: not statistically significant.
cedset$qualedu <-cedset$lnqual*cedset$edu
cedset$qualedun <- cedset$Environmental_Quality*cedset$edu
cedset$convedu <-cedset$Farm_Conversion*cedset$edu

# Does participating in downstream activities affect WTP for quality? 
cedset$qualact <- cedset$lnqual*cedset$activitydummy
cedset$qualactn <- cedset$Environmental_Quality*cedset$activitydummy

# Does having taken more rec trips to Puget Sound increase WTP for quality? 
cedset$qualtrips <- cedset$lnqual*cedset$Numrectrips
cedset$qualtripsn <- cedset$Environmental_Quality*cedset$Numrectrips