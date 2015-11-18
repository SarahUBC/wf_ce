# CE Attempt 2
Sarah Klain  
November 2, 2015  





```r
setwd("/Users/sarahklain/Documents/R_2015/wf_ce/CE")

#install.packages("support.CEs")
library(ggplot2)
library(ggthemes)
library(viridis)
suppressMessages(library(dplyr))
library(knitr)
library(tidyr)
library(broom)
library(support.CEs)
```

```
## Loading required package: DoE.base
## Loading required package: grid
## Loading required package: conf.design
## 
## Attaching package: 'DoE.base'
## 
## The following objects are masked from 'package:stats':
## 
##     aov, lm
## 
## The following object is masked from 'package:graphics':
## 
##     plot.design
## 
## The following object is masked from 'package:base':
## 
##     lengths
## 
## Loading required package: MASS
## 
## Attaching package: 'MASS'
## 
## The following object is masked from 'package:dplyr':
## 
##     select
## 
## Loading required package: simex
## Loading required package: RCurl
## Loading required package: bitops
## 
## Attaching package: 'RCurl'
## 
## The following object is masked from 'package:tidyr':
## 
##     complete
## 
## Loading required package: XML
```

```r
library(survival)
library(mlogit)
```

```
## Loading required package: Formula
## Loading required package: maxLik
## Loading required package: miscTools
## 
## Please cite the 'maxLik' package as:
## Henningsen, Arne and Toomet, Ott (2011). maxLik: A package for maximum likelihood estimation in R. Computational Statistics 26(3), 443-458. DOI 10.1007/s00180-010-0217-1.
## 
## If you have questions, suggestions, or comments regarding the 'maxLik' package, please use a forum or 'tracker' at maxLik's R-Forge site:
## https://r-forge.r-project.org/projects/maxlik/
```
Example #3 from support.CEs.pdf

## LMA design description from support.CEs.pdf

The L^MA method directly creates a choice experiment design from an orthogonal main-effect array (Johnson et al. 2007). In this method, an orthogonal main-effect array with M times A columns of L level factors is used to create each choice set that contains M alternatives of A attributes with L levels. Each row of the array corresponds to the alternatives of a choice set.

This method creates a labeled type choice experiment design that can contain both generic attributes and alternative-specific attributes: the generic attribute refers to that which is included in all the al- ternatives; the alternative-specific attribute is that which is included in only one alternative. The reader is referred to chapters 3 and 5 of Louviere et al. (2000) for details about the types of attribute—generic or alternative-specific—and the types of choice experiment design—labeled or unlabeled.

When this function is used, the combination of attributes and attribute levels, the number of alter- natives per choice set excluding an opt-out or common base option, and the number of blocks are respectively assigned to the arguments.

The combination of attributes and attribute levels are assigned to the argument attribute.names in list format. For example, let’s assume that the alternative has three attributes, each of which has three levels: an attribute X with the three levels of x1, x2, and x3; an attribute Y with the three levels of y1, y2, and y3; and an attribute Z with the three levels of 10, 20, and 30. In this case, the argument is set as follows:

attribute.names = list(X = c("x1", "x2", "x3"),
Y = c("y1", "y2", "y3"), Z = c("10", "20", "30"))

The number of alternatives per choice set is defined by the argument nalternatives: the number of alternatives does not include an opt-out option such as a "none of these" or a common base option.

Example 3 from support.CEs.pdf

des3 <- Lma.design(
 attribute.names = list(
  Region = c("Reg_A", "Reg_B", "Reg_C"),
  Eco = c("Conv.", "More", "Most"),
  Price = c("1", "1.1", "1.2")),
 nalternatives = 1,
 nblocks = 1,
 row.renames = FALSE,
 seed = 987)
des3

questionnaire(choice.experiment.design = des3, quote = FALSE)

desmat3 <- make.design.matrix(
 choice.experiment.design = des3,
 optout = TRUE,
 categorical.attributes = c("Region", "Eco"),
 continuous.attributes = c("Price"),
 unlabeled = TRUE,
 common = NULL,
 binary = TRUE)
data(syn.res3)

dataset3 <- make.dataset(
 respondent.dataset = syn.res3,
 choice.indicators =
  c("q1", "q2", "q3", "q4", "q5", "q6", "q7", "q8", "q9"),
 design.matrix = desmat3)

blout <- glm(RES ~ Reg_B + Reg_C + More + Most + Price,
             family = binomial(link = logit), data = dataset3)
summary(blout)

gofm(blout)

mwtp(output = blout,
 monetary.variables = c("Price"),
 nonmonetary.variables =
  c("Reg_B", "Reg_C", "More", "Most"),
 seed = 987)

### lma design


```r
lma_des_wf <- Lma.design(
     attribute.names = list(
       hab_qual = c("big loss", "small loss", "small gain", "big gain"), 
       own = c("state", "municipal", "private", "cooperative"),
       vis = c("mi1", "mi4", "mi8", "mi10"), bill = c("1", "5", "10", "20")), 
       nalternatives = 2,
       nblocks = 4, 
       row.renames = FALSE,
       seed = 987)
```

```
## The columns of the array have been used in order of appearance. 
## For designs with relatively few columns, 
## the properties can sometimes be substantially improved 
## using option columns with min3 or even min34.
```

```r
#lma_des_wf
```

### Create (and understand!) format of design matrix


```r
desmat_lma <-make.design.matrix(
  choice.experiment.design = lma_des_wf,
  optout = TRUE,
  categorical.attributes = c("hab_qual", "own", "vis"), 
  continuous.attributes = c("bill"),
  unlabeled = TRUE)

head(desmat_lma)
```

```
##   BLOCK QES ALT ASC small.loss small.gain big.gain municipal private
## 1     1   1   1   1          0          0        0         0       0
## 2     1   1   2   1          0          0        0         0       0
## 3     1   1   3   0          0          0        0         0       0
## 4     1   2   1   1          1          0        0         1       0
## 5     1   2   2   1          1          0        0         1       0
## 6     1   2   3   0          0          0        0         0       0
##   cooperative mi4 mi8 mi10 bill
## 1           0   0   0    0    1
## 2           0   0   0    0    1
## 3           0   0   0    0    0
## 4           0   1   0    0    5
## 5           0   1   0    0    5
## 6           0   0   0    0    0
```

```r
#save as csv in case I want to look at it outside of R

write.csv(desmat_lma, "desmat_lma.csv")
# If you modify the csv file, then read it back into R, be sure to remove the first column numbered 1,2,3 etc. that is automatically added when a table is written to csv. 
```

Manually match my JMP CE configuration to the format of the design matrix that R outputed (desmat_lma). I did this in excel and saved it as a csv. 

wf means wind farm choice experiment data


```r
desmat_lma_wf <- read.csv("desmat_lma_wf.csv")
```

Load my respondent dataset


```r
# data only on the 8 choice experiment choices
cer <- read.csv("cer_2015_10_23_min.csv")

# data with data on 8 choices and demographic and attitude data
cer_big <- read.csv("cer_2015_10_23.csv")

str(cer)
```

```
## 'data.frame':	400 obs. of  10 variables:
##  $ ID   : int  1 2 3 4 5 6 7 8 9 10 ...
##  $ BLOCK: int  1 1 1 1 1 1 1 1 1 1 ...
##  $ q1   : int  1 1 1 1 1 1 2 1 1 1 ...
##  $ q2   : int  2 2 2 1 2 2 2 2 1 1 ...
##  $ q3   : int  2 1 2 3 3 2 2 2 2 3 ...
##  $ q4   : int  2 2 2 3 2 2 2 2 2 3 ...
##  $ q5   : int  1 1 1 1 1 1 2 1 1 1 ...
##  $ q6   : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ q7   : int  1 1 1 1 1 1 2 1 1 3 ...
##  $ q8   : int  2 2 2 2 2 2 1 2 2 2 ...
```

Makes a data set using make.dataset. This will be used for a conditional logit model analysis with the function clogit in the package survival or for a binary choice model analysis with the function glm in the package stats.

The code is copied from support.CEs.pdf, which has an example with 9 questions in the CE:

make.dataset(respondent.dataset, design.matrix,
                 choice.indicators, detail = FALSE)

 choice.indicators = 
  c("q1", "q2", "q3", "q4", "q5", "q6", "q7", "q8", "q9"), 
 design.matrix = desmat2)
 

```r
dswf <- make.dataset(respondent.dataset = cer, design.matrix = desmat_lma_wf, choice.indicators = c("q1", "q2", "q3", "q4", "q5", "q6", "q7", "q8"), detail = TRUE)

head(dswf)
```

```
##   ID q1 q2 q3 q4 q5 q6 q7 q8 BLOCK QES ALT SELECT   RES ASC small.loss
## 1  1  1  2  2  2  1  1  1  2     1   1   1      1  TRUE   1          0
## 2  1  1  2  2  2  1  1  1  2     1   1   2      1 FALSE   1          1
## 3  1  1  2  2  2  1  1  1  2     1   1   3      1 FALSE   0          0
## 4  1  1  2  2  2  1  1  1  2     1   2   1      2 FALSE   1          1
## 5  1  1  2  2  2  1  1  1  2     1   2   2      2  TRUE   1          0
## 6  1  1  2  2  2  1  1  1  2     1   2   3      2 FALSE   0          0
##   small.gain big.gain municipal private cooperative mi4 mi8 mi10 bill STR
## 1          1        0         0       0           0   0   0    0    1 101
## 2          0        0         0       1           0   0   1    0    5 101
## 3          0        0         0       0           0   0   0    0    0 101
## 4          0        0         0       0           1   0   1    0    1 102
## 5          1        0         1       0           0   0   0    0    5 102
## 6          0        0         0       0           0   0   0    0    0 102
```

### Run conditional logistic regression


```r
wf_clogit <- clogit(RES ~ +small.loss + small.gain + big.gain + 
                      municipal + private + cooperative + 
                      mi4 + mi8 + mi10 + bill + 
                      strata(STR), data=dswf)
summary(wf_clogit)
```

```
## Call:
## coxph(formula = Surv(rep(1, 9600L), RES) ~ +small.loss + small.gain + 
##     big.gain + municipal + private + cooperative + mi4 + mi8 + 
##     mi10 + bill + strata(STR), data = dswf, method = "exact")
## 
##   n= 9594, number of events= 3198 
##    (6 observations deleted due to missingness)
## 
##                  coef exp(coef)  se(coef)       z Pr(>|z|)    
## small.loss   1.328649  3.775938  0.074989  17.718  < 2e-16 ***
## small.gain   2.868386 17.608578  0.092282  31.083  < 2e-16 ***
## big.gain     3.739393 42.072447  0.107598  34.753  < 2e-16 ***
## municipal   -0.155583  0.855916  0.076756  -2.027  0.04266 *  
## private     -0.486025  0.615067  0.079067  -6.147 7.89e-10 ***
## cooperative -0.315341  0.729540  0.100031  -3.152  0.00162 ** 
## mi4          0.214478  1.239215  0.077932   2.752  0.00592 ** 
## mi8          0.343995  1.410572  0.075872   4.534 5.79e-06 ***
## mi10         0.839584  2.315403  0.109167   7.691 1.47e-14 ***
## bill        -0.072229  0.930318  0.005429 -13.304  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
##             exp(coef) exp(-coef) lower .95 upper .95
## small.loss     3.7759    0.26483    3.2598    4.3738
## small.gain    17.6086    0.05679   14.6951   21.0996
## big.gain      42.0724    0.02377   34.0730   51.9500
## municipal      0.8559    1.16834    0.7364    0.9949
## private        0.6151    1.62584    0.5268    0.7182
## cooperative    0.7295    1.37073    0.5997    0.8876
## mi4            1.2392    0.80696    1.0637    1.4437
## mi8            1.4106    0.70893    1.2157    1.6367
## mi10           2.3154    0.43189    1.8694    2.8678
## bill           0.9303    1.07490    0.9205    0.9403
## 
## Rsquare= 0.268   (max possible= 0.519 )
## Likelihood ratio test= 2994  on 10 df,   p=0
## Wald test            = 1615  on 10 df,   p=0
## Score (logrank) test = 2784  on 10 df,   p=0
```

```r
wf_clogit
```

```
## Call:
## clogit(RES ~ +small.loss + small.gain + big.gain + municipal + 
##     private + cooperative + mi4 + mi8 + mi10 + bill + strata(STR), 
##     data = dswf)
## 
## 
##                 coef exp(coef) se(coef)      z       p
## small.loss   1.32865   3.77594  0.07499  17.72 < 2e-16
## small.gain   2.86839  17.60858  0.09228  31.08 < 2e-16
## big.gain     3.73939  42.07245  0.10760  34.75 < 2e-16
## municipal   -0.15558   0.85592  0.07676  -2.03  0.0427
## private     -0.48602   0.61507  0.07907  -6.15 7.9e-10
## cooperative -0.31534   0.72954  0.10003  -3.15  0.0016
## mi4          0.21448   1.23921  0.07793   2.75  0.0059
## mi8          0.34400   1.41057  0.07587   4.53 5.8e-06
## mi10         0.83958   2.31540  0.10917   7.69 1.5e-14
## bill        -0.07223   0.93032  0.00543 -13.30 < 2e-16
## 
## Likelihood ratio test=2994  on 10 df, p=0
## n= 9594, number of events= 3198 
##    (6 observations deleted due to missingness)
```

I adapted this code below from Puget Sound riparian buffer study. The model below includes demographic variables. 

escont <- clogit(RES~Environmental_Quality+Farm_Conversion+Annual_10Yr_Tax+ascsq+
                    sqinc+sqedu+sqact+sqtrips+
                    strata(STR), data=cedset, weights=WT)
            
                    
### Next steps
1. Understand why the first levels of my discreet attributes are dropped when the dataset is made and model is run (e.g., big loss, state owned, 1 mile away)

```r
# rescont <- clogit(RES~Environmental_Quality+Farm_Conversion+Annual_10Yr_Tax+ascsq+
                  #  sqinc+sqedu+sqact+sqtrips+
                   # strata(STR), data=cedset, weights=WT)
# summary(rescont)
wtpwf <- mwtp(wf_clogit, monetary.variables=c("bill"), 
              nonmonetary.variables=c("small.loss",  "small.gain", "big.gain", 
                                      "municipal","private", "cooperative", 
                                      "mi4", "mi8", "mi10"), nreplications=1000)
wtpwf
```

```
## 
##                MWTP    2.5%   97.5%
## small.loss  18.3949 15.7078 21.6989
## small.gain  39.7122 35.1280 45.7114
## big.gain    51.7711 45.5319 59.8823
## municipal   -2.1540 -4.3475 -0.1974
## private     -6.7289 -9.4186 -4.5366
## cooperative -4.3658 -7.1211 -1.5569
## mi4          2.9694  0.9035  5.3135
## mi8          4.7625  2.7950  6.9917
## mi10        11.6239  8.4417 15.0521
## 
## method = Krinsky and Robb
```

```r
#gofcont <- gofm(wtpwf)
#gofcont
```

2. Add demographic and attitude variables from my WF survey
Do I convert all or some to dummy variables?


