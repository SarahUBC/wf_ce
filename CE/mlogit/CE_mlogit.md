# CE_mlogit
Sarah Klain  
November 20, 2015  


```r
setwd("/Users/sarahklain/Documents/R_2015/wf_ce/CE/mlogit")

#install.packages("support.CEs")
library(ggplot2)
library(ggthemes)
library(viridis)
suppressMessages(library(dplyr))
library(knitr)
library(tidyr)
library(broom)
#library(support.CEs)
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

```r
library(stargazer)
```

```
## 
## Please cite as: 
## 
##  Hlavac, Marek (2015). stargazer: Well-Formatted Regression and Summary Statistics Tables.
##  R package version 5.2. http://CRAN.R-project.org/package=stargazer
```

input data
gather data (make it long form)
b1 mean Block 1

```r
b1 <- read.csv("B1_cer.csv")
b1_l <- tidyr::gather(b1, "ce_quest", "response", 2:9)
```

Follow along Viton's work

```r
#clogit <- read.csv("clogit.csv", na.strings= "-999")

# read in the data
library(foreign)
clogit <- read.csv("clogit.csv", col.names=c("mode","ttme","invc","invt","gc","chair","hinc", "psize","indj","indi","aasc","tasc","basc","casc","hinca","psizea","z","nij","ni"),na.strings="-999")
 
# check the data
summary(clogit)
```

```
##       mode           ttme            invc             invt       
##  Min.   :0.00   Min.   : 0.00   Min.   :  2.00   Min.   :  63.0  
##  1st Qu.:0.00   1st Qu.: 0.75   1st Qu.: 23.00   1st Qu.: 234.0  
##  Median :0.00   Median :35.00   Median : 39.00   Median : 397.0  
##  Mean   :0.25   Mean   :34.59   Mean   : 47.76   Mean   : 486.2  
##  3rd Qu.:0.25   3rd Qu.:53.00   3rd Qu.: 66.25   3rd Qu.: 795.5  
##  Max.   :1.00   Max.   :99.00   Max.   :180.00   Max.   :1440.0  
##        gc            chair             hinc           psize      
##  Min.   : 30.0   Min.   :0.0000   Min.   : 2.00   Min.   :1.000  
##  1st Qu.: 71.0   1st Qu.:0.0000   1st Qu.:20.00   1st Qu.:1.000  
##  Median :101.5   Median :0.0000   Median :34.50   Median :1.000  
##  Mean   :110.9   Mean   :0.2762   Mean   :34.55   Mean   :1.743  
##  3rd Qu.:144.0   3rd Qu.:1.0000   3rd Qu.:50.00   3rd Qu.:2.000  
##  Max.   :269.0   Max.   :1.0000   Max.   :72.00   Max.   :6.000  
##       indj              indi            aasc           tasc     
##  Min.   :-2.0000   Min.   :-1.00   Min.   :0.00   Min.   :0.00  
##  1st Qu.:-2.0000   1st Qu.:-1.00   1st Qu.:0.00   1st Qu.:0.00  
##  Median : 0.0000   Median :-0.50   Median :0.00   Median :0.00  
##  Mean   :-0.5262   Mean   :-0.25   Mean   :0.25   Mean   :0.25  
##  3rd Qu.: 0.2500   3rd Qu.: 0.25   3rd Qu.:0.25   3rd Qu.:0.25  
##  Max.   : 1.0000   Max.   : 1.00   Max.   :1.00   Max.   :1.00  
##       basc           casc          hinca            psizea      
##  Min.   :0.00   Min.   :0.00   Min.   : 0.000   Min.   :0.0000  
##  1st Qu.:0.00   1st Qu.:0.00   1st Qu.: 0.000   1st Qu.:0.0000  
##  Median :0.00   Median :0.00   Median : 0.000   Median :0.0000  
##  Mean   :0.25   Mean   :0.25   Mean   : 8.637   Mean   :0.4357  
##  3rd Qu.:0.25   3rd Qu.:0.25   3rd Qu.: 0.500   3rd Qu.:0.2500  
##  Max.   :1.00   Max.   :1.00   Max.   :72.000   Max.   :6.0000  
##        z             nij            ni   
##  Min.   :0.00   Min.   :1.0   Min.   :2  
##  1st Qu.:0.75   1st Qu.:2.5   1st Qu.:2  
##  Median :1.00   Median :3.0   Median :2  
##  Mean   :0.75   Mean   :2.5   Mean   :2  
##  3rd Qu.:1.00   3rd Qu.:3.0   3rd Qu.:2  
##  Max.   :1.00   Max.   :3.0   Max.   :2
```

```r
str(clogit)
```

```
## 'data.frame':	840 obs. of  19 variables:
##  $ mode  : int  0 0 0 1 0 0 0 1 0 0 ...
##  $ ttme  : int  69 34 35 0 64 44 53 0 69 34 ...
##  $ invc  : int  59 31 25 10 58 31 25 11 115 98 ...
##  $ invt  : int  100 372 417 180 68 354 399 255 125 892 ...
##  $ gc    : int  70 71 70 30 68 84 85 50 129 195 ...
##  $ chair : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ hinc  : int  35 35 35 35 30 30 30 30 40 40 ...
##  $ psize : int  1 1 1 1 2 2 2 2 1 1 ...
##  $ indj  : int  -2 0 0 1 -2 0 0 1 -2 0 ...
##  $ indi  : int  0 -1 -1 1 0 -1 -1 1 0 -1 ...
##  $ aasc  : int  1 0 0 0 1 0 0 0 1 0 ...
##  $ tasc  : int  0 1 0 0 0 1 0 0 0 1 ...
##  $ basc  : int  0 0 1 0 0 0 1 0 0 0 ...
##  $ casc  : int  0 0 0 1 0 0 0 1 0 0 ...
##  $ hinca : int  35 0 0 0 30 0 0 0 40 0 ...
##  $ psizea: int  1 0 0 0 2 0 0 0 1 0 ...
##  $ z     : int  0 1 1 1 0 1 1 1 0 1 ...
##  $ nij   : int  1 3 3 3 1 3 3 3 1 3 ...
##  $ ni    : int  2 2 2 2 2 2 2 2 2 2 ...
```

```r
head(clogit)
```

```
##   mode ttme invc invt gc chair hinc psize indj indi aasc tasc basc casc
## 1    0   69   59  100 70     0   35     1   -2    0    1    0    0    0
## 2    0   34   31  372 71     0   35     1    0   -1    0    1    0    0
## 3    0   35   25  417 70     0   35     1    0   -1    0    0    1    0
## 4    1    0   10  180 30     0   35     1    1    1    0    0    0    1
## 5    0   64   58   68 68     0   30     2   -2    0    1    0    0    0
## 6    0   44   31  354 84     0   30     2    0   -1    0    1    0    0
##   hinca psizea z nij ni
## 1    35      1 0   1  2
## 2     0      0 1   3  2
## 3     0      0 1   3  2
## 4     0      0 1   3  2
## 5    30      2 0   1  2
## 6     0      0 1   3  2
```

```r
# save a version of the data in internal format
save(clogit,file="clogit.rdata")
# read in the mlogit package
library(mlogit)
# read in the data we saved
load(file="clogit.rdata")
# provide a choice indicator, with names
clogit$mode.ids<-factor(rep(1:4,210),labels=c("air","train","bus","car"))

# for convenience, create a special form of the dataset: note that we exploit the case sensititivty here: clogit is the original dataset, while CLOGIT (all-caps) is the version for use with the mlogit package.

CLOGIT<-mlogit.data(clogit,shape="long",choice="mode",alt.var="mode.ids")

# first model : standard logit. We save the results into a variable and then view them. The first command uses the un-fixed dataset, while the second uses the mlogit-specific dataset, and is clearly easier to type in. Both produce the same output.

res1<-mlogit(mode~ttme+gc, data=clogit, shape="long",
                alt.var="mode.ids")
summary(res1)
```

```
## 
## Call:
## mlogit(formula = mode ~ ttme + gc, data = clogit, shape = "long", 
##     alt.var = "mode.ids", method = "nr", print.level = 0)
## 
## Frequencies of alternatives:
##     air   train     bus     car 
## 0.27619 0.30000 0.14286 0.28095 
## 
## nr method
## 5 iterations, 0h:0m:0s 
## g'(-H)^-1g = 0.000221 
## successive function values within tolerance limits 
## 
## Coefficients :
##                     Estimate Std. Error t-value  Pr(>|t|)    
## train:(intercept) -1.8533538  0.3700925 -5.0078 5.505e-07 ***
## bus:(intercept)   -2.5656173  0.3843251 -6.6756 2.461e-11 ***
## car:(intercept)   -5.7763487  0.6559187 -8.8065 < 2.2e-16 ***
## ttme              -0.0970904  0.0104351 -9.3042 < 2.2e-16 ***
## gc                -0.0157837  0.0043828 -3.6013 0.0003166 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Log-Likelihood: -199.98
## McFadden R^2:  0.29526 
## Likelihood ratio test : chisq = 167.56 (p.value = < 2.22e-16)
```

```r
res2<-mlogit(mode~ttme+gc,data=CLOGIT)
summary(res2)
```

```
## 
## Call:
## mlogit(formula = mode ~ ttme + gc, data = CLOGIT, method = "nr", 
##     print.level = 0)
## 
## Frequencies of alternatives:
##     air   train     bus     car 
## 0.27619 0.30000 0.14286 0.28095 
## 
## nr method
## 5 iterations, 0h:0m:0s 
## g'(-H)^-1g = 0.000221 
## successive function values within tolerance limits 
## 
## Coefficients :
##                     Estimate Std. Error t-value  Pr(>|t|)    
## train:(intercept) -1.8533538  0.3700925 -5.0078 5.505e-07 ***
## bus:(intercept)   -2.5656173  0.3843251 -6.6756 2.461e-11 ***
## car:(intercept)   -5.7763487  0.6559187 -8.8065 < 2.2e-16 ***
## ttme              -0.0970904  0.0104351 -9.3042 < 2.2e-16 ***
## gc                -0.0157837  0.0043828 -3.6013 0.0003166 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Log-Likelihood: -199.98
## McFadden R^2:  0.29526 
## Likelihood ratio test : chisq = 167.56 (p.value = < 2.22e-16)
```

```r
# model with income interacted with the mode-specific dummys
res3<-mlogit(mode~ttme+gc | hinc, data=CLOGIT)
summary(res3)
```

```
## 
## Call:
## mlogit(formula = mode ~ ttme + gc | hinc, data = CLOGIT, method = "nr", 
##     print.level = 0)
## 
## Frequencies of alternatives:
##     air   train     bus     car 
## 0.27619 0.30000 0.14286 0.28095 
## 
## nr method
## 5 iterations, 0h:0m:0s 
## g'(-H)^-1g = 0.000614 
## successive function values within tolerance limits 
## 
## Coefficients :
##                     Estimate Std. Error t-value  Pr(>|t|)    
## train:(intercept) -0.3249576  0.5763335 -0.5638  0.572866    
## bus:(intercept)   -1.7445354  0.6775004 -2.5750  0.010025 *  
## car:(intercept)   -5.8747921  0.8020903 -7.3244   2.4e-13 ***
## ttme              -0.0954602  0.0104732 -9.1147 < 2.2e-16 ***
## gc                -0.0109273  0.0045878 -2.3818  0.017226 *  
## train:hinc        -0.0511880  0.0147352 -3.4739  0.000513 ***
## bus:hinc          -0.0232100  0.0162306 -1.4300  0.152712    
## car:hinc           0.0053735  0.0115294  0.4661  0.641163    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Log-Likelihood: -189.53
## McFadden R^2:  0.33209 
## Likelihood ratio test : chisq = 188.47 (p.value = < 2.22e-16)
```

```r
 # model with gc arying by mode
res4<-mlogit(mode~ttme | hinc | gc, data=CLOGIT)
summary(res4)
```

```
## 
## Call:
## mlogit(formula = mode ~ ttme | hinc | gc, data = CLOGIT, method = "nr", 
##     print.level = 0)
## 
## Frequencies of alternatives:
##     air   train     bus     car 
## 0.27619 0.30000 0.14286 0.28095 
## 
## nr method
## 5 iterations, 0h:0m:0s 
## g'(-H)^-1g = 0.000795 
## successive function values within tolerance limits 
## 
## Coefficients :
##                     Estimate Std. Error t-value  Pr(>|t|)    
## train:(intercept)  1.8492221  1.0578547  1.7481 0.0804489 .  
## bus:(intercept)    0.2332096  1.2150069  0.1919 0.8477885    
## car:(intercept)   -3.5030068  1.1092306 -3.1581 0.0015883 ** 
## ttme              -0.0962846  0.0104908 -9.1780 < 2.2e-16 ***
## train:hinc        -0.0547168  0.0150964 -3.6245 0.0002895 ***
## bus:hinc          -0.0249718  0.0163743 -1.5251 0.1272444    
## car:hinc           0.0034443  0.0119876  0.2873 0.7738634    
## air:gc             0.0104705  0.0091005  1.1505 0.2499210    
## train:gc          -0.0088560  0.0050541 -1.7522 0.0797317 .  
## bus:gc            -0.0070853  0.0074225 -0.9546 0.3397949    
## car:gc            -0.0110825  0.0056119 -1.9748 0.0482870 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Log-Likelihood: -184.95
## McFadden R^2:  0.34822 
## Likelihood ratio test : chisq = 197.62 (p.value = < 2.22e-16)
```

```r
# mixed logit model  in which the alt-specific vars have independent normal distributions. We use Halton numbers for efficiency in computation, and use R=500 in our simulationsnote that the syntax for specifying random alt-specific dummys has changed.
# we set print.level to 1 to get some feedback

res5<-mlogit(mode~ttme+gc,data=CLOGIT,reflevel="car",
      rpar=c("air:(intercept)"="n","bus:(intercept)"="n",
      "train:(intercept)"="n"),R=500,halton=NA,print.level=1)
```

```
## Initial value of the function : 200.021471160969 
## iteration 1, step = 0.00390625, lnL = 200.00700181, chi2 = 42.89405615
## iteration 2, step = 0.015625, lnL = 200.00530375, chi2 = 0.23847745
## iteration 3, step = 0.125, lnL = 199.73904009, chi2 = 0.77798228
## iteration 4, step = 0.25, lnL = 199.71467864, chi2 = 0.25353518
## iteration 5, step = 0.5, lnL = 199.62259894, chi2 = 2.54825399
## iteration 6, step = 1, lnL = 198.92335441, chi2 = 0.9368018
## iteration 7, step = 1, lnL = 198.21614612, chi2 = 1.17814832
## iteration 8, step = 1, lnL = 197.78031543, chi2 = 0.73589678
## iteration 9, step = 1, lnL = 197.54249151, chi2 = 0.36853162
## iteration 10, step = 1, lnL = 197.48342838, chi2 = 0.0944876
## iteration 11, step = 1, lnL = 197.47108299, chi2 = 0.0189415
## iteration 12, step = 1, lnL = 197.46807699, chi2 = 0.00507095
## iteration 13, step = 1, lnL = 197.46781515, chi2 = 0.00046435
## iteration 14, step = 1, lnL = 197.46780495, chi2 = 1.855e-05
## iteration 15, step = 1, lnL = 197.46780438, chi2 = 8.2e-07
```

```r
summary(res5)
```

```
## 
## Call:
## mlogit(formula = mode ~ ttme + gc, data = CLOGIT, reflevel = "car", 
##     rpar = c(`air:(intercept)` = "n", `bus:(intercept)` = "n", 
##         `train:(intercept)` = "n"), R = 500, halton = NA, print.level = 1)
## 
## Frequencies of alternatives:
##     car     air   train     bus 
## 0.28095 0.27619 0.30000 0.14286 
## 
## bfgs method
## 15 iterations, 0h:0m:9s 
## g'(-H)^-1g = 8.22E-07 
## gradient close to zero 
## 
## Coefficients :
##                        Estimate Std. Error t-value  Pr(>|t|)    
## air:(intercept)       6.1592355  1.1386852  5.4091 6.335e-08 ***
## train:(intercept)     5.1095985  0.8174536  6.2506 4.088e-10 ***
## bus:(intercept)       4.1487347  0.9042209  4.5882 4.471e-06 ***
## ttme                 -0.1144355  0.0199023 -5.7499 8.932e-09 ***
## gc                   -0.0308410  0.0077524 -3.9782 6.943e-05 ***
## sd.air:(intercept)    2.9380453  0.9163970  3.2061  0.001346 ** 
## sd.train:(intercept)  0.0382613 21.4589826  0.0018  0.998577    
## sd.bus:(intercept)    0.0016575 76.6920349  0.0000  0.999983    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Log-Likelihood: -197.47
## McFadden R^2:  0.3041 
## Likelihood ratio test : chisq = 172.58 (p.value = < 2.22e-16)
## 
## random coefficients
##                   Min.  1st Qu.   Median     Mean  3rd Qu. Max.
## air:(intercept)   -Inf 4.177554 6.159236 6.159236 8.140917  Inf
## bus:(intercept)   -Inf 4.147617 4.148735 4.148735 4.149853  Inf
## train:(intercept) -Inf 5.083792 5.109599 5.109599 5.135405  Inf
```

```r
# print info on what was estimated for a random parameter
rpar(res5,"air:(intercept)")
```

```
## normal distribution with parameters 6.159 (mean) and 2.938 (sd)
```

```r
# same model, but multinomial probit (not discussed in the text)
res6<-mlogit(mode~ttme+gc,data=CLOGIT,reflevel="car",
             R=500,halton=NA,probit=TRUE,print.level=1)
```

```
## Initial value of the function : 215.418647680629 
## iteration 1, step = 0.5, lnL = 213.61784664, chi2 = 14.80558162
## iteration 2, step = 0.125, lnL = 212.2754711, chi2 = 34.36745357
## iteration 3, step = 0.25, lnL = 210.05785535, chi2 = 17.76901457
## iteration 4, step = 0.5, lnL = 204.26703195, chi2 = 16.98407384
## iteration 5, step = 0.5, lnL = 202.39553162, chi2 = 10.03139393
## iteration 6, step = 0.5, lnL = 202.14973342, chi2 = 6.88719287
## iteration 7, step = 1, lnL = 201.68388904, chi2 = 6.92736439
## iteration 8, step = 0.5, lnL = 200.77273356, chi2 = 5.46991772
## iteration 9, step = 1, lnL = 200.12671046, chi2 = 1.16464207
## iteration 10, step = 1, lnL = 200.04658848, chi2 = 0.25972241
## iteration 11, step = 1, lnL = 200.01330387, chi2 = 0.12859005
## iteration 12, step = 1, lnL = 199.99013992, chi2 = 0.05323501
## iteration 13, step = 1, lnL = 199.98098438, chi2 = 0.01646335
## iteration 14, step = 1, lnL = 199.9758249, chi2 = 0.01154571
## iteration 15, step = 1, lnL = 199.97435754, chi2 = 0.00349394
## iteration 16, step = 1, lnL = 199.97431526, chi2 = 0.00059583
## iteration 17, step = 1, lnL = 199.97418271, chi2 = 0.00026991
## iteration 18, step = 1, lnL = 199.97418201, chi2 = 2.05e-06
## iteration 19, step = 1, lnL = 199.97418191, chi2 = 2.1e-07
```

```r
summary(res6)
```

```
## 
## Call:
## mlogit(formula = mode ~ ttme + gc, data = CLOGIT, reflevel = "car", 
##     probit = TRUE, R = 500, halton = NA, print.level = 1)
## 
## Frequencies of alternatives:
##     car     air   train     bus 
## 0.28095 0.27619 0.30000 0.14286 
## 
## bfgs method
## 19 iterations, 0h:1m:7s 
## g'(-H)^-1g = 2.11E-07 
## gradient close to zero 
## 
## Coefficients :
##                     Estimate Std. Error t-value  Pr(>|t|)    
## air:(intercept)    1.2626287  0.3789006  3.3323 0.0008612 ***
## train:(intercept)  1.1797855  0.2293912  5.1431 2.702e-07 ***
## bus:(intercept)    1.0267652  0.1986496  5.1687 2.357e-07 ***
## ttme              -0.0245532  0.0056767 -4.3252 1.524e-05 ***
## gc                -0.0091184  0.0019520 -4.6712 2.994e-06 ***
## air.train          0.1243927  0.3748577  0.3318 0.7400102    
## air.bus            0.1106674  0.2849226  0.3884 0.6977110    
## train.train        0.5492593  0.1493835  3.6768 0.0002361 ***
## train.bus          0.2344933  0.1577833  1.4862 0.1372333    
## bus.bus            0.2838744  0.0968112  2.9322 0.0033652 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Log-Likelihood: -199.97
## McFadden R^2:  0.29527 
## Likelihood ratio test : chisq = 167.57 (p.value = < 2.22e-16)
```

```r
# Omega-sub-i for each mode:
res6$omega
```

```
## $car
##             air     train       bus
## air   1.0000000 0.1243927 0.1106674
## train 0.1243927 0.3171593 0.1425639
## bus   0.1106674 0.1425639 0.1478191
## 
## $air
##             car     train       bus
## car   1.0000000 0.8756073 0.8893326
## train 0.8756073 1.0683739 0.9075037
## bus   0.8893326 0.9075037 0.9264842
## 
## $train
##           car       air       bus
## car 0.3171593 0.1927666 0.1745955
## air 0.1927666 1.0683739 0.1608702
## bus 0.1745955 0.1608702 0.1798507
## 
## $bus
##               car        air       train
## car   0.147819071 0.03715164 0.005255204
## air   0.037151636 0.92648420 0.018980471
## train 0.005255204 0.01898047 0.179850665
```

Learn from the examples of Croissant (2011).

```r
library("mlogit")
data("Train", package = "mlogit")
head(Train, 3)
```

```
##   id choiceid  choice price1 time1 change1 comfort1 price2 time2 change2
## 1  1        1 choice1   2400   150       0        1   4000   150       0
## 2  1        2 choice1   2400   150       0        1   3200   130       0
## 3  1        3 choice1   2400   115       0        1   4000   115       0
##   comfort2
## 1        1
## 2        1
## 3        0
```

```r
Tr <- mlogit.data(Train, shape = "wide", choice = "choice", varying = 4:11, sep = "", alt.levels = c(1, 2), id = "id")
head(Tr, 3)
```

```
##     id choiceid choice alt price time change comfort chid
## 1.1  1        1   TRUE   1  2400  150      0       1    1
## 1.2  1        1  FALSE   2  4000  150      0       1    1
## 2.1  1        2   TRUE   1  2400  150      0       1    2
```

```r
head(index(Tr), 3)
```

```
##     chid alt id
## 1.1    1   1  1
## 1.2    1   2  1
## 2.1    2   1  1
```

```r
#change price and time to more meaningful units (euros, hours respectively)
Tr$price <- Tr$price/100 * 2.20371
Tr$time <- Tr$time/60

#We then estimate the model: both alternatives being virtual train trips, it is relevant to use only generic coefficients and to remove the intercept:

ml.Train <- mlogit(choice ~ price + time + change + comfort | -1, Tr)
summary(ml.Train)
```

```
## 
## Call:
## mlogit(formula = choice ~ price + time + change + comfort | -1, 
##     data = Tr, method = "nr", print.level = 0)
## 
## Frequencies of alternatives:
##       1       2 
## 0.50324 0.49676 
## 
## nr method
## 5 iterations, 0h:0m:0s 
## g'(-H)^-1g = 0.00014 
## successive function values within tolerance limits 
## 
## Coefficients :
##           Estimate Std. Error  t-value  Pr(>|t|)    
## price   -0.0673580  0.0033933 -19.8506 < 2.2e-16 ***
## time    -1.7205514  0.1603517 -10.7299 < 2.2e-16 ***
## change  -0.3263409  0.0594892  -5.4857 4.118e-08 ***
## comfort -0.9457256  0.0649455 -14.5618 < 2.2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Log-Likelihood: -1724.2
```

```r
#coefficients are not directly interpretable, but dividing them by the price coefficient, we get monetary values :
coef(ml.Train)[-1]/coef(ml.Train)[1]
```

```
##     time   change  comfort 
## 25.54337  4.84487 14.04028
```

```r
#Mixed logit
Tr <- mlogit.data(Train, shape = "wide", varying = 4:11, choice = "choice", sep = "", opposite = c("price", "time", "change", "comfort"), alt.levels = c("choice1", "choice2"), id = "id")

Train.ml <- mlogit(choice ~ price + time + change + comfort, Tr)
summary(Train.ml)
```

```
## 
## Call:
## mlogit(formula = choice ~ price + time + change + comfort, data = Tr, 
##     method = "nr", print.level = 0)
## 
## Frequencies of alternatives:
## choice1 choice2 
## 0.50324 0.49676 
## 
## nr method
## 5 iterations, 0h:0m:0s 
## g'(-H)^-1g = 0.000141 
## successive function values within tolerance limits 
## 
## Coefficients :
##                        Estimate  Std. Error t-value  Pr(>|t|)    
## choice2:(intercept) -0.03249805  0.04108023 -0.7911    0.4289    
## price                0.00148495  0.00007479 19.8550 < 2.2e-16 ***
## time                 0.02873396  0.00267475 10.7427 < 2.2e-16 ***
## change               0.32581324  0.05950424  5.4755 4.364e-08 ***
## comfort              0.94704645  0.06498665 14.5729 < 2.2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Log-Likelihood: -1723.8
## McFadden R^2:  0.15089 
## Likelihood ratio test : chisq = 612.66 (p.value = < 2.22e-16)
```

```r
Train.mxlc <- mlogit(choice ~ price + time + change + comfort, Tr, panel = TRUE, rpar = c(time = "cn", change = "n", comfort = "ln"), correlation = TRUE, R = 100, halton = NA)

summary(Train.mxlc)
```

```
## 
## Call:
## mlogit(formula = choice ~ price + time + change + comfort, data = Tr, 
##     rpar = c(time = "cn", change = "n", comfort = "ln"), R = 100, 
##     correlation = TRUE, halton = NA, panel = TRUE)
## 
## Frequencies of alternatives:
## choice1 choice2 
## 0.50324 0.49676 
## 
## bfgs method
## 4 iterations, 0h:0m:33s 
## g'(-H)^-1g = 9.84E+07 
## last step couldn't find higher value 
## 
## Coefficients :
##                        Estimate  Std. Error  t-value  Pr(>|t|)    
## choice2:(intercept) -3.2238e-02  4.8125e-02  -0.6699  0.502930    
## price                1.9854e-03  2.2597e-05  87.8631 < 2.2e-16 ***
## time                 2.7154e-02  1.1732e-03  23.1439 < 2.2e-16 ***
## change               3.4888e-01  4.3681e-02   7.9871 1.332e-15 ***
## comfort             -1.2996e-01  1.7720e-02  -7.3342 2.229e-13 ***
## time.time            1.1229e-01  1.0391e-03 108.0728 < 2.2e-16 ***
## time.change         -2.0032e-01  6.1117e-02  -3.2776  0.001047 ** 
## time.comfort         1.5170e-01  2.1163e-03  71.6845 < 2.2e-16 ***
## change.change        1.6070e+00  5.8650e-02  27.4001 < 2.2e-16 ***
## change.comfort       2.0601e+00  7.5552e-03 272.6705 < 2.2e-16 ***
## comfort.comfort      1.4507e+00  6.2804e-03 230.9887 < 2.2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Log-Likelihood: -1556.2
## McFadden R^2:  0.23345 
## Likelihood ratio test : chisq = 947.88 (p.value = < 2.22e-16)
## 
## random coefficients
##         Min.    1st Qu.     Median        Mean   3rd Qu. Max.
## time       0  0.0000000 0.02715351  0.05967886 0.1028945  Inf
## change  -Inf -0.7434137 0.34888437  0.34888437 1.4411824  Inf
## comfort    0  0.1600127 0.87813015 21.23842756 4.8190710  Inf
```

```r
Train.mxlu <- update(Train.mxlc, correlation = FALSE)
```

Try with my data


```r
wfml <- read.csv("dswf_ml.csv")

wfml2 <- mlogit.data(wfml, shape = "long", choice = "choice",
                     varying = 16:28, sep = "",
                     alt.levels = c(1, 2, 3), 
                     alt.var = "ALT", id = "id")
```

```
## Warning in mlogit.data(wfml, shape = "long", choice = "choice", varying =
## 16:28, : variable ALT exists and will be replaced
```

```r
head(wfml2, 3)
```

```
##     id q1 q2 q3 q4 q5 q6 q7 q8 BLOCK choiceid ALT SELECT choice ASC
## 1.1  1  1  2  2  2  1  1  1  2     1        1   1      1   TRUE   1
## 1.2  1  1  2  2  2  1  1  1  2     1        1   2      1  FALSE   1
## 1.3  1  1  2  2  2  1  1  1  2     1        1   3      1  FALSE   0
##     big.loss small.loss small.gain big.gain state municipal private
## 1.1        0          0          1        0     1         0       0
## 1.2        0          1          0        0     0         0       1
## 1.3        0          0          0        0     0         0       0
##     cooperative mi1 mi4 mi8 mi10 bill STR
## 1.1           0   1   0   0    0    1 101
## 1.2           0   0   0   1    0    5 101
## 1.3           0   0   0   0    0    0 101
```

```r
ml.wfml <- mlogit(choice ~ small.loss + small.gain + big.gain + 
                    municipal + private + cooperative +  mi4 + mi8 +
                    mi10 + bill  | -1, wfml2)

summary(ml.wfml)
```

```
## 
## Call:
## mlogit(formula = choice ~ small.loss + small.gain + big.gain + 
##     municipal + private + cooperative + mi4 + mi8 + mi10 + bill | 
##     -1, data = wfml2, method = "nr", print.level = 0)
## 
## Frequencies of alternatives:
##       1       2       3 
## 0.40650 0.48812 0.10538 
## 
## nr method
## 5 iterations, 0h:0m:0s 
## g'(-H)^-1g = 1.49E-07 
## gradient close to zero 
## 
## Coefficients :
##              Estimate Std. Error  t-value  Pr(>|t|)    
## small.loss   1.328649   0.074989  17.7180 < 2.2e-16 ***
## small.gain   2.868386   0.092282  31.0829 < 2.2e-16 ***
## big.gain     3.739393   0.107598  34.7534 < 2.2e-16 ***
## municipal   -0.155583   0.076756  -2.0270  0.042665 *  
## private     -0.486025   0.079067  -6.1470 7.895e-10 ***
## cooperative -0.315341   0.100031  -3.1524  0.001619 ** 
## mi4          0.214478   0.077932   2.7521  0.005921 ** 
## mi8          0.343995   0.075872   4.5339 5.791e-06 ***
## mi10         0.839584   0.109167   7.6908 1.465e-14 ***
## bill        -0.072229   0.005429 -13.3045 < 2.2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Log-Likelihood: -2016.5
```

```r
#these outputs are the same as when I used support.CEs package
```

### Mixed Logit


```r
wf.mxlc <- mlogit(choice ~ small.loss + small.gain + big.gain + 
                    municipal + private + cooperative +  
                    mi4 + mi8 + mi10 + bill,
                  wfml2, panel = TRUE, rpar = c(small.loss = "n", small.gain = "n", big.gain = "n", municipal = "n", private = "n", cooperative = "n", mi4 = "n", mi8 = "n", mi10 = "n", bill = "n"), correlation = TRUE, R = 100, halton = NA)

summary(wf.mxlc)
```

```
## 
## Call:
## mlogit(formula = choice ~ small.loss + small.gain + big.gain + 
##     municipal + private + cooperative + mi4 + mi8 + mi10 + bill, 
##     data = wfml2, rpar = c(small.loss = "n", small.gain = "n", 
##         big.gain = "n", municipal = "n", private = "n", cooperative = "n", 
##         mi4 = "n", mi8 = "n", mi10 = "n", bill = "n"), R = 100, 
##     correlation = TRUE, halton = NA, panel = TRUE)
## 
## Frequencies of alternatives:
##       1       2       3 
## 0.40650 0.48812 0.10538 
## 
## bfgs method
## 72 iterations, 0h:4m:21s 
## g'(-H)^-1g = 6.72E-07 
## gradient close to zero 
## 
## Coefficients :
##                          Estimate Std. Error t-value  Pr(>|t|)    
## 2:(intercept)           -0.056609   0.155751 -0.3635 0.7162636    
## 3:(intercept)           -0.180799   0.256877 -0.7038 0.4815347    
## small.loss               3.274092   0.274394 11.9321 < 2.2e-16 ***
## small.gain               8.716214   0.616562 14.1368 < 2.2e-16 ***
## big.gain                11.741587   0.817741 14.3586 < 2.2e-16 ***
## municipal               -0.086129   0.235953 -0.3650 0.7150912    
## private                 -1.429678   0.293209 -4.8760 1.083e-06 ***
## cooperative              0.630470   0.329757  1.9119 0.0558864 .  
## mi4                      0.869276   0.233811  3.7179 0.0002009 ***
## mi8                      1.505133   0.246378  6.1091 1.002e-09 ***
## mi10                     2.168809   0.393743  5.5082 3.625e-08 ***
## bill                    -0.221700   0.023386 -9.4801 < 2.2e-16 ***
## small.loss.small.loss    2.649275   0.309234  8.5672 < 2.2e-16 ***
## small.loss.small.gain    4.655032   0.441377 10.5466 < 2.2e-16 ***
## small.loss.big.gain      6.605068   0.567345 11.6421 < 2.2e-16 ***
## small.loss.municipal     0.875056   0.279456  3.1313 0.0017404 ** 
## small.loss.private      -0.191772   0.278920 -0.6876 0.4917354    
## small.loss.cooperative   0.731397   0.346235  2.1124 0.0346497 *  
## small.loss.mi4          -0.066439   0.251232 -0.2645 0.7914297    
## small.loss.mi8          -0.133378   0.264188 -0.5049 0.6136565    
## small.loss.mi10          0.218145   0.397678  0.5485 0.5833171    
## small.loss.bill          0.011442   0.021072  0.5430 0.5871389    
## small.gain.small.gain    3.603300   0.345994 10.4144 < 2.2e-16 ***
## small.gain.big.gain      4.942490   0.464208 10.6471 < 2.2e-16 ***
## small.gain.municipal    -1.870878   0.273097 -6.8506 7.353e-12 ***
## small.gain.private      -1.128610   0.290237 -3.8886 0.0001008 ***
## small.gain.cooperative  -0.129295   0.270520 -0.4780 0.6326843    
## small.gain.mi4          -0.690966   0.240540 -2.8726 0.0040715 ** 
## small.gain.mi8          -0.707794   0.228748 -3.0942 0.0019734 ** 
## small.gain.mi10         -0.580960   0.311873 -1.8628 0.0624893 .  
## small.gain.bill          0.047852   0.017861  2.6791 0.0073812 ** 
## big.gain.big.gain        1.679519   0.295685  5.6801 1.346e-08 ***
## big.gain.municipal       0.086846   0.244966  0.3545 0.7229483    
## big.gain.private         0.246165   0.255242  0.9644 0.3348271    
## big.gain.cooperative     0.217940   0.293451  0.7427 0.4576768    
## big.gain.mi4             2.098766   0.285977  7.3389 2.154e-13 ***
## big.gain.mi8             2.328950   0.274422  8.4867 < 2.2e-16 ***
## big.gain.mi10            3.390404   0.424994  7.9775 1.554e-15 ***
## big.gain.bill            0.068354   0.019965  3.4238 0.0006176 ***
## municipal.municipal     -0.531964   0.222892 -2.3866 0.0170028 *  
## municipal.private        1.696182   0.293967  5.7700 7.929e-09 ***
## municipal.cooperative   -0.938272   0.293410 -3.1978 0.0013847 ** 
## municipal.mi4            0.208082   0.217494  0.9567 0.3387059    
## municipal.mi8           -0.135475   0.214269 -0.6323 0.5272132    
## municipal.mi10          -0.504363   0.352289 -1.4317 0.1522363    
## municipal.bill          -0.043072   0.018527 -2.3248 0.0200832 *  
## private.private          1.309536   0.260263  5.0316 4.865e-07 ***
## private.cooperative      0.014762   0.289043  0.0511 0.9592680    
## private.mi4              0.127550   0.256300  0.4977 0.6187230    
## private.mi8             -0.188975   0.224636 -0.8412 0.4002093    
## private.mi10             0.408062   0.361547  1.1287 0.2590431    
## private.bill             0.068652   0.021144  3.2469 0.0011667 ** 
## cooperative.cooperative -0.405760   0.241816 -1.6780 0.0933530 .  
## cooperative.mi4          0.012814   0.202003  0.0634 0.9494198    
## cooperative.mi8         -0.318019   0.234525 -1.3560 0.1750935    
## cooperative.mi10        -0.896337   0.369115 -2.4283 0.0151681 *  
## cooperative.bill         0.075902   0.019640  3.8647 0.0001112 ***
## mi4.mi4                  0.954320   0.239953  3.9771 6.976e-05 ***
## mi4.mi8                  1.386271   0.251184  5.5189 3.411e-08 ***
## mi4.mi10                 2.652123   0.393271  6.7438 1.543e-11 ***
## mi4.bill                 0.037600   0.020668  1.8192 0.0688750 .  
## mi8.mi8                 -0.637837   0.199976 -3.1896 0.0014248 ** 
## mi8.mi10                -1.540382   0.349203 -4.4111 1.028e-05 ***
## mi8.bill                 0.190568   0.023813  8.0027 1.110e-15 ***
## mi10.mi10               -0.106348   0.396069 -0.2685 0.7883086    
## mi10.bill                0.094347   0.023932  3.9423 8.072e-05 ***
## bill.bill               -0.016334   0.019137 -0.8535 0.3933689    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Log-Likelihood: -1502
## McFadden R^2:  0.50722 
## Likelihood ratio test : chisq = 3092.1 (p.value = < 2.22e-16)
## 
## random coefficients
##             Min.    1st Qu.      Median        Mean     3rd Qu. Max.
## small.loss  -Inf  1.4871835  3.27409234  3.27409234  5.06100120  Inf
## small.gain  -Inf  4.7457033  8.71621405  8.71621405 12.68672475  Inf
## big.gain    -Inf  6.0632034 11.74158688 11.74158688 17.41997033  Inf
## municipal   -Inf -1.5258822 -0.08612911 -0.08612911  1.35362398  Inf
## private     -Inf -3.0767397 -1.42967841 -1.42967841  0.21738292  Inf
## cooperative -Inf -0.2344486  0.63046984  0.63046984  1.49538827  Inf
## mi4         -Inf -0.7630907  0.86927562  0.86927562  2.50164194  Inf
## mi8         -Inf -0.4528115  1.50513277  1.50513277  3.46307700  Inf
## mi10        -Inf -1.0321476  2.16880947  2.16880947  5.36976650  Inf
## bill        -Inf -0.3953996 -0.22170004 -0.22170004 -0.04800045  Inf
```

