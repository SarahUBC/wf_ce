# Tidier Multinomial Logit
Sarah Klain  
February 11, 2016  

---
title: "CE_mlogit"
author: "Sarah Klain"
date: "November 20, 2015"
output: 
  html_document: 
    keep_md: yes
---


```r
setwd("/Users/sarahklain/Documents/R_2015/wf_ce/CE/demog")

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

### Conditional Logit

Format data with demographic information using Tidyr.
To make a unique row for each observation including demographic data
I added column obs1-obs24 in excel.

These are columns 69-92 in cer_2016_01_08_dem2.csv
#```{r}
ce_d <- read.csv("cer_2016_01_08_dem2.csv")
#str(ce_d)
#head(ce_d)
dem_long <- tidyr::gather(ce_d, "obs", "obs1_24", 69:92)
#str(dem_long)
#summary(dem_long$ID)
#View(dem_long)
# 9624 observations

#I deleted NAs in excel

write.csv(dem_long, "dem_long.csv")
```
I copied and pasted the demographic data from dem_long.csv into dswf_ml_dem2.csv

### Multinomial logit model

```r
wfml_d <- read.csv("dswf_ml_dem2.csv")

wfml_d2 <- mlogit.data(wfml_d, shape = "long", choice = "choice",
                     varying = 16:28, sep = "",
                     alt.levels = c(1, 2, 3), 
                     alt.var = "ALT", id = "id")
```

```
## Warning in mlogit.data(wfml_d, shape = "long", choice = "choice", varying =
## 16:28, : variable ALT exists and will be replaced
```

```r
head(wfml_d2, 3)
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
##     cooperative mi1 mi4 mi8 mi10 bill STR white female age univ_degr
## 1.1           0   1   0   0    0    1 101     1      1  28         1
## 1.2           0   0   0   1    0    5 101     1      1  28         1
## 1.3           0   0   0   0    0    0 101     1      1  28         1
##     income wages self.emp pol_dem pol_ind pol_rep coast_rec mean_nep
## 1.1      6     0        0       0       0       0         0      4.6
## 1.2      6     0        0       0       0       0         0      4.6
## 1.3      6     0        0       0       0       0         0      4.6
##     mean_rel mean_met mean_inst mean_mor att_w_US oper const_st wf_rec sup
## 1.1     4.17     1.75         3        3        1    1        1      3   0
## 1.2     4.17     1.75         3        3        1    1        1      3   0
## 1.3     4.17     1.75         3        3        1    1        1      3   0
##     opp dis skep int af con cur ap enth app abuse_nep bal_r_nep
## 1.1   0   0    0   0  0   0   0  0    0   0         5         4
## 1.2   0   0    0   0  0   0   0  0    0   0         5         4
## 1.3   0   0    0   0  0   0   0  0    0   0         5         4
##     crisis_r_nep spaceship_nep bau_nep extract_r_ins loss_r_ins
## 1.1            5             5       4             2          4
## 1.2            5             5       4             2          4
## 1.3            5             5       4             2          4
##     decade_r_mor comm_rel wild_rel clean_inst tech tech_r iden_rel kin_rel
## 1.1            4        4        4          5    4      2        5       5
## 1.2            4        4        4          5    4      2        5       5
## 1.3            4        4        4          5    4      2        5       5
##     right_r_mor health_rel other_rel kin_met resp_met iden_met other_met
## 1.1           4          2         5       2        2        2         1
## 1.2           4          2         5       2        2        2         1
## 1.3           4          2         5       2        2        2         1
##          lat     long zip_code        ResponseID  obs obs1_24
## 1.1 42.18179 -71.1962     3820 R_0CJgoxE736KONEh obs1       1
## 1.2 42.18179 -71.1962     3820 R_0CJgoxE736KONEh obs2       2
## 1.3 42.18179 -71.1962     3820 R_0CJgoxE736KONEh obs3       3
```

```r
ml.wfml <- mlogit(choice ~ small.loss + small.gain + big.gain + 
                    municipal + private + cooperative +  mi4 + mi8 +
                    mi10 + bill  | -1, wfml_d2)

summary(ml.wfml)
```

```
## 
## Call:
## mlogit(formula = choice ~ small.loss + small.gain + big.gain + 
##     municipal + private + cooperative + mi4 + mi8 + mi10 + bill | 
##     -1, data = wfml_d2, method = "nr", print.level = 0)
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

#divide by bill coefficient to obtain WTP associated with each attribute and level
coef(ml.wfml)[-1]/coef(ml.wfml)[10]
```

```
##  small.gain    big.gain   municipal     private cooperative         mi4 
##  -39.712196  -51.771101    2.154016    6.728911    4.365826   -2.969403 
##         mi8        mi10        bill 
##   -4.762542  -11.623856    1.000000
```

Explore with all demographic variables

Building models with demographic variables added, one by one. 


```r
ml.wfml.dem2 <- mlogit(choice ~ small.loss + small.gain +
                        big.gain + 
                        municipal + private +                  
                        cooperative +  
                        mi4 + mi8 + mi10 + bill + 
                        age:ASC + female:ASC +
                        white:ASC + univ_degr:ASC + 
                        income:ASC + wages:ASC + 
                        self.emp:ASC + pol_dem:ASC + 
                        pol_ind:ASC + pol_rep:ASC + 
                        coast_rec:ASC
                      | 1, wfml_d2)

summary(ml.wfml.dem2)
```

```
## 
## Call:
## mlogit(formula = choice ~ small.loss + small.gain + big.gain + 
##     municipal + private + cooperative + mi4 + mi8 + mi10 + bill + 
##     age:ASC + female:ASC + white:ASC + univ_degr:ASC + income:ASC + 
##     wages:ASC + self.emp:ASC + pol_dem:ASC + pol_ind:ASC + pol_rep:ASC + 
##     coast_rec:ASC | 1, data = wfml_d2, method = "nr", print.level = 0)
## 
## Frequencies of alternatives:
##       1       2       3 
## 0.40650 0.48812 0.10538 
## 
## nr method
## 5 iterations, 0h:0m:0s 
## g'(-H)^-1g = 4.52E-07 
## gradient close to zero 
## 
## Coefficients :
##                 Estimate Std. Error  t-value  Pr(>|t|)    
## 2:(intercept) -0.0459744  0.0628305  -0.7317  0.464339    
## 3:(intercept)  0.1589707  0.3719058   0.4274  0.669052    
## small.loss     1.4956821  0.0961231  15.5601 < 2.2e-16 ***
## small.gain     3.0555266  0.1138552  26.8370 < 2.2e-16 ***
## big.gain       3.9148382  0.1255505  31.1814 < 2.2e-16 ***
## municipal     -0.0453323  0.0902503  -0.5023  0.615459    
## private       -0.4145051  0.0850782  -4.8720 1.104e-06 ***
## cooperative   -0.2512120  0.1045780  -2.4022  0.016299 *  
## mi4            0.3357082  0.0905389   3.7079  0.000209 ***
## mi8            0.4663470  0.0881779   5.2887 1.232e-07 ***
## mi10           0.9705291  0.1198765   8.0961 6.661e-16 ***
## bill          -0.0706400  0.0056360 -12.5338 < 2.2e-16 ***
## age:ASC       -0.0011757  0.0055857  -0.2105  0.833289    
## ASC:female     0.1443150  0.1243814   1.1603  0.245942    
## ASC:white      0.3327572  0.1501271   2.2165  0.026657 *  
## ASC:univ_degr -0.1753969  0.1340234  -1.3087  0.190635    
## ASC:income    -0.0233500  0.0242073  -0.9646  0.334752    
## ASC:wages     -0.0912478  0.1349245  -0.6763  0.498858    
## ASC:self.emp   0.1180486  0.2298464   0.5136  0.607533    
## ASC:pol_dem   -0.2958184  0.2347159  -1.2603  0.207552    
## ASC:pol_ind   -0.2056018  0.2364543  -0.8695  0.384563    
## ASC:pol_rep   -0.2555626  0.2947730  -0.8670  0.385952    
## ASC:coast_rec -0.0350914  0.1267363  -0.2769  0.781868    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Log-Likelihood: -2006
## McFadden R^2:  0.34188 
## Likelihood ratio test : chisq = 2084.1 (p.value = < 2.22e-16)
```

```r
stargazer(ml.wfml.dem2, type = "html")
```

```
## 
## <table style="text-align:center"><tr><td colspan="2" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td><em>Dependent variable:</em></td></tr>
## <tr><td></td><td colspan="1" style="border-bottom: 1px solid black"></td></tr>
## <tr><td style="text-align:left"></td><td>choice</td></tr>
## <tr><td colspan="2" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">2:(intercept)</td><td>-0.046</td></tr>
## <tr><td style="text-align:left"></td><td>(0.063)</td></tr>
## <tr><td style="text-align:left"></td><td></td></tr>
## <tr><td style="text-align:left">3:(intercept)</td><td>0.159</td></tr>
## <tr><td style="text-align:left"></td><td>(0.372)</td></tr>
## <tr><td style="text-align:left"></td><td></td></tr>
## <tr><td style="text-align:left">small.loss</td><td>1.496<sup>***</sup></td></tr>
## <tr><td style="text-align:left"></td><td>(0.096)</td></tr>
## <tr><td style="text-align:left"></td><td></td></tr>
## <tr><td style="text-align:left">small.gain</td><td>3.056<sup>***</sup></td></tr>
## <tr><td style="text-align:left"></td><td>(0.114)</td></tr>
## <tr><td style="text-align:left"></td><td></td></tr>
## <tr><td style="text-align:left">big.gain</td><td>3.915<sup>***</sup></td></tr>
## <tr><td style="text-align:left"></td><td>(0.126)</td></tr>
## <tr><td style="text-align:left"></td><td></td></tr>
## <tr><td style="text-align:left">municipal</td><td>-0.045</td></tr>
## <tr><td style="text-align:left"></td><td>(0.090)</td></tr>
## <tr><td style="text-align:left"></td><td></td></tr>
## <tr><td style="text-align:left">private</td><td>-0.415<sup>***</sup></td></tr>
## <tr><td style="text-align:left"></td><td>(0.085)</td></tr>
## <tr><td style="text-align:left"></td><td></td></tr>
## <tr><td style="text-align:left">cooperative</td><td>-0.251<sup>**</sup></td></tr>
## <tr><td style="text-align:left"></td><td>(0.105)</td></tr>
## <tr><td style="text-align:left"></td><td></td></tr>
## <tr><td style="text-align:left">mi4</td><td>0.336<sup>***</sup></td></tr>
## <tr><td style="text-align:left"></td><td>(0.091)</td></tr>
## <tr><td style="text-align:left"></td><td></td></tr>
## <tr><td style="text-align:left">mi8</td><td>0.466<sup>***</sup></td></tr>
## <tr><td style="text-align:left"></td><td>(0.088)</td></tr>
## <tr><td style="text-align:left"></td><td></td></tr>
## <tr><td style="text-align:left">mi10</td><td>0.971<sup>***</sup></td></tr>
## <tr><td style="text-align:left"></td><td>(0.120)</td></tr>
## <tr><td style="text-align:left"></td><td></td></tr>
## <tr><td style="text-align:left">bill</td><td>-0.071<sup>***</sup></td></tr>
## <tr><td style="text-align:left"></td><td>(0.006)</td></tr>
## <tr><td style="text-align:left"></td><td></td></tr>
## <tr><td style="text-align:left">age:ASC</td><td>-0.001</td></tr>
## <tr><td style="text-align:left"></td><td>(0.006)</td></tr>
## <tr><td style="text-align:left"></td><td></td></tr>
## <tr><td style="text-align:left">ASC:female</td><td>0.144</td></tr>
## <tr><td style="text-align:left"></td><td>(0.124)</td></tr>
## <tr><td style="text-align:left"></td><td></td></tr>
## <tr><td style="text-align:left">ASC:white</td><td>0.333<sup>**</sup></td></tr>
## <tr><td style="text-align:left"></td><td>(0.150)</td></tr>
## <tr><td style="text-align:left"></td><td></td></tr>
## <tr><td style="text-align:left">ASC:univ_degr</td><td>-0.175</td></tr>
## <tr><td style="text-align:left"></td><td>(0.134)</td></tr>
## <tr><td style="text-align:left"></td><td></td></tr>
## <tr><td style="text-align:left">ASC:income</td><td>-0.023</td></tr>
## <tr><td style="text-align:left"></td><td>(0.024)</td></tr>
## <tr><td style="text-align:left"></td><td></td></tr>
## <tr><td style="text-align:left">ASC:wages</td><td>-0.091</td></tr>
## <tr><td style="text-align:left"></td><td>(0.135)</td></tr>
## <tr><td style="text-align:left"></td><td></td></tr>
## <tr><td style="text-align:left">ASC:self.emp</td><td>0.118</td></tr>
## <tr><td style="text-align:left"></td><td>(0.230)</td></tr>
## <tr><td style="text-align:left"></td><td></td></tr>
## <tr><td style="text-align:left">ASC:pol_dem</td><td>-0.296</td></tr>
## <tr><td style="text-align:left"></td><td>(0.235)</td></tr>
## <tr><td style="text-align:left"></td><td></td></tr>
## <tr><td style="text-align:left">ASC:pol_ind</td><td>-0.206</td></tr>
## <tr><td style="text-align:left"></td><td>(0.236)</td></tr>
## <tr><td style="text-align:left"></td><td></td></tr>
## <tr><td style="text-align:left">ASC:pol_rep</td><td>-0.256</td></tr>
## <tr><td style="text-align:left"></td><td>(0.295)</td></tr>
## <tr><td style="text-align:left"></td><td></td></tr>
## <tr><td style="text-align:left">ASC:coast_rec</td><td>-0.035</td></tr>
## <tr><td style="text-align:left"></td><td>(0.127)</td></tr>
## <tr><td style="text-align:left"></td><td></td></tr>
## <tr><td colspan="2" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Observations</td><td>3,198</td></tr>
## <tr><td style="text-align:left">R<sup>2</sup></td><td>0.342</td></tr>
## <tr><td style="text-align:left">Log Likelihood</td><td>-2,006.004</td></tr>
## <tr><td style="text-align:left">LR Test</td><td>2,084.141<sup>***</sup> (df = 23)</td></tr>
## <tr><td colspan="2" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td style="text-align:right"><sup>*</sup>p<0.1; <sup>**</sup>p<0.05; <sup>***</sup>p<0.01</td></tr>
## </table>
```


#```{r}
ml.wfml.dem3 <- mlogit(choice ~ small.loss + small.gain +
                        big.gain + 
                        municipal + private +                  
                        cooperative +  
                        mi4 + mi8 + mi10 + bill + 
                        age:ASC + female:ASC +
                        white:ASC + univ_degr:ASC + 
                        income:ASC + wages:ASC + 
                        self.emp:ASC + pol_dem:ASC + 
                        pol_ind:ASC + pol_rep:ASC + 
                       coast_rec:ASC + mean_nep:ASC + 
                      mean_inst:ASC + mean_mor:ASC + 
                      att_w_US:ASC + oper:ASC + const_st:ASC +
                     wf_rec:ASC + sup:ASC + opp:ASC + dis:ASC +
                      skep:ASC + int:ASC + af:ASC + con:ASC + 
                        cur:ASC + ap:ASC + enth:ASC + app:ASC + 
                        abuse_nep:ASC + bal_r_nep:ASC + 
                        crisis_r_nep:ASC + spaceship_nep:ASC + 
                        bau_nep:ASC + extract_r_ins:ASC + 
                        loss_r_ins:ASC + decade_r_mor:ASC + 
                       comm_rel:ASC + wild_rel:ASC +
                        clean_inst:ASC + tech:ASC + tech_r:ASC + 
                        iden_rel:ASC + kin_rel:ASC + 
                        right_r_mor:ASC + health_rel:ASC + 
                        other_rel:ASC + kin_met:ASC + 
                        resp_met:ASC + iden_met:ASC + 
                        other_met:ASC
                      | 1, wfml_d2)

summary(ml.wfml.dem3)
```


```r
ml.wfml.dem4 <- mlogit(choice ~ small.loss + small.gain +
                        big.gain + 
                        municipal + private +                  
                        cooperative +  
                        mi4 + mi8 + mi10 + bill + 
                        age:ASC + female:ASC +
                        white:ASC + univ_degr:ASC + 
                        income:ASC +
                        self.emp:ASC + pol_dem:ASC + 
                        pol_ind:ASC + pol_rep:ASC 
                       + coast_rec:ASC + mean_nep:ASC + 
                        oper:ASC + const_st:ASC +
                        wf_rec:ASC+ app:ASC + 
                        abuse_nep:ASC + bal_r_nep:ASC + 
                        crisis_r_nep:ASC + spaceship_nep:ASC +
                      # bau_nep:ASC +
                     # extract_r_ins:ASC +
                      # loss_r_ins:ASC + decade_r_mor:ASC 
                      # comm_rel:ASC + wild_rel:ASC +
                    # clean_inst:ASC + tech:ASC + tech_r:ASC + 
                      iden_rel:ASC + kin_rel:ASC + 
                        right_r_mor:ASC + health_rel:ASC + 
                        other_rel:ASC + kin_met:ASC + 
                        resp_met:ASC + iden_met:ASC + 
                        other_met:ASC
                      | 1, wfml_d2)

summary(ml.wfml.dem4)
```

```
## 
## Call:
## mlogit(formula = choice ~ small.loss + small.gain + big.gain + 
##     municipal + private + cooperative + mi4 + mi8 + mi10 + bill + 
##     age:ASC + female:ASC + white:ASC + univ_degr:ASC + income:ASC + 
##     self.emp:ASC + pol_dem:ASC + pol_ind:ASC + pol_rep:ASC + 
##     coast_rec:ASC + mean_nep:ASC + oper:ASC + const_st:ASC + 
##     wf_rec:ASC + app:ASC + abuse_nep:ASC + bal_r_nep:ASC + crisis_r_nep:ASC + 
##     spaceship_nep:ASC + iden_rel:ASC + kin_rel:ASC + right_r_mor:ASC + 
##     health_rel:ASC + other_rel:ASC + kin_met:ASC + resp_met:ASC + 
##     iden_met:ASC + other_met:ASC | 1, data = wfml_d2, method = "nr", 
##     print.level = 0)
## 
## Frequencies of alternatives:
##       1       2       3 
## 0.40694 0.48762 0.10543 
## 
## nr method
## 7 iterations, 0h:0m:1s 
## g'(-H)^-1g = 2.5E-06 
## successive function values within tolerance limits 
## 
## Coefficients :
##                     Estimate Std. Error  t-value  Pr(>|t|)    
## 2:(intercept)     -0.0621216  0.0654025  -0.9498 0.3421961    
## 3:(intercept)     -2.0276822  1.1930758  -1.6995 0.0892171 .  
## small.loss         1.5218869  0.0985769  15.4386 < 2.2e-16 ***
## small.gain         3.2177042  0.1216375  26.4532 < 2.2e-16 ***
## big.gain           4.1018000  0.1331815  30.7986 < 2.2e-16 ***
## municipal         -0.0062655  0.0937322  -0.0668 0.9467054    
## private           -0.4095634  0.0890507  -4.5992 4.241e-06 ***
## cooperative       -0.2430732  0.1092463  -2.2250 0.0260811 *  
## mi4                0.3294559  0.0945640   3.4839 0.0004941 ***
## mi8                0.4954395  0.0931723   5.3175 1.052e-07 ***
## mi10               1.0476605  0.1269647   8.2516 2.220e-16 ***
## bill              -0.0762798  0.0060135 -12.6848 < 2.2e-16 ***
## age:ASC            0.0162093  0.0069222   2.3416 0.0191999 *  
## ASC:female         0.1845046  0.1560634   1.1822 0.2371099    
## ASC:white         -0.1237224  0.1838774  -0.6729 0.5010412    
## ASC:univ_degr     -0.0477986  0.1556082  -0.3072 0.7587119    
## ASC:income        -0.0870943  0.0306349  -2.8430 0.0044694 ** 
## ASC:self.emp       0.3034334  0.2401108   1.2637 0.2063296    
## ASC:pol_dem       -1.2969770  0.3243834  -3.9983 6.380e-05 ***
## ASC:pol_ind       -1.3789784  0.3258343  -4.2321 2.315e-05 ***
## ASC:pol_rep       -1.7488200  0.3821411  -4.5764 4.731e-06 ***
## ASC:coast_rec      0.1468950  0.1496546   0.9816 0.3263167    
## ASC:mean_nep       0.7899212  0.5275632   1.4973 0.1343148    
## ASC:oper          -1.3607772  0.1206843 -11.2755 < 2.2e-16 ***
## ASC:const_st       0.0723310  0.0822112   0.8798 0.3789572    
## ASC:wf_rec         0.6928594  0.1252433   5.5321 3.164e-08 ***
## ASC:app            1.6608210  0.3908501   4.2493 2.145e-05 ***
## ASC:abuse_nep      0.2060246  0.1814221   1.1356 0.2561204    
## ASC:bal_r_nep      0.0722398  0.1532293   0.4714 0.6373202    
## ASC:crisis_r_nep  -0.5966652  0.1727655  -3.4536 0.0005531 ***
## ASC:spaceship_nep -0.3445569  0.1521697  -2.2643 0.0235561 *  
## ASC:iden_rel      -0.0614175  0.1105752  -0.5554 0.5785960    
## ASC:kin_rel       -0.1360672  0.1156136  -1.1769 0.2392303    
## ASC:right_r_mor    0.2342321  0.0993706   2.3572 0.0184154 *  
## ASC:heh_rel       -0.1574577  0.0762563  -2.0648 0.0389374 *  
## ASC:other_rel     -0.1825931  0.1303460  -1.4008 0.1612637    
## ASC:kin_met        0.1852635  0.1038026   1.7848 0.0742991 .  
## ASC:resp_met      -0.0232925  0.1142734  -0.2038 0.8384853    
## ASC:iden_met      -0.1992929  0.0928647  -2.1461 0.0318685 *  
## ASC:other_met     -0.2941945  0.0916055  -3.2115 0.0013203 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Log-Likelihood: -1739.3
## McFadden R^2:  0.41353 
## Likelihood ratio test : chisq = 2452.8 (p.value = < 2.22e-16)
```



```r
ml.wfml.dem4 <- mlogit(choice ~ small.loss + small.gain +
                        big.gain + 
                        municipal + private +                  
                        cooperative +  
                        mi4 + mi8 + mi10 + bill + 
                        age:ASC + female:ASC +
                        white:ASC + univ_degr:ASC + 
                        income:ASC +
                        self.emp:ASC + pol_dem:ASC + 
                        pol_ind:ASC + pol_rep:ASC 
                       + coast_rec:ASC + mean_nep:ASC + 
                        oper:ASC + const_st:ASC +
                        wf_rec:ASC+ app:ASC
                      | 1, wfml_d2)

summary(ml.wfml.dem4)
```

```
## 
## Call:
## mlogit(formula = choice ~ small.loss + small.gain + big.gain + 
##     municipal + private + cooperative + mi4 + mi8 + mi10 + bill + 
##     age:ASC + female:ASC + white:ASC + univ_degr:ASC + income:ASC + 
##     self.emp:ASC + pol_dem:ASC + pol_ind:ASC + pol_rep:ASC + 
##     coast_rec:ASC + mean_nep:ASC + oper:ASC + const_st:ASC + 
##     wf_rec:ASC + app:ASC | 1, data = wfml_d2, method = "nr", 
##     print.level = 0)
## 
## Frequencies of alternatives:
##       1       2       3 
## 0.40603 0.48806 0.10591 
## 
## nr method
## 6 iterations, 0h:0m:0s 
## g'(-H)^-1g = 6.09E-07 
## gradient close to zero 
## 
## Coefficients :
##                 Estimate Std. Error  t-value  Pr(>|t|)    
## 2:(intercept) -0.0563547  0.0643440  -0.8758 0.3811204    
## 3:(intercept)  0.9021825  0.7241899   1.2458 0.2128446    
## small.loss     1.5212028  0.0974325  15.6129 < 2.2e-16 ***
## small.gain     3.2061405  0.1194822  26.8336 < 2.2e-16 ***
## big.gain       4.0792725  0.1307579  31.1971 < 2.2e-16 ***
## municipal      0.0017008  0.0921483   0.0185 0.9852738    
## private       -0.3964823  0.0874947  -4.5315 5.857e-06 ***
## cooperative   -0.2325200  0.1074527  -2.1639 0.0304698 *  
## mi4            0.3510123  0.0927845   3.7831 0.0001549 ***
## mi8            0.4999330  0.0913579   5.4722 4.444e-08 ***
## mi10           1.0299467  0.1242620   8.2885 2.220e-16 ***
## bill          -0.0753139  0.0058975 -12.7705 < 2.2e-16 ***
## age:ASC        0.0137858  0.0063753   2.1624 0.0305898 *  
## ASC:female     0.3927959  0.1402009   2.8017 0.0050840 ** 
## ASC:white     -0.0044180  0.1702526  -0.0259 0.9792975    
## ASC:univ_degr -0.1556560  0.1482812  -1.0497 0.2938397    
## ASC:income    -0.0748408  0.0281699  -2.6568 0.0078895 ** 
## ASC:self.emp   0.2872098  0.2265964   1.2675 0.2049784    
## ASC:pol_dem   -1.2299120  0.2991873  -4.1108 3.942e-05 ***
## ASC:pol_ind   -1.3370256  0.3004761  -4.4497 8.599e-06 ***
## ASC:pol_rep   -1.4858766  0.3576427  -4.1546 3.258e-05 ***
## ASC:coast_rec  0.1288621  0.1429504   0.9014 0.3673513    
## ASC:mean_nep   0.3487572  0.0975797   3.5741 0.0003515 ***
## ASC:oper      -1.2763627  0.1126089 -11.3345 < 2.2e-16 ***
## ASC:const_st   0.1156129  0.0758161   1.5249 0.1272809    
## ASC:wf_rec     0.6692482  0.1133155   5.9061 3.504e-09 ***
## ASC:app        1.4941645  0.3690999   4.0481 5.163e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Log-Likelihood: -1815.1
## McFadden R^2:  0.40196 
## Likelihood ratio test : chisq = 2440 (p.value = < 2.22e-16)
```

### Mixed Logit


```r
wf.mxlc <- mlogit(choice ~ small.loss + small.gain + big.gain + 
                    municipal + private + cooperative +  
                    mi4 + mi8 + mi10 + bill,
                  wfml_d2, panel = TRUE, rpar = c(small.loss = "n", small.gain = "n", big.gain = "n", municipal = "n", private = "n", cooperative = "n", mi4 = "n", mi8 = "n", mi10 = "n", bill = "n"), correlation = TRUE, R = 100, halton = NA)

summary(wf.mxlc)
```

```
## 
## Call:
## mlogit(formula = choice ~ small.loss + small.gain + big.gain + 
##     municipal + private + cooperative + mi4 + mi8 + mi10 + bill, 
##     data = wfml_d2, rpar = c(small.loss = "n", small.gain = "n", 
##         big.gain = "n", municipal = "n", private = "n", cooperative = "n", 
##         mi4 = "n", mi8 = "n", mi10 = "n", bill = "n"), R = 100, 
##     correlation = TRUE, halton = NA, panel = TRUE)
## 
## Frequencies of alternatives:
##       1       2       3 
## 0.40650 0.48812 0.10538 
## 
## bfgs method
## 72 iterations, 0h:4m:10s 
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
Ai

```r
wf.mxlc2 <- mlogit(choice ~ small.loss + small.gain + big.gain + 
                    municipal + private + cooperative +  
                    mi4 + mi8 + mi10 + bill,
                  wfml_d2, panel = TRUE, rpar = c(small.loss = "n", small.gain = "n", big.gain = "n", municipal = "n", private = "n", cooperative = "n", mi4 = "n", mi8 = "n", mi10 = "n", bill = "n"), R = 100)

summary(wf.mxlc2)
```

```
## 
## Call:
## mlogit(formula = choice ~ small.loss + small.gain + big.gain + 
##     municipal + private + cooperative + mi4 + mi8 + mi10 + bill, 
##     data = wfml_d2, rpar = c(small.loss = "n", small.gain = "n", 
##         big.gain = "n", municipal = "n", private = "n", cooperative = "n", 
##         mi4 = "n", mi8 = "n", mi10 = "n", bill = "n"), R = 100, 
##     panel = TRUE)
## 
## Frequencies of alternatives:
##       1       2       3 
## 0.40650 0.48812 0.10538 
## 
## bfgs method
## 25 iterations, 0h:0m:59s 
## g'(-H)^-1g = 3.37E-07 
## gradient close to zero 
## 
## Coefficients :
##                 Estimate Std. Error  t-value  Pr(>|t|)    
## 2:(intercept)   0.109596   0.094836   1.1556  0.247829    
## 3:(intercept)   0.178194   0.169074   1.0539  0.291911    
## small.loss      1.733415   0.135241  12.8172 < 2.2e-16 ***
## small.gain      4.367536   0.217243  20.1044 < 2.2e-16 ***
## big.gain        6.215177   0.322239  19.2875 < 2.2e-16 ***
## municipal      -0.089181   0.129145  -0.6905  0.489849    
## private        -0.652861   0.134088  -4.8689 1.122e-06 ***
## cooperative    -0.140984   0.160773  -0.8769  0.380536    
## mi4             0.260455   0.129323   2.0140  0.044010 *  
## mi8             0.641042   0.133278   4.8098 1.511e-06 ***
## mi10            1.174758   0.171437   6.8524 7.260e-12 ***
## bill           -0.107669   0.010506 -10.2487 < 2.2e-16 ***
## sd.small.loss   0.826917   0.172569   4.7918 1.653e-06 ***
## sd.small.gain   1.452117   0.189601   7.6588 1.887e-14 ***
## sd.big.gain     3.136396   0.314341   9.9777 < 2.2e-16 ***
## sd.municipal   -0.551406   0.196349  -2.8083  0.004980 ** 
## sd.private      0.895672   0.178724   5.0115 5.401e-07 ***
## sd.cooperative -0.431749   0.320913  -1.3454  0.178503    
## sd.mi4          0.446993   0.196793   2.2714  0.023124 *  
## sd.mi8         -0.682570   0.166193  -4.1071 4.007e-05 ***
## sd.mi10         0.855132   0.283989   3.0111  0.002603 ** 
## sd.bill         0.202657   0.014322  14.1506 < 2.2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Log-Likelihood: -1821.8
## McFadden R^2:  0.4023 
## Likelihood ratio test : chisq = 2452.5 (p.value = < 2.22e-16)
## 
## random coefficients
##             Min.    1st Qu.      Median        Mean     3rd Qu. Max.
## small.loss  -Inf  1.1756676  1.73341493  1.73341493  2.29116224  Inf
## small.gain  -Inf  3.3880981  4.36753595  4.36753595  5.34697374  Inf
## big.gain    -Inf  4.0997096  6.21517656  6.21517656  8.33064355  Inf
## municipal   -Inf -0.4610981 -0.08918059 -0.08918059  0.28273691  Inf
## private     -Inf -1.2569832 -0.65286138 -0.65286138 -0.04873960  Inf
## cooperative -Inf -0.4321940 -0.14098364 -0.14098364  0.15022674  Inf
## mi4         -Inf -0.0410372  0.26045509  0.26045509  0.56194737  Inf
## mi8         -Inf  0.1806561  0.64104223  0.64104223  1.10142840  Inf
## mi10        -Inf  0.5979801  1.17475771  1.17475771  1.75153529  Inf
## bill        -Inf -0.2443595 -0.10766907 -0.10766907  0.02902133  Inf
```
What does panel do?

#```{r}
#takes 10 min to run with no panel
wf.mxlc.np <- mlogit(choice ~ small.loss + small.gain + big.gain + 
                    municipal + private + cooperative +  
                    mi4 + mi8 + mi10 + bill,
                  wfml2, panel = FALSE, rpar = c(small.loss = "n", small.gain = "n", big.gain = "n", municipal = "n", private = "n", cooperative = "n", mi4 = "n", mi8 = "n", mi10 = "n", bill = "n"), correlation = TRUE, R = 100, halton = NA)
```

summary(wf.mxlc.np)

# not correlated

```r
wf.mxlu <- update(wf.mxlc, correlation = FALSE)
summary(wf.mxlu)
```

```
## 
## Call:
## mlogit(formula = choice ~ small.loss + small.gain + big.gain + 
##     municipal + private + cooperative + mi4 + mi8 + mi10 + bill, 
##     data = wfml_d2, rpar = c(small.loss = "n", small.gain = "n", 
##         big.gain = "n", municipal = "n", private = "n", cooperative = "n", 
##         mi4 = "n", mi8 = "n", mi10 = "n", bill = "n"), R = 100, 
##     correlation = FALSE, halton = NA, panel = TRUE)
## 
## Frequencies of alternatives:
##       1       2       3 
## 0.40650 0.48812 0.10538 
## 
## bfgs method
## 29 iterations, 0h:1m:14s 
## g'(-H)^-1g = 6.67E-07 
## gradient close to zero 
## 
## Coefficients :
##                 Estimate Std. Error  t-value  Pr(>|t|)    
## 2:(intercept)   0.025005   0.100779   0.2481   0.80405    
## 3:(intercept)   0.063445   0.173056   0.3666   0.71391    
## small.loss      1.849293   0.138490  13.3533 < 2.2e-16 ***
## small.gain      4.921515   0.266788  18.4473 < 2.2e-16 ***
## big.gain        6.959213   0.381195  18.2563 < 2.2e-16 ***
## municipal      -0.242495   0.141135  -1.7182   0.08577 .  
## private        -0.750936   0.144287  -5.2045 1.946e-07 ***
## cooperative    -0.082223   0.164001  -0.5014   0.61612    
## mi4             0.249785   0.137777   1.8130   0.06984 .  
## mi8             0.710314   0.138384   5.1329 2.853e-07 ***
## mi10            1.135770   0.182402   6.2267 4.762e-10 ***
## bill           -0.129491   0.012490 -10.3671 < 2.2e-16 ***
## sd.small.loss   1.119045   0.192529   5.8123 6.160e-09 ***
## sd.small.gain   2.241911   0.223099  10.0489 < 2.2e-16 ***
## sd.big.gain     4.126744   0.378114  10.9140 < 2.2e-16 ***
## sd.municipal   -0.762524   0.193541  -3.9399 8.153e-05 ***
## sd.private      1.079473   0.174730   6.1779 6.494e-10 ***
## sd.cooperative -0.239934   0.260029  -0.9227   0.35615    
## sd.mi4          0.539527   0.221437   2.4365   0.01483 *  
## sd.mi8          0.181021   0.185235   0.9773   0.32844    
## sd.mi10         1.259140   0.231424   5.4408 5.303e-08 ***
## sd.bill         0.208471   0.016156  12.9034 < 2.2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Log-Likelihood: -1798.3
## McFadden R^2:  0.41 
## Likelihood ratio test : chisq = 2499.4 (p.value = < 2.22e-16)
## 
## random coefficients
##             Min.    1st Qu.      Median        Mean     3rd Qu. Max.
## small.loss  -Inf  1.0945087  1.84929332  1.84929332  2.60407797  Inf
## small.gain  -Inf  3.4093693  4.92151497  4.92151497  6.43366069  Inf
## big.gain    -Inf  4.1757667  6.95921326  6.95921326  9.74265983  Inf
## municipal   -Inf -0.7568092 -0.24249451 -0.24249451  0.27182013  Inf
## private     -Inf -1.4790295 -0.75093626 -0.75093626 -0.02284299  Inf
## cooperative -Inf -0.2440567 -0.08222337 -0.08222337  0.07960991  Inf
## mi4         -Inf -0.1141203  0.24978542  0.24978542  0.61369110  Inf
## mi8         -Inf  0.5882172  0.71031430  0.71031430  0.83241139  Inf
## mi10        -Inf  0.2864937  1.13577045  1.13577045  1.98504719  Inf
## bill        -Inf -0.2701020 -0.12949052 -0.12949052  0.01112092  Inf
```

```

Translate to dollar values
Normalize with bill 

```r
big.gain.value <- rpar(wf.mxlc, "big.gain", norm = "bill")
summary(big.gain.value)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
##     -Inf 27.34868 52.96159 52.96159 78.57450      Inf
```

```r
med(big.gain.value)
```

```
## [1] 11.74159
```

```r
mean(big.gain.value)
```

```
## [1] 11.74159
```

```r
big.gain.value.nc <- rpar(wf.mxlu, "big.gain", norm = "bill")
summary(big.gain.value.nc)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
##     -Inf 32.24766 53.74303 53.74303 75.23840      Inf
```

```r
med(big.gain.value.nc)
```

```
## [1] 6.959213
```

```r
mean(big.gain.value.nc)
```

```
## [1] 6.959213
```

Use AIC to compare models

Interact with demographics 
e.g., age:mi4

goodness of fit
lrtest
AIC

demographics
test to see if panel = true or false, see how output differs

find best “base” model based on AIC and lrtest


