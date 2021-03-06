# WF_data_explore
Sarah Klain  
October 14, 2015  



## Wind Farm Data Cleaning and Exploration

### Data Cleaning
I deleted the following columns from my data files.

ResponseSet, Name, ExternalDataReference, EmailAddress, Status

I deleted incomplete surveys.

I made ethnic origin one column. 

I deleted the responses that incorrectly asnwered my "are you paying attnetion questions" Attention filter: if att1 = 0 and att2 = 5 then I deleted the row


```r
library(ggplot2)
library(ggthemes)
library(wesanderson)
library(viridis)
suppressMessages(library(dplyr))
library(knitr)
library(tidyr)
library(broom)
```

load data: c for coded, nc for not coded


```r
c <- read.csv("~/Documents/R_2015/wf_ce/Coded_WF_10_14_2015.csv")
nc <- read.csv("Not_coded_2015_10_20_WF_CE.csv")
```

## Plots of data using demographic variables

Income


```r
nc$income2 <- factor(nc$income, levels=c("Less than $10,000", "$10,000-$14,999","$15,000-$24,999", "$25,000-$34,999", "$35,000-$49,999", "$50,000-$74,999","$75,000-$99,999", "$100,000-$124,999","$125,000-$149,000",
"$150,000-$174,999", "$175,000-$199,999", "$250,000 and above"))

inc <- ggplot(data=nc, aes(x=income2)) + 
  geom_bar(fill = "blue4")+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis(discrete=TRUE, option = "plasma") +
  xlab("Income") + ylab("Count")

inc
```

![](WF_data_exploration_files/figure-html/unnamed-chunk-3-1.png) 

```r
ggsave(inc, file="/Users/sarahklain/Documents/R_2015/wf_ce/figs_demographic/income.pdf")
```

```
## Saving 7 x 5 in image
```

Education


```r
nc$education2 <- factor(nc$education, levels=c("Grade school", "Some high school","High school graduate", "Some college credit ", "Associate degree", "Bechelor degree", "Graduate degree or Professional degree ", "Professional degree"))
ed <- ggplot(data = nc, aes(x= education2, fill = gender))
edu <- ed + geom_bar() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Education") + ylab("Count") +
  scale_fill_viridis(discrete=TRUE)


edu
```

![](WF_data_exploration_files/figure-html/unnamed-chunk-4-1.png) 

```r
ggsave(edu, file="/Users/sarahklain/Documents/R_2015/wf_ce/figs_demographic/edu.pdf")
```

```
## Saving 7 x 5 in image
```

Employment


```r
emp <- ggplot(data=nc, aes(x=employment, fill = gender)) + geom_bar() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis(discrete=TRUE) +
    xlab("Employment") + ylab("Count")

edu
```

![](WF_data_exploration_files/figure-html/unnamed-chunk-5-1.png) 

```r
ggsave(emp, file="/Users/sarahklain/Documents/R_2015/wf_ce/figs_demographic/emp.pdf")
```

```
## Saving 7 x 5 in image
```

Gender

```r
gen <- ggplot(data=nc, aes(x=gender, fill = gender)) + geom_bar() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis(discrete=TRUE) +
    xlab("Gender") + ylab("Count")

gen
```

![](WF_data_exploration_files/figure-html/unnamed-chunk-6-1.png) 

```r
ggsave(gen, file="/Users/sarahklain/Documents/R_2015/wf_ce/figs_demographic/gen.pdf")
```

```
## Saving 7 x 5 in image
```

Political Party Affiliation


```r
pol <- ggplot(nc, aes(x = pol_party, fill = education2))+
  geom_bar() +
  scale_fill_viridis(discrete=TRUE, "Education", option = "magma") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Political Party") + ylab("Count") 

pol
```

![](WF_data_exploration_files/figure-html/unnamed-chunk-7-1.png) 

```r
ggsave(pol, file="/Users/sarahklain/Documents/R_2015/wf_ce/figs_demographic/pol.pdf")
```

```
## Saving 7 x 5 in image
```

Age


```r
age <- ggplot(nc, aes(x = age, fill = gender)) +
  geom_histogram(binwidth = 3) +
  scale_fill_viridis(discrete=TRUE) +
  xlab("Age") + ylab("Count")

age
```

![](WF_data_exploration_files/figure-html/unnamed-chunk-8-1.png) 

```r
ggsave(age, file="/Users/sarahklain/Documents/R_2015/wf_ce/figs_demographic/age.pdf")
```

```
## Saving 7 x 5 in image
```

Race

I will compare this to census tract data since my sample is *very* white. 


```r
race <- ggplot(c, aes(x = ethnic_or)) +
  geom_histogram(binwidth = 3, fill = "blue4") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Race") + ylab("Count")

race
```

![](WF_data_exploration_files/figure-html/unnamed-chunk-9-1.png) 

```r
ggsave(race, file="/Users/sarahklain/Documents/R_2015/wf_ce/figs_demographic/race.pdf")
```

```
## Saving 7 x 5 in image
```

## Opinions

What is your attitude toward developing wind power in the US?

```r
nc$att_w_US2 <- factor(nc$att_w_US, levels=c("Very positive","Positive", "Neutral", "Negative", "Very Negative"))
attUS <- ggplot(data = nc, aes(x= nc$att_w_US2, fill = pol_party)) +
  geom_bar() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Attitude") + ylab("Count") +
  scale_fill_viridis(discrete=TRUE, "Political Affiliation") +
  ggtitle("What is your attitude toward developing\nwind power in the US?")

attUS
```

![](WF_data_exploration_files/figure-html/unnamed-chunk-10-1.png) 

```r
ggsave(attUS, file="/Users/sarahklain/Documents/R_2015/wf_ce/figs_wf_att/attUS.pdf")
```

```
## Saving 7 x 5 in image
```

Have you seen a wind turbine in operation?

```r
oper <- ggplot(nc, aes(x = oper)) +
  geom_histogram(binwidth = 3, fill = "blue4") +
  xlab("Response") + ylab("Count") +
  ggtitle("Have you seen a wind turbine in operation?")

ggsave(oper, file="/Users/sarahklain/Documents/R_2015/wf_ce/figs_wf_att/oper.pdf")
```

```
## Saving 7 x 5 in image
```

```r
ggsave("fig_oper.pdf")
```

```
## Saving 7 x 5 in image
```

In your opinion, construction of offshore wind turbines off the coast of your state should be:
 

```r
nc$const_st2 <- factor(nc$const_st, levels=c("Encouraged", "Tolerated", "Discouraged", "Prohibited", "Not sure"))

constst <- ggplot(nc, aes(x = const_st2, fill = const_st2)) +
  geom_histogram(binwidth = 3) +
  scale_fill_viridis(discrete=TRUE, "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Response") + ylab("Count") +
  ggtitle("In your opinion, construction of offshore\nwind turbines off the coast of your state should be:")

constst
```

![](WF_data_exploration_files/figure-html/unnamed-chunk-12-1.png) 

```r
ggsave(constst, file="/Users/sarahklain/Documents/R_2015/wf_ce/figs_wf_att/constst.pdf")
```

```
## Saving 7 x 5 in image
```

Would the presence of a visible offshore wind farm make you more or less likely to go to the coast for recreational purposes (e.g., beach-going, boating, fishing, or walking along the coast)?


```r
nc$wf_rec2 <- factor(nc$wf_rec, levels=c("Much less likely","Less likely", "No difference", "More likely", "Much more likely"))

wfrec <- ggplot(nc, aes(x = wf_rec2)) +
  geom_histogram(binwidth = 3, fill = "blue4") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Response") + ylab("Count") +
  ggtitle("Would the presence of a visible offshore wind farm make you\nmore or less likely to go to the coast for recreational purposes, \ne.g., beach-going, boating, fishing, or walking along the coast?")
  
wfrec
```

![](WF_data_exploration_files/figure-html/unnamed-chunk-13-1.png) 

```r
ggsave(wfrec, file="/Users/sarahklain/Documents/R_2015/wf_ce/figs_wf_att/wfrec.pdf")
```

```
## Saving 7 x 5 in image
```

Imagine that a wind project off your state’s coast was the first of numerous North American offshore wind projects. Would this influence your attitude towards the wind project? For example, suppose that building 200 offshore wind farms could supply 30% of the electricity for New England coastal states. Together, these wind farms would have a substantially larger impact on how people currently use the ocean and the ocean environment than one wind farm. However, 200 wind farms could reduce air pollution and reliance on fossil fuels linked to climate change and sea level rise. If you knew that the farm near your state’s coast was the first of many offshore wind farms, would you be more or less likely to support the wind farm?


```r
nc$first_m2 <- factor(nc$first_of_many, levels=c("Much less likely to support", "Less likely to support", "No effect on my attitude", "More likely to support", "Much more likely to support"))

First_st <- ggplot(nc, aes(x = first_m2)) +
  geom_histogram(binwidth = 3, fill = "blue4") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Response") + ylab("Count") +
  ggtitle("If you knew that the farm near the coast of your state\nwas the first of many offshore wind farms, would you be\nmore or less likely to support the wind farm?")
  
First_st
```

![](WF_data_exploration_files/figure-html/unnamed-chunk-14-1.png) 

```r
ggsave(First_st, file="/Users/sarahklain/Documents/R_2015/wf_ce/figs_wf_att/First_st.pdf")
```

```
## Saving 7 x 5 in image
```

Do you recreate on the coast? This could be a range of coastal or ocean-based activities such as going to the beach, surfing, fishing, and/or boating. 


```r
nc$freq_rec2 <- factor(nc$freq_rec, levels=c("Frequently, 20+ times/year", "Sometimes, 10-20 times/year", "Every now and then, 5-10 times/year", "Rarely, 1-5 times/year", "Never"))

freqrec <- ggplot(nc, aes(x = freq_rec2)) +
  geom_histogram(binwidth = 3, fill = "blue4") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Response") + ylab("Count") +
  ggtitle("Do you recreate on the coast?\nThis could be a range of coastal or ocean-based activities \nsuch as going to the beach, surfing, fishing, and/or boating.")

freqrec
```

![](WF_data_exploration_files/figure-html/unnamed-chunk-15-1.png) 

```r
ggsave(freqrec, file="/Users/sarahklain/Documents/R_2015/wf_ce/figs_wf_att/freqrec.pdf")
```

```
## Saving 7 x 5 in image
```

