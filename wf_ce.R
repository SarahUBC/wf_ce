# Choice Experiment Data Analysis
# Sarah Klain
# Sep 20, 2015

install.packages(support.CEs)
library(support.CEs)
cedata  <- read.csv(file.choose())

# convert CE responses to factors
cedata$Q1  <- as.factor(cedata$Q1)
cedata$Q2  <- as.factor(cedata$Q2)
cedata$Q3  <- as.factor(cedata$Q3)
cedata$Q4  <- as.factor(cedata$Q4)
cedata$Q5  <- as.factor(cedata$Q5)
cedata$Q6  <- as.factor(cedata$Q6)
cedata$Q7  <- as.factor(cedata$Q7)
cedata$Q8  <- as.factor(cedata$Q8)

wfd <- cedata

library(ggplot2)
