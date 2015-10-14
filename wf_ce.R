# Choice Experiment Data Analysis
# Sarah Klain
# Sep 20, 2015


install.packages("support.CEs")
library(support.CEs)
library(ggplot2)

w <- "~/Documents/R_2015/wf_ce/2015_09_21_WF_CE.csv"
wfd <- read.csv(w) 
str(wfd)

# convert choice experimet (CE) responses to factors
wfd$Q1 <- as.factor(wfd$Q1)
wfd$Q2 <- as.factor(wfd$Q2)
wfd$Q3 <- as.factor(wfd$Q3)
wfd$Q4 <- as.factor(wfd$Q4)
wfd$Q5 <- as.factor(wfd$Q5)
wfd$Q6 <- as.factor(wfd$Q6)
wfd$Q7 <- as.factor(wfd$Q7)
wfd$Q8 <- as.factor(wfd$Q8)

variable.names(wfd)

wf_inc <- ggplot(data = wfd, aes(x= income))

#Choice Experiment
#make.dataset(wfd, cp,schoice.indicators, detail = FALSE)

# wfd$carb2 <- factor(mtcars$carb, levels=c("1", "2", "3", "6", "8", "4"))
# ggplot(data=mtcars, aes(y=carb2, x=mpg, colour=hp)) +
  # geom_point()

wfd$income2 <- factor(wfd$income, levels=c("Less than $10,000", "$10,000-$14,999","$15,000-$24,999", "$25,000-$34,999", "$35,000-$49,999", "$50,000-$74,999","$75,000-$99,999", "$100,000-$124,999","$125,000-$149,000",
"$150,000-$174,999", "$175,000-$199,999", "$250,000 and above"))

ggplot(data=wfd, aes(x=income2)) + geom_bar()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

wf_inc + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

wf_ed <- ggplot(data = wfd, aes(x= education))
wf_ed + geom_bar()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))


