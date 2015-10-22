# Choice Experiment Data Analysis
# Sarah Klain
# Oct 13, 2015

### Data Cleaning
# delete the following columns: 

# ResponseSet
# Name
# ExternalDataReference
# EmailAddress
# Status

# deleted incomplete surveys
# made ethnic origin one column
#  if att1 = 0 and att2 = 5 then delete 

install.packages("support.CEs")
library(support.CEs)
library(ggplot2)

w <- read.csv("~/Documents/R_2015/wf_ce/Coded_WF_10_14_2015.csv")
str(w)
summary(w)
head(w)

b <- ggplot(w, aes(x = income))

b + geom_bar()

ggplot(g, aes(x = continent, y = gdpPercap)) + 
  geom_jitter(position = position_jitter(width = 0.04, height = 0), color = "goldenrod1", alpha = 1/5) +
  stat_summary(fun.y = min, colour = "turquoise4", geom = "point", size = 4) +
  stat_summary(fun.y = max, colour = "red3", geom = "point", size = 4) +
  geom_boxplot(width=.2, outlier.shape = NA, alpha = 0.1) + theme_pander() +
  xlab("Continent") + ylab("GDP per Capita")





# convert choice experimet (CE) responses to factors
# wfd$Q1 <- as.factor(wfd$Q1)
# wfd$Q2 <- as.factor(wfd$Q2)
# wfd$Q3 <- as.factor(wfd$Q3)
# wfd$Q4 <- as.factor(wfd$Q4)
# wfd$Q5 <- as.factor(wfd$Q5)
# wfd$Q6 <- as.factor(wfd$Q6)
# wfd$Q7 <- as.factor(wfd$Q7)
# wfd$Q8 <- as.factor(wfd$Q8)

variable.names(wfd)

wf_inc <- ggplot(data = wfd, aes(x= income))

wfd$income2 <- factor(wfd$income, levels=c("Less than $10,000", "$10,000-$14,999","$15,000-$24,999", "$25,000-$34,999", "$35,000-$49,999", "$50,000-$74,999","$75,000-$99,999", "$100,000-$124,999","$125,000-$149,000",
"$150,000-$174,999", "$175,000-$199,999", "$250,000 and above"))

ggplot(data=wfd, aes(x=income2)) + geom_bar()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

wf_inc + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

wf_ed <- ggplot(data = wfd, aes(x= education))
wf_ed + geom_bar()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data=wfd, aes(x=employment)) + geom_bar()+ theme(axis.text.x = element_text(angle = 60, hjust = 1))

s  <- ggplot(wfd,aes(x=employment, fill = gender))
s + geom_bar(position="stack") + theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Choice Experiment
#make.dataset(wfd, cp,schoice.indicators, detail = FALSE)
