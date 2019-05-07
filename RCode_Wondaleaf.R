#Wondaleaf analysis for SAAIDS poster
#Created by: Nicole Dear
#Created on: May 6, 2019

install.packages("tidyr")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("gmodels")
install.packages("knitr")
install.packages("Hmisc")
library(tidyr)
library(tidyverse)
library(dplyr)
library(gmodels)
library(knitr)
library(Hmisc)

#setwd and import dataset
setwd("H:/Wondaleaf")
baseline <- read.csv("WondaleafCondomMainS_DATA_2019-05-06_1417.csv") %>%
  subset(redcap_event_name=="enrolment_arm_1") %>% 
  select(c(29:81))

names(baseline)

#subset dataset
x <- select(baseline, c(3,6:7,9:11,53))

#basic descriptives
mean(x$bl_q4_age)
sd(x$bl_q4_age)
mean(x$bl_q8_numchild)
sd(x$bl_q8_numchild)
CrossTable(x$bl_q6_edu)
CrossTable(x$bl_q7_occupation)
CrossTable(x$bl_q9_rship)
CrossTable(x$bl_q10_rship_length)
CrossTable(x$bl_q20_phair)

#correlation matrix
y <- as.data.frame(cor(x))

#correlation matrix, alt method w/ p-values
c.rcorr<- rcorr(as.matrix(x))
c.rcorr
c.coeff <- c.rcorr$r
c.p <- c.rcorr$P

#chi-square tests
CrossTable(x$bl_q6_edu, x$bl_q20_phair, expected = TRUE, chisq = TRUE, fisher = TRUE, format="SAS")
CrossTable(x$bl_q7_occupation, x$bl_q20_phair, expected = TRUE, chisq = TRUE, fisher = TRUE, format="SAS")
CrossTable(x$bl_q9_rship, x$bl_q20_phair, expected = TRUE, chisq = TRUE, fisher = TRUE, format="SAS")
CrossTable(x$bl_q10_rship_length, x$bl_q20_phair, expected = TRUE, chisq = TRUE, fisher = TRUE, format="SAS")

#ttests
t.test(x$bl_q4_age~x$bl_q20_phair)
t.test(x$bl_q8_numchild~x$bl_q20_phair)
