#load packages
library(ggplot2)
library(tidyverse)
library(lme4)

#Load data. 'OE_Master.txt' is for the plot, while 'OE_long.txt' is for the regression analysis

data = read_tsv("OE_master.txt")
long_data = read_tsv("OE_long.txt")
thomas = read.table("Thomas.txt", header=TRUE)
allen = read.table("Allen.txt", header=TRUE)
thomas$postnominal_genitives = thomas$postnominal_genitives/100
allen$postnominal_genitives = allen$postnominal_genitives/100

#Figure 1
ggplot(thomas,aes(x=century, y=postnominal_genitives, size=tokens, weight=tokens)) +  
  geom_jitter(width=0.05, heigth) + 
  coord_cartesian(ylim = c(0,1)) + 
  guides(size="none") +
  ggtitle("Thomas (1931)") +
  theme(plot.title = element_text(hjust = 0.5))

#Figure 2
ggplot(allen,aes(x=period, y=postnominal_genitives, size=tokens, weight=tokens)) +  
  geom_jitter(width=0.05, heigth) + 
  coord_cartesian(ylim = c(0,1)) + 
  guides(size="none") +
  ggtitle("Allen (2008)") +
  theme(plot.title = element_text(hjust = 0.5))


##################################
#Fig4 + Light genitive NPs
#################################

#Figure 4a
data4a <- data %>%
  filter(weight=='Light', type=='ProperN', is.na(century)==FALSE, total>9)

ggplot(data4a,aes(x=century, y=postnominal_genitive, size=total, colour=type)) +
  stat_smooth(method="lm") + 
 geom_jitter(width=0.05, height=0.01) + 
 coord_cartesian(ylim = c(0,1)) + guides(size="none") +
 scale_color_manual(values=c("#56B4E9")) +
 ggtitle("Light NPs, Proper Nouns")+
 theme(plot.title = element_text(hjust = 0.5))

#Figure 4b
data4b <- data %>%
  filter(weight=='Light', type!='ProperN', is.na(century)==FALSE, total>9)

ggplot(data4b,aes(x=century, y=postnominal_genitive, size=total, colour=type)) +
  stat_smooth(method="lm") + 
geom_jitter(width=0.05, height=0.01) + 
coord_cartesian(ylim = c(0,1)) +
guides(size="none") +
ggtitle("Light NPs, Common Nouns") +
theme(plot.title = element_text(hjust = 0.5))

#Regression analysis 4a
short_name <- long_data %>%
  filter(weight=="Light", type =="ProperN", is.na(century)==FALSE, total>9)

nullm4a = lmer(postnominal ~ (1|text), data=short_name)
m4a = lmer(postnominal ~ century + (1|text), data=short_name)
summary(m4a)
anova(nullm4a, m4a)

#Regression analysis 4b
short_noun <- long_data %>%
  filter(weight=="Light", type =="CommonN", is.na(century)==FALSE, total>9)

nullm4b = lmer(postnominal ~ (1|text), data=short_noun)
m4b = lmer(postnominal~ century + (1|text), data=short_noun)
summary(m4b)
anova(nullm4b, m4b)

##################################
#Fig5 + Modified Matrix NPs
#################################

#Figure 5a
data5a <- data %>%
  filter(weight=='Modified', type=='ProperN', is.na(century)==FALSE, total>9)

ggplot(data5a,aes(x=century, y=postnominal_genitive, size=total, colour=type)) + 
  stat_smooth(method="lm") + 
  geom_jitter(width=0.05, height=0.01) + 
  coord_cartesian(ylim = c(0,1))+
  guides(size="none") + 
  scale_color_manual(values=c("#56B4E9")) +
  ggtitle("Modified matrix NPs, Proper Nouns")+
  theme(plot.title = element_text(hjust = 0.5))

#Figure 5b
data5b <- data %>%
  filter(weight=='Modified', type!='ProperN', is.na(century)==FALSE, total>9)

ggplot(data5b,aes(x=century, y=postnominal_genitive, size=total, colour=type)) + 
  stat_smooth(method="lm") + 
  geom_jitter(width=0.05, height=0.01) +  
  coord_cartesian(ylim = c(0,1))  + 
  guides(size="none") +
  ggtitle("Modified matrix NPs, Common Nouns")+
  theme(plot.title = element_text(hjust = 0.5))

#Regression analysis 5a
mod_name <- long_data %>%
  filter(weight=="Modified", type =="ProperN", is.na(century)==FALSE, total>9) 

nullm5a = lmer(postnominal ~ (1|text), data=mod_name)
m5a = lmer(postnominal ~ century + (1|text), data=mod_name)
summary(m5a)
anova(nullm5a, m5a)

#Regression analysis 5b
mod_noun <- long_data %>%
  filter(weight=="Modified", type =="CommonN", is.na(century)==FALSE, total>9) 

nullm5b = lmer(postnominal ~ (1|text), data=mod_noun)
m5b = lmer(postnominal~ century + (1|text), data=mod_noun)
summary(m5b)
anova(nullm5b, m5b)

mod_det <- long_data %>%
  filter(weight=="Modified", type =="D+CommonN", is.na(century)==FALSE, total>9) 

nullm5bis = lmer(postnominal ~ (1|text), data=mod_det)
m5bis = lmer(postnominal~ century + (1|text), data=mod_det)
summary(m5bis)
anova(nullm5bis, m5bis)

##################################
#Fig6 + Heavy Genitive NPs
#################################

#Figure 6a
data6a <- data %>%
  filter(weight=='Heavy', type=='ProperN', is.na(century)==FALSE, total>9)

ggplot(data6a,aes(x=century, y=postnominal_genitive, size=total, colour=type)) + 
  stat_smooth(method="lm") + 
  geom_jitter(width=0.05, height=0.01) + 
  coord_cartesian(ylim = c(0,1))+
  guides(size="none") + 
  scale_color_manual(values=c("#56B4E9")) +
  ggtitle("Heavy genitive NPs, Proper Nouns")+
  theme(plot.title = element_text(hjust = 0.5))

#Figure 6b
data6b <- data %>%
  filter(weight=='Heavy', type!='ProperN', is.na(century)==FALSE, total>9)

ggplot(data6b,aes(x=century, y=postnominal_genitive, size=total, colour=type)) + 
  stat_smooth(method="lm") + 
  geom_jitter(width=0.05, height=0.01) +  
  coord_cartesian(ylim = c(0,1))  + 
  guides(size="none") +
  ggtitle("Heavy Genitive NPs, Common Nouns")+
  theme(plot.title = element_text(hjust = 0.5))

#Regression analysis 6a
heavy_name <- long_data %>%
  filter(weight=="Heavy", type =="ProperN", is.na(century)==FALSE, total>9) 

nullm6a = lmer(postnominal ~ (1|text), data=heavy_name)
m6a = lmer(postnominal ~ century + (1|text), data=heavy_name)
summary(m6a)
anova(nullm6a, m6a)

#Regression analysis 6b
heavy_noun <- long_data %>%
  filter(weight=="Heavy", type =="Simple", is.na(century)==FALSE, total>9) 

heavy_mod <- long_data %>%
  filter(weight=="Heavy", type =="Complex", is.na(century)==FALSE, total>9) 

nullm6b = lmer(postnominal ~ (1|text), data=heavy_noun)
m6b = lmer(postnominal~ century + (1|text), data=heavy_noun)
summary(m6b)
anova(nullm6b, m6b)

nullm6bis = lmer(postnominal ~ (1|text), data=heavy_mod)
m6bis = lmer(postnominal~ century + (1|text), data=heavy_mod)
summary(m6bis)
anova(nullm6bis, m6bis)

#Testing the Constant Rate Effect
data_test = rbind(heavy_noun, heavy_mod)

m1=lmer(postnominal ~ century + type + (1|text), data=data_test)
m2=lmer(postnominal ~ century + type + type*century + (1|text), data=data_test)

