#load packages
library(ggplot2)
library(tidyverse)
library(lme4)

#Load data. 'OE_Master.txt' is for the plot, while 'OE_long.txt' is for the regression analysis
#Figs: 5.79 x 4.45

data = read_tsv("OE_master.txt")
long_data = read_tsv("OE_long.txt")
thomas = read.table("Thomas.txt", header=TRUE)
allen = read.table("Allen.txt", header=TRUE)
thomas$postnominal_genitives = thomas$postnominal_genitives/100
allen$postnominal_genitives = allen$postnominal_genitives/100

#Figure 1
ggplot(thomas,aes(x=century, y=postnominal_genitives, size=tokens, weight=tokens)) +  
  geom_jitter(width=0.05) + 
  coord_cartesian(ylim = c(0,1)) + 
  guides(size="none") +
  ggtitle("Thomas (1931)") +
  theme(plot.title = element_text(hjust = 0.5))

#Figure 2
ggplot(allen,aes(x=period, y=postnominal_genitives, size=tokens, weight=tokens)) +  
  geom_jitter(width=0.05) + 
  coord_cartesian(ylim = c(0,1)) + 
  guides(size="none") +
  ggtitle("Allen (2008)") +
  theme(plot.title = element_text(hjust = 0.5))


##################################
#Fig3 + Light genitive NPs
#################################

#Figure 3a
data3a <- data %>%
  filter(weight=='Light', type=='ProperN', total>9)

ggplot(data3a,aes(x=century, y=postnominal_genitive, size=total, colour=type)) +
  stat_smooth(method="glm", method.args=list(family="binomial"),se=FALSE) + 
  geom_jitter(width=0.05, height=0.01) + 
 coord_cartesian(ylim = c(0,1)) + guides(size="none") +
 scale_color_manual(values=c("#56B4E9")) +
 ggtitle("Light NPs, Proper Nouns")+
 theme(plot.title = element_text(hjust = 0.5))

#Figure 3b
data3b <- data %>%
  filter(weight=='Light', type!='ProperN', total>9)

ggplot(data3b,aes(x=century, y=postnominal_genitive, size=total, colour=type)) +
  stat_smooth(method="glm", method.args=list(family="binomial"),se=FALSE) + 
geom_jitter(width=0.05, height=0.01) + 
coord_cartesian(ylim = c(0,1)) +
guides(size="none") +
ggtitle("Light NPs, Common Nouns") +
theme(plot.title = element_text(hjust = 0.5))

#Regression analysis 3a
short_name <- long_data %>%
  filter(weight=="Light", type =="ProperN", total>9)

nullm3a = lmer(postnominal ~ (1|text), data=short_name)
m3a = lmer(postnominal ~ century + (1|text), data=short_name)
summary(m3a)
anova(nullm3a, m3a)

#Regression analysis 3b
short_noun <- long_data %>%
  filter(weight=="Light", type =="CommonN", total>9)

nullm3b = lmer(postnominal ~ (1|text), data=short_noun)
m3b = lmer(postnominal~ century + (1|text), data=short_noun)
summary(m3b)
anova(nullm3b, m3b)

##################################
#Fig4 + Modified Matrix NPs
#################################

#Figure 4a
data4a <- data %>%
  filter(weight=='Modified', type=='ProperN', is.na(century)==FALSE, total>9)

ggplot(data4a,aes(x=century, y=postnominal_genitive, size=total, colour=type)) + 
  stat_smooth(method="glm", method.args=list(family="binomial"),se=FALSE) + 
  geom_jitter(width=0.05, height=0.01) + 
  coord_cartesian(ylim = c(0,1))+
  guides(size="none") + 
  scale_color_manual(values=c("#56B4E9")) +
  ggtitle("Modified matrix NPs, Proper Nouns")+
  theme(plot.title = element_text(hjust = 0.5))

#Figure 4b
data4b <- data %>%
  filter(weight=='Modified', type!='ProperN', total>9)

ggplot(data4b,aes(x=century, y=postnominal_genitive, size=total, colour=type)) + 
  stat_smooth(method="glm", method.args=list(family="binomial"),se=FALSE) + 
  geom_jitter(width=0.05, height=0.01) +  
  coord_cartesian(ylim = c(0,1))  + 
  guides(size="none") +
  ggtitle("Modified matrix NPs, Common Nouns")+
  theme(plot.title = element_text(hjust = 0.5))

#Regression analysis 4a
mod_name <- long_data %>%
  filter(weight=="Modified", type =="ProperN", total>9) 

nullm4a = lmer(postnominal ~ (1|text), data=mod_name)
m4a = lmer(postnominal ~ century + (1|text), data=mod_name)
summary(m4a)
anova(nullm4a, m4a)

#Regression analysis 4b
mod_noun <- long_data %>%
  filter(weight=="Modified", type =="CommonN", total>9) 

nullm4b = lmer(postnominal ~ (1|text), data=mod_noun)
m4b = lmer(postnominal~ century + (1|text), data=mod_noun)
summary(m4b)
anova(nullm4b, m4b)

mod_det <- long_data %>%
  filter(weight=="Modified", type =="D+CommonN", total>9) 

nullm4bis = lmer(postnominal ~ (1|text), data=mod_det)
m4bis = lmer(postnominal~ century + (1|text), data=mod_det)
summary(m4bis)
anova(nullm4bis, m4bis)

##################################
#Fig5 + Heavy Genitive NPs
#################################

#Figure 5a
data5a <- data %>%
  filter(weight=='Heavy', type=='ProperN', total>9)

ggplot(data5a,aes(x=century, y=postnominal_genitive, size=total, colour=type)) + 
  stat_smooth(method="glm", method.args=list(family="binomial"),se=FALSE) + 
  geom_jitter(width=0.05, height=0.01) + 
  coord_cartesian(ylim = c(0,1))+
  guides(size="none") + 
  scale_color_manual(values=c("#56B4E9")) +
  ggtitle("Heavy genitive NPs, Proper Nouns")+
  theme(plot.title = element_text(hjust = 0.5))

#Figure 5b
data5b <- data %>%
  filter(weight=='Heavy', type!='ProperN', total>9)

ggplot(data5b,aes(x=century, y=postnominal_genitive, size=total, colour=type)) + 
  stat_smooth(method="glm", method.args=list(family="binomial"),se=FALSE) + 
  geom_jitter(width=0.05, height=0.01) +  
  coord_cartesian(ylim = c(0,1))  + 
  guides(size="none") +
  ggtitle("Heavy Genitive NPs, Common Nouns")+
  theme(plot.title = element_text(hjust = 0.5))

#Regression analysis 5a
heavy_name <- long_data %>%
  filter(weight=="Heavy", type =="ProperN", total>9) 

nullm5a = lmer(postnominal ~ (1|text), data=heavy_name)
m5a = lmer(postnominal ~ century + (1|text), data=heavy_name)
summary(m5a)
anova(nullm5a, m5a)

#Regression analysis 5b
heavy_noun <- long_data %>%
  filter(weight=="Heavy", type =="Simple", total>9) 

heavy_mod <- long_data %>%
  filter(weight=="Heavy", type =="Complex", total>9) 

nullm5b = lmer(postnominal ~ (1|text), data=heavy_noun)
m5b = lmer(postnominal~ century + (1|text), data=heavy_noun)
summary(m5b)
anova(nullm5b, m5b)

nullm5bis = lmer(postnominal ~ (1|text), data=heavy_mod)
m5bis = lmer(postnominal~ century + (1|text), data=heavy_mod)
summary(m5bis)
anova(nullm5bis, m5bis)

#Testing the Constant Rate Effect
data_test = rbind(heavy_noun, heavy_mod)

m1=lmer(postnominal ~ century + type + (1|text), data=data_test)
m2=lmer(postnominal ~ century + type + type*century + (1|text), data=data_test)

summary(m2)
anova(m1,m2)



data5b %>%
  group_by(century, weight, type) %>%
  summarize(mean = mean(postnominal_genitive)) 
  
