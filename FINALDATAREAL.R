options(scipen = 999)
install.packages("AER")
install.packages("stargazer")
install.packages("ggplot2")
install.packages("corrplot")
install.packages("GGally") # note may not need this - just for nice correlation plots - maybe remove
later
install.packages("arsenal") # helpful for categorical variables summary stats
install.packages("car")
install.packages("lmtest")
install.packages("rmarkdown")

library(AER)
library(stargazer)
library(ggplot2)
library(corrplot)
library(GGally)
library(lmtest)
library(car) 
library(arsenal)
library(rmarkdown)

install.packages("hps_1_.csv")
View(hps_1_)
finaldata <- hps_2_

summary(finaldata)
plot(finaldata) #summary of data

#Dependent variable: GETVAC

finaldata_clean <- finaldata[!is.na(finaldata$GETVAC), ]


finaldata_clean <- finaldata_clean[!is.na(finaldata_clean$TBIRTH_YEAR), ]
finaldata_clean <- finaldata_clean[!is.na(finaldata_clean$EGENID_BIRTH), ]
finaldata_clean <- finaldata_clean[!is.na(finaldata_clean$THHLD_NUMPER), ]
finaldata_clean <- finaldata_clean[!is.na(finaldata_clean$HADCOVID), ]
finaldata_clean <- finaldata_clean[!is.na(finaldata_clean$WRKLOSSRV), ]
finaldata_clean <- finaldata_clean[!is.na(finaldata_clean$ANYWORK), ]
finaldata_clean <- finaldata_clean[!is.na(finaldata_clean$KINDWORK), ] #3000 observations left
finaldata_clean <- finaldata_clean[!is.na(finaldata_clean$WKVOL), ]
finaldata_clean <- finaldata_clean[!is.na(finaldata_clean$SETTING), ]
finaldata_clean <- finaldata_clean[!is.na(finaldata_clean$EXPNS_DIF), ]
finaldata_clean <- finaldata_clean[!is.na(finaldata_clean$ACTIVITY1), ]
finaldata_clean <- finaldata_clean[!is.na(finaldata_clean$ACTIVITY2), ]
finaldata_clean <- finaldata_clean[!is.na(finaldata_clean$CURFOODSUF), ]
finaldata_clean <- finaldata_clean[!is.na(finaldata_clean$TENURE), ]
finaldata_clean <- finaldata_clean[!is.na(finaldata_clean$INCOME), ]

View(finaldata_clean)


#Independent variables:  
  
finaldata_clean$GETVAC <- as.categorical(finaldata_clean$GETVAC)
hist(finaldata_clean$GETVAC)
hist(finaldata_clean$GETVAC)


View(finaldata_clean)

corrplot(cor(finaldata_clean[, c("GETVAC", "TBIRTH_YEAR", "EGENID_BIRTH", "INCOME", "THHLD_NUMPER", "KINDWORK", "WKVOL")]))

plot(finaldata_clean$GETVAC)

Correlations <- cor(finaldata_clean[, c("GETVAC", "TBIRTH_YEAR", "EGENID_BIRTH", "THHLD_NUMPER", "INCOME", "KINDWORK")])

stargazer(Correlations[, c("GETVAC", "TBIRTH_YEAR", "EGENID_BIRTH", "THHLD_NUMPER", "INCOME", "KINDWORK")],
          type = "text",
          out="Correlations.htm",
          flip = TRUE,
          title = "Correlations") 

ggpairs(finaldata_clean[, c("GETVAC", "TBIRTH_YEAR", "EGENID_BIRTH", "THHLD_NUMPER", "INCOME", "KINDWORK")])


reg1 <- lm(GETVAC ~ WKVOL, data=finaldata_clean)
summary(reg1)

reg2 <- lm(GETVAC ~ WKVOL + EGENID_BIRTH, data=finaldata_clean)
summary(reg2)

reg3 <- lm(GETVAC ~ WKVOL + AGE + THHLD_NUMPER + INCOME + KINDWORK + WKVOL*EGENID_BIRTH, data= finaldata_clean)
summary(reg3)

table(finaldata_clean$THHLD_NUMBER)

finaldata_clean$AGE <- 2021-finaldata_clean$TBIRTH_YEAR

table2 <- stargazer(reg1, reg2, reg7, type = "text", out = "reg.htm") 
write2html(table2, "Model", title="Overview model")



vif2 <- vif(reg2)
vif7 <- vif(reg7)
vif7

sqrt(vif2) #the square root of the vif is the factor by with the s.e. will be inflated due to MC
sqrt(vif7) #the square root of the vif is the factor by with the s.e. will be inflated due to MC

vif_manually=1/(1-0.03261)


#Tolerence
1/vif(reg3)

plot(reg3)


plot(reg3, 1)

violin <- finaldata_clean%>%ggplot(aes(x = WKVOL, y = GETVAC, fill = WKVOL)) +
  geom_violin() + scale_fill_grey() + coord_flip()

ggplot(finaldata_clean, aes(x=WKVOL, y=GETVAC, fill= WKVOL)) + 
  geom_violin() + scale_fill_grey() + coord_flip()



# Density plot of dependent variable
ggplot(data = finaldata_clean, aes(x = GETVAC)) +
  geom_histogram(aes(y = ..density..), bins = 15, fill = "blue") +
  geom_density(color = "black", size = 1) +
  theme_minimal() +
  labs(x="Decision to get vaccinated", y="Density", title = "Density of vaccination")



?ifelse

cor(finaldata_clean$GETVAC, finaldata_clean$WKVOL)

plot(finaldata_clean$GETVAC, finaldata_clean$WKVOL)

finaldata_clean$INCOME <- as.factor(finaldata_clean$INCOME)

finaldata_clean$WKVOL <- as.factor(finaldata_clean$WKVOL)
finaldata_clean$WKVOL <- as.character(finaldata_clean$WKVOL)


finaldata_clean$EGENID_BIRTH
head(finaldata_clean$EGENID_BIRTH)
sum(finaldata_clean$EGENID_BIRTH)
class(finaldata_clean$EGENID_BIRTH)




ggplot(data= finaldata_clean, aes ( x= WKVOL, y= GETVAC))

ggplot(data = finaldata_clean, aes(x=WKVOL, y=GETVAC)) + 
  geom_boxplot() +
  coord_flip()


plot(reg3)
hist(finaldata_clean$WKVOL, 
     col = "green",
     breaks = 20,
     xlab = "Work/Volunteer",
     main = "Histogram Work/volunteer")



class(finaldata_clean$WKVOL)

finaldata_clean$EGENID_BIRTH <- as.factor(finaldata_clean$EGENID_BIRTH)

finaldata_clean$WKVOL <- as.factor(finaldata_clean$WKVOL)


summary(finaldata_clean[, c("WKVOL", "EGENID_BIRTH", "TBIRTH_YEAR", "THHLD_NUMPER", "KINDWORK", "SETTING" , "INCOME")])




finaldata_clean$WKVOL <- as.factor(finaldata_clean$WKVOL)


ggplot(data = finaldata_clean, aes(x = WKVOL, y = GETVAC, group = EGENID_BIRTH, color = EGENID_BIRTH)) + 
  geom_point(position = "jitter") +
  theme_minimal() +
  labs(x="work/volunteer", y="Unwillingness to get vaccinatedr", color="EGENID_BIRTH", title = "Unwillingness to get vaccinated")

cor(finaldata_clean$WKVOL, finaldata_clean$ANYWORK, finaldata_clean$WRKLOSSRV, finaldata_clean$KINDWORK, finaldata_clean$SETTING, finaldata_clean$ACTIVITY1, finaldata_clean$ACTIVITY2)
corrplot(cor(finaldata_clean[, c("GETVAC", "ANYWORK", "WRKLOSSRV", "KINDWORK")]))

(cor(finaldata_clean[,c("", "ANYWORK", "WRKLOSSRV", "KINDWORK", "ACTIVITY1", "ACTIVITY2")]))

workcor <- cor(finaldata_clean[,c("GETVAC", "WKVOL", "ANYWORK", "WRKLOSSRV", "KINDWORK", "ACTIVITY1", "ACTIVITY2", "SETTING")])
workcor
avPlot(reg3 ~ WKVOL)

stargazer(finaldata_clean, type = "text")
stargazer(finaldata_clean[, c("GETVAC", "WKVOL", "EGENID_BIRTH", "TBIRTH_YEAR", "THHLD_NUMPER", "KINDWORK" , "INCOME")],
          type = "text",
          out="summary_stats.txt",
          summary.stat = c("n", "mean", "median", "sd", "min", "max", "p25", "p75"),
          title="Summary statistics",
          digits = 2)




class()
table(finaldata_clean$WKVOL)
finaldata_clean$WKVOLr <- 2-finaldata_clean$WKVOL

#Basically when you clean the kind of work it deletes alot of the 0 
#this means that there might be alot of unemployed people or working in a private company where tehy can work from home 
#mention this in the paper 

class(finaldata$THHLD_NUMPER)





