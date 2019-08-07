#######Set up#########

library(tidyr)
library(tidyverse)
library(ggplot2)
library(SASxport)
library(psych)
library(expss)
library(rattle)


BRFSS.dirty <- read.xport(file.choose())

names(BRFSS.dirty)

BRFSSVarList <- c("X.STATE",
                  "X.AGE.G",
                  "WEIGHT2",
                  "EMPLOY1",
                  "PHYSHLTH",
                  "MENTHLTH",
                  "GENHLTH",
                  "SEX",
                  "X_MRACE1",
                  "USEMRJN1",
                  "SMOKER3",
                  "MAXDRNKS",
                  "ECIGNOW")

BRFSS1 <- BRFSS.dirty %>%
  select(X.STATE,
         X.AGE.G,
         WEIGHT2,
         EMPLOY1,
         PHYSHLTH,
         MENTHLTH,
         GENHLTH,
         SEX,
         X.MRACE1,
         USEMRJN1,
         X.SMOKER3,
         MAXDRNKS,
         ECIGNOW)

BRFSS2 <- BRFSS.dirty %>%
  select(X.STATE,
         X.AGE.G,
         WEIGHT2,
         EMPLOY1,
         PHYSHLTH,
         MENTHLTH,
         GENHLTH,
         SEX,
         X.MRACE1,
         USEMRJN1,
         X.SMOKER3,
         MAXDRNKS,
         ECIGNOW)

BRFSS2[] <- as.numeric(as.matrix(BRFSS2))
BRFSS1 <- na.omit(BRFSS1)


BRFSS1 = apply_labels(BRFSS1,
                      X.STATE = c("AL" = 1,
                                  "AK" = 2,
                                  "AZ" = 4,
                                  "AR" = 5,
                                  "CA" = 6,
                                  "CO" = 8,
                                  "CT" = 9,
                                  "DE" = 10,
                                  "DC" = 11,
                                  "FL" = 12,
                                  "GA" = 13,
                                  "HI" = 15,
                                  "ID" = 16,
                                  "IL" = 17,
                                  "IN" = 18,
                                  "IA" = 19,
                                  "KS" = 20,
                                  "KY" = 21,
                                  "LA" = 22,
                                  "ME" = 23,
                                  "MD" = 24,
                                  "MA" = 25,
                                  "MI" = 26,
                                  "MN" = 27,
                                  "MS" = 28,
                                  "MO" = 29,
                                  "MT" = 30,
                                  "NE" = 31,
                                  "NV" = 32,
                                  "NH" = 33,
                                  "NJ" = 34,
                                  "NM" = 35,
                                  "NY" = 36,
                                  "NC" = 37,
                                  "ND" = 38,
                                  "OH" = 39,
                                  "OK" = 40,
                                  "OR" = 41,
                                  "PA" = 42,
                                  "RI" = 44,
                                  "SC" = 45,
                                  "SD" = 46,
                                  "TN" = 47,
                                  "TX" = 48,
                                  "UT" = 49,
                                  "VT" = 50,
                                  "VA" = 51,
                                  "WA" = 53,
                                  "WV" = 54,
                                  "WI" = 55,
                                  "WY" = 56,
                                  "GUAM"= 66,
                                  "PR" = 72),
                      X.AGE.G = c("18-24" = 1,
                                  "25-34" = 2,
                                  "35-44" = 3,
                                  "45-54" = 4,
                                  "55-64" = 5,
                                  "65+" = 6),
                      WEIGHT2 = c("LBS" = 50:0999,
                                  "Unknown" = 7777,
                                  "Metric" = 9000:9998,
                                  "Refused" = 9999),
                      EMPLOY1 = c("Wage Emp" = 1,
                                  "Self Emp" = 2,
                                  "Unemp 1yr<" = 3,
                                  "Unemp >1yr" = 4,
                                  "Homemaker" = 5,
                                  "Student" = 6,
                                  "Retired" = 7,
                                  "Disabled" = 8,
                                  "Refused" = 9),
                      PHYSHLTH = c("# of Days Not Good" = 1:30,
                                   "None" = 88,
                                   "Unknown" = 77,
                                   "Refused" = 99),
                      MENTHLTH = c("# of Days Not Good" = 1:30,
                                   "None" = 88,
                                   "Unknown" = 77,
                                   "Refused" = 99),
                      GENHLTH = c("Excellent" = 1,
                                  "Very Good" = 2,
                                  "Good" = 3,
                                  "Fair" = 4,
                                  "Poor" = 5,
                                  "Unknown" = 7,
                                  "Refused" = 9),
                      SEX = c("Male" = 1,
                              "Female" = 2,
                              "Refused" = 9),
                      X.MRACE1 = c("white" = 1,
                                   "Black" = 2,
                                   "American Indian" = 3,
                                   "Asian" = 4,
                                   "Pacific Islander" = 5,
                                   "Other" = 6,
                                   "Multiracial" = 7,
                                   "Unknown" = 77,
                                   "Refused" = 99),
                      USEMRJN1 = c("Smoke it" = 1,
                                   "Eat it" = 2,
                                   "Drink it" = 3,
                                   "Vape it" = 4,
                                   "Dab it" = 5,
                                   "Other" = 6,
                                   "Unknown" = 7,
                                   "Refused" = 9),
                      X.SMOKER3 = c("Everyday" = 1,
                                    "Some Days" = 2,
                                    "Former" = 3,
                                    "Never" = 4,
                                    "Refused" = 9),
                      MAXDRNKS = c("# of Drinks" = 1:76,
                                   "Unkown" = 77,
                                   "Refused" = 99),
                      ECIGNOW = c("Everyday" = 1,
                                  "Some Days" = 2,
                                  "Never" = 3,
                                  "Unknown" = 7,
                                  "Refused" = 9))

BRFSS1_factor <- as_factor(BRFSS1)

BRFSS2_factor <- as_factor(BRFSS2)

BRFSS2_Fac_clean <- na.rm(BRFSS2_factor)

BRFSS_Factor <- as_factor(BRFSS.dirty)


NY <- data.frame(BRFSS2$X.STATE==36)
NJ <- data.frame(BRFSS2$X.STATE==34)
CT <- data.frame(BRFSS2$X.STATE==9)

Students <- data.frame(BRFSS2$EMPLOY1==6)
na.omit(Students)



########## Explore###########

hist(BRFSS1)
hist(BRFSS1_factor)
plot(BRFSS1)
plot(BRFSS1_factor)
describe(BRFSS1)
summary(BRFSS1)

########## Overall Health Boxplots#########

boxplot(X.AGE.G ~ GENHLTH, data = BRFSS2,
        labs(title="General Health by Age",
        x="General Health",
        y="Age Group"))

boxplot(X.AGE.G ~ PHYSHLTH, data = BRFSS2,
        labs(title="Physical Health by Age",
        x="Physical Health",
        y="Age Group",
        axis(side = 1, at= BRFSS2$PHYSHLTH, labels = TRUE),
        axis(side = 2, at= BRFSS2$X.AGE.G, labels = TRUE)))

boxplot(X.AGE.G ~ MENTHLTH, data = BRFSS1,
        labs(title="Mental Health by Age",
        x="Mental Health",
        y="Age Group",
        axis(side = 1, at= BRFSS2$MENTHLTH, labels = TRUE),
        axis(side = 2, at= BRFSS2$X.AGE.G, labels = TRUE)))

boxplot(SEX ~ GENHLTH, data = BRFSS2,
        labs(title="General Health by Sex",
        x="Mental Health",
        y="Sex",
        axis(side = 1, at= BRFSS2$MENTHLTH, labels = TRUE),
        axis(side = 2, at= BRFSS2$SEX, labels = TRUE)))

boxplot(SEX ~ PHYSHLTH, data = BRFSS2,
        labs(title="Physical Health by Sex",
        x="Physical Health",
        y="Sex",
        axis(side = 1, at= BRFSS2$MENTHLTH, labels = TRUE),
        axis(side = 2, at= BRFSS2$SEX, labels = TRUE)))

boxplot(SEX ~ MENTHLTH, data = BRFSS2,
        labs(title="Mental Health by Sex",
        x="Mental Health",
        y="Sex",
        axis(side = 1, at= BRFSS2$MENTHLTH, labels = TRUE),
        axis(side = 2, at= BRFSS2$SEX, labels = TRUE)))


############GGPlots#############

ggplot(BRFSS2, aes(x=X.STATE, y=EMPLOY1, color=ECIGNOW))+ 
  geom_smooth(se=FALSE)

ggplot(BRFSS2_factor) + geom_point(aes(x=X.STATE, y=EMPLOY1, color = ECIGNOW))

ggplot(BRFSS2_factor) + geom_point(aes(x=X.STATE, y=ECIGNOW, color=EMPLOY1)) 

ggplot(BRFSS2_factor) + geom_point(aes(x=X.AGE.G, y=ECIGNOW, color=SEX)) + facet_wrap( ~ X.STATE, ncol = 5)


ggplot(BRFSS2_factor, aes(ECIGNOW)) + 
  geom_density(aes(fill=X.STATE==36), alpha=0.05) + 
  labs(title="Density plot", 
       subtitle="ECIG Use in NY",
       caption="Source: ECIGNOW",
       x="ECIGNOW",
       fill="State")

ggplot(BRFSS2_factor, aes(ECIGNOW)) + 
  geom_density(aes(fill=factor(X.STATE==34)), alpha=0.05) + 
  labs(title="Density plot", 
       subtitle="ECIG Use in NJ",
       caption="Source: ECIGNOW",
       x="ECIGNOW",
       fill="State")

ggplot(BRFSS2_factor, aes(ECIGNOW)) + 
  geom_density(aes(fill=factor(X.STATE ==9)), alpha=0.05) + 
  labs(title="Density plot", 
       subtitle="ECIG Use in CT",
       caption="Source: ECIGNOW",
       x="ECIGNOW",
       fill="State")

ggplot(BRFSS2, aes(x=EMPLOY1, y=X.SMOKER3)) + 
  geom_point(aes(col=SEX, size=GENHLTH)) + 
  xlim(c(0, 10)) + 
  ylim(c(0, 10)) + 
  labs(subtitle="Employment to Max Drinks", 
       y="Max Drinks", 
       x="Employment", 
       title="Drinks to Employment Scatterplot")

ggplot(BRFSS2, aes(x=EMPLOY1, y=USEMRJN1)) + 
  geom_point(aes(col=SEX, size=GENHLTH)) + 
  xlim(c(0, 10)) + 
  ylim(c(0, 10)) + 
  labs(subtitle="Employment to Max Drinks", 
       y="Max Drinks", 
       x="Employment", 
       title="Drinks to Employment Scatterplot")

ggplot(BRFSS2, aes(x=EMPLOY1, y=MAXDRNKS)) + 
  geom_point(aes(col=SEX, size=GENHLTH)) + 
  xlim(c(0, 10)) + 
  ylim(c(0, 10)) + 
  labs(subtitle="Employment to Max Drinks", 
       y="Max Drinks", 
       x="Employment", 
       title="Drinks to Employment Scatterplot")

ggplot(BRFSS2, aes(x=EMPLOY1, y=ECIGNOW)) + 
  geom_point(aes(col=SEX, size=GENHLTH)) + 
  xlim(c(0, 10)) + 
  ylim(c(0, 10)) + 
  labs(subtitle="Employment to Max Drinks", 
       y="Max Drinks", 
       x="Employment", 
       title="Drinks to Employment Scatterplot")

######### Linear Regression Models##########

BRFSS1.PhysREG <- lm(PHYSHLTH ~ MAXDRNKS + ECIGNOW + X.SMOKER3 + USEMRJN1 + SEX + X.AGE.G + EMPLOY1==6, data=BRFSS2)
BRFSS1.MentREG <- lm(MENTHLTH ~ MAXDRNKS + ECIGNOW + X.SMOKER3 + USEMRJN1 + SEX + X.AGE.G + EMPLOY1==6, data=BRFSS2)
BRFSS1.GenREG <- lm(GENHLTH ~ MAXDRNKS + ECIGNOW + X.SMOKER3 + USEMRJN1 + SEX + X.AGE.G + EMPLOY1==6, data=BRFSS2)

BRFSS1.GenREG
BRFSS1.MentREG
BRFSS1.PhysREG

anova(BRFSS1.GenREG)
anova(BRFSS1.MentREG)
anova(BRFSS1.PhysREG)

BRFSS1.PhysREG2 <- lm(PHYSHLTH ~ MAXDRNKS + SEX + X.AGE.G + EMPLOY1 + X.MRACE1, data=BRFSS2)
BRFSS1.MentREG2 <- lm(MENTHLTH ~ MAXDRNKS + SEX + X.AGE.G + EMPLOY1 + X.MRACE1, data=BRFSS2)
BRFSS1.GenREG2 <- lm(GENHLTH ~ MAXDRNKS + SEX + X.AGE.G + EMPLOY1 + X.MRACE1, data=BRFSS2)

BRFSS1.GenREG2
BRFSS1.MentREG2
BRFSS1.PhysREG2

anova(BRFSS1.GenREG2)
anova(BRFSS1.MentREG2)
anova(BRFSS1.PhysREG2)

BRFSS1.PhysREG3 <- lm(PHYSHLTH ~ X.SMOKER3 + SEX + X.AGE.G + EMPLOY1 + X.MRACE1, data=BRFSS2)
BRFSS1.MentREG3 <- lm(MENTHLTH ~ X.SMOKER3 + SEX + X.AGE.G + EMPLOY1 + X.MRACE1, data=BRFSS2)
BRFSS1.GenREG3 <- lm(GENHLTH ~ X.SMOKER3 + SEX + X.AGE.G + EMPLOY1 + X.MRACE1, data=BRFSS2)

BRFSS1.GenREG3
BRFSS1.MentREG3
BRFSS1.PhysREG3

anova(BRFSS1.GenREG3)
anova(BRFSS1.MentREG3)
anova(BRFSS1.PhysREG3)

BRFSS1.PhysREG4 <- lm(PHYSHLTH ~ USEMRJN1 + SEX + X.AGE.G + EMPLOY1 + X.MRACE1, data=BRFSS2)
BRFSS1.MentREG4 <- lm(MENTHLTH ~ USEMRJN1 + SEX + X.AGE.G + EMPLOY1 + X.MRACE1, data=BRFSS2)
BRFSS1.GenREG4 <- lm(GENHLTH ~ USEMRJN1 + SEX + X.AGE.G + EMPLOY1 + X.MRACE1, data=BRFSS2)

BRFSS1.GenREG4
BRFSS1.MentREG4
BRFSS1.PhysREG4

anova(BRFSS1.GenREG4)
anova(BRFSS1.MentREG4)
anova(BRFSS1.PhysREG4)

BRFSS1.PhysREG5 <- lm(PHYSHLTH ~ ECIGNOW + SEX + X.AGE.G + EMPLOY1 + X.MRACE1, data=BRFSS2)
BRFSS1.MentREG5 <- lm(MENTHLTH ~ ECIGNOW + SEX + X.AGE.G + EMPLOY1 + X.MRACE1, data=BRFSS2)
BRFSS1.GenREG5 <- lm(GENHLTH ~ ECIGNOW + SEX + X.AGE.G + EMPLOY1 + X.MRACE1, data=BRFSS2)

BRFSS1.GenREG5
BRFSS1.MentREG5
BRFSS1.PhysREG5

anova(BRFSS1.GenREG5)
anova(BRFSS1.MentREG5)
anova(BRFSS1.PhysREG5)


######### Rattle Results############

rattle()

# General Correlations
#USEMRJN1   X.SMOKER3      X.MRACE1       EMPLOY1      ECIGNOW     PHYSHLTH      X.AGE.G
#USEMRJN1   1.00000000  0.15718681  0.0627885477  0.0236984488 -0.095026182 -0.032067521  0.018083397
#X.SMOKER3  0.15718681  1.00000000  0.0455562764 -0.0293439700 -0.014339116  0.053037140 -0.019680472
#X.MRACE1   0.06278855  0.04555628  1.0000000000 -0.0007007437  0.013774243  0.009832568 -0.041244449
#EMPLOY1    0.02369845 -0.02934397 -0.0007007437  1.0000000000 -0.003636386 -0.107708739  0.458771616
#ECIGNOW   -0.09502618 -0.01433912  0.0137742433 -0.0036363861  1.000000000  0.012588689  0.034069626
#PHYSHLTH  -0.03206752  0.05303714  0.0098325684 -0.1077087386  0.012588689  1.000000000 -0.011763895
#X.AGE.G    0.01808340 -0.01968047 -0.0412444492  0.4587716157  0.034069626 -0.011763895  1.000000000
#WEIGHT2    0.02887360  0.09722706  0.0871714905  0.0101111818  0.007519489  0.018991282  0.011003858
#MAXDRNKS  -0.03393299 -0.08492968  0.0507613073 -0.0224000315 -0.009511834  0.007098585 -0.094540542
#MENTHLTH  -0.01108543  0.06395926  0.0135505287  0.0278792423  0.017039556  0.244631654  0.187768760
#SEX        0.01401106  0.04282294 -0.0052948120  0.0957381328  0.019645878 -0.056031822  0.068404774
#GENHLTH   -0.02292138 -0.10558942  0.0366894155  0.2675926913 -0.014615759 -0.317550445  0.149451929
#X.STATE   -0.07728184 -0.01725868 -0.0075727247 -0.0046385414 -0.003583063 -0.002504171 -0.002311413
#WEIGHT2     MAXDRNKS     MENTHLTH          SEX     GENHLTH       X.STATE
#USEMRJN1  0.0288735990 -0.033932989 -0.011085428  0.014011061 -0.02292138 -0.0772818383
#X.SMOKER3 0.0972270616 -0.084929679  0.063959262  0.042822940 -0.10558942 -0.0172586839
#X.MRACE1  0.0871714905  0.050761307  0.013550529 -0.005294812  0.03668942 -0.0075727247
#EMPLOY1   0.0101111818 -0.022400032  0.027879242  0.095738133  0.26759269 -0.0046385414
#ECIGNOW   0.0075194888 -0.009511834  0.017039556  0.019645878 -0.01461576 -0.0035830626
#PHYSHLTH  0.0189912815  0.007098585  0.244631654 -0.056031822 -0.31755045 -0.0025041713
#X.AGE.G   0.0110038582 -0.094540542  0.187768760  0.068404774  0.14945193 -0.0023114132
#WEIGHT2   1.0000000000  0.022215468  0.028701989  0.109078148  0.01263382  0.0001092471
#MAXDRNKS  0.0222154676  1.000000000 -0.007961221 -0.077425532  0.05576791  0.0022010116
#MENTHLTH  0.0287019890 -0.007961221  1.000000000 -0.090066790 -0.16075075  0.0027647716
#SEX       0.1090781481 -0.077425532 -0.090066790  1.000000000  0.01282268  0.0036720947
#GENHLTH   0.0126338160  0.055767910 -0.160750748  0.012822679  1.00000000  0.0149291009
#X.STATE   0.0001092471  0.002201012  0.002764772  0.003672095  0.01492910  1.0000000000














