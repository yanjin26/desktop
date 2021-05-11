###11052021
###for testing the branches
#
##
rm(list = ls(all = TRUE))             # Clear workspace
setwd("C:/Users/Yan.Jin/Desktop/NFS paper_LCSFM")
library(tidyverse)
library(dplyr)
library(pastecs)
library(tidyverse)
library(vroom)
library(fs)
library(glue)
library(sfaR)
library(ggplot2)
library(EnvStats)
library(car)
library(REdaS)
library(devtools)
library(factoextra)

data <- read.csv("data_try.csv")
adjustment <- read.csv("index adjustment.csv")
data2 <- read.csv("data_try2.csv")
#count(data[data$farmgo<0,])
data <- data %>% filter(farmgo > 0)       #exclude those without gross output or negative output
#data <- data %>% filter(ftotallu != 0)    #exclude those without herd size
#data <- data %>% filter(D_TOTAL_MILK_PRODUCTION_LT != 0) #exclude those that are not dairy farms
#data <- data %>% filter(HOURS_WORKED != 0)      #exclude those without labour AWU
#data <- data %>% filter(fainvfrm != 0)      #exclude those without estimated value of farm
#using average to approximate the missing labour and investment
data <- left_join(data,adjustment,by="year")
data <- left_join(data, data2, by=c("year", "farmcode"))

#count(data[data$fdairygo>0,])
#count(data[data$fcatlego>0,])
#count(data[data$fsheepgo>0,])
#count(data[data$fcropsgo>0,])
#count(data[data$fcatlego>0 & data$fdairygo <=0 & data$fsheepgo <=0,])
##########################################
#   only focus on dairy farms
##########################################
#data <- data %>% filter(fdairygo>0)
##########################################
#   only focus on cattle farms
##########################################
data <- data %>% filter(fcatlego>0)
##########################################
#   only focus on sheep farms
##########################################
#data <- data %>% filter(fsheepgo>0)
##########################################
#   only focus on tillage farms
##########################################
#data <- data %>% filter(fcropsgo>0)
#extracting variables in the production function
#data$output <- (data$farmgo-data$dirpayts) / data$output_price_index
#data <- data[data$output>0,]
data$LU <- data$ftotallu 
data$LU_cattle <- data$cpnolu
data$output <- (data$fcatlego - data$dirpayts * (data$LU_cattle/data$LU) )/ data$output_price_index
data <- data[data$output>0,]
#data$output <- data$farmgo / data$output_price_index

data$yield <- data$output / data$LU_cattle
data$land <- data$fsizuaa * (data$LU_cattle/data$LU)
data$landvalue_ha <- data$lnlandval_ha1
data <- data[complete.cases(data[,"landvalue_ha"]),] 
data$intensity <- data$output / data$land
data$labour <- data$HOURS_WORKED * (data$LU_cattle/data$LU)
data$capital <- (data$fainvfrm - data$fainvlst) * (data$LU_cattle/data$LU) / data$input_price_index
data <- data[data$capital>0,]
data$capital_ha <- data$capital / data$land
#consumption for cattle, estimated pro rata of LU
data$consumption <- data$farmdc / data$input_price_index
data$consumption_cattle <- data$consumption * (data$LU_cattle/data$LU)
#data$consumption_ha <- data$consumption / data$land
data$consumption_cattle_LU <- data$consumption_cattle / data$LU_cattle
#extracting inefficient determinants
data$lfa[data$lfa!=0] <- 1
data$AES <- data$fsubreps / data$land / data$output_price_index
data$operational_subsidies <- (data$dirpayts-data$AES) / data$land / data$output_price_index
data$hired_labour <- data$D_LABOUR_UNITS_PAID / data$D_LABOUR_UNITS_TOTAL * (data$LU_cattle/data$LU)

data <- data[complete.cases(data[,"hired_labour"]),]    #excluding both D_LABOUR_UNITS_PAID and D_LABOUR_UNITS_TOTAL==0 and create Na=0/0
data$rented_land <- data$LAND_RENTED_IN_HA / data$land * (data$LU_cattle/data$LU)
data$stocking_density <- data$LU /data$land
data$age <- data$ogagehld
data$education <- data$FORMAL_AGRICULTURAL_TRAINING_YN
data$concentrate_LU <- data$CONC_PURCHASED_50KGBAGS_NO / data$LU
data <- data[complete.cases(data[,"concentrate_LU"]),] 
#data$dairy_forage <- data$daforare
#data <- data[data$dairy_forage>0,]
data$cattle_forage <- data$cpforacs
data <- data[data$cattle_forage>0,]
#data$sheep_forage <- data$spforacs
#data <- data[data$sheep_forage>0,]
#data$tillage <- data$tillage_area
#data <- data[data$tillage>0,]

#extracting agronomic variables
#replace NAs with 0 for physio variables
#physio <- read.csv("physio.csv")
#physio[is.na(physio)] <- 0
#write.csv(physio, "physio_clean.csv")
data$Conrad_index <- data$cntnental
data <- data[data$Conrad_index != -9999,]
data$distance <- data$d2sea
data$grass_growth <- data$spgrassgr
data$grass_cover <- data$sgrasscov
data$temperature <- data$temp
data$rainfall <- data$rain

attach(data)
nfs <- data.frame(farmcode, year, output, LU, LU_cattle, land, labour, capital, consumption, consumption_cattle,consumption_cattle_LU,
                  lfa, region, AES, operational_subsidies, hired_labour, rented_land, stocking_density, age, education, concentrate_LU, prin_1, prin_2, prin_3, prin_4, 
                  prin_5, prin_6, prin_7, prin_8, prin_9, prin_10, prin_12, prin_13, prin_14, prin_15, prin_16, prin_17, prin_18, 
                  prin_19, prin_20, prin_21, prin_23, prin_24, prin_26, prin_28, physio_1, physio_2, physio_3, physio_5, 
                  physio_7, Conrad_index, distance, grass_growth, grass_cover, temperature, rainfall, yield, intensity, landvalue_ha, 
                  capital_ha, cattle_forage)
detach(data)
###averaging the separating variables over time for each obs
myvars2 <- nfs %>% 
  group_by(farmcode) %>% 
  summarise_at(c("temperature","rainfall","stocking_density","concentrate_LU"), mean)

myvars2 <- myvars2 %>% 
  rename(temperature_ave = temperature,
         rainfall_ave = rainfall,
         stocking_density_ave = stocking_density,
         concentrate_LU_ave = concentrate_LU)
nfs <- left_join(nfs,myvars2,by="farmcode")


nfs <- nfs %>% filter(year != 2015)       #due to missing values for AES in 2015
summary(nfs)
count(data)


#discriptive statistics
format(stat.desc(nfs), scientific=FALSE, digit=1)
des_sta <- stat.desc(nfs)
#write.csv(des_sta, "des_sta.csv")
#checking the mean
check <- nfs %>% group_by(year) %>% summarise(across(everything(), list(mean)))
write.csv(check, "check.csv")

#################################
#     OLS
#################################
CD_formula <- formula(
  log(yield)~ 
    #log(land)+
    log(LU_cattle)+
    log(labour)+
    log(capital)+
    log(consumption_cattle)+
    prin_1 + prin_2 + prin_2 + prin_3 + prin_4 + prin_5 + prin_6 + prin_7 + prin_8 + 
    prin_9 + prin_10 + prin_12 + prin_13 + prin_14 + prin_15 + prin_16 + prin_17 + prin_18 + 
    prin_19 + prin_20 + prin_21 + prin_23 + prin_24 + prin_26 + prin_28 + 
    physio_1 + physio_2 + physio_3 + physio_5 + physio_7 + 
    Conrad_index + distance + grass_growth + log(grass_cover) + temperature + rainfall +
    lfa + year + region
)
CD_formula <- formula(
  log(intensity)~ 
    log(land)+
    log(LU_cattle)+
    log(labour)+
    log(capital)+
    log(consumption_cattle)+
    prin_1 + prin_2 + prin_2 + prin_3 + prin_4 + prin_5 + prin_6 + prin_7 + prin_8 + 
    prin_9 + prin_10 + prin_12 + prin_13 + prin_14 + prin_15 + prin_16 + prin_17 + prin_18 + 
    prin_19 + prin_20 + prin_21 + prin_23 + prin_24 + prin_26 + prin_28 + 
    physio_1 + physio_2 + physio_3 + physio_5 + physio_7 + 
    Conrad_index + distance + grass_growth + grass_cover + temperature + rainfall +
    lfa + year + region
)
CD_formula <- formula(
  log(output)~ 
    #log(land)+
    log(LU_cattle)+
    log(labour)+
    log(capital)+
    log(consumption_cattle)+
    #prin_1 + prin_2 + prin_2 + prin_3 + prin_4 + prin_5 + prin_6 + prin_7 + prin_8 + 
    #prin_9 + prin_10 + prin_12 + prin_13 + prin_14 + prin_15 + prin_16 + prin_17 + prin_18 + 
    #prin_19 + prin_20 + prin_21 + prin_23 + prin_24 + prin_26 + prin_28 + 
    physio_1 + physio_2 + physio_3 + physio_5 + physio_7 + 
    #Conrad_index + 
    #distance + grass_growth + 
    grass_cover + temperature + rainfall +
    year +
    lfa +
    region
)
ols_CD <- lm(CD_formula, data = nfs)
summary(ols_CD)
vif(ols_CD)
res_ols <- resid(ols_CD)

par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(ols_CD)
par(mfrow = c(1, 1))
plot(fitted(ols_CD), res_ols)
abline(0,0)

par(mfrow = c(1, 1))
qqnorm(res_ols)
qqline(res_ols)

#################################
#       SFM
#################################
SF_CD <- sfacross(formula = CD_formula,
                  uhet =~ operational_subsidies + AES + hired_labour + rented_land + age, 
                  data = nfs)
summary(SF_CD)

###################################
#       LCSFA
###################################
model <- lcmcross(formula = CD_formula,
                  uhet =~ operational_subsidies + AES + hired_labour + rented_land + age, 
                  data = nfs, printInfo = TRUE)
summary(model)

model <- lcmcross(formula = CD_formula,
                  thet =~ stocking_density + concentrate_LU + temperature + rainfall + region,
                  uhet =~ operational_subsidies + AES + hired_labour + rented_land + age, 
                  data = nfs, printInfo = TRUE)
summary(model)

#thet =~ stocking_density + concentrate_LU + temperature + rainfall + region +
#prin_1 + prin_2 + prin_2 + prin_3 + prin_4 + prin_5 + prin_6 + prin_7 + prin_8 + 
#prin_9 + prin_10 + prin_12 + prin_13 + prin_14 + prin_15 + prin_16 + prin_17 + prin_18 + 
#prin_19 + prin_20 + prin_21 + prin_23 + prin_24 + prin_26 + prin_28 + 
#physio_1 + physio_2 + physio_3 + physio_5 + physio_7 + 
#Conrad_index + distance + grass_growth + grass_cover,

#efficiency
effi <- efficiencies(model)
head(effi)
table(effi$Group_c==1)

summary(effi$teJLMS_c[effi$Group_c==1])
summary(effi$teJLMS_c[effi$Group_c==2])
summary(effi$PosteriorProb_c1[effi$Group_c==1])
summary(effi$PosteriorProb_c2[effi$Group_c==2])

#index of those in class 1
c1 <- match(effi$teJLMS_c[effi$Group_c==1],effi$teJLMS_c)
#head(myvars$lg_input_land[c1])
pro_parameter <- nfs %>% select(land,stocking_density,labour,capital,consumption,
                                   LFA,D2013,D2014,region,operational_subsidies,hired_labour,rented_land,
                                   output, LU, yield, net_value_added, 
                                   private_revenue_cost_ratio,
                                   public_revenue_cost_ratio,
                                   private_revenue_cost_ratio_remuneration,
                                   public_revenue_cost_ratio_remuneration,
                                   product_land,product_labour,product_capital,product_expense,market_orientation,equity_ratio
                                )
#pro_parameter[c1,]
summary(pro_parameter[c1,])
c2 <- match(effi$teJLMS_c[effi$Group_c==2],effi$teJLMS_c)
summary(pro_parameter[c2,])
summary(fadn$share_permanent_grassland)

###performing sample t test of mean equality
output1 <- nfs$output[effi$Group_c==1]
output2 <- nfs$output[effi$Group_c==2]
t.test(output1, output2)



























count(data[data$ftotallu>0,])

count(data[data$cpnolu>0,])
count(data[data$cpnolu>0 & data$spnolu>0,])
count(data[data$fdairygo<0,])


















summary(data$FORMAL_AGRICULTURAL_TRAINING_YN)

count(data[data$farmgo <0,])
count(data[data$fainvfrm==0 & data$year==2015,])
count(data[data$farmdc==0,])
count(data)
list(is.na(nfs))      #land value of 19 farms in 2013 cannot be approximated
which(is.na(nfs$D_LABOUR_UNITS_PAID))
nfs[is.na(data$D_LABOUR_UNITS_PAID),]
nfs[23,]
unique(data$farmcode[data$fainvfrm==0])
unique(data$farmcode[data$HOURS_WORKED==0])


sum(is.na(data$hired_labour))
sum(is.na(data$D_LABOUR_UNITS_PAID))
sum(is.na(data$D_LABOUR_UNITS_TOTAL))

count(data)
count(data$D_LABOUR_UNITS_TOTAL)

count(data[data$D_LABOUR_UNITS_PAID==0 & data$D_LABOUR_UNITS_TOTAL==0,])












###approximate missing land value for 2013
#calculate the mean of land value in 2014 and 2015 for each farm and rename it
approximate <- fadn %>% 
  filter(YEAR != 2013) %>% 
  group_by(id) %>% 
  summarise_at("ALNDAGR_CV", mean)
approximate <- approximate %>% 
  rename(ALNDAGR_CV_ave = ALNDAGR_CV)

#join the average with fadn2013 by matching the farm's id
fadn2013 <- left_join(fadn2013,approximate,by="id")
str(fadn2014)

#check the number of farms in 2013 but not in 2014 or 2015, delete those farms
sum(is.na(fadn2013$ALNDAGR_CV_ave))      #land value of 19 farms in 2013 cannot be approximated
fadn2013 <- fadn2013[complete.cases(fadn2013),]

#replace land value in those matched farms with the mean of 2014 and 2015
fadn2013$ALNDAGR_CV <- fadn2013$ALNDAGR_CV_ave
#delete the extra col and combine with fadn2014 and fadn2015
fadn2013$ALNDAGR_CV_ave <- NULL
fadn <- rbind(fadn2013, fadn2014, fadn2015)

fadn$ALNDAGR_CV[YEAR==2013]  <-  approximate$ALNDAGR_CV_ave
fadn <- rbind(fadn2013, fadn2014, fadn2015)

nfs <- data.frame(year, output, LU, land, labour, capital, consumption, lfa, region, AES, operational_subsidies,
                  hired_labour, rented_land, stocking_density, age, education, concentrate_LU, prin_1, prin_2, prin_3, prin_4, 
                  prin_5, prin_6, prin_7, prin_8, prin_9, prin_10, prin_12, prin_13, prin_14, prin_15, prin_16, prin_17, prin_18, 
                  prin_19, prin_20, prin_21, prin_23, prin_24, prin_26, prin_28, physio_1, physio_2, physio_3, physio_5, 
                  physio_7, Conrad_index, distance, grass_growth, grass_cover, temperature, rainfall, yield, intensity, landvalue_ha, 
                  capital_ha, consumption_ha, cattle_forage)

###Principal Component Analysis
cor(nfs[c("prin_1", "prin_2", "prin_3", "prin_4", 
          "prin_5", "prin_6", "prin_7", "prin_8", "prin_9", "prin_10", "prin_12", "prin_13", "prin_14", "prin_15", "prin_16", 
          "prin_17", "prin_18", 
          "prin_19", "prin_20", "prin_21", "prin_23", "prin_24", "prin_26", "prin_28", "physio_1", "physio_2", "physio_3", "physio_5", 
          "physio_7","rainfall","temperature","landvalue_ha" )])

KMOS(nfs[c("prin_1", "prin_2", "prin_3", "prin_4", 
           "prin_5", "prin_6", "prin_7", "prin_8", "prin_9", "prin_10", "prin_12", "prin_13", "prin_14", "prin_15", "prin_16", 
           "prin_17", "prin_18", 
           "prin_19", "prin_20", "prin_21", "prin_23", "prin_24", "prin_26", "prin_28", "physio_1", "physio_2", "physio_3", "physio_5", 
           "physio_7","rainfall","temperature","landvalue_ha" )], use = "pairwise.complete.obs")
pca <- prcomp(nfs[,c("prin_1", "prin_2", "prin_3", "prin_4", 
              "prin_5", "prin_6", "prin_7", "prin_8", "prin_9", "prin_10", "prin_12", "prin_13", "prin_14", "prin_15", "prin_16", 
              "prin_17", "prin_18", 
              "prin_19", "prin_20", "prin_21", "prin_23", "prin_24", "prin_26", "prin_28", "physio_1", "physio_2", "physio_3", "physio_5", 
              "physio_7","rainfall","temperature","landvalue_ha")], center=TRUE, scale = TRUE)
summary(pca)

#var_explained = pca$sdev^2 / sum(pca$sdev^2)
#library(ggplot2)
#qplot(c(1:32), var_explained) + 
#  geom_line() + 
#  xlab("Principal Component") + 
#  ylab("Variance Explained") +
#  ggtitle("Scree Plot") +
#  ylim(0, 1)

get_eig(pca)
fviz_eig(pca, addlabels=TRUE, ncp = 32)
fviz_eig(pca, choice = "eigenvalue", addlabels=TRUE,ncp = 32)
