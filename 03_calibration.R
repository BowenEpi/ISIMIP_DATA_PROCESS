## CALIBRATION
## using the gfdl-esm4, ssp126, 2015-2020 mean temperature as an example
library(tidyverse);library(raster);library("rgdal");library("rgeos");library(tmap)
library(reshape2);library(ggplot2);library(lubridate);library(splines);library(gridExtra)
library(sf)

##Load dataset for validation
load("data/Aus_validation_ssp126.RData")
ssp_valid <- ssp_valid %>%
  mutate(year=year(DATE),month=month(DATE),
         day_of_year=yday(DATE))

head(ssp_valid)

## Using linear regression model
m1<-lm(TEMP~temp_ssp,data=ssp_valid)
summary(m1)$r.squared##R2=0.7044099
## Note: other predicotrs could also be added

m2<-lm(TEMP~temp_ssp+year++ns(day_of_year,df=6)+ns(LATITUDE,df=3)+
        ns(LONGITUDE,df=3),data=ssp_valid)
summary(m2)$r.squared##R2=0.784

## Using Random forest model
library(ranger)
m3<-ranger(TEMP~temp_ssp+year+day_of_year+LATITUDE+LONGITUDE,data=ssp_valid)
m3$r.squared##R2=0.949

## model performance using 10-fold CV
## split data into 10 folds
station <- unique(ssp_valid[,c(1,3,4)])
station_list<-station$STATION[sample(nrow(station))]##make random order
folds <- cut(1:length(station_list),breaks=10,labels=FALSE)

CV_data1<-list()

for (i in 1:10) {
  # Segement your data by fold
  select_station <- station_list[folds==i]
  testData <- filter(ssp_valid,STATION%in%select_station)
  trainData <- filter(ssp_valid,!(STATION%in%select_station))
  
  # build model and predict
  system.time(m1<-ranger(TEMP~temp_ssp+year+day_of_year+LATITUDE+LONGITUDE,data=trainData))
  testData$temp_pred<-predict(m1,testData)$predictions
  rm(m1)
  gc()
  
  # save data
  CV_data1[[i]]<-testData
  print(i)
}

CV_data1<-bind_rows(CV_data1)

## calculate error
RMSE <- function(observed, predicted) {
  a<-sqrt(mean((predicted - observed)^2, na.rm=TRUE))
  round(a,digits = 3)
}

MAE <- function(observed, predicted) {
  a<-mean(abs(predicted - observed), na.rm=TRUE)
  round(a,digits = 3)
}

MBE <- function(observed, predicted) {
  mean(predicted - observed, na.rm=TRUE)
}

summary(lm(TEMP~temp_pred,CV_data1))$r.squared##0.92
cor(CV_data1[,c("TEMP","temp_pred")])##0.958
RMSE(CV_data1$TEMP,CV_data1$temp_pred)#2.13
MAE(CV_data1$TEMP,CV_data1$temp_pred)#1.55
MBE(CV_data1$TEMP,CV_data1$temp_pred)#0.012


set.seed(10086)
(p <- ggplot(aes(TEMP,temp_pred),data = CV_data1[sample(1:nrow(CV_data1),10^4),]) +
  geom_point()+
  geom_smooth(method = "lm",color="red")+
    labs(x="Observed temperature",y="Calibrated Temperature")+
  theme_classic())

ggsave(p,filename = "Figure/cor_after_validation.png",
       width = 9,height = 6,dpi = 1200)

## save the model
m <- ranger(TEMP~temp_ssp+year+day_of_year+LATITUDE+LONGITUDE,data=ssp_valid)
save(m,file="calibration_model_RF.RData")




