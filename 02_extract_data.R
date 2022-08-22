## EXTRACT THE PROJECTION DATA FOR EACH STATION
library(raster);library(sp);library(ncdf4);library(dplyr);library(sf)
library(lubridate);library(tidyverse)


gridfile <- "data/rawdata/ssp126/gfdl-esm4/tas/gfdl-esm4_r1i1p1f1_w5e5_ssp126_tasAdjust_global_daily_2015_2020.nc"

dailyclimate_year <-  brick(gridfile)

## load station data
load("data/NOAA_Station/aus_station.RData")
station <- unique(all_data[,c(1,3,4)])

station_sf <- st_as_sf(station,
                       coords = c("LONGITUDE", "LATITUDE"),
                       crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

## extract, select the 2015 to make it faster
temp_2015 <- raster::extract(dailyclimate_year[[1:365]],
                             station_sf,methods="simple")


temp_2015_2 <- as.data.frame(temp_2015) %>%
  mutate(STATION=station$STATION) %>%
  gather(time_UTC,temp_ssp,-STATION) %>%
  mutate(y=substr(time_UTC,2,5),
         m=substr(time_UTC,7,8),
         d=substr(time_UTC,10,11))%>%
  mutate(DATE=as_date(paste(y,"-",m,"-",d,sep = "")))%>%
  dplyr::select(STATION,DATE,temp_ssp)


## VALIDATION
all_data$DATE <- as_date(all_data$DATE)
all_data <- all_data[all_data$DATE%in%temp_2015_2$DATE,]

## combine
ssp_valid<-all_data%>%
  left_join(temp_2015_2)%>%
  mutate(temp_ssp=temp_ssp-273.15)%>%
  filter(!(is.na(temp_ssp))|!is.na(TEMP))

## CHECK CORRELATION
cor(ssp_valid[,c("TEMP","temp_ssp")])##0.839

## PLOT
set.seed(10086)
p <- ggplot(aes(TEMP,temp_ssp),data = ssp_valid[sample(1:nrow(ssp_valid),10^4),]) +
  geom_point()+
  geom_smooth(method = "lm",color="red")+labs(x="Observed temperature",y="SSP Temperature")+
  theme_classic()

ggsave(p,filename = "Figure/cor_before_validation.png",
       width = 9,height = 6,dpi = 1200)

## save the validation data
save(ssp_valid,file = "data/Aus_validation_ssp126.RData")


## Extract ssp temperature for Australia
aus_sa4 <-st_read("data/aus_2016_sa4_shape/STE_2016_AUST.shp") %>%
  st_transform("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
aus_sa4 <- aus_sa4[1:8,] ## to make it faster

au_ssp_temp <- raster::extract(dailyclimate_year[[1:365]],
                               aus_sa4,fun=mean,na.rm=T,df=T)

au_ssp_temp2 <- au_ssp_temp%>%
  gather(time_UTC,temp_ssp,-ID) %>%
  mutate(y=substr(time_UTC,2,5),
         m=substr(time_UTC,7,8),
         d=substr(time_UTC,10,11))%>%
  mutate(DATE=as_date(paste(y,"-",m,"-",d,sep = "")))%>%
  dplyr::select(ID,DATE,temp_ssp)

au_ssp_temp2$temp_ssp <- au_ssp_temp2$temp_ssp-273.15

save(au_ssp_temp2,file = "data/Aus_ssp126.RData")