## PREPARE THE OBSERVED TEMPERATURE FROM WEATHER STATION
library(raster);library(sp);library(ncdf4);library(dplyr)
library(lubridate);library(ggplot2);library(sf);
library(ggpubr);library(ggsci);library(scales);library(viridis)


## PROCESS STATION DATA
## read station list
station<-read.csv("data/NOAA_Station/isd-history.csv")
summary(station)

## Select station of Australia
region<-c(96,168,-44,-9)##lon: min/max; lat: min/max 
time_frame<-c(as.Date("2015-01-01"),as.Date("2020-12-31"))
year<-c(2015:2020)

## select region
station_select<-station%>%
  filter(LON>=region[1]&LON<=region[2]&
           LAT>=region[3]&LAT<=region[4]&CTRY=="AS")%>%
  mutate(BEGIN_date=as.Date(paste(substr(BEGIN,1,4),
                                  substr(BEGIN,5,6),
                                  substr(BEGIN,7,8),sep = "-")))%>%
  mutate(END_date=as.Date(paste(substr(END,1,4),
                                substr(END,5,6),
                                substr(END,7,8),sep = "-")))%>%
  filter(!(BEGIN_date>time_frame[2]|END_date<time_frame[1]))

plot(station_select$LON,station_select$LAT)


## import station data
station_select<-station_select%>%
  mutate(c=paste(USAF,WBAN,sep = ""))%>%
  mutate(begin_year=year(BEGIN_date),
         end_year=year(END_date))

all_data <- data.frame()

## read by year
for (i in 2015:2020) {
  noaa_file <- paste0("data/NOAA_Station/",i)
  aus_files <- paste0(station_select$c[station_select$begin_year<=i&station_select$end_year>=i],".csv")
  aus_files <- aus_files[aus_files%in%list.files(noaa_file)]
  file<-paste0(noaa_file,"/",aus_files)
  data_list<-lapply(file,read.csv)
  
  ## select necessary variables
  var_list <- c("STATION","DATE","LATITUDE","LONGITUDE","NAME","TEMP")
  data_list<-lapply(data_list, function(x) x[,var_list])
  ## assign same attribute
  sharedColTypes<-sapply(data_list[[1]], class)
  matchColClasses<- function(df2){
    for (i in 1:length(var_list)) {
      class(df2[,i]) <- sharedColTypes[i]
    }
    return(df2)
  }
  data_list<-lapply(data_list, matchColClasses)
  
  ## to data.frame
  data_list2<-bind_rows(data_list)
  
  all_data <- rbind(all_data,data_list2)
}

## transform to Celsius degree
all_data$TEMP <- (all_data$TEMP-32)/1.8
summary(all_data$TEMP)

## save
save(all_data,file = "data/NOAA_Station/aus_station.RData")

## PLOT FILE
aus_sa4 <-st_read("data/aus_2016_sa4_shape/STE_2016_AUST.shp")
aus_sa4 <- st_transform(aus_sa4,"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

station <- unique(all_data[,c(1,3,4)])

# Remove plot axis
no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank(),
                 panel.grid = element_blank(),legend.position = c(0.8,0.2),
                 legend.text = element_text(size = 12),legend.title = element_text(size = 13,face = "bold"))

## plot station
p.station <- ggplot() +
  geom_sf(data=aus_sa4, color="grey30", size=0.1, show.legend = T) +
  geom_point(aes(x=LONGITUDE,y=LATITUDE),data=station)+
  theme_minimal() +
  theme(title = element_text(size = 10,family = "serif",face = "bold"),
        text = element_text(size = 10,family = "serif"))+
  no_axis


ggsave(p.station,filename = "Figure/NOAA_station_australia.png",
       width = 9,height = 6,dpi = 1200)
