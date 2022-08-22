## CALIBRATE THE PROJECTION DATA USING THE CALIBRATION MODEL
library(ranger);library(tidyverse);library(ggpubr)

load("calibration_model_RF.RData")
load("data/Aus_ssp126.RData")

## 
aus_sa4 <-st_read("data/aus_2016_sa4_shape/STE_2016_AUST.shp") %>%
  st_transform("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
aus_sa4 <- aus_sa4[1:8,] ## to make it faster

state_center <- st_centroid(aus_sa4)
aus_centre_data<-st_coordinates(state_center)%>%
  as.data.frame()%>%##get lon an lat
  bind_cols(state_center[,1:2])%>%
  as.data.frame()

names(aus_centre_data)[1:3] <- c("LONGITUDE","LATITUDE","ID")
aus_centre_data$ID <- as.numeric(aus_centre_data$ID)

## ADD LAT AND LON OF CENTROID
au_ssp_temp <- au_ssp_temp2 %>%
  mutate(year=year(DATE),month=month(DATE),
         day_of_year=yday(DATE)) %>%
  left_join(aus_centre_data[,1:3])

au_ssp_temp$temp_valid<-predict(m,au_ssp_temp)$predictions


data <- au_ssp_temp %>%
  group_by(DATE) %>%
  summarise(temp_ssp=mean(temp_ssp),
            temp_valid=mean(temp_valid)) %>%
  gather(Group,temp,-DATE)

data$Group <- factor(data$Group,levels = c("temp_ssp","temp_valid"),
                     labels = c("Uncalibrated", "Calibrated"))

(p <- ggplot(data) +
  geom_line(aes(DATE,temp,color=Group)) +
  labs(x="Date",y="Temperature")+
    theme_pubr()+
    theme(axis.title.x=element_text(size=18,family = "serif" ),
          plot.title = element_text(size = 18,family = "serif"),
          axis.title.y=element_text(size=16,family = "serif"),
          axis.text.x=element_text(size=16,family = "serif"),
          axis.text.y=element_text(size=16, family = "serif"),
          strip.text = element_text(size=16, family = "serif"),
          legend.title = element_text(size=16,family = "serif"),
          legend.text = element_text(size=16,family = "serif"),
          legend.key.width=unit(13,'mm'),legend.text.align = 0,
          legend.position = "top")+
  ggsci::scale_color_lancet(name=""))

ggsave(p,filename = "Figure/Temperature_series.png",
       width = 11,height = 6,dpi = 1200)
