
### load predictions code first
### Make a map

library(ggmap)
library(sf)
library(ggsn)
library(tidyverse)
library(patchwork)

### load data ###
#load data
Cdata<-read.csv('Data/chemicaldata_maunalua.csv', stringsAsFactors = TRUE)
#remove rows with NAs
Cdata<-Cdata[complete.cases(Cdata),]
Cdata$Tide<-droplevels(Cdata$Tide) #this removes levels that don'e exist anymore (empty spaces for example)
levels(Cdata$Tide)<-c("H","H","L","L") # this makes H1 and H2 both H and same for L1 and L2

# filter out the zones so that it is only diffures, ambient, and transition
Cdata<-Cdata %>% 
  dplyr::filter(Zone != 'Offshore', Tide =="H", Day_Night =="Day", Season =="FALL") %>%
  droplevels()
# only pull out one point for location

###### Make Map ############
register_google(key = "") ### use your own API in between the ""


# Wailupe point
WP<-data.frame(lon = -157.7621, lat = 21.27427)

#Blackpoint point
BP<-data.frame(lon = -157.7898, lat = 21.25891 )

M1<-get_map(WP,zoom = 17, maptype = 'satellite')

Wmap<-ggmap(M1)+
  scalebar(x.min = -157.768, x.max = -157.757,y.min = 21.2690, y.max = 21.2800,
           model = 'WGS84', box.fill = c("yellow", "white"), st.color = "white",
           location =  "bottomleft", transform = TRUE, dist_unit = "km", dist = .25)+
  
  geom_point(data = Cdata, mapping = aes(x=Long, y=Lat), size=2,color = 'yellow')+
  ggtitle('Wailupe')+
  xlab("")+
  ylab("")

M2<-get_map(BP,zoom = 18, maptype = 'satellite')
BPmap<- ggmap(M2)+
  scalebar(x.min = -157.796, x.max = -157.784,y.min = 21.2540, y.max = 21.2650,
           model = 'WGS84', box.fill = c("yellow", "white"), st.color = "white",
           location =  "bottomleft", transform = TRUE, dist_unit = "km", dist = .25)+
  
  geom_point(data = Cdata, mapping = aes(x=Long, y=Lat), size=2,color = 'yellow')+
  ggtitle('Black Point')+
  xlab("")+
  ylab("")

M3<-get_map('Oahu',zoom = 10, maptype = 'satellite')
Oahumap<- ggmap(M3)+
  scalebar(x.min = -158.4, x.max = -157.6,y.min = 21.1, y.max = 21.8,
           model = 'WGS84', box.fill = c("yellow", "white"), st.color = "white",
           location =  "topleft", transform = TRUE, dist_unit = "km", dist = 20)+
  geom_point(data = BP, mapping = aes(x=lon, y=lat), size=4,color = 'yellow')+
  geom_point(data = WP, mapping = aes(x=lon, y=lat), size=4,color = 'yellow')+
  xlab("")+
  ylab("")

(BPmap+Wmap) +ggsave("Output/SiteMap.pdf", width = 10, height = 8)

Oahumap+ggsave("Output/OahuMap.pdf", width = 10, height = 8)

### Map with predictions data. Need to source the predictioncode.R first 
#Wailupe
Wmappredict<-function(category){ggmap(M1)+
    # scalebar(x.min = -157.768, x.max = -157.757,y.min = 21.2690, y.max = 21.2800,
    #          model = 'WGS84', box.fill = c("yellow", "white"), st.color = "white",
    #          location =  "bottomleft", transform = TRUE, dist_unit = "km", dist = .25)+
    # 
    geom_point(data = mappredictions %>%filter(Site =="Wailupe", .category ==category), 
               mapping = aes(x=Long, y=Lat, color = mean.val_per), size=4)+
    #  ggtitle('Wailupe')+
    xlab("")+
    ylab("")}

#50% decrease
Wmappredict_neg<-function(category){ggmap(M1)+
    # scalebar(x.min = -157.768, x.max = -157.757,y.min = 21.2690, y.max = 21.2800,
    #          model = 'WGS84', box.fill = c("yellow", "white"), st.color = "white",
    #          location =  "bottomleft", transform = TRUE, dist_unit = "km", dist = .25)+
    # 
    geom_point(data = mappredictions %>%filter(Site =="Wailupe", .category ==category), 
               mapping = aes(x=Long, y=Lat, color = mean.val_perneg50), size=4)+
    #  ggtitle('Wailupe')+
    xlab("")+
    ylab("")}

WpH<-Wmappredict(category = "pHstd")+
  scale_color_gradient2(low = "#d8b365", high = "#5ab4ac", midpoint = 0,mid = "white", name = expression(paste("%",Delta,"pH")),
                        limits = c(-0.4,0.4))

WpH_neg<-Wmappredict_neg(category = "pHstd")+
  scale_color_gradient2(low = "#d8b365", high = "#5ab4ac", midpoint = 0,mid = "white", name = expression(paste("%",Delta,"pH")),
                        limits = c(-0.4,0.4)
  )

WNEC<-Wmappredict(category = "TAdiffstd")+
  scale_color_gradient2(low = "#8c510a", high = "#01665e", midpoint = 0,mid = "white", name = expression(paste("%",Delta,"NEC")),
                        limits = c(-15,15)
  )
WNEC_neg<-Wmappredict_neg(category = "TAdiffstd")+
  scale_color_gradient2(low = "#8c510a", high = "#01665e", midpoint = 0,mid = "white", name = expression(paste("%",Delta,"NEC")),
                        limits = c(-15,15)
  )


WNEP<-Wmappredict(category = "DICdiffstd")+
  scale_color_gradient2(low = "#c51b7d", high = "#4d9221", midpoint = 0,mid = "white", name = expression(paste("%",Delta,"NEP")),
                        limits = c(-20,20)
  )
WNEP_neg<-Wmappredict_neg(category = "DICdiffstd")+
  scale_color_gradient2(low = "#c51b7d", high = "#4d9221", midpoint = 0,mid = "white", name = expression(paste("%",Delta,"NEP")),
                        limits = c(-20,20)
  )
WTemp<-Wmappredict(category = "Tempinstd")+
  scale_color_gradient2(low = "darkgreen", high = "red", midpoint = 0,mid = "white", name = expression(paste("%",Delta,"Temperature")),
                        limits = c(-1,2)
  )
WTemp_neg<-Wmappredict_neg(category = "Tempinstd")+
  scale_color_gradient2(low = "darkgreen", high = "red", midpoint = 0,mid = "white", name = expression(paste("%",Delta,"Temperature")),
                        limits = c(-1,2)
  )


WlogNN<-Wmappredict(category = "logNNstd")+
  scale_color_gradient2(low = "#1b7837", high = "#762a83", midpoint = 0,mid = "white", name = expression(paste("%",Delta,"logNN")),
                        limits = c(-90,80)
  )
WlogNN_neg<-Wmappredict_neg(category = "logNNstd")+
  scale_color_gradient2(low = "#1b7837", high = "#762a83", midpoint = 0,mid = "white", name = expression(paste("%",Delta,"logNN")),
                        limits = c(-90,80)
  )

#Black Point
BPmappredict<-function(category){ggmap(M2)+
    geom_point(data = mappredictions %>%filter(Site =="Kupikipiki'o", .category ==category), mapping = aes(x=Long, y=Lat, color = mean.val_per), size=4)+
    xlab("")+
    ylab("")
}
BPmappredict_neg<-function(category){ggmap(M2)+
    geom_point(data = mappredictions %>%filter(Site =="Kupikipiki'o", .category ==category), mapping = aes(x=Long, y=Lat, color = mean.val_perneg50), size=4)+
    xlab("")+
    ylab("")
}

BPpH<-BPmappredict(category = "pHstd")+
  scale_color_gradient2(low = "#d8b365", high = "#5ab4ac", midpoint = 0,mid = "white", name = expression(paste("%",Delta,"pH")),
                        limits = c(-0.8,0.4)
  )

BPpH_neg<-BPmappredict_neg(category = "pHstd")+
  scale_color_gradient2(low = "#d8b365", high = "#5ab4ac", midpoint = 0,mid = "white", name = expression(paste("%",Delta,"pH")),
                        limits = c(-0.8,0.4)
  )


BPNEC<-BPmappredict(category = "TAdiffstd")+
  scale_color_gradient2(low = "#8c510a", high = "#01665e", midpoint = 0,mid = "white", name = expression(paste("%",Delta,"NEC")),
                        limits = c(-10,15),
  )
BPNEC_neg<-BPmappredict_neg(category = "TAdiffstd")+
  scale_color_gradient2(low = "#8c510a", high = "#01665e", midpoint = 0,mid = "white", name = expression(paste("%",Delta,"NEC")),
                        limits = c(-10,15)
  )

BPNEP<-BPmappredict(category = "DICdiffstd")+
  scale_color_gradient2(low = "#c51b7d", high = "#4d9221", midpoint = 0,mid = "white", name = expression(paste("%",Delta,"NEP")),
                        limits = c(-200,250),
  )
BPNEP_neg<-BPmappredict_neg(category = "DICdiffstd")+
  scale_color_gradient2(low = "#c51b7d", high = "#4d9221", midpoint = 0,mid = "white", name = expression(paste("%",Delta,"NEP")),
                        limits = c(-200,250)
  )

BPTemp<-BPmappredict(category = "Tempinstd")+
  scale_color_gradient2(low = "darkgreen", high = "red", midpoint = 0,mid = "white", name = expression(paste("%",Delta,"Temperature")),
                        limits = c(-1,1)
  )
BPTemp_neg<-BPmappredict_neg(category = "Tempinstd")+
  scale_color_gradient2(low = "darkgreen", high = "red", midpoint = 0,mid = "white", name = expression(paste("%",Delta,"Temperature")),
                        limits = c(-1,1)
  )

BPlogNN<-BPmappredict(category = "logNNstd")+
  scale_color_gradient2(low = "#1b7837", high = "#762a83", midpoint = 0,mid = "white", name = expression(paste("%",Delta,"logNN")),
                        limits = c(-150,80)
  )
BPlogNN_neg<-BPmappredict_neg(category = "logNNstd")+
  scale_color_gradient2(low = "#1b7837", high = "#762a83", midpoint = 0,mid = "white", name = expression(paste("%",Delta,"logNN")),
                        limits = c(-150,80)
  )

(BPNEP+WNEP)/(BPNEC+WNEC) +plot_annotation(tag_levels = "A")+
  plot_layout(guides = 'collect') +
  ggsave("Output/MapPredictions_ecometab.pdf", height = 10, width = 10, useDingbats = FALSE)

(BPpH +WpH)/(BPTemp+WTemp)/(BPlogNN+WlogNN)+plot_annotation(tag_levels = "A")+
  plot_layout(guides = 'collect') +
  ggsave("Output/MapPredictions_enviro.pdf", height = 12, width = 10, useDingbats = FALSE)

## plots by % SGD
(WNEP_neg + WNEP)/(WNEC_neg +WNEC)+plot_annotation(tag_levels = "A")+
  plot_layout(guides = 'collect')+
  ggsave("Output/MapPredictions_Wailupe.pdf", height = 10, width = 10, useDingbats = FALSE)

(BPNEP_neg + BPNEP)/(BPNEC_neg +BPNEC)+plot_annotation(tag_levels = "A")+
  plot_layout(guides = 'collect')+
  ggsave("Output/MapPredictions_Kupi.pdf", height = 10, width = 10, useDingbats = FALSE)


(BPpH_neg +BPpH)/(BPTemp_neg+BPTemp)/(BPlogNN_neg+BPlogNN)+plot_annotation(tag_levels = "A")+
  plot_layout(guides = 'collect') +
  ggsave("Output/MapPredictions_enviro_kupi.pdf", height = 12, width = 10, useDingbats = FALSE)

(WpH_neg +WpH)/(WTemp_neg+WTemp)/(WlogNN_neg+WlogNN)+plot_annotation(tag_levels = "A")+
  plot_layout(guides = 'collect') +
  ggsave("Output/MapPredictions_enviro_W.pdf", height = 12, width = 10, useDingbats = FALSE)
