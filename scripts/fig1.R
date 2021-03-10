# Figures 

# Map

library(sp)
library(sf)
library(tidyverse)
library(ggmap)
library(ggplot2)

pond_masks = dir("./data/mask_nic_20191126", pattern=".shp",full.names=T)

ids = pond_masks %>% 
  str_remove("(.*)/") %>% 
  str_remove("_(.*)")

pm = st_read(pond_masks[1])[,-1] %>% 
  mutate("id"=ids[1])

for(i in 2:length(pond_masks)){
  pmx = st_read(pond_masks[i])[,-1] %>% 
    mutate("id"=ids[i])
  pm = rbind(pm,pmx)
  }

bm = ggmap::get_stamenmap(bbox =  bbox(as_Spatial(pm)) + matrix(data=c(-0.005,-0.005,0.005,0.005), ncol=2),
                          maptype = "terrain",
                          zoom = 15)


ggmap(bm)+
  geom_sf(data = pm,
          # aes(fill = id),
          inherit.aes = F) +
  geom_sf_label(data = pm,
            aes(label = id),
            size = 3,
            inherit.aes = F)
