#Script for getting appropriate milkweed (and maybe monarch) data
#Natalie Melkonoff
#nmelkonoff@dbg.org
#2021-06-21


#Loading appropriate packages
library(tidyverse)
library(spocc)
library(mapr)
library(ggmap)
library(scrubr)
library(lubridate)
library(readxl)
library(daymetr)
library(tidync)
library(FedData)
library(ncdf4)
library(dismo)

#register google api for mapping stuff
register_google(key = "AIzaSyDyAqUc4o9p_DOBSF_JOXH5c_JXPqoU4Yw")

#query GBIF for Asclepias records -----------------------------------------------

subulata <- occ(query = "Asclepias subulata*", from ="gbif",
               has_coords = TRUE, limit = 20000)

asperula <- occ(query = "Asclepias asperula*", from = "gbif",
               has_coords = TRUE, limit = 20000)

linaria <- occ(query = "Asclepias linaria*", from = "gbif",
              has_coords = TRUE, limit = 20000)

angustifolia <- occ(query = "Asclepias angustifolia*", from = "gbif",
                   has_coords = TRUE, limit = 20000)

erosa <- occ(query = "Asclepias erosa*", from = "gbif",
            has_coords = TRUE, limit = 20000)

#Aggregate records
subulata_df <- occ2df(subulata)
asperula_df <- occ2df(asperula)
linaria_df <- occ2df(linaria)
angustifolia_df <- occ2df(angustifolia)
erosa_df <- occ2df(erosa)

#find geographic extents of Asclepias

subulata_min_lat <- min(subulata_df$latitude)
subulata_min_long <- min(subulata_df$longitude)
subulata_max_lat <- max(subulata_df$latitude)
subulata_max_long <- max(subulata_df$longitude)

asperula_min_lat <- min(asperula_df$latitude)
asperula_min_long <- min(asperula_df$longitude)
asperula_max_lat <- max(asperula_df$latitude)
aspeula_max_long <- max(asperula_df$longitude)

linaria_min_lat <- min(linaria_df$latitude)
linaria_min_long <- min(linaria_df$longitude)
linaria_max_lat <- max(linaria_df$latitude)
linaria_max_long <- max(linaria_df$longitude)

angustifolia_min_lat <- min(angustifolia_df$latitude)
angustifolia_min_long <- min(angustifolia_df$longitude)
angustifolia_max_lat <- max(angustifolia_df$latitude)
angustifolia_max_long <- max(angustifolia_df$longitude)

erosa_min_lat <- min(erosa_df$latitude)
erosa_min_long <- min(erosa_df$longitude)
erosa_max_lat <- max(erosa_df$latitude)
erosa_max_long <- max(erosa_df$longitude)

#get map
base_map <- get_map(location = c(min_long, min_lat, max_long, max_lat))

#ggmap(base_map)
#plot map with points
ggmap(base_map) + 
  geom_point(data = asperula_df, mapping = aes(x = longitude, y = latitude))

#Initial mapping
#asperula_df %>%
  #mutate(longitude = as.numeric(longitude),
         #latitude = as.numeric(latitude)) %>%
  #map_ggmap()
  #ggmap()

#Lots of naming errors (let's do some filtering)
#swallowtail_df = swallowtail_df %>%
 # filter(str_detect(name, 'Papilio cresphontes'))

#let's check out time periods
#asperula_df %>%
  #mutate(year = year(date)) %>%
  #filter(year >= 1960) %>%
  #group_by(year) %>%
  #summarize(n())

#removing duplicates, filtering older data and restricting data to N. America
subulata_df <- subulata_df %>%
  filter(year(date) > 1959) %>%
  distinct()
  #filter(latitude < 49 & latitude > 28) %>%
  #filter(longitude < -55 & longitude > -127)

asperula_df <- asperula_df %>%
  filter(year(date) > 1959) %>%
  distinct()
  #filter(latitude < 49 & latitude > 28) %>%
  #filter(longitude < -55 & longitude > -127)

linaria_df <- linaria_df %>%
  filter(year(date) > 1959) %>%
  distinct()
  #filter(latitude < 49 & latitude > 28) %>%
  #filter(longitude < -55 & longitude > -127)

angustifolia_df <- angustifolia_df %>%
  filter(year(date) > 1959) %>%
  distinct()
  #filter(latitude < 49 & latitude > 28) %>%
  #filter(longitude < -55 & longitude > -127)

erosa_df <- erosa_df %>%
  filter(year(date) > 1959) %>%
  distinct()
  #filter(latitude < 49 & latitude > 28) %>%
  #filter(longitude < -55 & longitude > -127)

southwest <- get_map("Arizona", zoom = 3)
ggmap(southwest) +
  geom_point(data = subulata_df, aes(x = longitude, y = latitude))

#import monarch data -------------------------------------------------
danaus_plexippus <- occ(query = "Danaus plexippus*", from = "gbif",
               has_coords = TRUE, limit = 20000)

#Aggregating records
monarch_df <- occ2df(danaus_plexippus)

#find geographic extent of monarchs
monarch_min_lat <- min(monarch_df$latitude)
monarch_min_long <- min(asperula_df$longitude)
monarch_max_lat <- max(asperula_df$latitude)
monarch_max_long <- max(asperula_df$longitude)

#get map
monarch_base_map <- get_map(location = c(monarch_min_long, monarch_min_lat, monarch_max_long, monarch_max_lat))

#plot map with monarch points
ggmap(base_map) + 
  geom_point(data = monarch_df, mapping = aes(x = longitude, y = latitude))

#removing duplicates, filtering older data and restricting data to N. America
monarch_df <- monarch_df %>%
  filter(year(date) > 1959) %>%
  distinct() %>%
  filter(latitude < 49 & latitude > 28) %>%
  filter(longitude < -55 & longitude > -127)

ggmap(southwest) +
  geom_point(data = monarch_df, aes(x = longitude, y = latitude))

#generating features that will be useful later on -------- ended up not using this, but maybe need to do something here for prep data function?
# monarch_df_current <- monarch_df %>%
#   mutate(year = year(date),
#          time_frame = ifelse(year >= 2000, "current") %>%
#   select(-data_source))

#importing Bioclim data and cropping ---------------------------- don't actually need to do this, OR DO YOU

bioclim.data <- raster::getData(name = "worldclim",
                                var = "bio",
                                res = 2.5,
                                path = "./data/")

#detemine geographic extent of monarchs
monarch_geographic_extent <- extent(x = c(monarch_min_long, monarch_min_lat, monarch_max_long, monarch_max_lat))

#crop bioclim data to geographic extent of erosa
bioclim.data <- crop(x = bioclim.data, y = monarch_geographic_extent)

#importint CRU files as raster stacks -----------stopped here 
precip_raster <- raster::stack("./data/cru_ts4.05.1901.2020.pre.dat.nc")
tmin_raster <- raster::stack("./data/cru_ts4.05.1901.2020.tmn.dat.nc")
tmax_raster <- raster::stack("./data/cru_ts4.05.1901.2020.tmx.dat.nc")

#precip_raster = raster::stack("./data/cru_ts4.05.1901.2020.pre.dat.nc")
#tmin_raster = raster::stack("./data/cru_ts4.05.1901.2020.tmn.dat.nc")
#tmax_raster = raster::stack("./data/cru_ts4.05.1901.2020.tmx.dat.nc")

#let's try splitting rasters into one time group (2000-2019)

#starting point for current
(2000-1901)

#QC
(1380-1188)/12

precip_raster_current <- precip_raster[[1188:1380]]
tmin_raster_current <- tmin_raster[[1188:1380]]
tmax_raster_current <- tmax_raster[[1188:1380]]

#following Keaton, test on one iteration
precip_test = precip_raster_current[[1:12]]
tmin_test = tmin_raster_current[[1:12]]
tmax_test = tmax_raster_current[[1:12]]

biovars_test = biovars(prec = precip_test,
                       tmin = tmin_test,
                       tmax = tmax_test)

#again following Keaton, this works, outputs a rasterbrick (idk exactly what this code is doing)
biovar_list <- list()
length = dim(precip_raster_current)[3]/12
seq = 1:12

for(i in 1:length) {
  precip_sub = precip_raster_current[[seq]]
  tmin_sub = tmin_raster_current[[seq]]
  tmax_sub = tmax_raster_current[[seq]]
  
  biovar_list[[i]] = biovars(prec = precip_sub,
                             tmin = tmin_sub,
                             tmax = tmax_sub)
  seq = seq + 12
  print(seq)
}

#Workflow 
#pull out a list of raster layers for each bioclim variable
#turn those layers into a rasterStack
#Compute average
#Recombine

biovar_avg_combined_current <- raster::brick(nrows = 360, ncols = 720)
for(i in 1:19) {
  biovar_sublist = lapply(biovar_list, '[[', i) #pulls out each bioclim variable iteratively
  biovar_substack = stack(biovar_sublist) #combines all years into a raster stack
  biovar_avg = calc(biovar_substack, fun = mean) #Calculates the average for each var
  biovar_avg_combined_current[[i]] = biovar_avg #binding each averaged layer back into a brick
}

#comparing structure of calculated bioclim versus what the function outputs
bioclim.data
biovar_avg_combined_current
plot(biovar_avg_combined[[1]])

#future climate data -----------------------------------------------don't do this, taken care of in modeling script, update this code
# #future climate data is read in in modeling script b/c biovars don't need to be calculated, they were downloaded from WorldClim
# 
# precip_raster_future <- raster::stack("./data/wc2.1_2.5m_prec_CNRM-CM6-1_ssp126_2021-2040.tif")
# tmin_raster_future <- raster::stack("./data/wc2.1_2.5m_tmin_CNRM-CM6-1_ssp126_2021-2040.tif")
# tmax_raster_future <- raster::stack("./data/wc2.1_2.5m_tmax_CNRM-CM6-1_ssp126_2021-2040.tif")
# 
# biovar_list <- list()
# length = dim(precip_raster_future)[3]/12
# seq = 1:12
# 
# for(i in 1:length) {
#   precip_sub = precip_raster_future[[seq]]
#   tmin_sub = tmin_raster_future[[seq]]
#   tmax_sub = tmax_raster_future[[seq]]
# 
#   biovar_list[[i]] = biovars(prec = precip_sub,
#                              tmin = tmin_sub,
#                              tmax = tmax_sub)
#   seq = seq + 12
#   print(seq)
# }
# 
# biovar_avg_combined_future <- raster::brick(nrows = 360, ncols = 720)
# for(i in 1:19) {
#   biovar_sublist = lapply(biovar_list, '[[', i) #pulls out each bioclim variable iteratively
#   biovar_substack = stack(biovar_sublist) #combines all years into a raster stack
  #previous version for annual data
  #biovar_avg = calc(biovar_substack, fun = mean) #Calculates the average for each var
  #biovar_avg_combined_future[[i]] = biovar_avg #binding each averaged layer back into a brick
  #modified for data that are already averaged
#   biovar_avg_combined_future[[i]] = biovar_substack
# }

#saving current data object
writeRaster(biovar_avg_combined_current, "./data/biovar_avg_combined_current")

#writing monarch and milkweed records
write.csv(monarch_df, "./data/monarch_data.csv")

write.csv(subulata_df, "./data/subulata_data.csv")
