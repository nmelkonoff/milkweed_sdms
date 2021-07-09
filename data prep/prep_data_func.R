#Function to bring in dataframes and prepare data for modeling
#Natalie Melkonoff
#nmelkonoff@dbg.org
#2021-07-06

library(dismo)
library(tidyverse)
library(raster)
#Need these bioclim data to run successfully
##Environmental Data 

prep_data = function(df = NULL, biovar_master = NULL) {
  #Step 1. Split by time period into two data frames
  # df_current = df %>%
  #   filter(time_frame == "current")
  # df_future = df %>%
  #   filter(time_frame == "future")
  
  #Step 1. Create dataframe with all occurence points
  df_master <- df

  # #Step 2. Inspect the dataframe
  glimpse(df_master)
  #glimpse(df_future)
  
    #Step 3. Generate 10k background points for each one. 
  #bg_current <- dismo::randomPoints(biovar_current, 10000)
  #colnames(bg_current) = c("longitude", "latitude")
  
  #bg_future = randomPoints(biovar_future, 10000)
  #colnames(bg_future) = c("longitude", "latitude")
  
  #Step 3. Generate 10k background points
  bg_master <- dismo::randomPoints(biovar_master, 10000)
  colnames(bg_master) = c("longitude", "latitude")
  
  #Step. 4 Merging background data and occurence data
  df_comb_master = data.frame(df_master) %>%
    mutate(pb = 1) %>%
    dplyr::select(pb, longitude, latitude) %>%
    bind_rows(data.frame(bg_master) %>% 
                mutate(pb = 0))  %>%
    mutate(Species = as.integer(pb)) %>%
    dplyr::select(-pb)
  
  # df_comb_future = data.frame(df_future) %>%
  #   mutate(pb = 1) %>%
  #   dplyr::select(pb, longitude, latitude) %>%
  #   bind_rows(data.frame(bg_future) %>% 
  #               mutate(pb = 0)) %>%
  #   mutate(Species = as.integer(pb)) %>%
  #   dplyr::select(-pb)
  # 
  df_comb <- data.frame()
  
  #Step 5. Changing to a spatial points data frame
  df_sp_master <- SpatialPointsDataFrame(df_comb_master[,c("longitude","latitude")], 
                                    df_comb_master, 
                                    proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  # df_sp_future = SpatialPointsDataFrame(df_comb_future[,c("longitude","latitude")], 
  #                                  df_comb_future, 
  #                                  proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  # 
  #Converting to a list with the two dataframes
  prepared_data_list = df_sp_master
  prepared_data_list
}

#Test
prep_st = prep_data(monarch)
