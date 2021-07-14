#blockCV and Modeling Script for monarch and milkweed data
#Natalie Melkonoff
#nmelkonoff@dbg.org
#2021-06-29

#libraries
library(blockCV)
library(tidyverse)
library(raster)
library(maxnet)
library(dismo)
library(ENMeval)

# Data Preparation --------------------------------------------------------
#setting seed for reproducibility down the line
set.seed(223)

#importing monarch, milkweed, and environmental data
#monarch
monarch <- read_csv("./data/monarch_data.csv")
monarch <- monarch[,-1] %>%
  dplyr::select(longitude, latitude, date)

#subulata
subulata <- read_csv("./data/subulata_data.csv") %>%
  dplyr::select(-1, longitude, latitude, date)

#asperula
asperula <- read.csv("./data/asperula_data.csv") %>%
  dplyr::select(-1, longitude, latitude, date)

#angustifolia
angustifolia <- read_csv("./data/angustifolia_data.csv") %>%
  dplyr::select(-1, longitude, latitude, date)

#linaria
linaria <- read_csv("./data/linaria_data.csv") %>%
  dplyr::select(-1, longitude, latitude, date)

#erosa
erosa <- read_csv("./data/erosa_data.csv") %>%
  dplyr::select(-1, longitude, latitude, date)

#importing environmental data
bv_current <- raster::brick("./data/biovar_avg_combined_current.gri")
bv_future <- raster::brick("./data/wc2.1_2.5m_bioc_GFDL-ESM4_ssp126_2021-2040.tif")

#hostplant_2 = hp %>%
  #filter(str_detect(name, "Zanthoxylum clava-herculis"))

#hostplant_3 = hp %>%
  #filter(str_detect(name, "Ptelea trifoliata"))

#bv_t1 = raster::brick("./data/raw_data/biovar_avg_t1.gri")
#bv_t2 = raster::brick("./data/raw_data/biovar_avg_t2.gri")

#renaming
names_seq <- paste("Bio",seq(1:19), sep = "")
names(bv_current) <- names_seq
names(bv_future) <- names_seq

# Determine geographic extent of our data - specific extents for each species
# Function to get extents

get_extent <- function(species_data = NULL) {
  max_lat <- ceiling(max(species_data$latitude))
  min_lat <- floor(min(species_data$latitude))
  max_lon <- ceiling(max(species_data$longitude))
  min_lon <- floor(min(species_data$longitude))
  geographic_extent = extent(x = c(min_lon, 
                                   max_lon, 
                                   min_lat, 
                                   max_lat))
  return(geographic_extent)
}

geographic_extent_monarch <- get_extent(monarch)
geographic_extent_subulata <- get_extent(subulata)
geographic_extent_asperula <- get_extent(asperula)
geographic_extent_angustifolia <- get_extent(angustifolia)
geographic_extent_linaria <- get_extent(linaria)
geographic_extent_erosa <- get_extent(erosa)

# Crop current environmental data to geographic extent of monarchs
bv_current_monarch <- crop(x = bv_current, y = geographic_extent_monarch)
#bv_t2_st <- crop(x = bv_t2, y = geographic_extent_st)

#crop future environmental data to geographic extent of monarchs
bv_future_monarch <- crop(x = bv_future, y = geographic_extent_monarch)

# Crop current environmental data to geographic extent of subulata
bv_current_subulata <- crop(x = bv_current, y = geographic_extent_subulata)
#bv_t2_hp_1 <- crop(x = bv_t2, y = geographic_extent_hp_1)

#crop future environmental data to geographic extent of subulata
bv_future_subulata <- crop(x = bv_future, y = geographic_extent_subulata)

# Crop current environmental data to geographic extent of asperula
bv_current_asperula <- crop(x = bv_current, y = geographic_extent_asperula)
#bv_t2_hp_1 <- crop(x = bv_t2, y = geographic_extent_hp_1)

#crop future environmental data to geographic extent of asperula
bv_future_asperula <- crop(x = bv_future, y = geographic_extent_asperula)

# Crop current environmental data to geographic extent of angustifolia
bv_current_angustifolia <- crop(x = bv_current, y = geographic_extent_angustifolia)
#bv_t2_hp_1 <- crop(x = bv_t2, y = geographic_extent_hp_1)

#crop future environmental data to geographic extent of angustifolia
bv_future_angustifolia <- crop(x = bv_future, y = geographic_extent_angustifolia)

# Crop current environmental data to geographic extent of linaria
bv_current_linaria <- crop(x = bv_current, y = geographic_extent_linaria)
#bv_t2_hp_1 <- crop(x = bv_t2, y = geographic_extent_hp_1)

#crop future environmental data to geographic extent of linaria
bv_future_linaria <- crop(x = bv_future, y = geographic_extent_linaria)

# Crop current environmental data to geographic extent of erosa
bv_current_erosa <- crop(x = bv_current, y = geographic_extent_erosa)
#bv_t2_hp_1 <- crop(x = bv_t2, y = geographic_extent_hp_1)

#crop future environmental data to geographic extent of erosa
bv_future_erosa <- crop(x = bv_future, y = geographic_extent_erosa)

#using the custom prep_data function to ready the data for blockCV
source("./data prep/prep_data_func.R") 

monarch_prepared_data <- prep_data(df = monarch, biovar_master = bv_future_monarch)
subulata_prepared_data <- prep_data(df = subulata, biovar_master = bv_future_subulata)
asperula_prepared_data <- prep_data(df = asperula, biovar_master = bv_future_asperula)
angustifolia_prepared_data <- prep_data(df = angustifolia, biovar_master = bv_future_angustifolia)
linaria_prepared_data <- prep_data(df = linaria, biovar_master = bv_future_linaria)

#--------------------------------------------------------------------------stopped here w/ editing for additional species
#hostplant1_prepared_data = prep_data(hostplant_1, bv_t1_hp_1, bv_t2_hp_1)
#hostplant2_prepared_data = prep_data(hostplant_2, bv_t1_hp_2, bv_t2_hp_2)
#hostplant3_prepared_data = prep_data(hostplant_3, bv_t1_hp_3, bv_t2_hp_3)

#Merging all the mini-lists into a large list of dataframes
prepared_data_master <- c(monarch_prepared_data, subulata_prepared_data)

names(prepared_data_master) = c("monarch", "subulata")
# prepared_data_master = c(swallowtail_prepared_data, 
#                          hostplant1_prepared_data, 
#                          hostplant2_prepared_data, 
#                          hostplant3_prepared_data)

# names(prepared_data_master) = c("st_t1", "st_t2", "hp1_t1", "hp1_t2",
#                                 "hp2_t1", "hp2_t2", "hp3_t1", "hp3_t2")


# blockCV Train-Test Split for all 4 models ------------------------------------------------
#Running spatialBlock function over every dataframe in the list
#
block_list = list()
for (i in 1:length(prepared_data_master)) {
  # if (str_detect(names(prepared_data_master[i]), "t1") == TRUE) {
  #   raster = bv_t1 
  # } else {
  #   raster = bv_t2
  # }
  raster = bv_current
  
  block_list[[i]] = spatialBlock(speciesData = prepared_data_master[[i]],
                               species = "Species",
                               rasterLayer = raster,
                               theRange = 400000,
                               k = 5, 
                               selection = "random", 
                               iteration = 250, 
                               biomod2Format = TRUE, 
                               xOffset = 0, 
                               yOffset = 0, 
                               progress = T)
}

#Saving Spatial CV splits - these actually take a surprising amount of time to run, and are necessary building blocks for threshold maps in the figures script
saveRDS(block_list, "./data/block_list.rds")

#Getting dataframes to feed into the model (dropping NAs)
#monarch

model_data_list = list()
for (i in 1:length(prepared_data_master)) {
  # if (str_detect(names(prepared_data_master[i]), "t1") == TRUE) {
  #   raster = bv_t1 
  # } else {
  #   raster = bv_t2
  # }
  raster = bv_current
  
  model_data_list[[i]] = raster::extract(raster, prepared_data_master[[i]][,-3], df = TRUE) %>%
    bind_cols(as.data.frame(prepared_data_master[[i]])) %>%
    drop_na() %>%
    dplyr::select(-ID, Species, longitude, latitude, Bio1:Bio19)
}

#vectors of presence-background
pb_list = lapply(prepared_data_master,  function(x) '['(x, 3))

#folds for each model
fold_list = lapply(block_list, function(x) '[['(x, 1))

#Writing a function that unlists and combines all training and test indices for each species-time period combination
extract_index = function(list_of_folds = NULL) {
for(k in 1:length(list_of_folds)){
  train_index <- unlist(list_of_folds[[k]][1]) # extract the training set indices
  test_index <- unlist(list_of_folds[[k]][2])# extract the test set indices
}
  mini_list = list(train_index, test_index)
  mini_list
}

#Applying the function to the list of folds
train_test_index_list = lapply(fold_list, extract_index)

train_test_data_list = list()
for (i in 1:length(model_data_list)) {
    train_index = train_test_index_list[[i]][[1]]
    test_index = train_test_index_list[[i]][[2]]
  
    train_data = model_data_list[[i]][train_index,]
    test_data = model_data_list[[i]][test_index,]
    
    mini_list = list(train_data, test_data)
    train_test_data_list[[i]] = mini_list
}

str(train_test_data_list)

#Adding on T1, T2 designations
# for (i in 1:length(train_test_data_list)) {
#   for (j in 1:length(train_test_data_list[[i]])) {
#     if (j == 1) {
#       train_test_data_list[[i]][[j]]$time_period = 1
#     } else {
#       train_test_data_list[[i]][[j]]$time_period = 2
#     }
#   }
# }

# Modeling ----------------------------------------------------------------
if (Sys.getenv("JAVA_HOME")!="")
  Sys.setenv(JAVA_HOME="")
library(rJava)

model_func <- function(data = NULL, env_data = NULL) {
  data_occ <- data[[1]] %>%  #Generating occurence lat long
    filter(Species == 1) %>%
    dplyr::select(longitude, latitude)
  
  data_bg <- data[[1]] %>% #Generating background lat long
    filter(Species == 0) %>%
    dplyr::select(longitude, latitude)
  
  # if (data[[1]]$time_period[1] == 1) { #Setting the appropriate environmental layer for the time period
  #   env_data = bv_t1
  # } else {
  #   env_data = bv_t2
  
#env_data <- bv_current
   
  
  #Running the model
  eval <- ENMevaluate(occ = data_occ, 
                     bg.coords = data_bg,
                     env = env_data,
                     method = 'randomkfold', 
                     #kfolds = 5, #for older versions of package
                     partition.settings = list(kfolds = 5),
                     algorithm = 'maxent.jar', 
                     tune.args = list(fc = c("L", "LQ", "H", "LQH", "QH", "LH"), rm = 1:5))
                     #fc = c("L", "LQ", "H", "LQH", "QH", "LH"),
                     
  return(eval)
}
#test on one dataset
#one_model <- model_func(data = train_test_data_list[[2]], env_data = bv_current)

start = Sys.time()
#Running the model function over the list of data -- this takes a long time!
big_model_list <- lapply(train_test_data_list, model_func, env_data = bv_current)

#Saving this bad boy
saveRDS(big_model_list, "./data/big_model_list.rds")

end = Sys.time()

big_model_list <- readRDS(file = "./data/big_model_list.rds") #use this to load in models
# Model Evaluation --------------------------------------------------------

#Function to build set of evaluation plots - just plug in the appropriate eval model object from above

eval_plots <- function(eval_object = NULL, stats = NULL) {
  par(mfrow=c(2,3))
  evalplot.stats(e = eval_object@results)
  evalplot.stats(e = eval_object@results, stats = "avg.test.AUC", legend = F)
  evalplot.stats(e = eval_object@results, stats = 'avg.diff.AUC', legend = F)
  evalplot.stats(e = eval_object@results, stats = 'avg.test.or10pct', legend = F)
  evalplot.stats(e = eval_object@results, stats = 'avg.test.orMTP', legend = F)
  plot(eval_object@results$avg.test.AUC, eval_object@results$delta.AICc, bg=factor(eval_object@results$features), pch=21, cex= eval_object@results$rm/2, xlab = "avg.test.AUC", ylab = 'delta.AICc', cex.lab = 1.5)
  legend("topright", legend=unique(eval_object@results$features), pt.bg=factor(eval_object@results$features), pch=21)
  mtext("Circle size proportional to regularization multiplier value", cex = 0.6)
}


#Evaluation plots ------------------------this isn't working currently, skipped for now
#Feed in a list of models and it outputs and saves the evaluation plots for each one
for (i in 1:length(big_model_list)) {
  name = paste("eval_plot", i, sep = "_")
  png(filename = paste0("./output/", name, ".png"), 
      width = 1080, height = 720)
  plot = eval_plots(big_model_list[[i]])
  dev.off()
}

#Picking the best model based on highest AUC for each set
#Pulling out indices of the "best" model based on AUC scores - if there are two models that are equal, it pulls the first.

model_selection_index_list <- list()

for (i in 1:length(big_model_list)) {
  cat("Finding best AUC for dataset", i)
  model_selection_index_list[[i]] = as.numeric(row.names(big_model_list[[i]]@results[which(big_model_list[[i]]@results$avg.test.AUC== max(big_model_list[[i]]@results$avg.test.AUC)),]))[1]
}

best_model_list <- list()

for (i in 1:length(big_model_list)) {
  index = model_selection_index_list[[i]]
  model = big_model_list[[i]]@models[[index]]
  best_model_list[[i]] = model
}

#combining models and data into a master list

master_list <- list()
for (i in 1:length(train_test_data_list)) {
  master_list[[i]] = append(train_test_data_list[[i]], best_model_list[[i]])
}
#Generating evaluate objects on test data
# 
# evaluate_models = function(master_list_sub = NULL) {
#   test_data_occ = master_list_sub[[2]] %>%
#     filter(Species == 1) %>%
#     dplyr::select(longitude, latitude)
#   
#   bg_data = master_list_sub[[2]] %>%
#     filter(Species == 0) %>%
#     dplyr::select(longitude, latitude)
#   
#   if (master_list_sub[[2]]$time_period[1] == 1) {
#     env_data = bv_t1
#   } else {
#     env_data = bv_t2
#   }
#   
#   model_sub = master_list_sub[[3]]
#   
#   ev = evaluate(test_data_occ, a = bg_data, model = model_sub, x = env_data)
#   ev
# }


names(master_list) <- c("monarch", 
                           "subulata")

names_list <- c("bv_current_monarch", 
               "bv_current_subulata")

evaluations = list()
for(i in 1:length(master_list)) {
  test_data_occ = master_list[[i]][[2]] %>%
    filter(Species == 1) %>%
    dplyr::select(longitude, latitude)
  
  bg_data = master_list[[i]][[2]] %>%
    filter(Species == 0) %>%
    dplyr::select(longitude, latitude)
  
  env_data = get(names_list[i])
  
  model_sub = master_list[[i]][[3]]
  
  ev = evaluate(test_data_occ, a = bg_data, model = model_sub, x = env_data)
  evaluations[[i]] = ev
  print(paste("Finished", names_list[i]))
}



#Saving evaluations
saveRDS(evaluations, file = "./data/evaluations.rds")

# Selecting Final Models and Running on All Data --------------------------
#Let's build final models

full_model <- function(models = NULL, full_data = NULL, best_model_index = NULL, name = NULL, env_data_t1 = NULL, env_data_t2 = NULL) {
  auc_mod <- models@results[best_model_index,]
  FC_best <- as.character(auc_mod$features[1])
  rm_best <- auc_mod$rm
  maxent.args <- ENMeval::make.args(RMvalues = rm_best, fc = FC_best)
  
  if (full_data$time_frame[1] == "T1") {
    env_data = env_data_t1
  } else {
    env_data = env_data_t2
  }
  
  full_mod = maxent(env_data, as.matrix(full_data[,1:2]), args = maxent.args[[1]])
  saveRDS(full_mod, paste0("./data/", name, ".rds" ))
  
  full_mod
}

# As of 2020-12-21, this includes cropped versions of env vars
#Creating each master model
swallowtail_t1 = full_model(models = big_model_list[[1]], best_model_index = model_selection_index_list[[1]], 
           full_data = swallowtail %>% filter(time_frame == "T1"), name = "swallowtail_t1", 
           env_data_t1 = bv_t1_st, env_data_t2 = bv_t2_st)

swallowtail_t2 = full_model(models = big_model_list[[2]], best_model_index = model_selection_index_list[[2]], 
                            full_data = swallowtail %>% filter(time_frame == "T2"), name = "swallowtail_t2", 
                            env_data_t1 = bv_t1_st, env_data_t2 = bv_t2_st)

hostplant_1_t1 = full_model(models = big_model_list[[3]], best_model_index = model_selection_index_list[[3]], 
                            full_data = hostplant_1[,-1] %>% filter(time_frame == "T1"), name = "hostplant_1_t1", 
                            env_data_t1 = bv_t1_hp_1, env_data_t2 = bv_t2_hp_1)

hostplant_1_t2 = full_model(models = big_model_list[[4]], best_model_index = model_selection_index_list[[4]], 
                            full_data = hostplant_1[,-1] %>% filter(time_frame == "T2"), name = "hostplant_1_t2", 
                            env_data_t1 = bv_t1_hp_1, env_data_t2 = bv_t2_hp_1)

hostplant_2_t1 = full_model(models = big_model_list[[5]], best_model_index = model_selection_index_list[[5]], 
                            full_data = hostplant_2[,-1] %>% filter(time_frame == "T1"), name = "hostplant_2_t1", 
                            env_data_t1 = bv_t1_hp_2, env_data_t2 = bv_t2_hp_2)

hostplant_2_t2 = full_model(models = big_model_list[[6]], best_model_index = model_selection_index_list[[6]], 
                            full_data = hostplant_2[,-1] %>% filter(time_frame == "T2"), name = "hostplant_2_t2", 
                            env_data_t1 = bv_t1_hp_2, env_data_t2 = bv_t2_hp_2)

hostplant_3_t1 = full_model(models = big_model_list[[7]], best_model_index = model_selection_index_list[[7]], 
                            full_data = hostplant_3[,-1] %>% filter(time_frame == "T1"), name = "hostplant_3_t1", 
                            env_data_t1 = bv_t1_hp_3, env_data_t2 = bv_t2_hp_3)

hostplant_3_t2 = full_model(models = big_model_list[[8]], best_model_index = model_selection_index_list[[8]], 
                            full_data = hostplant_3[,-1] %>% filter(time_frame == "T2"), name = "hostplant_3_t2", 
                            env_data_t1 = bv_t1_hp_3, env_data_t2 = bv_t2_hp_3)

