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

#query iNat and GBIF for A. subulata records
subulata = occ(query = "Asclepias subulata*", from = c("inat", "gbif"),
               has_coords = TRUE, limit = 20000)

#filter out anything from iNat that isn't research grade
subulata$inat$data$`Asclepias subulata*` = subulata$inat$data$`Asclepias subulata*` %>%
  filter(quality_grade == "research")
