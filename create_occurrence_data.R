# Load the required library
library(rgbif)
library(rWCVP)
library(rgdal)
library(dplyr)
library(tidyr)

#load cleistogamous species data from Culley 2007
path_to_files <- "~/"
bh_species_raw <- read.csv(paste(path_to_files,"CL_SpeciesList_100205 - CL_review.csv", sep = ""))

# Function to query GBIF and retrieve occurrence records
retrieve_occurrences <- function(species_list) {
  occurrence_data <- data.frame()
  
  for (species in species_list) {
    # Query GBIF for occurrences
    #occurrence_result <- occ_download(pred_in("scientificName", species), pred_in("basisOfRecord", "PRESERVED_SPECIMEN"), user=user,pwd=pwd,email=email)
    occurrence_result <- occ_search(scientificName = species, basisOfRecord = "PRESERVED_SPECIMEN")
    full_dataframe <- data.frame(occurrence_result$data)
    cols_names <- c("scientificName", "phylum", "class", "order", "family", "continent", "decimalLatitude", "decimalLongitude", "elevation", "habitat")
    if ("decimalLatitude" %in% colnames(full_dataframe)) {
      new_occurrence_data <- full_dataframe %>% select_if(names(.) %in% cols_names)
      new_occurrence_data$strategy <- bh_species_raw$Type.of.CL[bh_species_raw$scientificName == species]
      occurrence_data <- bind_rows(occurrence_data, new_occurrence_data)
    }
  }
  
  occurrence_data
}

# Retrieve occurrence data from GBIF
bh_species_cons <- subset(bh_species_raw, Type.of.CL == "complete")
occurrence_data_cons <- retrieve_occurrences(bh_species_cons$scientificName)

bh_species_div1 <- subset(bh_species_raw, Type.of.CL == "dimorphic")[1:232,]
occurrence_data_div1 <- retrieve_occurrences(bh_species_div1$scientificName)

bh_species_div2 <- subset(bh_species_raw, Type.of.CL == "dimorphic")[233:462,]
occurrence_data_div2 <- retrieve_occurrences(bh_species_div2$scientificName)

occurrence_data_full <- rbind(occurrence_data, occurrence_data_div1, occurrence_data_div2)

#write dataframe to CSV
write.csv(occurrence_data_full, "~/angiosperm_cleistogamy_full.csv")
