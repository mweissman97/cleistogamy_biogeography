library(rWCVP)
library(rgdal)
library(stringr)
library(kewr)
library(sf)
library(raster)
library(sp)
library(dplyr)

#load data
occ_df <- read.csv("/Users/mayaweissman/Documents/GitHub/Chapter3/ch3_csvfiles/angiosperm_cleistogamy_full.csv")
occ_df <- occ_df[!is.na(occ_df$decimalLatitude) & !is.na(occ_df$decimalLongitude),]
occ_df$species_name <- word(occ_df$scientificName, 1,2, sep=" ")
occ_df$real_species_name <- NA
occ_df$native_status <- NA

#function that reconciles species names 
check_species_name <- function(species){
  powo <- search_powo(species)
  search_result <- as.data.frame(tidy(powo))
  accepted <- search_result[search_result$accepted == "TRUE",]
  species_name <- accepted$name[1]
  return(species_name)
}

#make sure that geographic coordinates are within species native range
check_native_range <- function(occurrence_row){
  distribution <- wcvp_distribution(occurrence_row$real_species_name, taxon_rank = "species")
  pt <- SpatialPointsDataFrame(coords=occurrence_row[,c(4,3)], data=occurrence_row, proj4string=CRS("+init=epsg:4326"))
  pt_sf <- st_as_sf(pt, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
  joined_data <- st_join(pt_sf, distribution, join = st_within)
  native_status <- joined_data$occurrence_type
  return(native_status)
}

in_range <- function(occurrence_row, distribution){
  pt <- SpatialPointsDataFrame(coords=occurrence_row[,c(4,3)], data=occurrence_row, proj4string=CRS("+init=epsg:4326"))
  pt_sf <- st_as_sf(pt, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
  joined_data <- st_join(pt_sf, distribution, join = st_within)
  native_status <- joined_data$occurrence_type
  return(native_status)
}

#for a select few species, we encountered issues using the KEW API. For these, we manually created a new search function that just uses continents
check_dinebra_panicea <- function(continent){
  native_continents <- c("NORTH_AMERICA", "SOUTH_AMERICA", "ASIA", "OCEANIA")
  invasive_continents <- c("AFRICA")
  if (continent %in% native_continents) {
    native_status = "native"
  } else if (continent %in% invasive_continents) {
    native_status = "introduced"
  } else {
    native_status = NA
  }
  return(native_status)
}
check_viola_scopulorum <- function(continent){
  native_continents <- c("NORTH_AMERICA")
  if (continent %in% native_continents) {
    native_status = "native"
  } else {
    native_status = NA
  }
  return(native_status)
}

check_microbriza_poomorpha <- function(continent){
  native_continents <- c("SOUTH_AMERICA")
  if (continent %in% native_continents) {
    native_status = "native"
  } else {
    native_status = NA
  }
  return(native_status)
}

check_dichanthelium_perlongum <- function(continent){
  native_continents <- c("NORTH_AMERICA")
  if (continent %in% native_continents) {
    native_status = "native"
  } else {
    native_status = NA
  }
  return(native_status)
}

check_digitaria_henryi <- function(continent){
  native_continents <- c("ASIA")
  if (continent %in% native_continents) {
    native_status = "native"
  } else {
    native_status = NA
  }
  return(native_status)
}

#manually changing species names for select species that the auto species name reconciler wasn't able to get
occ_df$species_name[occ_df$species_name == "Plantago argyraea"] <- "Plantago argyrea"
occ_df$species_name[occ_df$species_name == "Briza poaemorpha" | occ_df$species_name == "Microbriza poimorpha"] <- "Microbriza poomorpha"
occ_df$species_name[occ_df$species_name == "Deschampsia berteroana"] <- "Deschampsia berteroniana"
occ_df$species_name[occ_df$species_name == "Trisetum berteroanum"] <- "Deschampsia berteroniana"
occ_df$species_name[occ_df$species_name == "Triodia decumbens"] <- "Danthonia decumbens"
occ_df$species_name[occ_df$species_name == "Aremonia agrimonioides"] <- "Aremonia agrimonoides"
occ_df$species_name[occ_df$species_name == "Hedyotis caerulea"] <- "Houstonia caerulea"
occ_df$species_name[occ_df$species_name == "Viola triloba"] <- "Viola palmata"
occ_df$species_name[occ_df$species_name == "Commelina forskalaei"] <- "Commelina forskaolii"
occ_df$species_name[occ_df$species_name == "Vaccinium berberifolium"] <- "Vaccinium reticulatum"
occ_df$species_name[occ_df$species_name == "Amphicarpa bracteata"] <- "Amphicarpaea bracteata"
occ_df$species_name[occ_df$species_name == "Astragalus cymbicarpos"] <- "Astragalus cymbaecarpos"
occ_df$species_name[occ_df$species_name == "Salvia verbenacea"] <- "Salvia verbenaca"
occ_df$species_name[occ_df$species_name == "Epifagus virginianus"] <- "Epifagus virginiana"
occ_df$species_name[occ_df$species_name == "Setaria pallidifusca"] <- "Setaria pumila"
occ_df$species_name[occ_df$species_name == "Nuttalanthus canadensis"] <- "Nuttallanthus canadensis"
occ_df$species_name[occ_df$species_name == "Viola rafinesquii"] <- "Viola rafinesquei"
occ_df$species_name[occ_df$species_name == "Viola nuevoleonensis"] <- "Viola grahamii"
occ_df$species_name[occ_df$species_name == "Viola eriocarpon"] <- "Viola eriocarpa"
occ_df$species_name[occ_df$species_name == "Antirrhinum watsonii"] <- "Sairocarpus watsonii"
occ_df$species_name[occ_df$species_name == "Eriachne armittii"] <- "Eriachne tawadae"

unique_species <- unique(occ_df$species_name)

#for all species, determine if points are within native range
for (j in 581:length(unique_species)){
  if (unique_species[j] == "Hybanthus concolor") {
    real_name = "Hybanthus concolor"
  } else if (unique_species[j] == "Vulpia persica"){
    real_name = "Vulpia persica"
  } else if (unique_species[j] == "Viola epipsiloides"){
    real_name = "Viola epipsiloides"
  } else if (unique_species[j] == "Viola ×abundans"){
    real_name = "Viola × abundans"
  } else if (unique_species[j] == "Viola alpestris"){
    real_name = "Viola tricolor"
  } else {
    real_name <- check_species_name(unique_species[j])
  }
  idx <- which(occ_df$species_name == unique_species[j])
  if (length(idx) > 0){
    occ_df$real_species_name[occ_df$species_name == unique_species[j]] <- real_name
    if (real_name == "Dinebra panicea"){
      for (i in idx){
        occ_df$native_status[i] <- check_dinebra_panicea(occ_df$continent[i])
      }
    } else if (real_name == "Viola scopulorum" | real_name == "Viola pratincola" | real_name == "Viola × venustula" | real_name == "Viola × abundans"){
      for (i in idx){
        occ_df$native_status[i] <- check_viola_scopulorum(occ_df$continent[i])
      }
    } else if (real_name == "Microbriza poomorpha"){
      for (i in idx){
        occ_df$native_status[i] <- check_microbriza_poomorpha(occ_df$continent[i])
      }
    } else if (real_name == "Dichanthelium perlongum"){
      for (i in idx){
        occ_df$native_status[i] <- check_dichanthelium_perlongum(occ_df$continent[i])
      }
    } else if (real_name == "Digitaria henryi"){
      for (i in idx){
        occ_df$native_status[i] <- check_digitaria_henryi(occ_df$continent[i])
      }
    } else {
      native_range <- wcvp_distribution(real_name, taxon_rank = "species")
      for (i in idx){
        occ_df$native_status[i] <- in_range(occ_df[i,], native_range)
      }
    }
  }
}

occ_native <- subset(occ_df, native_status == "native")

## add biome + ecoregion data
occ_coors <- occ_native[,c(4,3)]
occ_spat <- SpatialPointsDataFrame(coords=occ_coors,data=occ_native,proj4string=CRS("+init=epsg:4326"))

poly<-readOGR(dsn="/Users/mayaweissman/Documents/GitHub/Chapter3/ch3_csvfiles/wwf_terr_ecos", layer="wwf_terr_ecos") 
o <- over(occ_spat, poly)
occ_spat@data = cbind(occ_spat@data, o)

occ_spat_df <- occ_spat@data
occ_spat_df <- subset(occ_spat_df, BIOME != "NA")
occ_spat_df <- subset(occ_spat_df, BIOME < 15)

occ_spat_df_trim <- occ_spat_df %>% select('scientificName','decimalLatitude', 'decimalLongitude', 'class', 'order', 'family', 'continent', 'strategy', 'ECO_NAME', 'BIOME', 'ECO_ID', 'ECO_SYM')
occ_spat_df_trim$biome_name[occ_spat_df_trim$BIOME == 1] <- "Tropical & Subtropical Moist Broadleaf Forests"
occ_spat_df_trim$biome_name[occ_spat_df_trim$BIOME == 2] <- "Tropical & Subtropical Dry Broadleaf Forests"
occ_spat_df_trim$biome_name[occ_spat_df_trim$BIOME == 3] <- "Tropical & Subtropical Coniferous Forests"
occ_spat_df_trim$biome_name[occ_spat_df_trim$BIOME == 4] <- "Temperate Broadleaf & Mixed Forests"
occ_spat_df_trim$biome_name[occ_spat_df_trim$BIOME == 5] <- "Temperate Conifer Forests"
occ_spat_df_trim$biome_name[occ_spat_df_trim$BIOME == 6] <- "Boreal Forests/Taiga"
occ_spat_df_trim$biome_name[occ_spat_df_trim$BIOME == 7] <- "Tropical & Subtropical Grasslands, Savannas & Shrublands"
occ_spat_df_trim$biome_name[occ_spat_df_trim$BIOME == 8] <- "Temperate Grasslands, Savannas & Shrublands"
occ_spat_df_trim$biome_name[occ_spat_df_trim$BIOME == 9] <- "Flooded Grasslands & Savannas"
occ_spat_df_trim$biome_name[occ_spat_df_trim$BIOME == 10] <- "Montane Grasslands & Shrublands"
occ_spat_df_trim$biome_name[occ_spat_df_trim$BIOME == 11] <- "Tundra"
occ_spat_df_trim$biome_name[occ_spat_df_trim$BIOME == 12] <- "Mediterranean Forests, Woodlands & Scrub"
occ_spat_df_trim$biome_name[occ_spat_df_trim$BIOME == 13] <- "Deserts & Xeric Shrublands"
occ_spat_df_trim$biome_name[occ_spat_df_trim$BIOME == 14] <- "Mangroves"

write.csv(occ_spat_df_trim, "cleistogamy_native_occ_norm.csv")
