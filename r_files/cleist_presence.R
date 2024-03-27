library(rgdal)
library(sf)
library(dplyr)
library(raster)
library(rstatix)
library(ggpubr)

#list of all unique ecoregions in the world
poly<-readOGR(dsn="/Users/mayaweissman/Documents/GitHub/Chapter3/ch3_csvfiles/wwf_terr_ecos", layer="wwf_terr_ecos")
centroids <- coordinates(poly)

ecodf <- data.frame(
  ECO_NAME = poly$ECO_NAME,
  BIOME = poly$BIOME,
  lat = centroids[, 2],
  long = centroids[, 1]
)
ecodf$biome_name[ecodf$BIOME == 1] <- "Tropical & Subtropical Moist Broadleaf Forests"
ecodf$biome_name[ecodf$BIOME == 2] <- "Tropical & Subtropical Dry Broadleaf Forests"
ecodf$biome_name[ecodf$BIOME == 3] <- "Tropical & Subtropical Coniferous Forests"
ecodf$biome_name[ecodf$BIOME == 4] <- "Temperate Broadleaf & Mixed Forests"
ecodf$biome_name[ecodf$BIOME == 5] <- "Temperate Conifer Forests"
ecodf$biome_name[ecodf$BIOME == 6] <- "Boreal Forests/Taiga"
ecodf$biome_name[ecodf$BIOME == 7] <- "Tropical & Subtropical Grasslands, Savannas & Shrublands"
ecodf$biome_name[ecodf$BIOME == 8] <- "Temperate Grasslands, Savannas & Shrublands"
ecodf$biome_name[ecodf$BIOME == 9] <- "Flooded Grasslands & Savannas"
ecodf$biome_name[ecodf$BIOME == 10] <- "Montane Grasslands & Shrublands"
ecodf$biome_name[ecodf$BIOME == 11] <- "Tundra"
ecodf$biome_name[ecodf$BIOME == 12] <- "Mediterranean Forests, Woodlands & Scrub"
ecodf$biome_name[ecodf$BIOME == 13] <- "Deserts & Xeric Shrublands"
ecodf$biome_name[ecodf$BIOME == 14] <- "Mangroves"

ecodf$zone <- "polar"
ecodf$zone[abs(ecodf$lat) > 35 & abs(ecodf$lat) < 66.5] <- "temperate"
ecodf$zone[abs(ecodf$lat) > 23.5 & abs(ecodf$lat) < 35] <- "subtropic"
ecodf$zone[abs(ecodf$lat) < 23.5] <- "tropic"
ecodf$zone <- factor(ecodf$zone, levels = c("tropic", "subtropic", "temperate", "polar"))

ecodf <- ecodf %>% distinct(ECO_NAME, .keep_all = TRUE)

#occurrence data
occdf <- read.csv("/Users/mayaweissman/Documents/GitHub/Chapter3/ch3_csvfiles/cleistogamy_native_occ.csv")
occdf$genus <- gsub( " .*$", "", occdf$real_species_name)
occdf$zone <- "polar"
occdf$zone[abs(occdf$lat) > 35 & abs(occdf$lat) < 66.5] <- "temperate"
occdf$zone[abs(occdf$lat) > 23.5 & abs(occdf$lat) < 35] <- "subtropic"
occdf$zone[abs(occdf$lat) < 23.5] <- "tropic"
occdf$zone <- factor(occdf$zone, levels = c("tropic", "subtropic", "temperate", "polar"))

byecor <- occdf %>%
  group_by(ECO_NAME) %>%
  summarize(strategy = ifelse(length(unique(strategy)) > 1, "both", unique(strategy)))

#generate pretty map for figure 1
world_coordinates <- map_data("world")

poly_sf <- st_as_sf(poly)

polydf <- merge(poly_sf, byecor, by="ECO_NAME", all.x = TRUE)
polydf$strategy[is.na(polydf$strategy)] <- "absent"
polydf$strategy[polydf$BIOME > 14] <- NA

ggplot() +
  geom_sf(data = polydf, aes(fill = strategy), color = "white", linewidth = 0.03) +
  scale_fill_manual(values = c("dimorphic" = "#cc4778", "complete" = "#0D0887", "both" = "#CF9BFD", "absent" = "#f0f921"), na.value = "darkgrey",
                    name = "", guide = "legend", labels = c("dimorphic" = "dimorphic only", "complete" = "complete only", "both" = "both present", "absent" = "absent")) +
  theme_dark() +
  geom_hline(yintercept = c(23.5, 35, 66.5, -23.5, -35, -66.5)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylab("Latitude") + xlab("Longitude") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(text = element_text(size = 30, family="Times New Roman")) +
  theme(legend.position = "bottom")

#create usable presence vs. absence dataframe
ecor_pa <- merge(ecodf, byecor, by = "ECO_NAME", all.x = TRUE)
ecor_pa$strategy[is.na(ecor_pa$strategy)] <- "none"
ecor_pa$cleist <- "present"
ecor_pa$cleist[ecor_pa$strategy == "none"] <- "absent"


#figure 2: presence vs absence by climate

fracs <- ecor_pa %>%
  group_by(zone, cleist) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

counts <- ecor_pa %>%
  group_by(zone)  %>%
  summarise(n = n())

x_presence <- subset(fracs, cleist == "present")$n
n_counts <- counts$n

prop.test(x_presence, n_counts)

#2a - latzone
fig2a <- ggplot(data = ecor_pa) + 
  geom_bar(aes(x = zone, fill = cleist), position=position_fill()) +
  scale_fill_manual(values = c("absent" = "#f0f921", "present" = "#7e03a8"), na.value = "white", name = "", guide = "legend") +
  theme_bw()+
  geom_hline(yintercept = 0.7424426) +
  ylab("Frequency") +
  xlab("Latitudinal Zone") +
  theme(text = element_text(size = 35, family="Times New Roman")) ; fig2a

#2B: ecoregion

latzone_order_df <- ecor_pa %>%
  group_by(biome_name, cleist) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))
latzone_order_df <- na.omit(latzone_order_df)
lat2 <- data.frame(biome_name = "Mediterranean Forests, Woodlands & Scrub", cleist = "absent", n = 0, freq = 0)
latzone_order_df <- rbind(latzone_order_df, lat2)

eco_table <- as.table(rbind(
  subset(latzone_order_df, cleist == "present")$n,
  subset(latzone_order_df, cleist == "absent")$n
))
rownames(eco_table) <- c("present", "absent")
biome_names <- subset(latzone_order_df, cleist == "present")$biome_name
colnames(eco_table) <- biome_names

eco_table <- t(eco_table)
result <- row_wise_prop_test(eco_table)
sigresult <- subset(result, p.adj.signif != "ns")

latzone_order_df <- subset(latzone_order_df, cleist == "present")

counts_biome <- ecor_pa %>%
  group_by(biome_name)  %>%
  summarise(n = n())
counts_biome <- na.omit(counts_biome)
counts_biome$label <- paste0(counts_biome$biome_name, " (", counts_biome$n, ")")
counts_biome$biome_name <- factor(counts_biome$biome_name, levels = c("Flooded Grasslands & Savannas", "Tundra", 
                                                            "Montane Grasslands & Shrublands", "Mangroves", "Tropical & Subtropical Moist Broadleaf Forests", "Tropical & Subtropical Dry Broadleaf Forests",
                                                            "Deserts & Xeric Shrublands", "Boreal Forests/Taiga", "Tropical & Subtropical Coniferous Forests",
                                                            "Tropical & Subtropical Grasslands, Savannas & Shrublands",
                                                            "Temperate Conifer Forests", "Temperate Grasslands, Savannas & Shrublands", "Temperate Broadleaf & Mixed Forests",
                                                            "Mediterranean Forests, Woodlands & Scrub"))

x_presence_biome <- latzone_order_df$n
n_counts_biome <- counts_biome$n[1:14]

biome_sig <- chisq.test(x_presence_biome, n_counts_biome, p = rep(0.7424426, each =14))

ecor_pa$biome_name <- factor(ecor_pa$biome_name, levels = c("Flooded Grasslands & Savannas", "Tundra", 
                                                               "Montane Grasslands & Shrublands", "Mangroves", "Tropical & Subtropical Moist Broadleaf Forests", "Tropical & Subtropical Dry Broadleaf Forests",
                                                               "Deserts & Xeric Shrublands", "Boreal Forests/Taiga", "Tropical & Subtropical Coniferous Forests",
                                                                "Tropical & Subtropical Grasslands, Savannas & Shrublands",
                                                               "Temperate Conifer Forests", "Temperate Grasslands, Savannas & Shrublands", "Temperate Broadleaf & Mixed Forests",
                                                               "Mediterranean Forests, Woodlands & Scrub"))
  
fig2b <- ggplot() +
  geom_bar(data = subset(ecor_pa, !is.na(biome_name)), 
           aes(y = biome_name, fill=cleist), position = "fill") +
  theme_bw() +
  geom_vline(xintercept = 0.7424426) +
  xlab("Proportion") + ylab("Biome") +
  #facet_wrap(~zone, nrow = 1) +
  scale_fill_manual(values = c("absent" = "#f0f921", "present" = "#7e03a8"), na.value = "white", name = "", guide = "legend") +
  scale_y_discrete(limits = counts_biome$biome_name, labels = counts_biome$label) +
  theme(text = element_text(size = 35, family="Times New Roman")) +
  theme(legend.position="none"); fig2b

#fig 2C: climate
climate <- getData("worldclim",var="bio",res=2.5)

cc <- ecor_pa[,c(4,3)]
coords_spat <- SpatialPointsDataFrame(coords=cc,data=ecor_pa,proj4string=CRS("+init=epsg:4326"))
values <- raster::extract(climate,coords_spat)
colnames(values) <- c("mean_temp", "diurnal_range", "isotherm", "temp_season", "max_temp", "min_temp", "temp_range", "temp_wet", "temp_dry", "temp_warm", "temp_cold", "mean_prec", "prec_wet_month", "prec_dry_month", "prec_season", "prec_wet_quarter", "prec_dry_quarter", "prec_warm", "prec_cold")

ecodf_clim <- cbind.data.frame(ecor_pa,values)
ecodf_clim <- na.omit(ecodf_clim)
ecodf_long <- gather(ecodf_clim, key = "var", value, 9:27)

stat.test <- ecodf_long %>%
  group_by(var, zone) %>%
  t_test(value ~ cleist) %>%
  adjust_pvalue(method = "BH") %>%
  add_significance()
stat.test

edfl1 <- subset(ecodf_long, var == "mean_temp" & zone == "tropic")

stat_test_sig <- subset(stat.test, p.adj.signif != "ns")

sig_clim_var <- c("mean_temp", "temp_range", "mean_prec", "prec_season")
  
stat_test_sig <- stat.test[(stat.test$var %in% sig_clim_var),]
stat_test_sig$var[stat_test_sig$var =="mean_temp"] <- "Mean Temperature"
stat_test_sig$var[stat_test_sig$var =="temp_range"] <- "Temperature Range"
stat_test_sig$var[stat_test_sig$var =="mean_prec"] <- "Mean Precipitation"
stat_test_sig$var[stat_test_sig$var =="prec_season"] <- "Precipitation Seasonality"
stat_test_sig$var <- factor(stat_test_sig$var, levels = c("Mean Temperature", "Temperature Range", "Mean Precipitation", "Precipitation Seasonality"))

stat_test_sig$y.position[stat_test_sig$var =="Temperature Range"] <- 65
stat_test_sig$y.position[stat_test_sig$var =="Mean Temperature"] <- 35
stat_test_sig$y.position[stat_test_sig$var =="Mean Precipitation"] <- 750
stat_test_sig$y.position[stat_test_sig$var =="Precipitation Seasonality"] <- 20

ecodf_long_sig <- ecodf_long[(ecodf_long$var %in% sig_clim_var),]
ecodf_long_sig$value[ecodf_long_sig$var != "prec_season" | ecodf_long_sig$var != "mean_prec"] <- ecodf_long_sig$value/10
ecodf_long_sig$var[ecodf_long_sig$var =="mean_temp"] <- "Mean Temperature"
ecodf_long_sig$var[ecodf_long_sig$var =="temp_range"] <- "Temperature Range"
ecodf_long_sig$var[ecodf_long_sig$var =="mean_prec"] <- "Mean Precipitation"
ecodf_long_sig$var[ecodf_long_sig$var =="prec_season"] <- "Precipitation Seasonality"
ecodf_long_sig$var <- factor(ecodf_long_sig$var, levels = c("Mean Temperature", "Temperature Range", "Mean Precipitation", "Precipitation Seasonality"))

climate_violins <- ggplot(ecodf_long_sig) +
  geom_violin(aes(y = value, x = zone, fill = cleist, color = cleist), scale = "width", draw_quantiles = c(0.5)) +
  theme_bw() +
  xlab("") + ylab("") +
  scale_fill_manual(values = c("absent" = "#f0f921", "present" = "#7e03a8"), na.value = "white", name = "", guide = "legend") +
  scale_color_manual(values = c("absent" = "black", "present" = "lightgrey"), na.value = "white", name = "", guide = "legend") +
  theme(legend.position="none") +
  facet_wrap(~var, scales = "free") +
  theme(text = element_text(size = 30, family="Times New Roman")); climate_violins

ggplot(ecodf_long_sig) +
  geom_violin(aes(y = value, x = cleist, fill = cleist, color = cleist), scale = "width", draw_quantiles = c(0.5)) +
  theme_bw() +
  xlab("") + ylab("") +
  scale_fill_manual(values = c("absent" = "#f0f921", "present" = "#7e03a8"), na.value = "white", name = "", guide = "legend") +
  scale_color_manual(values = c("absent" = "black", "present" = "lightgrey"), na.value = "white", name = "", guide = "legend") +
  theme(legend.position="none") +
  facet_wrap(~var, scales = "free") +
  theme(text = element_text(size = 30)); climate_violins

climate_violins + 
  stat_pvalue_manual(stat_test_sig, x = "zone", y.position = "y.position", label = "{p.adj.signif}", size = 5) +
  theme(text = element_text(size = 35, family="Times New Roman"))

#pretty table

stat_test_tab <- stat.test[,c("zone", "var", "p.adj", "p.adj.signif")]
stat_test_tab$p_val <- ifelse(stat_test_tab$p.adj.signif == "ns", round(stat_test_tab$p.adj, digits = 4), paste(round(stat_test_tab$p.adj, digits = 4), stat_test_tab$p.adj.signif, sep = ""))
stat_test_tab <- stat_test_tab[,c("zone", "var", "p_val")]
stat_tab_wide <- spread(stat_test_tab, 
                        key=zone, 
                        value=p_val) 

xtable(stat_tab_wide)
