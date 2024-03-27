#libraries
library(dplyr)
library(tidyr)
library(ggpubr)
library(rstatix)
library(raster)
library(ggplot2)

#load data
occdf <- read.csv("/Users/mayaweissman/Documents/GitHub/Chapter3/ch3_csvfiles/cleistogamy_native_occ_norm.csv")
occdf$genus <- gsub( " .*$", "", occdf$real_species_name)
occdf$zone <- "polar"
occdf$zone[abs(occdf$lat) > 35 & abs(occdf$lat) < 66.5] <- "temperate"
occdf$zone[abs(occdf$lat) > 23.5 & abs(occdf$lat) < 35] <- "subtropic"
occdf$zone[abs(occdf$lat) < 23.5] <- "tropic"
occdf$zone <- factor(occdf$zone, levels = c("tropic", "subtropic", "temperate", "polar"))

occdf$pollinator <- "Other (biotic)"
occdf$pollinator[occdf$family == "Poaceae"] <- "Poaceae"

species <- occdf %>%
  group_by(real_species_name, strategy) %>%
  summarize(ecoregions = n())

ggplot(data = species) + 
  geom_bar(aes(x = strategy, fill = strategy)) +
  scale_fill_manual(values = c("dimorphic" = "#cc4778", "complete" = "#0d0887")) +
  theme_bw()+
  ylab("Number of species") +
  xlab("Strategy") +
  theme(legend.position = "none") 
theme(text = element_text(size = 35)); f3a


#zone plot
f3a <- ggplot(data = occdf) + 
  geom_bar(aes(x = zone, fill = interaction(pollinator, strategy)), position = position_dodge2(width = 0.9, preserve = "single")) +
  scale_fill_manual(values = c("Other (biotic).dimorphic" = "#cc4778", "Other (biotic).complete" = "#0d0887", 
                               "Poaceae.dimorphic" = "#e090ae", "Poaceae.complete" = "#6d6ab7"), na.value = "white", name = "", guide = "legend") +
  theme_bw()+
  ylab("Number of species occurrences") +
  xlab("Latitudinal Zone") +
  theme(text = element_text(size = 35, family="Times New Roman")); f3a

propd <- nrow(subset(occdf,strategy == "dimorphic"))/nrow(occdf)

zone_summary <- occdf %>%
  group_by(zone, strategy) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))
zone_summary <- na.omit(zone_summary)

prop.test(x = c(182, 285, 192), n = c(182+864, 285+1309, 192+2629))

propcw <- nrow(subset(occdf,strategy == "complete" & pollinator == "Poaceae (wind)"))/nrow(subset(occdf, pollinator == "Poaceae (wind)"))
nrow(subset(occdf,strategy == "complete" & pollinator != "Poaceae (wind)" & zone == "tropic"))/nrow(subset(occdf, pollinator != "Poaceae (wind)" & zone == "tropic"))

f3b <- ggplot(data = occdf) + 
  geom_bar(aes(x = zone, fill = interaction(pollinator, strategy)), position = position_fill()) +
  scale_fill_manual(values = c("Other (biotic).dimorphic" = "#cc4778", "Other (biotic).complete" = "#0d0887", 
                               "Poaceae.dimorphic" = "#e090ae", "Poaceae.complete" = "#6d6ab7"), na.value = "white", name = "", guide = "legend") +
  theme_bw()+
  ylab("Proportion") +
  geom_hline(yintercept = propd) +
  xlab("Latitudinal Zone") +
  facet_wrap( ~ pollinator) +
  theme(legend.position = "none") +
  theme(text = element_text(size = 35, family="Times New Roman")); f3b


ggplot(data = occdf) + 
  geom_bar(aes(x = zone, fill = strategy), position = position_fill()) +
  scale_fill_manual(values = c("dimorphic" = "#cc4778", "complete" = "#0d0887"), name = "", guide = "legend") +
  theme_bw()+
  ylab("Proportion") +
  geom_hline(yintercept = propd) +
  xlab("Latitudinal Zone") +
  theme(text = element_text(size = 35))

frac <- occdf %>%
  group_by(zone, strategy) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

temp_freq <- prop.test(x = c(1693, 936), n = c(1817, 1004))

comp_freq <- prop.test(x = c(359, 300), n = c((359+2948), (300+1870)))

comp_freq_subtropic <- prop.test(x = c(182, 285), n = c((182+864), (285+1309)))

#ecoregion plot

pol_biome_tab <- occdf %>%
  group_by(biome_name, pollinator) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))
strat_biome_tab <- na.omit(pol_biome_tab)


pol_biome_table <- as.table(rbind(
  subset(pol_biome_tab, pollinator == "Other (biotic)")$n,
  subset(pol_biome_tab, pollinator == "Poaceae (wind)")$n
))
rownames(pol_biome_table) <- c("Other (biotic)", "Poaceae (wind)")
biome_names <- subset(pol_biome_tab, pollinator == "Other (biotic)")$biome_name
colnames(pol_biome_table) <- biome_names

pol_biome_table <- t(pol_biome_table)
pol_biome_result <- row_wise_prop_test(pol_biome_table)
sigresult_pol_biome <- subset(pol_biome_result, p.adj.signif != "ns")

occdf$biome_name <- factor(occdf$biome_name, levels = c("Mangroves", "Tropical & Subtropical Dry Broadleaf Forests", "Deserts & Xeric Shrublands",
                                                        "Tropical & Subtropical Coniferous Forests", "Montane Grasslands & Shrublands", "Tropical & Subtropical Grasslands, Savannas & Shrublands", 
                                                        "Tropical & Subtropical Moist Broadleaf Forests", "Temperate Grasslands, Savannas & Shrublands", "Temperate Conifer Forests", 
                                                        "Mediterranean Forests, Woodlands & Scrub", "Temperate Broadleaf & Mixed Forests", "Flooded Grasslands & Savannas",
                                                        "Boreal Forests/Taiga", "Tundra"))

counts_biome <- occdf %>%
  group_by(biome_name)  %>%
  summarise(n = n())
counts_biome <- na.omit(counts_biome)
counts_biome$label <- paste0(counts_biome$biome_name, " (", counts_biome$n, ")")
counts_biome$biome_name <- factor(counts_biome$biome_name, levels = c("Mangroves", "Tropical & Subtropical Dry Broadleaf Forests", "Deserts & Xeric Shrublands",
                                                                      "Tropical & Subtropical Coniferous Forests", "Montane Grasslands & Shrublands", "Tropical & Subtropical Grasslands, Savannas & Shrublands", 
                                                                      "Tropical & Subtropical Moist Broadleaf Forests", "Temperate Grasslands, Savannas & Shrublands", "Temperate Conifer Forests", 
                                                                      "Mediterranean Forests, Woodlands & Scrub", "Temperate Broadleaf & Mixed Forests", "Flooded Grasslands & Savannas",
                                                                      "Boreal Forests/Taiga", "Tundra"))

f3c <- ggplot(data = occdf) + 
  geom_bar(aes(y = biome_name, fill = interaction(pollinator, strategy)), position = position_fill()) +
  scale_fill_manual(values = c("Other (biotic).dimorphic" = "#cc4778", "Other (biotic).complete" = "#0d0887", 
                               "Poaceae.dimorphic" = "#e090ae", "Poaceae.complete" = "#6d6ab7"), na.value = "white", name = "", guide = "legend") +
  theme_bw()+
  xlab("Proportion") +
  ylab("Biome") +
  theme(legend.position = "none") +
  geom_vline(xintercept = propd) +
  scale_y_discrete(limits = counts_biome$biome_name, labels = counts_biome$label) +
  theme(text = element_text(size = 35, family="Times New Roman")); f3c

# by pollinator type
prop_wind <- nrow(subset(occdf, pollinator == "wind"))/nrow(occdf)

ggplot(data = occdf) + 
  geom_bar(aes(x = strategy, fill = pollinator), position = position_fill()) +
  scale_fill_manual(values = c("insect" = "black", "wind" = "grey"), na.value = "white", name = "", guide = "legend") +
  theme_bw()+
  facet_wrap( ~ zone) + 
  geom_hline(yintercept = prop_wind, color = "white") +
  theme(text = element_text(size = 30, family="Times New Roman"))

poll_order <- occdf %>%
  group_by(biome_name, pollinator) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))
poll_order <- subset(poll_order, pollinator == "wind")

occdf$biome_name <- factor(occdf$biome_name, levels = c("Boreal Forests/Taiga", "Tundra", "Tropical & Subtropical Moist Broadleaf Forests",
                                                        "Temperate Broadleaf & Mixed Forests", "Mediterranean Forests, Woodlands & Scrub", "Tropical & Subtropical Dry Broadleaf Forests",
                                                        "Tropical & Subtropical Coniferous Forests", "Temperate Conifer Forests", "Mangroves",
                                                        "Temperate Grasslands, Savannas & Shrublands", "Flooded Grasslands & Savannas", "Deserts & Xeric Shrublands",
                                                        "Montane Grasslands & Shrublands", "Tropical & Subtropical Grasslands, Savannas & Shrublands"))

f3a <- ggplot(data = occdf) + 
  geom_bar(aes(y = biome_name, fill = pollinator), position = position_fill()) +
  scale_fill_manual(values = c("insect" = "black", "wind" = "grey"), na.value = "white", name = "", guide = "legend") +
  theme_bw()+
  geom_vline(xintercept = prop_wind, color = "white") +
  facet_wrap(~strategy) +
  xlab("Proportion") +
  ylab("Biome") +
  theme(text = element_text(size = 30, family="Times New Roman")); f3a

#climate data
climate <- getData("worldclim",var="bio",res=2.5)

cc <- occdf[,c(11,10)]
coords_spat <- SpatialPointsDataFrame(coords=cc,data=occdf,proj4string=CRS("+init=epsg:4326"))
values <- raster::extract(climate,coords_spat)
colnames(values) <- c("mean_temp", "diurnal_range", "isotherm", "temp_season", "max_temp", "min_temp", "temp_range", "temp_wet", "temp_dry", "temp_warm", "temp_cold", "mean_prec", "prec_wet_month", "prec_dry_month", "prec_season", "prec_wet_quarter", "prec_dry_quarter", "prec_warm", "prec_cold")
#values <- values[,c("mean_temp", "temp_range", "mean_prec", "prec_season")]

occ_clim_wind <- cbind.data.frame(occdf,values)
occ_clim_wind <- na.omit(occ_clim_wind)
occ_clim_wind_long <- gather(occ_clim_wind, key = "var", value, 21:39)
occ_clim_wind_long$pol_strat <- interaction(occ_clim_wind_long$pollinator, occ_clim_wind_long$strategy)

stat_test_wind <- subset(occ_clim_wind_long) %>%
  group_by(var, strategy) %>%
  t_test(value ~ pollinator) %>%
  adjust_pvalue(method = "BH") %>%
  add_significance()

stat_test_tab <- stat_test_wind[,c("strategy", "var", "p.adj", "p.adj.signif")]
stat_test_tab$p_val <- ifelse(stat_test_tab$p.adj.signif == "ns", round(stat_test_tab$p.adj, digits = 4), paste(round(stat_test_tab$p.adj, digits = 4), stat_test_tab$p.adj.signif, sep = ""))
stat_test_tab <- stat_test_tab[,c("strategy", "var", "p_val")]
stat_tab_wide <- spread(stat_test_tab, 
                        key=strategy, 
                        value=p_val) 

xtable(stat_tab_wide)

sig_clim_var <- c("mean_temp", "temp_range", "mean_prec", "prec_season")
occ_clim_wind_long <- occ_clim_wind_long[(occ_clim_wind_long$var %in% sig_clim_var),]
occ_clim_wind_long$value[occ_clim_wind_long$var != "prec_season" | occ_clim_wind_long$var != "mean_prec"] <- occ_clim_wind_long$value/10
occ_clim_wind_long$var[occ_clim_wind_long$var =="mean_temp"] <- "Mean Temperature"
occ_clim_wind_long$var[occ_clim_wind_long$var =="temp_range"] <- "Temperature Range"
occ_clim_wind_long$var[occ_clim_wind_long$var =="mean_prec"] <- "Mean Precipitation"
occ_clim_wind_long$var[occ_clim_wind_long$var =="prec_season"] <- "Precipitation Seasonality"
occ_clim_wind_long$var <- factor(occ_clim_wind_long$var, levels = c("Mean Temperature", "Temperature Range", "Mean Precipitation", "Precipitation Seasonality"))

stat_test_wind <- stat_test_wind[(stat_test_wind$var %in% sig_clim_var),]
stat_test_wind$var[stat_test_wind$var =="mean_temp"] <- "Mean Temperature"
stat_test_wind$var[stat_test_wind$var =="temp_range"] <- "Temperature Range"
stat_test_wind$var[stat_test_wind$var =="mean_prec"] <- "Mean Precipitation"
stat_test_wind$var[stat_test_wind$var =="prec_season"] <- "Precipitation Seasonality"
stat_test_wind$var <- factor(stat_test_wind$var, levels = c("Mean Temperature", "Temperature Range", "Mean Precipitation", "Precipitation Seasonality"))
stat_test_wind$y.position[stat_test_wind$var =="Temperature Range"] <- 70
stat_test_wind$y.position[stat_test_wind$var =="Mean Temperature"] <- 32
stat_test_wind$y.position[stat_test_wind$var =="Mean Precipitation"] <- 750
stat_test_wind$y.position[stat_test_wind$var =="Precipitation Seasonality"] <- 20

clim <- ggplot(subset(occ_clim_wind_long)) +
  geom_violin(aes(y = value, x = strategy, fill = pollinator), color = "lightgrey", scale = "width", draw_quantiles = c(0.5)) +
  theme_bw() +
  xlab("") + ylab("") +
  scale_fill_manual(values = c("Other (biotic)" = "black", "Poaceae" = "grey"), na.value = "white", name = "", guide = "legend") +
  facet_wrap(~ var, scales = "free_y") +
  theme(text = element_text(size = 35, family="Times New Roman"))
clim + 
  stat_pvalue_manual(stat_test_wind, label = "{p.adj.signif}", x = "strategy",
                     y.position = "y.position", size = 5) +
  theme(text = element_text(size = 35, family="Times New Roman"))