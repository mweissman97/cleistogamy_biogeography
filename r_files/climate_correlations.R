library(corrplot)
library(sf)
library(raster)
library(sp)
library(dplyr)
library(ggplot2)

#Using PRISM 40 year climate data, compare within year variability to between year variability
#because of PRISM's scope, this analysis is constrained to us
usclim <- read.csv("/Users/mayaweissman/Documents/GitHub/Chapter3/ch3_csvfiles/cleistogamy_us_prefilter.csv")

mean(usclim$ppt_mean)
mean(usclim$temp_var)

byeco <- usclim %>%
  group_by(ECO_NAME, biome_name) %>%
  summarize(lat = mean(dcmlLtt),
            long = mean(dcmlLng),
            temp_mean = mean(temp_mean),
            temp_var = mean(temp_var),
            temp_autocor = mean(temp_autocor),
            ppt_mean = mean(ppt_mean),
            ppt_var = mean(ppt_var),
            ppt_autocor = mean(ppt_autocor),
            prop_drought = mean(prop_drought),
            total_var = mean(ppt_mean)/751.2784 + mean(temp_var)/0.5099687)

byeco$biome_name = reorder(byeco$biome_name, byeco$total_var, mean)

climate <- getData("worldclim",var="bio",res=2.5)

cc <- byeco[,c(4,3)]
coords_spat <- SpatialPointsDataFrame(coords=cc,data=byeco,proj4string=CRS("+init=epsg:4326"))
values <- raster::extract(climate,coords_spat)
colnames(values) <- c("mean_temp", "diurnal_range", "isotherm", "temp_season", "max_temp", "min_temp", "temp_range", "temp_wet", "temp_dry", "temp_warm", "temp_cold", "mean_prec", "prec_wet_month", "prec_dry_month", "prec_season", "prec_wet_quarter", "prec_dry_quarter", "prec_warm", "prec_cold")

occdfclim <- cbind.data.frame(byeco,values)

tempcor <- cor.test(occdfclim$temp_range, occdfclim$temp_var)
ggplot(data = occdfclim, aes(x = temp_range/10, y = temp_var)) +
  geom_point(alpha = 0.5) +
  xlab("Temp Range (WorldClim)") +
  ylab("Temp Variance (Prism)") +
  ggtitle(paste("R = ", round(tempcor$estimate, 4), ", p =", signif(tempcor$p.value, 5))) +
  stat_smooth(method = "lm") +
  theme_bw() +
  theme(text = element_text(family="Times New Roman"))
  
preccor <- cor.test(occdfclim$prec_season, occdfclim$ppt_var)
ggplot(data = occdfclim, aes(x = prec_season, y = ppt_var)) +
  geom_point(alpha = 0.5) +
  xlab("Prec Season (WorldClim)") +
  ylab("Prec Variance (Prism)") +
  ggtitle(paste("R = ", round(preccor$estimate, 4), ", p =", signif(preccor$p.value, 5))) +
  stat_smooth(method = "lm") +
  theme_bw() +
  theme(text = element_text(family="Times New Roman"))


varcor <- cor.test(occdfclim$temp_var, occdfclim$ppt_var)
ggplot(data = occdfclim, aes(x = temp_var, y = ppt_var)) +
  geom_point(alpha = 0.5) +
  xlab("Temp Variance (Prism)") +
  ylab("Prec Variance (Prism)") +
  theme_bw() +
  ggtitle(paste("R = ", round(varcor$estimate, 4), ", p =", signif(varcor$p.value, 5))) +
  stat_smooth(method = "lm") +
  theme(text = element_text(family="Times New Roman"))


#for worldwide climate data, find correlation b/w all worldclim bioclimatic variables
occdf <- read.csv("/Users/mayaweissman/Documents/GitHub/Chapter3/ch3_csvfiles/cleistogamy_native_occ.csv")
cc2 <- occdf[,c(11,10)]
coords_spat2 <- SpatialPointsDataFrame(coords=cc2,data=occdf,proj4string=CRS("+init=epsg:4326"))
clim_vals <- raster::extract(climate,coords_spat2)
colnames(clim_vals) <- c("mean_temp", "diurnal_range", "isotherm", "temp_season", "max_temp", "min_temp", 
                       "temp_range", "temp_wet", "temp_dry", "temp_warm", "temp_cold", "mean_prec", "prec_wet_month", 
                       "prec_dry_month", "prec_season", "prec_wet_quarter", "prec_dry_quarter", "prec_warm", "prec_cold")
clim_values <- as.data.frame(clim_vals)
col_order <- c("mean_temp", "temp_range", "temp_season", "diurnal_range", "isotherm", "max_temp", "min_temp", "temp_cold", 
               "temp_dry", "temp_warm", "temp_wet", "mean_prec", "prec_season", "prec_cold", "prec_dry_month", 
               "prec_dry_quarter", "prec_warm", "prec_wet_month", "prec_wet_quarter")

clim_values <- select(clim_values, col_order)
clim_values <- na.omit(clim_values)
cormat <- cor(clim_values)

corrplot(cormat, method="color", tl.col="black")

