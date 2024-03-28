markov <- read.csv("/Users/mayaweissman/Documents/GitHub/SignInversionNoise/matlab/1223cleist_simple_markov.csv")
markov$c_e_ratio <- markov$cleist/markov$ext

manual_zones <- function(r) {
  if (r[4] < 0.1 & r[6] < 0.05){
    zone <- "polar"
  }else if (r[7] < 0.12) {
    zone <- "temperate"
  }else if (r[7] < 0.25) {
    if (as.double(r[4]) > 0.45) {
      zone <- "subtropic"
    }else {
      zone <- "tropic"
    }
  }else {
    zone <- NA
  }
  zone
}

#manually determines zones based on parameter ranges. Used to establish baseline subset of tropical points
markov$manual_zone <- apply(markov, 1, FUN = manual_zones)

#simulations: randomly pull point in parameter space for each lat zone
numsims <- 2000

sim_df <- data.frame(cleist=c(0),
                     ext=c(0),
                     c_e_ratio=c(0), 
                     zone=c(0),
                     nsim=c(0)) 

possible_trop <- subset(markov, manual_zone == "tropic" & cleist < 0.065 )

for (i in 1:numsims) {
  #select tropic point at random
  trop_pt <- possible_trop[sample(nrow(possible_trop), 1), ]
  sim_df <- rbind(sim_df, c(trop_pt$cleist, trop_pt$ext, trop_pt$c_e_ratio, "tropic", i))
  
  #given constraints of tropic point, create dataframe of all possible subtropic points
  possible_subtrop <- subset(markov, cleist>trop_pt$cleist & ext>trop_pt$ext & cleist<0.12 & dim_freq>trop_pt$dim_freq & com_freq>=trop_pt$com_freq)
  if (nrow(possible_subtrop) > 0) { #so long as there is at least one point available, continue
    subtrop_pt <- possible_subtrop[sample(nrow(possible_subtrop), 1), ]
    sim_df <- rbind(sim_df, c(subtrop_pt$cleist, subtrop_pt$ext, subtrop_pt$c_e_ratio, "subtropic", i))
    
    #given constraints of subtropic point, create dataframe of all possible temperatre points
    possible_temp <- subset(markov, cleist>subtrop_pt$cleist & ext>subtrop_pt$ext & dim_freq>subtrop_pt$dim_freq & com_freq<subtrop_pt$com_freq & cleist < 1 & ext < 1)
    
    if (nrow(possible_temp) > 0) { #so long as there is at least one point available, continue
      temp_pt <- possible_temp[sample(nrow(possible_temp), 1), ]
      sim_df <- rbind(sim_df, c(temp_pt$cleist, temp_pt$ext, temp_pt$c_e_ratio, "temperate", i))
    } else { #if no temperate points are possible given previous lat zone points, skip
      sim_df <- rbind(sim_df, c(NA, NA, NA, "temperate", i))
    }
    
  } else { #if no subtropic points are possible given previous lat zone points, skip
    sim_df <- rbind(sim_df, c(NA, NA, NA, "subtropic", i))
    sim_df <- rbind(sim_df, c(NA, NA, NA, "temperate", i))
  }
}
sim_df$zone <- factor(sim_df$zone, levels = c("tropic", "subtropic", "temperate", "polar"))

#remove simulations where we had to skip at least one lat zone
na_idx <- which(is.na(sim_df$cleist))
sims <- sim_df$nsim[na_idx]

sim_df2 <- sim_df[! sim_df$nsim %in% sims,]
sim_df2 <- subset(sim_df2, nsim > 0)

#visualizing 
sim_df_long <- gather(sim_df2, parameter, value, cleist:c_e_ratio)
sim_df_long$parameter[sim_df_long$parameter == "cleist"] <- "Cleistogamy transition"
sim_df_long$parameter[sim_df_long$parameter == "ext"] <- "Extinction rate"
sim_df_long$parameter[sim_df_long$parameter == "c_e_ratio"] <- "c / e ratio"
sim_df_long$parameter <- factor(sim_df_long$parameter, levels = c("Cleistogamy transition", "Extinction rate", "c / e ratio"))

ggplot(data = subset(sim_df_long, nsim > 0), aes(x = zone, y = as.double(value))) +
  geom_line(aes(group = nsim), alpha = 0.3) +
  geom_point(aes(color = zone), size = 3, alpha = 0.3) +
  scale_color_manual(values = c("polar" = "#00ACCD", "temperate" = "#B8DB9B", "subtropic" = "#007932", "tropic" = "#D09B2C")) +
  ylab("Parameter estimate") +
  xlab("Latitudinal Zone") +
  facet_wrap(~ parameter) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(text = element_text(size = 35, family="Times New Roman"))


