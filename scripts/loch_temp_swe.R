source("scripts/libraries.R")

# loch data - outlet temp record and bear lake snotel 

# read in loch data 
  # temp for all sites - will subset for loch outlet below but this is all here 
  # if we want to look at other loch sites
loch_data <- read.csv("data/loch_chem.csv") %>%
  select(2:7, 9)

# filter for loch outlet
  # SUMMER MONTHS ONLY
loch_outlet <- loch_data %>%
  filter(SITE == "LOCH.O" & TYPE == "NORMAL") %>%
  #mutate(MONTH = as.factor(MONTH)) %>%
  filter(MONTH == 6 | MONTH == 7 | MONTH == 8)

# filter for lake surface - purely because i'm curious
loch_surface <- loch_data %>%
  filter(SITE == "LOCH.LS" & TYPE == "NORMAL") %>%
  #mutate(MONTH = as.factor(MONTH)) %>%
  filter(MONTH == 6 | MONTH == 7 | MONTH == 8)
# this record is less consistent than the outlet record

# create a summary df for max loch outlet temp
loch_outlet_max <- loch_outlet %>%
  select(YEAR, TEMP) %>%
  group_by(YEAR) %>%
  slice(which.max(TEMP)) %>%
  rename(year = YEAR)

# create summary df for average loch outlet temp
loch_outlet_avg <- loch_outlet %>%
  select(YEAR, TEMP) %>%
  group_by(YEAR) %>%
  summarize(mean_temp = mean(TEMP, na.rm = TRUE)) %>%
  rename(year = YEAR)

# may need to ungroup these and convert to dataframes to work with them further - stay tuned

# create a summary df for max loch surface temp
loch_surface_max <- loch_surface %>%
  select(YEAR, TEMP) %>%
  group_by(YEAR) %>%
  slice(which.max(TEMP)) %>%
  rename(year = YEAR)

# create summary df for average loch surface temp
loch_surface_avg <- loch_surface %>%
  select(YEAR, TEMP) %>%
  group_by(YEAR) %>%
  summarize(mean_temp = mean(TEMP, na.rm = TRUE)) %>%
  rename(year = YEAR)

# let's look at sky pond while we're at it
sky_data <- read.csv("data/sky_chem.csv") %>%
  select(2:7, 9)

sky_surface <- sky_data %>%
  filter(SITE == "SKY.LS" & TYPE == "NORMAL") %>%
  #mutate(MONTH = as.factor(MONTH)) %>%
  filter(MONTH == 6 | MONTH == 7 | MONTH == 8)

# create a summary df for max sky surface temp
sky_surface_max <- sky_surface %>%
  select(YEAR, TEMP) %>%
  group_by(YEAR) %>%
  slice(which.max(TEMP)) %>%
  rename(year = YEAR)

# create summary df for average sky surface temp
sky_surface_avg <- sky_surface %>%
  select(YEAR, TEMP) %>%
  group_by(YEAR) %>%
  summarize(mean_temp = mean(TEMP, na.rm = TRUE)) %>%
  rename(year = YEAR)



# read in rmnp swe
  # using bear lake snotel as a proxy for loch swe - it's worth noting that there 
  # is roughly a 1000 ft difference in elevation between these sites. there is likely a 
  # better way to standardize this. 

# reading in post - 1981 swe record, avg for each month. using april swe as a proxy

bearlake_snotel <- read.table("data/bear_lake_snotel1.txt", skip = 61, sep = ",") %>%
  select(1, 4) %>%
  rename(month_year = 1, swe = 2) %>%
  mutate(month_year = my(month_year)) %>%
  mutate(month = month(month_year)) %>%
  mutate(year = year(month_year)) %>%
  filter(month == 4) %>%
  relocate(year, .before = month_year) %>%
  relocate(swe, .after = year) %>%
  select(1, 2)

# join both for plotting
loch_temp_swe_avg <- left_join(loch_outlet_avg, bearlake_snotel, by = "year") %>%
  as.data.frame()
loch_temp_swe_max <- left_join(loch_outlet_max, bearlake_snotel, by = "year") %>%
  rename(max_temp = TEMP) %>%
  as.data.frame()

lsurface_temp_swe_avg <- left_join(loch_surface_avg, bearlake_snotel, by = "year") %>%
  as.data.frame()
lsurface_temp_swe_max <- left_join(loch_surface_max, bearlake_snotel, by = "year") %>%
  rename(max_temp = TEMP) %>%
  as.data.frame()


skysurface_temp_swe_avg <- left_join(sky_surface_avg, bearlake_snotel, by = "year") %>%
  as.data.frame()
skysurface_temp_swe_max <- left_join(sky_surface_max, bearlake_snotel, by = "year") %>%
  rename(max_temp = TEMP) %>%
  as.data.frame()

# plot 

# average loch outlet summer temperature 
avgtemp_swe <- ggplot(data = loch_temp_swe_avg, aes(x = swe, y = mean_temp)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "deepskyblue") +
  #geom_smooth(method = "gam", colour = "purple") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01, label.x.npc = 0.4, color = "deepskyblue")
avgtemp_swe

# max loch outlet summer temperature
maxtemp_swe <- ggplot(data = loch_temp_swe_max, aes(x = swe, y = max_temp)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "deepskyblue") +
  #geom_smooth(method = "gam", colour = "purple") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01, label.x.npc = 0.4, color = "deepskyblue")
maxtemp_swe

# average loch surface summer temperature
savgtemp_swe <- ggplot(data = lsurface_temp_swe_avg, aes(x = swe, y = mean_temp)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "deepskyblue") +
  #geom_smooth(method = "gam", colour = "purple") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01, label.x.npc = 0.4, color = "deepskyblue")
savgtemp_swe

# max loch surface summer temperature 
smaxtemp_swe <- ggplot(data = lsurface_temp_swe_max, aes(x = swe, y = max_temp)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "deepskyblue") +
  #geom_smooth(method = "gam", colour = "purple") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01, label.x.npc = 0.4, color = "deepskyblue")
smaxtemp_swe

# average sky surface summer temperature
skyavgtemp_swe <- ggplot(data = skysurface_temp_swe_avg, aes(x = swe, y = mean_temp)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "deepskyblue") +
  #geom_smooth(method = "gam", colour = "purple") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01, label.x.npc = 0.4, color = "deepskyblue")
skyavgtemp_swe

# max sky surface summer temperature
skymaxtemp_swe <- ggplot(data = skysurface_temp_swe_max, aes(x = swe, y = max_temp)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "deepskyblue") +
  #geom_smooth(method = "gam", colour = "purple") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01, label.x.npc = 0.4, color = "deepskyblue")
skymaxtemp_swe























