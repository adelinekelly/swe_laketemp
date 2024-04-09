source("scripts/libraries.R")

# loch data - outlet temp record and bear lake snotel 

# read in loch data 
  # temp for all sites - will subset for loch outlet below but this is all here 
  # if we want to look at other loch sites
loch_data <- read.csv("data/loch_chem.csv") %>%
  select(2:7, 9)

# filter for loch outlet 
loch_outlet <- loch_data %>%
  filter(SITE == "LOCH.O" & TYPE == "NORMAL")

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

# plot 
avgtemp_swe <- ggplot(data = loch_temp_swe_avg, aes(x = swe, y = mean_temp)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "deeppink") +
  #geom_smooth(method = "gam", colour = "purple") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01, label.x.npc = 0.4, color = "deeppink")
avgtemp_swe

maxtemp_swe <- ggplot(data = loch_temp_swe_max, aes(x = swe, y = max_temp)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "deeppink") +
  #geom_smooth(method = "gam", colour = "purple") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01, label.x.npc = 0.4, color = "deeppink")
maxtemp_swe



























