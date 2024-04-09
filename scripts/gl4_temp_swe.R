source("scripts/libraries.R")

# read in GL4 data
gl4_lake <- read.csv("data/water_quality_glv_dm.data.csv", sep = ",") %>%
  select(local_site, year, location, depth, temp) %>%
  mutate(local_site = as.factor(local_site)) %>%
  filter(local_site == "GL4") %>%
  mutate(location = as.factor(location)) %>%
  filter(location == "LAKE") %>%
  mutate(depth = as.factor(depth)) %>%
  filter(depth == 0)

# create summary df for average lake temp. looking only at surface temp right now, 
  # feels more comparable to loch outlet
gl4_lake_avg <- gl4_lake %>%
  group_by(year) %>%
  summarize(mean_temp = mean(temp, na.rm = TRUE)) 

# summary df for max lake temp
gl4_lake_max <- gl4_lake %>%
  group_by(year) %>%
  select(year, temp) %>%
  slice(which.max(temp)) %>%
  rename(max_temp = temp)

# read in niwot ridge snow survey data
gl4_swe <- read.csv("data/snowateq.mw.data.csv", sep = ",") %>%
  mutate(local_site = as.factor(local_site)) %>%
  filter(local_site == "GL4") %>%
  mutate(date = ymd(date)) %>%
  # mutate(month = month(date)) %>%
  mutate(year = year(date)) %>%
  select(samp_loc, year, swe) 

gl4_swe_sum <- gl4_swe %>%
  group_by(year) %>%
  summarize(mean_swe = mean(swe, na.rm = TRUE)) 

# join temp and swe df
gl4_temp_swe_avg <- left_join(gl4_lake_avg, gl4_swe_sum, by = "year") %>%
  as.data.frame()
gl4_temp_swe_max <- left_join(gl4_lake_max, gl4_swe_sum, by = "year") %>%
  as.data.frame()

# plot 
gavgtemp_swe <- ggplot(data = gl4_temp_swe_avg, aes(x = mean_swe, y = mean_temp)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "deeppink") +
  #geom_smooth(method = "gam", colour = "purple") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01, label.x.npc = 0.4, color = "deeppink")
gavgtemp_swe

gmaxtemp_swe <- ggplot(data = gl4_temp_swe_max, aes(x = mean_swe, y = max_temp)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "deeppink") +
  #geom_smooth(method = "gam", colour = "purple") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01, label.x.npc = 0.4, color = "deeppink")
gmaxtemp_swe











































