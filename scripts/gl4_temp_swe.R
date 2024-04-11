source("scripts/libraries.R")



# need to verify that max temperature is occurring in the summer. 
  # leave month grouping in place (rather than dropping and just having year), 
  # filter for summer months and work with that. 
  # summer is defined as june through august

# read in GL4 data
gl4_lake <- read.csv("data/water_quality_glv_dm.data.csv", sep = ",") %>%
  select(local_site, date, year, location, depth, temp) %>%
  mutate(local_site = as.factor(local_site)) %>%
  mutate(month = month(date)) %>%
  filter(local_site == "GL4") %>%
  mutate(location = as.factor(location)) %>%
  filter(location == "LAKE") %>%
  mutate(depth = as.factor(depth)) %>%
  filter(depth == 0)

# read in all glv data, i'm curious

glv_data <- read.csv("data/water_quality_glv_dm.data.csv", sep = ",") %>%
  select(local_site, date, year, location, depth, temp) %>%
  mutate(local_site = as.factor(local_site)) %>%
  mutate(month = month(date)) %>%
  #filter(local_site == "GL4") %>%
  mutate(location = as.factor(location)) #%>%
  #filter(location == "LAKE") %>%
  #mutate(depth = as.factor(depth)) %>%
  #filter(depth == 0)

# working with GL1 data 
gl1_lake <- read.csv("data/water_quality_glv_dm.data.csv", sep = ",") %>%
  select(local_site, date, year, location, depth, temp) %>%
  mutate(local_site = as.factor(local_site)) %>%
  mutate(month = month(date)) %>%
  filter(local_site == "GL1") %>%
  mutate(location = as.factor(location)) %>%
  filter(location == "LAKE") %>%
  mutate(depth = as.factor(depth)) %>%
  filter(depth == 0)

# create summary df for average lake temp. looking only at surface temp right now
  # average summer temp - GL4 is only sampled during the ice-free season (at least sonde profiles 
                                                        # are only taken during the summer)
gl4_lake_avg <- gl4_lake %>%
  group_by(year) %>%
  summarize(mean_temp = mean(temp, na.rm = TRUE)) 

# summary df for max lake temp
  # again, interested in max summer temp but this dataset is only summer
gl4_lake_max <- gl4_lake %>%
  group_by(year) %>%
  select(year, temp) %>%
  slice(which.max(temp)) %>%
  rename(max_temp = temp)

# read in niwot ridge snow survey data
gl4_swe_data <- read.csv("data/snowateq.mw.data.csv", sep = ",") %>%
  mutate(local_site = as.factor(local_site)) %>%
  filter(local_site == "GL4") %>%
  mutate(date = ymd(date)) %>%
  # mutate(month = month(date)) %>%
  mutate(year = year(date)) %>%
  select(samp_loc, year, swe) 

glv_swe_data <- read.csv("data/snowateq.mw.data.csv", sep = ",") %>%
  mutate(local_site = as.factor(local_site)) %>%
  #filter(local_site == "GL4") %>%
  mutate(date = ymd(date)) %>%
  # mutate(month = month(date)) %>%
  mutate(year = year(date)) #%>%
  #select(samp_loc, year, swe) 

# these data include a few different sites around GL4 - there is one sample per year 
  # per site. I'm averaging them & using that value as SWE for GL4 (rather than april for RMNP)
  # note that the snow survey is generally conducted in may at niwot ridge
gl4_swe <- gl4_swe_data %>%
  group_by(year) %>%
  summarize(mean_swe = mean(swe, na.rm = TRUE)) 

# join temp and swe df
gl4_temp_swe_avg <- left_join(gl4_lake_avg, gl4_swe, by = "year") %>%
  as.data.frame()
gl4_temp_swe_max <- left_join(gl4_lake_max, gl4_swe, by = "year") %>%
  as.data.frame()

# plot 
gavgtemp_swe <- ggplot(data = gl4_temp_swe_avg, aes(x = mean_swe, y = mean_temp)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "deepskyblue") +
  #geom_smooth(method = "gam", colour = "purple") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01, label.x.npc = 0.4, color = "deepskyblue")
gavgtemp_swe

gmaxtemp_swe <- ggplot(data = gl4_temp_swe_max, aes(x = mean_swe, y = max_temp)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "deepskyblue") +
  #geom_smooth(method = "gam", colour = "purple") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01, label.x.npc = 0.4, color = "deepskyblue")
gmaxtemp_swe











































