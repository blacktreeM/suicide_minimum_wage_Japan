# stats for back of envelope calculation
load('suicideMW.RDa'); table(data$year); library(dplyr)
# suicide rate
sr = data %>% filter(pref == '全国') %>% select(pref, year, age, gender, suicide, pop) %>% 
  mutate(id = paste0(gender, age)) %>% filter(id %in% c('male20-29', 'female20-29')) %>% 
  group_by(id) %>% 
  mutate(suicide = sum(suicide), pop = sum(pop), rate = 100000*suicide/pop, ID = row_number()) %>%  
  filter(ID == 1) %>% select(id, rate); head(sr)

# suicide number
s = data %>% filter(pref == '全国') %>% select(pref, year, age, gender, suicide, pop) %>% 
  mutate(id = paste0(gender, age)) %>% filter(id %in% c('male20-29', 'female20-29')) %>% 
  group_by(id) %>% 
  mutate(suicide = sum(suicide), pop = mean(pop), annual = suicide/15, ID = row_number()) %>%  
  filter(ID == 1) %>% select(id, suicide, pop, annual); head(s)
2.7*.70*6606000/100000
2.7*.44*6293000/100000

# mw increase from 2009 to 2024
(a = mean(subset(data, year == 2024 & pref != '全国')$mw))
(b = mean(subset(data, year == 2009 & pref != '全国')$mw))
(change = a - b)
100*change/b
change/(2024-2009)
change/(2024-2009)*170*12
change/(2024-2009)*160*12 # women

# average annual increase
mw = data %>% filter(pref != '全国' & gender == 'male' & age == '20-29') %>% 
  group_by(year) %>% mutate(mw = sum(mw), mw = mw/47, id = row_number()) %>% 
  filter(id == 1) %>% select(year, mw); mw
mw$growth = c(NA, (mw$mw[-1] - mw$mw[-nrow(mw)]) / mw$mw[-nrow(mw)] * 100); mw
mean(mw$growth, na.rm = T)

