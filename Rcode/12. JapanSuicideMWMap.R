setwd("C:/Users/masan/Documents/suicide")
rm(list = ls())
# MW map
load('suicideMW.RDa'); library(dplyr);library(ggplot2); library(NipponMap); library(zoo)
mw = data %>% group_by(pref) %>% mutate(fips = ifelse(year >= 2023 & is.na(fips), # 2023 fips missing
                                                        na.locf(fips, na.rm = FALSE), fips)) %>% ungroup(); table(mw$year)
mw = mw %>% filter(gender == 'male' & age == '20-29', year == 2024 & pref != '全国') %>%  select(fips, mw) %>% arrange(fips); mw
hist(mw$mw)
n = 7 # categories
(breaks = (seq(from = min(mw$mw), to = max(mw$mw), length.out = 1 + n)))
breaks = quantile(mw$mw, prob = seq(0, 1, length.out = n + 1)); breaks
(breaks = as.character(round(breaks, 0)))
(MW = sort(unique(mw$mw)))
(MW_cut = cut(MW, breaks = breaks, include.lowest = T, labels = F))
(color_palette = colorRampPalette(c("white", "black"))(n))  
(fill = color_palette[MW_cut])
# Assign colors based on which break the mw value falls into
(color_mapping = tibble(mw = MW, fill))
map_data = mw %>% 
  left_join(color_mapping, by = "mw"); head(map_data)
JapanPrefMap(col = map_data$fill, border = 'black', axes = F, inset = T) # inset for Okinawa
dev.off()
# legend
(legend_labels <- paste0(head(breaks, -1), "-", tail(breaks, -1)))
png(filename = "mapMW.png", width = 800, height = 800)
JapanPrefMap(col = map_data$fill, border = 'black', axes = F, inset = T) # inset for Okinawa
legend(x = 142, y = 38, legend = legend_labels, fill = color_palette, bty = 'n',
       border = "white", title = "Minimum wage in 2024",  cex = 2) 
dev.off()

# suicide map
load('suicideMW.RDa'); library(dplyr);library(ggplot2); library(NipponMap); library(zoo)
data = subset(data, age %in% c('20-29', '30-39', '40-49'))
data = data %>% group_by(pref) %>% mutate(fips = ifelse(year >= 2023 & is.na(fips), # 2023 fips missing
                                                        na.locf(fips, na.rm = FALSE), fips)) %>% ungroup()
data = data %>% mutate(sex = ifelse(gender == 'male', 'Men', 'Women'), id = paste(sex, age)) %>% 
  filter(pref != '全国') %>% select(fips, year, id, pop, suicide); table(data$id); head(data)
aggregate(pop~year, data = data, mean)
data = data %>% group_by(fips, id) %>% mutate(pop = sum(pop), suicide = sum(suicide), rate = 100000*suicide/pop, ID = row_number()) %>%
  ungroup() %>% filter(ID==1) %>% select(fips, id, rate); head(data)
aggregate(rate ~ id, data = data, mean)
aggregate(rate ~ id, data = data, max)
aggregate(rate ~ id, data = data, min)
#
men = sort(unique(data$id))[1:3]; men
women = sort(unique(data$id))[4:6]; women
mapping = function(i, x, y, n){
  df = data %>% filter(id == i) %>%  
    arrange(fips) %>% ungroup() %>%  select(fips, rate)
  n = n # categories
  (breaks = (seq(from = x, to = y, length.out = 1 + n)))
  (breaks = as.character(round(breaks, 1)))
  (suicide = sort(unique(df$rate)))
  (suicide_cut = cut(suicide, breaks = breaks, include.lowest = T, labels = F))
  (color_palette = colorRampPalette(c("white", "black"))(n))
  (fill = color_palette[suicide_cut])
  (color_mapping = tibble(rate = suicide, fill))
  map_data = df %>% 
    left_join(color_mapping, by = "rate")
  (legend_labels <- paste0(head(breaks, -1), "-", tail(breaks, -1)))
  JapanPrefMap(col = map_data$fill, border = 'black', axes = F, inset = T)
  legend(x = 142, y = 38, legend = legend_labels, fill = color_palette, bty = 'n',
         border = "white", title = paste0(i, ": 2009-2024"),  cex = 1.5) 
  print(i)
}
png(filename = 'mapSR.png', width = 800, height = 1200)
par(mfcol = c(3, 2), mar = c(0, 0, 0, 0)) # instead of mfrow
for (i in men) mapping(i, 22, 45, 7)
for (i in women) mapping(i, 7, 16, 7)
dev.off()
#png(filename = 'mapMen.png', width = 800, height = 1200)
#par(mfrow = c(3, 2), mar = c(0, 0, 0, 0))
#for (i in men) mapping(i, 20, 53, 7); dev.off()
#png(filename = 'mapWomen.png', width = 800, height = 1200)
#par(mfrow = c(3, 2), mar = c(0, 0, 0, 0))
#for (i in women) mapping(i, 7, 23, 7); dev.off()

# cancer map
load('suicideMW.RDa'); library(dplyr);library(ggplot2); library(NipponMap); library(zoo)
data = subset(data, age %in% c('20-29', '30-39', '40-49'))
data = data %>% group_by(pref) %>% mutate(fips = ifelse(year >= 2023 & is.na(fips), # 2023 fips missing
                                                        na.locf(fips, na.rm = FALSE), fips)) %>% ungroup()
data = data %>% mutate(sex = ifelse(gender == 'male', 'Men', 'Women'), id = paste(sex, age)) %>% 
  filter(pref != '全国') %>% select(fips, year, id, pop, cancer); table(data$id); head(data)
data = data %>% group_by(fips, id) %>% mutate(pop = sum(pop, na.rm = T), cancer = sum(cancer, na.rm = T),
                                              rate = 100000*cancer/pop, ID = row_number()) %>%
  ungroup() %>% filter(ID==1) %>% select(fips, id, rate); head(data)
aggregate(rate ~ id, data = data, mean)
#
men = sort(unique(data$id))[1:3]; men
women = sort(unique(data$id))[4:6]; women
mapping = function(i, x, y, n){
  df = data %>% filter(id == i) %>%  
    arrange(fips) %>% ungroup() %>%  select(fips, rate)
  n = n # categories
  (breaks = (seq(from = x, to = y, length.out = 1 + n)))
  (breaks = as.character(round(breaks, 1)))
  (suicide = sort(unique(df$rate)))
  (suicide_cut = cut(suicide, breaks = breaks, include.lowest = T, labels = F))
  (color_palette = colorRampPalette(c("white", "black"))(n))
  (fill = color_palette[suicide_cut])
  (color_mapping = tibble(rate = suicide, fill))
  map_data = df %>% 
    left_join(color_mapping, by = "rate")
  (legend_labels <- paste0(head(breaks, -1), "-", tail(breaks, -1)))
  JapanPrefMap(col = map_data$fill, border = 'black', axes = F, inset = T)
  legend(x = 142, y = 38, legend = legend_labels, fill = color_palette, bty = 'n',
         border = "white", title = paste0(i, ": 2009-2024"),  cex = 1.5) 
  print(i)
}
aggregate(rate ~ id, data = data, max)
aggregate(rate ~ id, data = data, min)
png(filename = 'mapCancer.png', width = 800, height = 1200)
par(mfcol = c(3, 2), mar = c(0, 0, 0, 0)) # instead of mfrow
for (i in men) mapping(i, 2, 45, 7)
for (i in women) mapping(i, 2, 56, 7)
dev.off()
