library(ggplot2); library(dplyr); library(tidyr); library("gridExtra")
##### reproduce
Age = c(1519, 2024, 2529, 3034, 3539, 4044, 4549, 5054, 5559)
Men = c(48.1, 19.7, 6.9, 4.7, 3, 2.4, 2.4, 2.9, 3.6)
Women = c(54.6, 22.7, 11.9, 14, 15.1, 17.1, 18.3, 18.8, 19)
average = 14.1
data = data.frame(age = Age, Men = Men, Women = Women)
data = data %>% mutate(age_group = paste0(substring(age, 1, 2), '-', substring(age, 3, 4))) %>%
  mutate(age_group = factor(age_group, levels = unique(age_group))); data
data = data %>% pivot_longer(cols = c(Men, Women), names_to = "gender", values_to = "share"); data
ggplot(data, aes(x = age_group, y = share, fill = gender)) +
  geom_col(position = "dodge") +
  labs(x = "Age", y = "Share of minimum wage workers", fill = "") +
  scale_fill_manual(values = c("Men" = "lightgrey", "Women" = "darkgrey")) + # Custom colors
  theme_classic() + scale_y_continuous(expand = c(0,0), limits=c(0, 60)) +
  theme(axis.text.x = element_text(hjust = 1), legend.position = 'bottom')+
  geom_text(
    aes(label = sprintf("%.1f", share)), 
    position = position_dodge(width = 0.9),
    vjust = -0.5, size = 3.5,  color = "black")
ggsave('minimumWageAge.png', width = 8, height = 5)
######
rm(list=ls()) # 2009 = heisei 21
load('suicideMW.RDa'); colnames(data); table(data$age)
# which age groups committ more suicide?
young = c('20', '20-29', '30-39', '40-49')
data %>% filter(pref == '全国' & gender=='male') %>% group_by(age) %>% summarize(suicide = sum(suicide))
data %>% filter(pref == '全国' & gender=='female') %>% group_by(age) %>% summarize(suicide = sum(suicide))
data %>% filter(pref == '全国' & gender=='male') %>% mutate(young = ifelse(age %in% young, 'young', 'old')) %>% 
  group_by(young) %>% summarize(suicide = sum(suicide))
data %>% filter(pref == '全国' & gender=='female') %>% mutate(young = ifelse(age %in% young, 'young', 'old')) %>% 
  group_by(young) %>% summarize(suicide = sum(suicide))
men = data %>% filter(pref == '全国' & gender == 'male' & age %in% young) %>%
  select(year, age, rate, gender) %>% mutate(gender = ifelse(gender == 'male', 'Men', 'Women'),
                                             age = ifelse(age=='20', '15-19', age)); table(data$year)
women = data %>% filter(pref == '全国' & gender == 'female' & age %in% young) %>%
  select(year, age, rate, gender) %>% mutate(gender = ifelse(gender == 'male', 'Men', 'Women'),
                                             age = ifelse(age=='20', '15-19', age)); table(data$year)
start_color <- "lightgrey"
end_color <- "black"
library(scales)
mk = seq_gradient_pal(start_color, end_color)(seq(0, 1, length.out = length(unique(men$age))))
ggplot(data = men, aes(x = year, y = rate, group = age, color = age)) +
  theme_classic() +
  geom_line(size = 1.5) +
  scale_x_continuous(breaks = seq(2009, 2024, 1), limits = c(2009, 2024), name='') +
  scale_y_continuous(name = "Suicide rate (per 100,000)", limits=c(10, 50), expand=c(0,0)) +
  scale_color_manual(values = c('15-19'=mk[1], "20-29" = mk[2], "30-39" = mk[3], "40-49" = mk[4])) +
                                #"50-59" = mk[4], "60-69" = mk[5], "70-79" = mk[6])) +
  theme(legend.position='bottom', legend.title = element_blank(), legend.key.size = unit(2, "cm"),
        legend.margin = margin(-0.8, 0, -0.8, 0, unit = "cm")) +
  guides(colour = guide_legend(nrow = 1))
ggsave('suicide_men.png', width = 8, height = 5)
ggplot(data = women, aes(x = year, y = rate, group = age, color = age)) +
  theme_classic() +
  geom_line(size = 1.5) +
  scale_x_continuous(breaks = seq(2009, 2024, 1), limits = c(2009, 2024), name='') +
  scale_y_continuous(name = "Suicide rate (per 100,000)", limits=c(4, 18), expand=c(0,0)) +
  scale_color_manual(values = c('15-19'=mk[1], "20-29" = mk[2], "30-39" = mk[3], "40-49" = mk[4])) +
  #"50-59" = mk[4], "60-69" = mk[5], "70-79" = mk[6])) +
  theme(legend.position='bottom', legend.title = element_blank(), legend.key.size = unit(2, "cm"),
        legend.margin = margin(-0.8, 0, -0.8, 0, unit = "cm"))+
  guides(colour = guide_legend(nrow = 1))
ggsave('suicide_women.png', width = 8, height = 5)

# scatter plot
if(F){load('suicideMW.RDa'); library(dplyr);library(tidyr);library(ggrepel)
  load('fips.RDa')
  s2024 = data %>% left_join(fips, by = 'pref') %>% filter(year == 2024 & pref!='全国') %>% select(Pref, age, gender, rate, mw)
  s2009 = data %>% left_join(fips, by = 'pref') %>% filter(year == 2009 & pref!='全国') %>% select(Pref, age, gender, rate, mw)
  scatter = s2024 %>% left_join(s2009, by = c('Pref', 'age', 'gender')) %>% 
    mutate(y = rate.x - rate.y, x = log(mw.x) - log(mw.y)) %>% 
    mutate(sex = ifelse(gender == 'male', 'Men', 'Women'), id = paste(sex, age)) %>% 
    select(Pref, id, x, y); head(scatter); summary(scatter)
  ss = function(i){
    ggplot(subset(scatter, id == i), aes(x = x, y = y, label = Pref)) + theme_bw()+
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "darkslategray") +
      geom_text_repel() +
      labs(title = i,
           x = "Change in ln(minimum wage)",
           y = "Change in suicide rate")
  }
  png("scatter_men.png", width = 800, height = 1100)
  grid.arrange(ss('Men 20-29'), ss('Men 30-39'), ss('Men 40-49'),
               ss('Women 20-29'), ss('Women 30-39'),ss('Women 40-49'), ncol = 2)
  dev.off()
}
# scatter plot above: problem is that suicide rates fluctuation due to small population prefectures
load('suicideMW.RDa'); library(dplyr);library(tidyr);library(ggrepel)
# create lagged cpi and real minimum wage
subset(data, pref== '全国', select=c(year, cpi))
cpi1 = data %>% filter(gender=='male' & age == '20-29' & year != 2024) %>% select(pref, year, cpi) %>%
  mutate(year=year+1, lagcpi = cpi) %>% select(-cpi); tail(cpi1)
cpi = data %>% filter(gender=='male' & age == '20-29' & year == 2009) %>% mutate(lagcpi = cpi) %>%
  select(pref, year, lagcpi) %>% bind_rows(cpi1); table(cpi$year); head(cpi)
data = data %>% left_join(cpi, by = c('pref', 'year')) %>% mutate(realmw = 100 * lagmw / lagcpi)
load('fips.RDa')
data = data %>% left_join(fips, by = 'pref') %>% filter(pref!='全国') %>%
  select(Pref, year, age, gender, suicide, pop, mw, realmw, cpi); head(data); table(data$year)
unique(data$Pref)
data = data %>% mutate(Pref = ifelse(Pref == 'Gumma', 'Gunma', Pref))
young = c('20-29', '30-39', '40-49')
unique(data$year)
before = data %>% filter(year < 2012 & age %in% young) %>% group_by(Pref, age, gender) %>% 
  mutate(suicide = sum(suicide), pop = sum(pop),
         rate = 100000*suicide/pop, mw = 100*sum(mw)/sum(cpi), 
         id = row_number()) %>% filter(id==1) %>% ungroup() %>% select(Pref, age, gender, rate, mw) ; head(before,7)
after = data %>% filter(year > 2021 & age %in% young) %>% group_by(Pref, age, gender) %>% 
  mutate(suicide = sum(suicide), pop = sum(pop),
         rate = 100000*suicide/pop, mw = 100*sum(mw)/sum(cpi), 
         id = row_number()) %>% filter(id==1) %>% ungroup() %>% select(Pref, age, gender, rate, mw) ; head(after,7)
scatter = after %>% left_join(before, by = c('Pref', 'age', 'gender')) %>% 
  mutate(y = rate.x - rate.y, x = log(mw.x) - log(mw.y)) %>% 
  mutate(sex = ifelse(gender == 'male', 'Men', 'Women'), id = paste(sex, age)) %>% 
  select(Pref, id, x, y); head(scatter); summary(scatter)
ss = function(i){
  p = ggplot(subset(scatter, id == i), aes(x = x, y = y, label = Pref)) + theme_bw()+
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "darkslategray") +
    geom_text_repel() +
    labs(title = i,
         x = "Change in ln(real minimum wage)",
         y = "Change in suicide rate") +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  return(p)
}
png("scatter.png", width = 800, height = 1000)
sc = lapply(sort(unique(scatter$id)), function(x) ss(x))
marrangeGrob(sc, ncol = 2, nrow =3, top=NULL)
dev.off()

## national time series for reasons
load('suicideReason.RDa'); head(data)
reason = data %>% select(pref, year, gender, Family, Health, Money, Work, nojob) %>% filter(pref == '全国'); summary(reason)
load('suicideMW.RDa'); table(data$year, data$gender); colnames(data)
data = data %>% filter(pref == '全国' & age == '20-29') %>% 
  select(pref, year, gender, Men15, Women15, Men1564, Women1564); head(data)  
data = data %>% left_join(reason, by = c('pref', 'year', 'gender')); summary(data)
(men = data %>%
    filter(pref == '全国' & gender == 'male') %>% rename('Financial' = 'Money') %>% 
    mutate(across(c(Family, Health, Financial, Work), ~100000 * . / Men15)) %>% 
    select(year, Family, Health, Financial, Work, nojob) %>% 
    pivot_longer(names_to = 'reason', cols = c(Family, Health, Financial, Work)) %>% 
    mutate(reason = factor(reason, levels = c('Health', 'Financial', 'Family', 'Work'))))
(women = data %>% 
    filter(pref == '全国' & gender == 'female') %>% rename('Financial' = 'Money') %>% 
    mutate(across(c(Family, Health, Financial, Work), ~100000 * . / Men15)) %>% 
    select(year, Family, Health, Financial, Work, nojob) %>% 
    pivot_longer(names_to = 'reason', cols = c(Family, Health, Financial, Work)) %>% 
    mutate(reason = factor(reason, levels = c('Health', 'Financial', 'Family', 'Work'))))
aggregate(value ~ reason, data = men, mean)
aggregate(value ~ reason, data = women, mean)
start_color <-"black"
end_color <-  "lightgrey"
library(scales)
mk = seq_gradient_pal(start_color, end_color)(seq(0, 1, length.out = 4))
ggplot(data = men, aes(x = year, y = value, group = reason, color = reason)) +
  theme_classic() +
  geom_line(size = 1.5) +
  scale_x_continuous(breaks = seq(2009, 2024, 1), limits = c(2009, 2024), name='') +
  scale_y_continuous(name = "Suicide rate (per 100,000)", limits=c(0, 20), expand=c(0,0)) +
  scale_color_manual(values = c("Health" = mk[1], "Financial" = mk[2], "Family" = mk[3], 'Work' = mk[4]))+
  theme(legend.position='bottom', legend.title = element_blank(), legend.key.size = unit(2, "cm"),
        legend.margin = margin(-0.8, 0, -0.8, 0, unit = "cm")) +
  guides(colour = guide_legend(nrow = 1)); ggsave('suicide_men_reason.png', width = 8, height = 5)
ggplot(data = women, aes(x = year, y = value, group = reason, color = reason)) +
  theme_classic() +
  geom_line(size = 1.5) +
  scale_x_continuous(breaks = seq(2009, 2024, 1), limits = c(2009, 2024), name='') +
  scale_y_continuous(name = "Suicide rate (per 100,000)", limits=c(0, 15), expand=c(0,0)) +
  scale_color_manual(values = c("Health" = mk[1], "Financial" = mk[2], "Family" = mk[3], 'Work' = mk[4]))+
  theme(legend.position='bottom', legend.title = element_blank(), legend.key.size = unit(2, "cm"),
        legend.margin = margin(-0.8, 0, -0.8, 0, unit = "cm")) +
  guides(colour = guide_legend(nrow = 1))
ggsave('suicide_women_reason.png', width = 8, height = 5)
# unemployed
(men = data %>%
    filter(pref == '全国' & gender == 'male') %>% mutate(nojob = 100000 * nojob / Men1564)%>% 
    select(year, nojob, gender) %>% mutate(gender = 'Men'))
(women = data %>% 
    filter(pref == '全国' & gender == 'female') %>% mutate(nojob = 100000 * nojob / Men1564)%>% 
    select(year, nojob, gender) %>% mutate(gender = 'Women'))
(nojob = rbind(men, women))
ggplot(data = nojob, aes(x = year, y = nojob, group = gender, color = gender)) +
  theme_classic() +
  geom_line(size = 1.5) +
  scale_x_continuous(breaks = seq(2009, 2024, 1), limits = c(2009, 2024), name='') +
  scale_y_continuous(name = "Suicide rate (per 100,000)", limits=c(0, 5.5), expand=c(0,0)) +
  scale_color_manual(values = c("Men" = 'black', "Women" = 'darkgrey'))+
  theme(legend.position='bottom', legend.title = element_blank(), legend.key.size = unit(2, "cm"),
        legend.margin = margin(-0.8, 0, -0.8, 0, unit = "cm")) +
  guides(colour = guide_legend(nrow = 1))
ggsave('suicide_unemployed.png', width = 8, height = 5)

################ scatter reasons
rm(list=ls())
library(ggplot2)
setwd('C:/Users/masan/Documents/suicide')
load('suicideReason.RDa'); head(data)
reason = subset(data, select = c(pref, year, gender, Money), pref != '全国'); summary(reason)
load('suicideMW.RDa'); table(data$year, data$gender); colnames(data)
data = data %>% filter(pref != '全国' & age == '20') %>% 
  select(pref, year, gender, Men15, Men1564, Women15, Women1564, mw, cpi)
reason = reason %>% mutate(pref = gsub('県|府', '', pref), pref = gsub("東京都", "東京", pref))
data = data %>% left_join(reason, by = c('pref', 'year', 'gender')); head(data)
load('fips.RDa'); head(fips)
data = data %>% left_join(fips, by = 'pref') %>% filter(pref!='全国'); head(data)
beforeMen = data %>% filter(year < 2012 & gender == 'male') %>% group_by(Pref) %>% 
  mutate(rate = 100000 * sum(Money) / sum(Men15), mw = 100*sum(mw)/sum(cpi), 
         id = row_number()) %>% filter(id==1) %>% ungroup() %>% select(Pref, rate, mw, gender) ; head(beforeMen,7)
beforeWomen = data %>% filter(year < 2012 & gender == 'female') %>% group_by(Pref) %>% 
  mutate(rate = 100000 * sum(Money) / sum(Women15), mw = 100*sum(mw)/sum(cpi), 
         id = row_number()) %>% filter(id==1) %>% ungroup() %>% select(Pref, rate, mw, gender) ; head(beforeWomen,7)
before = rbind(beforeMen, beforeWomen)
afterMen = data %>% filter(year > 2021 & gender == 'male') %>% group_by(Pref) %>% 
  mutate(rate = 100000 * sum(Money) / sum(Men15), mw = 100*sum(mw)/sum(cpi), 
         id = row_number()) %>% filter(id==1) %>% ungroup() %>% select(Pref, rate, mw, gender) ; head(beforeMen,7)
afterWomen = data %>% filter(year > 2021 & gender == 'female') %>% group_by(Pref) %>% 
  mutate(rate = 100000 * sum(Money) / sum(Women15), mw = 100*sum(mw)/sum(cpi), 
         id = row_number()) %>% filter(id==1) %>% ungroup() %>% select(Pref, rate, mw, gender) ; head(beforeWomen,7)
after = rbind(afterMen, afterWomen)
scatter = after %>% left_join(before, by = c('Pref', 'gender')) %>% 
  mutate(y = rate.x - rate.y, x = log(mw.x) - log(mw.y)) %>% 
  mutate(sex = ifelse(gender == 'male', 'Men', 'Women')) %>% 
  select(Pref, sex, x, y); head(scatter); summary(scatter)
ss = function(i){
  p = ggplot(subset(scatter, sex == i), aes(x = x, y = y, label = Pref)) + theme_bw()+
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "darkslategray") +
    geom_text_repel() +
    labs(title = i,
         x = "Change in ln(real minimum wage)",
           y = "Change in suicide rate caused by financial difficulties") +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  return(p)
}
ss('Men')
dev.off()
png("scatterFinancial.png", width = 800, height = 400)
marrangeGrob(list(ss('Men'), ss('Women')), ncol = 2, nrow =1, top=NULL)
dev.off()

#### scatter unemployed
setwd('C:/Users/masan/Documents/suicide')
load('suicideReason.RDa'); head(data)
reason = subset(data, select = c(pref, year, gender, nojob), pref != '全国'); summary(reason)
load('suicideMW.RDa'); table(data$year, data$gender); colnames(data)
data = data %>% filter(pref != '全国' & age == '20') %>% 
  select(pref, year, gender, Men15, Men1564, Women15, Women1564, mw, cpi)
reason = reason %>% mutate(pref = gsub('県|府', '', pref), pref = gsub("東京都", "東京", pref))
data = data %>% left_join(reason, by = c('pref', 'year', 'gender')); head(data)
load('fips.RDa'); head(fips)
data = data %>% left_join(fips, by = 'pref') %>% filter(pref!='全国'); head(data)
beforeMen = data %>% filter(year < 2012 & gender == 'male') %>% group_by(Pref) %>% 
  mutate(rate = 100000 * sum(nojob) / sum(Men1564), mw = 100*sum(mw)/sum(cpi), 
         id = row_number()) %>% filter(id==1) %>% ungroup() %>% select(Pref, rate, mw, gender) ; head(beforeMen,7)
beforeWomen = data %>% filter(year < 2012 & gender == 'female') %>% group_by(Pref) %>% 
  mutate(rate = 100000 * sum(nojob) / sum(Women1564), mw = 100*sum(mw)/sum(cpi), 
         id = row_number()) %>% filter(id==1) %>% ungroup() %>% select(Pref, rate, mw, gender) ; head(beforeWomen,7)
before = rbind(beforeMen, beforeWomen)
afterMen = data %>% filter(year > 2021 & gender == 'male') %>% group_by(Pref) %>% 
  mutate(rate = 100000 * sum(nojob) / sum(Men1564), mw = 100*sum(mw)/sum(cpi), 
         id = row_number()) %>% filter(id==1) %>% ungroup() %>% select(Pref, rate, mw, gender) ; head(beforeMen,7)
afterWomen = data %>% filter(year > 2021 & gender == 'female') %>% group_by(Pref) %>% 
  mutate(rate = 100000 * sum(nojob) / sum(Women1564), mw = 100*sum(mw)/sum(cpi), 
         id = row_number()) %>% filter(id==1) %>% ungroup() %>% select(Pref, rate, mw, gender) ; head(beforeWomen,7)
after = rbind(afterMen, afterWomen)
scatter = after %>% left_join(before, by = c('Pref', 'gender')) %>% 
  mutate(y = rate.x - rate.y, x = log(mw.x) - log(mw.y)) %>% 
  mutate(sex = ifelse(gender == 'male', 'Men', 'Women')) %>% 
  select(Pref, sex, x, y); head(scatter); summary(scatter)
ss = function(i){
  p = ggplot(subset(scatter, sex == i), aes(x = x, y = y, label = Pref)) + theme_bw()+
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "darkslategray") +
    geom_text_repel() +
    labs(title = i,
         x = "Change in ln(real minimum wage)",
         y = "Change in suicide rate by unemployed") +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  return(p)
}
ss('Men')
dev.off()
png("scatterUnemployed.png", width = 800, height = 400)
marrangeGrob(list(ss('Men'), ss('Women')), ncol = 2, nrow =1, top=NULL)
dev.off()