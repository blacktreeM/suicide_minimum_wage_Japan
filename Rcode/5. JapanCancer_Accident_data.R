# https://ganjoho.jp/reg_stat/statistics/data/dl/index.html
# https://ganjoho.jp/reg_stat/statistics/data/dl/excel/pref_AllCancer_mortality(1995-2023).xls
library(readxl); library(dplyr)
setwd("C:/Users/masan/Documents/suicide")
data = read_xls('pref_AllCancer_mortality(1995-2023).xls', sheet = 2) #cancer
data = data[, -c(5:10)]# delete total, young and old
data = data[, 1:16]
colnames(data) = c(c('fips', 'pref', 'year', 'gender'), 
                   paste0("age", gsub("-|歳", "", colnames(data)[5:16])))
data$gender = ifelse(data$gender == '男', 'male', ifelse(data$gender == '女', 'female', 'total'))
head(data); tail(data)
data = data %>% mutate(age2029 = age2024 + age2529, age3039 = age3034 + age3539, age4049 = age4044 + age4549,
                       age5059 = age5054 + age5559, age6069 = age6064 + age6569, age7079 = age7074 + age7579) %>% 
  select(fips, pref, year, gender, age2029, age3039, age4049, age5059, age6069, age7079); head(data)
cancer = data %>% pivot_longer(cols = -c('pref', 'year', 'fips', 'gender'), 
                             names_to = "age", values_to = "cancer") %>% mutate(age = gsub('age', '', age)); head(cancer)
data = read_xls('pref_AllCancer_mortality(1995-2023).xls', sheet = 3) #pop
data = data[, -c(5:10)]# delete total, young and old
data = data[, 1:16]
colnames(data) = c(c('fips', 'pref', 'year', 'gender'), 
                   paste0("age", gsub("-|歳", "", colnames(data)[5:16])))
data$gender = ifelse(data$gender == '男', 'male', ifelse(data$gender == '女', 'female', 'total'))
head(data)
data = data %>% mutate(age2029 = age2024 + age2529, age3039 = age3034 + age3539, age4049 = age4044 + age4549,
                       age5059 = age5054 + age5559, age6069 = age6064 + age6569, age7079 = age7074 + age7579) %>% 
  select(fips, pref, year, gender, age2029, age3039, age4049, age5059, age6069, age7079); head(data)
pop = data %>% pivot_longer(cols = -c('pref', 'year', 'fips', 'gender'), 
                               names_to = "age", values_to = "pop") %>% mutate(age = gsub('age', '', age)); head(pop)
cancer = merge(cancer, pop, by = c('pref', 'year', 'fips', 'gender', 'age'))
cancer = cancer %>% mutate(cancer_rate = 100000*cancer/pop, age = gsub('0', '0-', age)); head(cancer)
save(cancer, file = 'cancer.RDa')

###### accidental deaths
file = 'accidental_death.xlsx'
length(excel_sheets(file))
data = read_excel(file, sheet = 1)
year = substring(data[2,2], 1, 4)
(var = as.character(data[5, seq(4, ncol(data), 2)])) # only even numbers columns, starts at 4
data = data[6:nrow(data), c(1, 2, seq(4, ncol(data), 2))]
colnames(data) = c('fips', 'pref', var)
data$year = year
colnames(data)
for (i in 2:15){
  df = read_excel(file, sheet = i)
  year = substring(df[2,2], 1, 4)
  var = as.character(df[5, seq(4, ncol(df), 2)]) 
  df = df[6:nrow(df), c(1, 2, seq(4, ncol(df), 2))]
  colnames(df) = c('fips', 'pref', var)
  df$year = year
  data = bind_rows(data, df); print(i)
}
table(data$year)
colnames(data)
data = data %>% filter(nchar(fips) >= 5)# keep only prefectures
colnames(data) = ifelse(grepl("交通", names(data)), "car_death", names(data))
colnames(data) = ifelse(grepl("不慮", names(data)), "accidental_death", names(data))
data[, 3:ncol(data)] = lapply(data[, 3:ncol(data)], function(x) as.numeric(x))
data$pref = gsub('県|府', '', data$pref)
data$pref = gsub('東京都', '東京', data$pref)
summary(data)
accident = data
save(accident, file = 'accident.RDa')
