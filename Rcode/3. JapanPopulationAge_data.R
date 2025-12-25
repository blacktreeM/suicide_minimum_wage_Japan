library(readxl); library(dplyr); library(tidyr)
setwd("C:/Users/masan/Documents/suicide")
file = 'data_age.xlsx'
length(excel_sheets(file))
data = read_excel(file, sheet = 1)
year = substring(data[2,2], 1, 4)
var = as.character(data[5, seq(4, ncol(data), 2)]) # only even numbers columns, starts at 4
data = data[6:nrow(data), c(1, 2, seq(4, ncol(data), 2))]
colnames(data) = c('fips', 'pref', var)
data$year = year
colnames(data)
for (i in 2:16){
  df = read_excel(file, sheet = i)
  year = substring(df[2,2], 1, 4)
  var = as.character(df[5, seq(4, ncol(df), 2)]) 
  df = df[6:nrow(df), c(1, 2, seq(4, ncol(df), 2))]
  colnames(df) = c('fips', 'pref', var)
  df$year = year
  data = bind_rows(data, df); print(i)
}
table(data$year); colnames(data); head(data)
### age 15-19
file = 'data_teen.xlsx'
length(excel_sheets(file))
teen = read_excel(file, sheet = 1)
year = substring(teen[2,2], 1, 4)
(var = as.character(teen[5, seq(4, ncol(teen), 2)])) # only even numbers columns, starts at 4
teen = teen[6:nrow(teen), c(1, 2, seq(4, ncol(teen), 2))]
colnames(teen) = c('fips', 'pref', var)
teen$year = year
colnames(teen)
for (i in 2:16){
  df = read_excel(file, sheet = i)
  year = substring(df[2,2], 1, 4)
  var = as.character(df[5, seq(4, ncol(df), 2)]) 
  df = df[6:nrow(df), c(1, 2, seq(4, ncol(df), 2))]
  colnames(df) = c('fips', 'pref', var)
  df$year = year
  teen = bind_rows(teen, df); print(i)
}
table(teen$year); head(teen)
pop = merge(data, teen, by = c('fips', 'year', 'pref')); colnames(pop)
pop = pop %>% filter(nchar(fips) >= 5); unique(pop$pref)# keep only prefectures
(names(pop) <- gsub("[[:space:]]", "", names(pop)))  # Remove spaces
(names(pop) <- gsub("（|）|歳人口|～", "", names(pop)))    # Remove parentheses (need to escape them)
(names(pop) <- gsub("【人】", "", names(pop)))
(names(pop) <- gsub("男", "men", names(pop)))
(names(pop) <- gsub("女", "women", names(pop)))
(names(pop)[4:ncol(pop)] <- substring(names(pop)[4:ncol(pop)], 9, 17))
head(pop)
pop[, c(2, 4:ncol(pop))] = lapply(pop[, c(2, 4:ncol(pop))], function(x) as.numeric(gsub(',', '', x))); summary(pop)
pop = pop %>% mutate(men1519 = `1519men`, women1519 = `1519women`,
                     men2029 = `2024men` + `2529men`,
                     women2029 = `2024women` + `2529women`,
                     men3039 = `3034men` + `3539men`, 
                     women3039 = `3034women` + `3539women`,
                     men4049 = `4044men` + `4549men`,
                     women4049 = `4044women` + `4549women`,
                     men5059 = `5054men` + `5559men`,
                     women5059 = `5054women` + `5559women`,
                     men6069 = `6064men` + `6569men`, 
                     women6069 = `6064women` + `6569women`,
                     men7079 = `7074men` + `7579men`,
                     women7079 = `7074women` + `7579women`)
men = pop %>% select(pref, year, paste0('men', c(1519, 2029, 3039, 4049, 5059, 6069, 7079))) %>% 
  pivot_longer(cols = -c(pref, year), names_to = "age", values_to = 'pop') %>% mutate(gender = 'male'); head(men) 
women = pop %>% select(pref, year, paste0('women', c(1519, 2029, 3039, 4049, 5059, 6069, 7079))) %>% 
  pivot_longer(cols = -c(pref, year), names_to = "age", values_to = 'pop') %>% mutate(gender = 'female'); head(women) 
pop = rbind(men, women); head(pop)
pop$age = gsub("men|women", '', pop$age); table(pop$age); table(pop$year)
save(pop, file = 'pop.RDa')
