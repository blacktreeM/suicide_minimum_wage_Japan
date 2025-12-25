### summary
load('suicideMW.RDa'); library(dplyr);library(tidyr); head(data)
data = data %>% mutate(mw = 100 * mw / cpi)
summary_vars = c('income', 'nonregular', "unemployedMen", "unemployedWomen",
                 'hourMen', 'hourWomen', 'stayer', 'welfare',
                 'college', 'married', 'alone', 'mental')
summary(data$hourMen)
subset(data, hourMen==0)
means = data %>% summarize(across(all_of(summary_vars), .fns = list(mean = mean), .names = "{.col}_{.fn}")); means
SD = data %>% summarize(across(all_of(summary_vars), .fns = list(sd = sd), .names = "{.col}_{.fn}")); SD
Max = data %>% summarize(across(all_of(summary_vars), .fns = list(max = max), .names = "{.col}_{.fn}"))
Min = data %>% summarize(across(all_of(summary_vars), .fns = list(min = min), .names = "{.col}_{.fn}"))
Vars = c('Income per capita (in 1,000 yen)', 'Share of nonregular workers',
         'Male unemployed rate', 'Female unemployed rate',
         'Average monthly hours worked (male)', 'Average monthly hours worked (female)',
         'Percent employed within the prefecture',
         'Households receiving public assistance (per 1,000 residents)', 
         'Percent college graduate',  'Percent married', 
         "Percent living alone", "New admissions in psychiatric hospitals (per 100,000 residents)"); length(vars)
summary_table = data.frame(var = Vars, means = t(means), min = t(Min), max = t(Max), SD = t(SD))
summary_table[,-1] = lapply(summary_table[,-1], function(x) format(round(x, 2), nsmall = 2)); summary_table
colnames(summary_table) = c('Variables', 'Mean', 'Min', 'Max', 'SD')
library(htmlTable)
writeLines(htmlTable(summary_table, rnames = F), "Table1Summary.html")

# minimum wage table
load('suicideMW.RDa'); library(dplyr);library(tidyr); head(data)
load('fips.RDa')
data = data %>% left_join(fips, by = 'pref') %>% filter(age == '20-29' & gender == 'male' & pref != '全国') %>% 
  arrange(fips) %>% select(year, Pref, mw) %>%
  pivot_wider(names_from = year, values_from = mw); head(data)
colnames(data)[1] = "Prefecture"
library(htmlTable)
writeLines(htmlTable(data, rnames = F), "TableA1MinimumWage.html")

#####
# combine all tables
tables = paste0(c('Table1Summary', 'Table2age', 'Table3unemployed', 'Table4financial',
                  'Table5Cancer', 'Table6Accident', 'Table7NonRegular', 'Table8Welfare', 'Table9age',
           'TableA1MinimumWage', 'TableA2_availability'), '.html'); tables
for (file in tables) {
  file_content = readLines(file)
  cat(file, file_content, file = 'Tables.html', append = TRUE, sep = "\n")
  cat("\n", file = 'Tables.html', append = T) 
}

# excel tables
library(rvest); library(writexl)
html_content <- read_html('Tables.html')
tables <- html_content %>% html_table()
tables[[1]]
write_xlsx(tables, "Tables.xlsx")
##

if(F){
  ### raw suicide data tables by group
  load('suicideMW.RDa'); library(dplyr);library(tidyr); library(htmlTable)
  load('fips.RDa')
  young = c('20-29', '30-39', '40-49')
  data = data %>% left_join(fips, by = 'pref') %>% filter(age %in% young) %>% 
    mutate(sex = ifelse(gender == 'male', 'Men', 'Women'), id = paste(sex, age)) 
  for (i in sort(unique(data$id))){
    df = data %>% filter(id == i) %>%  mutate(Pref = ifelse(is.na(Pref), 'Japan', Pref)) %>% 
      arrange(fips) %>% select(year, Pref, suicide) %>% mutate(suicide = as.character(suicide)) %>% 
      pivot_wider(names_from = year, values_from = suicide)
    colnames(df)[1] = "Prefecture"
    writeLines(htmlTable(df, rnames = F), paste("Table", i, "suicide.html"))
    print(i)
  }
  for (file in paste("Table", sort(unique(data$id)), "suicide.html")) {
    file_content = readLines(file)
    cat(file, file_content, file = 'TableSuicide.html', append = TRUE, sep = "\n")
    cat("\n", file = 'TableSuicide.html', append = T) 
  }
  ####
  ### raw cancer data tables by group
for (i in sort(unique(data$id))){
  df = data %>% filter(id == i & year<2024) %>%  mutate(Pref = ifelse(is.na(Pref), 'Japan', Pref)) %>% 
    arrange(fips) %>% select(year, Pref, cancer) %>% mutate(cancer = as.character(cancer)) %>% 
    pivot_wider(names_from = year, values_from = cancer)
  colnames(df)[1] = "Prefecture"
  writeLines(htmlTable(df, rnames = F), paste("Table", i, "cancer.html"))
  print(i)
}
for (file in paste("Table", sort(unique(data$id)), "cancer.html")) {
  file_content = readLines(file)
  cat(file, file_content, file = 'TableCancer.html', append = TRUE, sep = "\n")
  cat("\n", file = 'TableCancer.html', append = T) 
}
}
## show volatility, for reviewer#1
load('suicideMW.RDa'); library(dplyr);library(tidyr); library(htmlTable)
load('fips.RDa')
data = data %>% left_join(fips, by = 'pref') %>% 
  filter(age == '20-29') %>% 
  mutate(sex = ifelse(gender == 'male', 'Men', 'Women')) %>% 
  select(year, sex, pref, pop, suicide, rate) %>% 
  filter(pref=='鳥取' | pref=='東京'); data
data$pop = format(data$pop,  big.mark = ",", scientific = FALSE)
men1 = data %>% filter(sex == 'Men' & pref=='東京') %>% select(-sex, -pref) %>% 
  mutate(rate = round(rate, 1)); men1
men1$blank = NA; men1
men2 = data %>% filter(sex == 'Men' & pref=='鳥取') %>% select(-sex, -pref, -year) %>% 
  mutate(rate = round(rate, 1)); men2
(men = cbind(men1, men2))
colnames(men) = c('Year', 'Tokyo<br>population', 'Tokyo<br>suicide', 'Tokyo<br>suicide rate', '',
                  'Tottori<br>population', 'Tottori<br>suicide', 'Tottori<br>suicide rate'); men
###
women1 = data %>% filter(sex == 'Women' & pref=='東京') %>% select(-sex, -pref) %>% 
  mutate(rate = round(rate, 1))
women1$blank = NA; men1
women2 = data %>% filter(sex == 'Women' & pref=='鳥取') %>% select(-sex, -pref, -year) %>% 
  mutate(rate = round(rate, 1))
(women = cbind(women1, women2))
colnames(women) = c('Year', 'Tokyo<br>population', 'Tokyo<br>suicide', 'Tokyo<br>suicide rate', '',
                    'Tottori<br>population', 'Tottori<br>suicide', 'Tottori<br>suicide rate')
###
library(htmlTable)
writeLines(htmlTable(men, rnames = F), "TableMen20-29.html")
writeLines(htmlTable(women, rnames = F), "TableWomen20-29.html")
