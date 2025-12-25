library(readxl); library(dplyr); library(htmlTable)
setwd("C:/Users/masan/Documents/suicide")
file = 'X.xlsx'
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
table(data$year)
colnames(data)
X = data
### non-regular employment share
data = read_excel('regular_worker.xlsx', sheet = 1)
year = substring(data[2,2], 1, 4)
var = as.character(data[5, seq(4, ncol(data), 2)]) # only even numbers columns, starts at 4
data = data[6:nrow(data), c(1, 2, seq(4, ncol(data), 2))]
colnames(data) = c('fips', 'pref', var)
data$year = year
colnames(data)
for (i in 2:10){
  df = read_excel('regular_worker.xlsx', sheet = i)
  year = substring(df[2,2], 1, 4)
  var = as.character(df[5, seq(4, ncol(df), 2)]) 
  df = df[6:nrow(df), c(1, 2, seq(4, ncol(df), 2))]
  colnames(df) = c('fips', 'pref', var)
  df$year = year
  data = bind_rows(data, df); print(i)
}
table(data$year)
colnames(data); head(data)
X = X %>% left_join(data, by = c('pref', 'fips', 'year'))
###）
data = X %>% filter(nchar(fips) >= 5); table(data$year)# keep only prefectures
colnames(data) <- ifelse(grepl("精神", names(data)), "mental", names(data))
colnames(data) <- ifelse(grepl("未婚", names(data)), "married", names(data))
colnames(data) <- ifelse(grepl("単独世帯", names(data)), "alone", names(data))
colnames(data) <- ifelse(grepl("生活保護", names(data)), "welfare", names(data))
colnames(data) <- ifelse(grepl("完全失業率（女）", names(data)), "unemployedWomen", names(data))
colnames(data) <- ifelse(grepl("完全失業率（男）", names(data)), "unemployedMen", names(data))
colnames(data) <- ifelse(grepl("時間数（女）（～2019", names(data)), "hourWomen2", names(data))
colnames(data) <- ifelse(grepl("時間数（男）（～2019", names(data)), "hourMen2", names(data))
colnames(data) <- ifelse(grepl("時間数（女", names(data)), "hourWomen1", names(data))
colnames(data) <- ifelse(grepl("時間数（男", names(data)), "hourMen1", names(data))
colnames(data) <- ifelse(grepl("最終学歴が大学", names(data)), "college", names(data))
colnames(data) <- ifelse(grepl("15～64歳人口（女）", names(data)), "Women1564", names(data))
colnames(data) <- ifelse(grepl("15～64歳人口（男）", names(data)), "Men1564", names(data))
colnames(data) <- ifelse(grepl("15歳以上人口（女", names(data)), "Women15", names(data))
colnames(data) <- ifelse(grepl("15歳以上人口（男", names(data)), "Men15", names(data))
colnames(data) <- ifelse(grepl("有効求人倍率", names(data)), "application", names(data))
colnames(data) <- ifelse(grepl("県内就業者比率", names(data)), "stayer", names(data))
colnames(data) <- ifelse(grepl("県民所得", names(data)), "income", names(data))
colnames(data) <- ifelse(grepl("L04414_消費者物価地域差指数（総合", names(data)), "cpi", names(data))
colnames(data) <- ifelse(grepl("雇用者数（国勢調査結果", names(data)), "workers", names(data))
colnames(data) <- ifelse(grepl("雇用者数（正規の職員・従業員", names(data)), "regular_workers", names(data))
colnames(data)
data = data %>% mutate(across(everything(), ~gsub(',', '', .))); head(data)
data = data %>% mutate(across(!pref, ~as.numeric(.)))
data = data %>% mutate(across(c(hourMen1, hourWomen1, hourMen2, hourWomen2), ~ifelse(is.na(.), 0, .)),
                       hourMen = hourMen1+hourMen2, hourWomen = hourWomen1+hourWomen2) %>% 
  select(-hourMen1, -hourWomen1, -hourMen2, -hourWomen2) %>% 
  mutate(married = 100 - married,
         nonregular = 100 - 100*(regular_workers/workers)) %>% select(-workers, -regular_workers)
summary(data)
# creating a data availability table for appendix
data1 = data %>% filter(fips==0) %>% select(year, income, nonregular, unemployedMen, hourMen,  # need only one gender
                                            mental, stayer, 
                                            welfare, college,  married, alone, cpi) %>%
  mutate(across(!year, ~ifelse(.>0, '✓', ''))) %>% arrange(year) %>% 
  select(year, everything());data1 
colnames(data1)
colnames(data1) = c('Year', 'ln(Income per capita) (in 1,000 yen)', 
                    "Share of Non-regular workers", 'Unemployed rate', 
                    'Average monthly hours worked', 
                    "New admissions in psychiatric hospitals", 'Percent employed within the prefecture',
                    'Households receiving public assistance','Percent college graduate', 'Percent married',
                    "Percent living alone", 'Index of consumer prices')
writeLines(htmlTable(t(data1)), "TableA2_availability.html")

# interpolate
data = data %>% arrange(pref, year) %>% mutate(hourMen = ifelse(hourMen==0, NA, hourMen), # some reason 2024, zero
                                               hourWomen = ifelse(hourWomen==0, NA, hourWomen))
if(!require(zoo)) install.packages("zoo"); library(zoo)
impute_cols <- c('nonregular', "unemployedMen", "unemployedWomen",'hourMen', 'hourWomen', 'application', 'stayer', 'mental',
                 'welfare', 'alone', "college", "married", "income", 'cpi')
X = data %>%
  group_by(pref) %>%
  mutate(across(all_of(impute_cols), ~ {
    interpolated = na.approx(., na.rm = FALSE)
    forward_filled <- na.locf(interpolated, na.rm = FALSE)
    na.locf(forward_filled, fromLast = TRUE, na.rm = FALSE)
  })) %>% ungroup(); X
summary(X)
save(X, file = 'X.RDa')
table(X$year)
###################################
# spline interpolation
# Structure for spline interpolation within a panel data frame:
X_spline = data %>%
  group_by(pref) %>%
  mutate(across(all_of(impute_cols), ~ {
    interpolated = na.spline(., na.rm = FALSE) 
    forward_filled <- na.locf(interpolated, na.rm = FALSE)
    na.locf(forward_filled, fromLast = TRUE, na.rm = FALSE)
  })) %>% ungroup(); X_spline
summary(X_spline) # doesn't work, negative and often ridiculous values for some!
# curious
data %>% filter(year == 2020) %>% arrange(welfare) 
data %>% filter(year == 2020) %>% arrange(desc(welfare))
data %>% filter(year == 2020) %>% arrange(cpi) 
data %>% filter(year == 2020) %>% arrange(desc(cpi))