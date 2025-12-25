rm(list=ls())
library(dplyr)
setwd("C:/Users/masan/Documents/suicide")
load('suicideMen.RDa'); colnames(data)
vars = c("都道府県コード", "都道府県名", "自殺者数", "家庭問題", "健康問題", 
         "経済・生活問題", "勤務問題", "男女問題", "学校問題", 'year')
men = subset(data, select = vars)
men$gender = 'male'
load('suicideWomen.RDa')
women = subset(data, select = vars)
women$gender = 'female'
data = rbind(men, women); head(data)
colnames(data) = c('code', 'pref', 'n', 'Family', 'Health', 'Money', 'Work', 'love', 
                     'school', 'year', 'gender'); head(data)
(numeric_cols = names(data)[names(data) != 'pref' & names(data) != 'gender']) 
data[numeric_cols] <- lapply(data[numeric_cols], as.numeric);head(data)
data[data$code == 0, ]
# perhaps more than one reason is allowed
data$total = rowSums(data[, 4:9]); head(data)
data$total = NULL
reason = subset(data, pref != '不明')
aggregate(Family ~ gender, data = reason, mean)
aggregate(Health ~ gender, data = reason, mean)
aggregate(Money ~ gender, data = reason, mean)
aggregate(Work ~ gender, data = reason, mean)#
########
# unemployed
load('suicideMen.RDa'); colnames(data)
vars = c("都道府県コード", "都道府県名", "自殺者数", "失業者", 'year')
men = subset(data, select = vars)
men$gender = 'male'
load('suicideWomen.RDa')
women = subset(data, select = vars)
women$gender = 'female'
nojob = rbind(men, women); head(data)
colnames(nojob) = c('code', 'pref', 'n', 'nojob', 'year', 'gender')
(numeric_cols = names(nojob)[names(nojob) != 'pref' & names(nojob) != 'gender']) 
nojob[numeric_cols] <- lapply(nojob[numeric_cols], as.numeric);head(nojob)
print(subset(nojob, code==0), n = 16)
nojob = subset(nojob, select = -n, pref != '不明')
data = reason %>% left_join(nojob, by = c('year', 'gender', 'pref', 'code')); head(data)
save(data, file = 'suicideReason.RDa')

######## not used below
if(F){
# divide data based on category
suicide = data[,c(1:4, ncol(data))]
colnames(suicide) = c('code', 'pref', 'n', 'rate', 'year')
numeric_cols = names(suicide)[names(suicide) != 'pref'] 
suicide[numeric_cols] <- lapply(suicide[numeric_cols], as.numeric)
summary(suicide)
check = aggregate(rate ~ pref, data = suicide, mean)
check[order(check$rate), ]
aggregate(rate ~ year, data = suicide, mean)
suicide[suicide$code==0, c('year', 'n', 'rate')]
# by method
colnames(data)[35:41]
method = data[,c(1:3, 35:41, ncol(data))]
colnames(method) = c('code', 'pref', 'n', 'hang', 'poison', 'rentan', 'jump', 'drown', 'other', 'unknown', 'year')
head(method)
(numeric_cols = names(method)[names(method) != 'pref']) 
method[numeric_cols] <- lapply(method[numeric_cols], as.numeric)
method[method$code == 0, ]
# method$total = rowSums(method[, 4:ncol(method)]); head(method) #checks out
method_percent = method %>% mutate(hang = hang/n, poison = poison/n, rentan = rentan/n, jump = jump/n,
                                   drown = drown/n, other = other/n, unknown = unknown/n); head(method_percent)
method_percent[4:10] = lapply(method_percent[4:10], function(x) round(100*x, 1))
method = merge(method, method_percent, by = c('code', 'pref', 'year', 'n'))
method[method$code == 0, ]

# combine "自営業・家族従業者" "被data# combinmethodsPackageMetaName()# combine "自営業・家族従業者" "被datamethod# combine "自営業・家族従業者" "被data# combinmethodsPackageMetaName()# combine "自営業・家族従業者" "被data# combine "自営業・家族従業者" "被雇用・勤め人" to "有職者"
data = data %>% mutate(emp = as.numeric("自営業・家族従業者") + as.numeric("被雇用・勤め人") )
setdiff(colnames(data), colnames(df))
setdiff(colnames(df), colnames(data))
numeric_cols = names(data)[names(data) != "都道府県名"] 
data[numeric_cols] <- lapply(data[numeric_cols], as.numeric)
summary(data)


data = rbind(data, df); print(i)
table(data$year)
"自営業・家族従業者" "被雇用・勤め人"     "学生・生徒等"       "経済・生活問題"     "男女問題"          
> setdiff(colnames(df), colnames(data))
[1] "有職者"           "学生・生徒"       "職業\n不詳"       "経済・\n生活問題" "交際問題"     

method_percent = method %>% mutate(hang = hang/n, poison = poison/n, rentan = rentan/n, jump = jump/n,
                                   drown = drown/n, other = other/n, unknown = unknown/n); head(method_percent)
method_percent[4:10] = lapply(method_percent[4:10], function(x) round(100*x, 1))
method = merge(method, method_percent, by = c('code', 'pref', 'year', 'n'))
method[method$code == 0, ]
}
