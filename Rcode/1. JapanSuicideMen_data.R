library(readxl); library(dplyr)
setwd("C:/Users/masan/Documents/suicide")
# https://www.mhlw.go.jp/content/001464821.zip for 2024
(files = list.files(getwd(), pattern = "\\.xls$")); length(files)
df = read_xls(files[1], sheet = 2) # sheet 2 is men
mk = df[3:6,]
(overall = mk[1, 1:5])
(a = mk[1, 6:ncol(mk)])
(category = a[1, colSums(is.na(a)) == 0])
(sub1 = mk[2, ])
(sub2 = mk[3, colSums(is.na(mk[3,])) == 0])
(sub3 = mk[4, colSums(is.na(mk[4,])) == 0])
(occupation = cbind(sub2, sub3))
sub1[1, 21:26] = occupation
mk = cbind(overall, sub1[1, 6:ncol(sub1)])
data = df[7:nrow(df),]
colnames(data) = mk[1,]
data$year = 2009
for (i in 2:5){
  df = read_xls(files[i], sheet = 2)
  mk = df[3:6,]
  (overall = mk[1, 1:5])
  (a = mk[1, 6:ncol(mk)])
  (category = a[1, colSums(is.na(a)) == 0])
  (sub1 = mk[2, ])
  (sub2 = mk[3, colSums(is.na(mk[3,])) == 0])
  (sub3 = mk[4, colSums(is.na(mk[4,])) == 0])
  (occupation = cbind(sub2, sub3))
  sub1[1, 21:26] = occupation
  mk = cbind(overall, sub1[1, 6:ncol(sub1)])
  df = df[7:nrow(df),]
  colnames(df) = mk[1,]
  df$year = i + 2008
  data = rbind(data, df); print(i)
}
table(data$year)
# for 2014, 対前年比columns added 4, 6, 8
df = read_xls(files[6], sheet = 2)
df = df[, -c(4,6,8)]
mk = df[3:6,]
(overall = mk[1, 1:5])
(a = mk[1, 6:ncol(mk)])
(category = a[1, colSums(is.na(a)) == 0])
(sub1 = mk[2, ])
(sub2 = mk[3, colSums(is.na(mk[3,])) == 0])
(sub3 = mk[4, colSums(is.na(mk[4,])) == 0])
(occupation = cbind(sub2, sub3))
sub1[1, 21:26] = occupation
mk = cbind(overall, sub1[1, 6:ncol(sub1)])
df = df[7:nrow(df),]
colnames(df) = mk[1,]
df$year = 2014
data = rbind(data, df); table(data$year)
for (i in 7:13){
  df = read_xls(files[i], sheet = 2)
  mk = df[3:6,]
  (overall = mk[1, 1:5])
  (a = mk[1, 6:ncol(mk)])
  (category = a[1, colSums(is.na(a)) == 0])
  (sub1 = mk[2, ])
  (sub2 = mk[3, colSums(is.na(mk[3,])) == 0])
  (sub3 = mk[4, colSums(is.na(mk[4,])) == 0])
  (occupation = cbind(sub2, sub3))
  sub1[1, 21:26] = occupation
  mk = cbind(overall, sub1[1, 6:ncol(sub1)])
  df = df[7:nrow(df),]
  colnames(df) = mk[1,]
  df$year = i + 2008
  data = rbind(data, df); print(i)
}
table(data$year)
# 2022-2024
df1 = read_xls(files[14], sheet = 2)[-c(1:6),]
(mk = df1[1,])
colnames(df1) = mk[1,]
df1 = df1[-1,]
df1$year = 2022
df2 = read_xls(files[15], sheet = 2)[-c(1:6),]
colnames(df2) = colnames(df1)
df2 = df2[-1,]
df2$year = 2023
df3 = read_xls(files[16], sheet = 2)[-c(1:6),]
colnames(df3) = colnames(df1)
df3 = df3[-1,]
df3$year = 2024
df = rbind(df1, df2, df3); table(df$year)
# 2022 and 2023 have "有職者", previous years have "自営業・家族従業者" "被雇用・勤め人"
colnames(df)[18:26]
colnames(data)[18:27]
# insert a blank column
df = data.frame(df[, 1:18], new_col = NA, df[, 19:ncol(df)])
colnames(df) = colnames(data)
data = rbind(data, df)
table(data$year)
save(data, file = 'suicideMen.RDa')