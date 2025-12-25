if(!require(pdftools)){install.packages("pdftools")}; library(pdftools)
if(!require(tidyverse)){install.packages("tidyverse")}; library(tidyverse)
if(!require(tidyr)){install.packages("tidyr")}; library(tidyr)
#平成６-令和６
year = 1994:2024
pdf_text <- pdf_text("001309125.pdf") # https://www.mhlw.go.jp/content/11200000/001309125.pdf
lines = str_split(pdf_text[1], "\n")[[1]]
lines = lines[lines != ""]
lines = gsub("\\s+", " ", lines)
lines = gsub(",", "", lines)
df = read.table(text = lines, fill = TRUE)
df = df[-c(1:2, nrow(df)),]; df[1, 1:3]; df[nrow(df), 1:3]
# 北海道、神奈川、和歌山、鹿児島
three = c('北','神','和','鹿')
colnames(df)[1] = 'pref'
df$pref = ifelse(!(df$pref %in% three), paste0(df$pref, df$V2), paste0(df$pref, df$V2, df$V3));df[,1]
df[,-1]  = lapply(df[,-1], function(x) as.numeric(x))
df[,-1]  = lapply(df[,-1], function(x) ifelse(x < 600 | is.na(x), 0, x))
df = df[, colSums(df == 0) != nrow(df)]
df$x <- character(nrow(df))  # Initialize the new column
for (i in 1:nrow(df)) {
  mk <- df[i, -1][df[i,-1] > 0]
  df$x[i] <- paste(mk, collapse = ", ")
}
page1 = subset(df, select = c(pref, x)); head(page1)
page1$x = gsub('\\ ', '', page1$x)
mk = strsplit(page1$x, ",")
page1$mw1 <- sapply(mk, function(x) as.numeric(x[1]))
page1$mw2 <- sapply(mk, function(x) as.numeric(x[2]))
page1$mw3 <- sapply(mk, function(x) as.numeric(x[3]))
page1$mw4 <- sapply(mk, function(x) as.numeric(x[4]))
page1$mw5 <- sapply(mk, function(x) as.numeric(x[4]))
page1$x = NULL; summary(page1)
page1 = page1 %>% pivot_longer(cols = starts_with('mw'), names_to = 'year', values_to = 'mw')
page1$year = rep(2002:2006, 47); head(page1)
#
lines = str_split(pdf_text[2], "\n")[[1]]
lines = lines[lines != ""]
lines = gsub("\\s+", " ", lines)
lines = gsub(",", "", lines)
df = read.table(text = lines, fill = TRUE)
df = df[-c(1:2, nrow(df)),]; df[1, 1:3]; df[nrow(df), 1:3]
# 北海道、神奈川、和歌山、鹿児島
three = c('北','神','和','鹿')
colnames(df)[1] = 'pref'
df$pref = ifelse(!(df$pref %in% three), paste0(df$pref, df$V2), paste0(df$pref, df$V2, df$V3));df[,1]
df[,-1]  = lapply(df[,-1], function(x) as.numeric(x))
df[,-1]  = lapply(df[,-1], function(x) ifelse(x < 600 | is.na(x), 0, x))
df = df[, colSums(df == 0) != nrow(df)]
df$x <- character(nrow(df))  # Initialize the new column
for (i in 1:nrow(df)) {
  mk <- df[i, -1][df[i,-1] > 0]
  df$x[i] <- paste(mk, collapse = ", ")
}
page2 = subset(df, select = c(pref, x)); head(page1)
page2 = page2[-which(df$x == ""), ]
page2$x = gsub('\\ ', '', page2$x)
mk = strsplit(page2$x, ",")
page2$mw1 <- sapply(mk, function(x) as.numeric(x[1]))
page2$mw2 <- sapply(mk, function(x) as.numeric(x[2]))
page2$mw3 <- sapply(mk, function(x) as.numeric(x[3]))
page2$mw4 <- sapply(mk, function(x) as.numeric(x[4]))
page2$mw5 <- sapply(mk, function(x) as.numeric(x[5]))
page2$x = NULL; summary(page2) # there is one NA
page2[is.na(page2$mw5),]
page2[is.na(page2$mw5), 'mw5'] = 647; summary(page2)
page2 = page2 %>% pivot_longer(cols = starts_with('mw'), names_to = 'year', values_to = 'mw')
page2$year = rep(2007:2011, 47); head(page2)
#
lines = str_split(pdf_text[3], "\n")[[1]]
lines = lines[lines != ""]
lines = gsub("\\s+", " ", lines)
lines = gsub(",", "", lines)
df = read.table(text = lines, fill = TRUE)
df = df[-c(1:2, nrow(df)),]; df[1, 1:3]; df[nrow(df), 1:3]
# 北海道、神奈川、和歌山、鹿児島
three = c('北','神','和','鹿')
colnames(df)[1] = 'pref'
df$pref = ifelse(!(df$pref %in% three), paste0(df$pref, df$V2), paste0(df$pref, df$V2, df$V3));df[,1]
df[,-1]  = lapply(df[,-1], function(x) as.numeric(x))
df[,-1]  = lapply(df[,-1], function(x) ifelse(x < 600 | is.na(x), 0, x))
df = df[, colSums(df == 0) != nrow(df)]
df$x <- character(nrow(df))  
for (i in 1:nrow(df)) {
  mk <- df[i, -1][df[i,-1] > 0]
  df$x[i] <- paste(mk, collapse = ", ")
}
page3 = subset(df, select = c(pref, x)); head(page3)
page3 = page3[page3$pref %in% page1$pref, ]
page3$x = gsub('\\ ', '', page3$x)
mk = strsplit(page3$x, ",")
page3$mw1 <- sapply(mk, function(x) as.numeric(x[1]))
page3$mw2 <- sapply(mk, function(x) as.numeric(x[2]))
page3$mw3 <- sapply(mk, function(x) as.numeric(x[3]))
page3$mw4 <- sapply(mk, function(x) as.numeric(x[4]))
page3$mw5 <- sapply(mk, function(x) as.numeric(x[5]))
page3$x = NULL; summary(page3) # there are two NA
page3[is.na(page3$mw5),]
page3[is.na(page3$mw5), 'mw5'] = c(726, 770); summary(page2)
page3 = page3 %>% pivot_longer(cols = starts_with('mw'), names_to = 'year', values_to = 'mw')
page3$year = rep(2012:2016, 47); head(page3)
#
lines = str_split(pdf_text[4], "\n")[[1]]
lines = lines[lines != ""]
lines = gsub("\\s+", " ", lines)
lines = gsub(",", "", lines)
df = read.table(text = lines, fill = TRUE)
df = df[-c(1:2, nrow(df)),]; df[1, 1:3]; df[nrow(df), 1:3]
# 北海道、神奈川、和歌山、鹿児島
three = c('北','神','和','鹿')
colnames(df)[1] = 'pref'
df$pref = ifelse(!(df$pref %in% three), paste0(df$pref, df$V2), paste0(df$pref, df$V2, df$V3));df[,1]
df[,-1]  = lapply(df[,-1], function(x) as.numeric(x))
df[,-1]  = lapply(df[,-1], function(x) ifelse(x < 600 | is.na(x), 0, x))
df = df[, colSums(df == 0) != nrow(df)]
df$x <- character(nrow(df))  # Initialize the new column
for (i in 1:nrow(df)) {
  mk <- df[i, -1][df[i,-1] > 0]
  df$x[i] <- paste(mk, collapse = ", ")
}
page4 = subset(df, select = c(pref, x))
page4 = page4[page4$pref %in% page1$pref, ]
page4$x = gsub('\\ ', '', page4$x)
mk = strsplit(page4$x, ",");mk
page4$mw1 <- sapply(mk, function(x) as.numeric(x[1]))
page4$mw2 <- sapply(mk, function(x) as.numeric(x[2]))
page4$mw3 <- sapply(mk, function(x) as.numeric(x[3]))
page4$mw4 <- sapply(mk, function(x) as.numeric(x[4]))
page4$mw5 <- sapply(mk, function(x) as.numeric(x[5]))
page4$x = NULL; summary(page4) # there are two NA
page4 = page4 %>% pivot_longer(cols = starts_with('mw'), names_to = 'year', values_to = 'mw')
page4$year = rep(2017:2021, 47); head(page4)
#
lines = str_split(pdf_text[5], "\n")[[1]]
lines = lines[lines != ""]
lines = gsub("\\s+", " ", lines)
lines = gsub(",", "", lines)
df = read.table(text = lines, fill = TRUE)
df = df[-c(1:2, nrow(df)),]; df[1, 1:3]; df[nrow(df), 1:3]
# 北海道、神奈川、和歌山、鹿児島
three = c('北','神','和','鹿')
colnames(df)[1] = 'pref'
df$pref = ifelse(!(df$pref %in% three), paste0(df$pref, df$V2), paste0(df$pref, df$V2, df$V3));df[,1]
df[,-1]  = lapply(df[,-1], function(x) as.numeric(x))
df[,-1]  = lapply(df[,-1], function(x) ifelse(x < 600 | is.na(x), 0, x))
df = df[, colSums(df == 0) != nrow(df)]
df$x <- character(nrow(df))  # Initialize the new column
for (i in 1:nrow(df)) {
  mk <- df[i, -1][df[i,-1] > 0]
  df$x[i] <- paste(mk, collapse = ", ")
}
page5 = subset(df, select = c(pref, x)); head(page5)
page5 = page5[page5$pref %in% page1$pref, ]
page5$x = gsub('\\ ', '', page5$x)
mk = strsplit(page5$x, ",");mk
page5$mw1 <- sapply(mk, function(x) as.numeric(x[1]))
page5$mw2 <- sapply(mk, function(x) as.numeric(x[2]))
page5$mw3 <- sapply(mk, function(x) as.numeric(x[3]))
page5$x = NULL; summary(page5) # there are two NA
page5 = page5 %>% pivot_longer(cols = starts_with('mw'), names_to = 'year', values_to = 'mw')
page5$year = rep(2022:2024, 47); head(page5)
(mk = unique(page1$pref))
data = bind_rows(page1, page2, page3, page4, page5) %>% 
  mutate(pref = factor(pref, levels = mk))%>% arrange(pref); print(head(data, 23),n=23)
summary(data)
save(data, file = 'JapanMW.RDa')