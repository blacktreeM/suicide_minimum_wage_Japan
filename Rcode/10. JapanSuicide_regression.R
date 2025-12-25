setwd('C:/Users/masan/Documents/suicide')
packages = c('stargazer', 'lfe', 'fixest', 'dplyr', 'tidyr'); sapply(packages, library, character.only = TRUE)
load('suicideMW.RDa'); table(data$year); summary(data)
if(F){# create lagged cpi and real minimum wage
subset(data, pref== '全国', select=c(year, cpi))
cpi1 = data %>% filter(gender=='male' & age == '20-29' & year != 2024) %>% select(pref, year, cpi) %>%
  mutate(year=year+1, lagcpi = cpi) %>% select(-cpi); tail(cpi1)
cpi = data %>% filter(gender=='male' & age == '20-29' & year == 2009) %>% mutate(lagcpi = cpi) %>%
  select(pref, year, lagcpi) %>% bind_rows(cpi1); table(cpi$year); head(cpi)
data = data %>% left_join(cpi, by = c('pref', 'year')) %>% mutate(realmw = 100 * lagmw / lagcpi)
}
data = data %>% mutate(realmw = 100 * mw / cpi)
data = subset(data, pref != '全国'); summary(data)
# fixed effects interactions
data$age_year = paste0(data$age, data$year); unique(data$age_year)
data$age_pref = paste0(data$age, data$pref); unique(data$age_pref)
data$year_region = paste0(data$year, data$region); unique(data$year_region)
data$trend = data$year - 2009; table(data$trend)
data$trend2 = data$trend^2; table(data$trend2)
fe = 'age_year + age_pref + year_region'
young = c('20-29', '30-39', '40-49')
data = data %>% mutate(age20 = ifelse(age=='20-29', 1, 0), 
                       age30 = ifelse(age=='30-39', 1, 0),
                       age40 = ifelse(age=='40-49', 1, 0),
                       age20mw = age20*log(realmw), age30mw = age30*log(realmw), age40mw = age40*log(realmw),
                       age20n = age20*nonregular, age30n = age30*nonregular, age40n = age40*nonregular,
                       age20w = age20*welfare, age30w = age30*welfare, age40w = age40*welfare,
                       age20mwn = age20*log(realmw)*nonregular, 
                       age30mwn = age30*log(realmw)*nonregular,
                       age40mwn = age40*log(realmw)*nonregular,
                       age20mww = age20*log(realmw)*welfare, 
                       age30mww = age30*log(realmw)*welfare,
                       age40mww = age40*log(realmw)*welfare)
men = data %>% filter(gender == 'male' & age %in% young ) %>% rename('unemp' = 'unemployedMen', 'hour' = 'hourMen')
women = data %>% filter(gender == 'female' & age %in% young) %>% rename('unemp' = 'unemployedWomen', 'hour' = 'hourWomen')
table(men$year, men$age)

# age-specific regression
X = '+ log(income) + nonregular + unemp + hour + stayer + welfare + college + married + alone + mental |'
(spec1 = as.formula(paste0('rate ~ age20mw + age30mw + age40mw|',  fe, '| 0 | pref')))
(spec2 = as.formula(paste0('rate ~ age20mw + age30mw + age40mw', X, fe, '| 0 | pref')))
(spec3 = as.formula(paste0('rate ~ age20mw + age30mw + age40mw + pref:trend', X, fe, '| 0 | pref')))
resultMen = lapply(c(spec1, spec2, spec3), function(x) felm(x, data = men, weights = men$pop))
resultWomen = lapply(c(spec1, spec2, spec3), function(x) felm(x, data = women, weights = women$pop))
stargazer(list(resultMen, resultWomen), type='text', no.space = T, omit.stat = c("rsq","ser"),
          star.cutoffs = c(0.05, 0.01, 0), keep = 'mw')
length(unique(data$year)) * length(unique(data$pref)) * 3
vars = c('ln(Minimum wage) × Age 20-29',
         'ln(Minimum wage) × Age 30-39', 'ln(Minimum wage) × Age 40-49',
         'ln(Income per capita) (in 1,000 yen)', 'Share of nonregular workers',
         'Unemployed rate', 'Average monthly hours worked',
         'Percent employed within the prefecture',
         'Households receiving public assistance',
         'Percent college graduate', 'Percent married', 
          "Percent living alone", "New admissions in psychiatric hospitals"); length(vars)
(m1 = round(aggregate(rate~age, data = men, mean)[1, 2], 1))
(m2 = round(aggregate(rate~age, data = men, mean)[2, 2], 1))
(m3 = round(aggregate(rate~age, data = men, mean)[3, 2], 1))
(f1 = round(aggregate(rate~age, data = women, mean)[1, 2], 1))
(f2 = round(aggregate(rate~age, data = women, mean)[2, 2], 1))
(f3 = round(aggregate(rate~age, data = women, mean)[3, 2], 1))
stargazer(list(resultMen, resultWomen), out = 'Table2age.html', type = 'text', omit = 'trend',
          digits = 3, omit.stat = c("rsq","ser"), 
          dep.var.caption = 'Dependent variable: Suicide rate (per 100,000)',
          dep.var.labels.include = FALSE, no.space = T, star.cutoffs = c(0.05, 0.01, 0),
          covariate.labels = vars,
          column.labels   = c("Men", "Women"), column.separate = c(3, 3),
          notes = paste('Significance levels: * p<0.05, ** p<0.01.',
                        'Standard errors clustered at the prefecture level are shown in parentheses.',
                        'All models include prefecture-by-age, year-by-age, year-by-region fixed effects.'),
          notes.align = "l",notes.append = F, notes.label = '', 
          title = 'Table 2. Regression results',
          add.lines = list(c('Prefecture-specific linear trend', c(rep(c('no','no', 'yes'), 2))),
                           c('Mean suicide rate (Age 20-29)', c(rep(m1, 3), rep(f1, 3))),
                           c('Mean suicide rate (Age 30-39)', c(rep(m2, 3), rep(f2, 3))),
                           c('Mean suicide rate (Age 40-49)', c(rep(m3, 3), rep(f3, 3)))))
# women with quadratic trend
(spec3 = as.formula(paste0('rate ~ age20mw + age30mw + age40mw + pref:trend + pref:trend2', X, fe, '| 0 | pref')))
stargazer(felm(spec3, data = women, weights = women$pop),
          type='text', no.space = T, omit.stat = c("rsq","ser"), report=('vc*p'),
          star.cutoffs = c(0.05, 0.01, 0), keep = 'mw')

### only 2010, 2015, 2020 and unweighted
men2 = men %>% filter(year %in% c(2010, 2015, 2020)) 
women2 = women %>% filter(year %in% c(2010, 2015, 2020)) 
X = '+ nonregular + unemp + hour + stayer + welfare + alone + mental |'
(spec2 = as.formula(paste0('rate ~ age20mw + age30mw + age40mw', X, fe, '| 0 | pref')))
resultA = list(felm(spec1, data = men),
               felm(spec1, data = women),
               felm(spec2, data = men2, weights = men2$pop),
               felm(spec2, data = women2, weights = women2$pop))
stargazer(resultA, type='html', omit.stat = c("rsq","ser"),
          dep.var.caption = 'Dependent variable: Suicide rate (per 100,000)',
          dep.var.labels.include = FALSE, no.space = T, 
          column.labels   = c("Men <br>Unweighted OLS", "Women <br>Unweighted OLS",
                              'Men <br>(2010, 2015, 2020)', 'Women <br>(2010, 2015, 2020)'),
          covariate.labels = vars, star.cutoffs = c(0.05, 0.01, 0))

# cancer
X = '+ log(income) + nonregular + unemp + hour + stayer + welfare + college + married + alone + mental |'
(spec1 = as.formula(paste0('cancer_rate ~ age20mw + age30mw + age40mw', X, fe, '| 0 | pref')))
(spec2 = as.formula(paste0('cancer_rate ~ age20mw + age30mw + age40mw + pref:trend', X, fe, '| 0 | pref')))
resultMen = lapply(c(spec1, spec2), function(x) felm(x, data = men, weights = men$pop))
resultWomen = lapply(c(spec1, spec2), function(x) felm(x, data = women, weights = women$pop))
stargazer(list(resultMen, resultWomen), type='text', no.space = T, omit.stat = c("rsq","ser"),
          star.cutoffs = c(0.05, 0.01, 0), keep = 'mw')
stargazer(list(resultMen, resultWomen), type='text', no.space = T, omit.stat = c("rsq","ser"),
          star.cutoffs = c(0.05, 0.01, 0), omit = 'trend')
aggregate(cancer_rate ~ age, data = data, mean)
aggregate(cancer_rate ~ year, data = data, mean)
(m1 = round(aggregate(cancer_rate~age, data = men, mean)[1, 2], 1))
(m2 = round(aggregate(cancer_rate~age, data = men, mean)[2, 2], 1))
(m3 = round(aggregate(cancer_rate~age, data = men, mean)[3, 2], 1))
(f1 = round(aggregate(cancer_rate~age, data = women, mean)[1, 2], 1))
(f2 = round(aggregate(cancer_rate~age, data = women, mean)[2, 2], 1))
(f3 = round(aggregate(cancer_rate~age, data = women, mean)[3, 2], 1))
stargazer(list(resultMen, resultWomen), out = 'Table5cancer.html', type = 'html', omit = 'trend',
          digits = 3, omit.stat = c("rsq","ser"),
          dep.var.caption = 'Dependent variable: Cancer deaths (per 100,000)',
          dep.var.labels.include = FALSE, no.space = T, star.cutoffs = c(0.05, 0.01, 0),
          covariate.labels = vars,
          column.labels   = c("Men", "Women"), column.separate = c(2, 2),
          notes = paste('Significance levels: * denotes p<0.05, ** denotes p<0.01.',
                        'Standard errors clustered at the prefecture level are shown in parentheses.',
                        'All models include prefecture-by-age, year-by-age, year-by-region fixed effects.'),
          notes.align = "l",notes.append = F, notes.label = '', 
          title = 'Table 5. Regression results using cancer mortality',
          add.lines = list(c('Prefecture-specific linear trend', c(rep(c('no', 'yes'), 2))),
                           c('Mean cancer deaths (Age 20-29)', c(rep(m1, 2), rep(f1, 2))),
                           c('Mean cancer deaths (Age 30-39)', c(rep(m2, 2), rep(f2, 2))),
                           c('Mean cancer deaths (Age 40-49)', c(rep(m3, 2), rep(f3, 2)))))

# accident. not car accidents because studies suggest higher minimum wage increase drunk driving
accident = data %>% filter(gender == 'male' & age=='20-29' & year!=2011) %>% # exclude 2011 because tohoku had many deaths
  mutate(pop = Men1564 + Women1564, # total pop for weight
         unemp = (unemployedMen + unemployedWomen)/2, # average
         hour = (hourMen + hourWomen)/2)
car = data %>% filter(gender == 'male' & age=='20-29') %>% # exclude 2011 because tohoku had many deaths
  mutate(pop = Men1564 + Women1564, # total pop for weight
         unemp = (unemployedMen + unemployedWomen)/2, # average
         hour = (hourMen + hourWomen)/2)
# this variable is not age-specific so keep only one
(spec1 = as.formula(paste0('accidental_death ~ log(realmw)', X, 'pref+year_region | 0 | pref')))
(spec2 = as.formula(paste0('accidental_death ~ log(realmw)+ pref:trend', X, 'pref+year_region | 0 | pref')))
accident_result = lapply(c(spec1, spec2), function(x) felm(x, data = accident, weights = accident$pop))
(spec1 = as.formula(paste0('car_death ~ log(realmw)', X, 'pref+year_region | 0 | pref')))
(spec2 = as.formula(paste0('car_death ~ log(realmw)+ pref:trend', X, 'pref+year_region | 0 | pref')))
car_result = lapply(c(spec1, spec2), function(x) felm(x, data = car, weights = car$pop))
stargazer(list(accident_result, car_result), type='text', no.space = T, omit.stat = c("rsq","ser"),
          star.cutoffs = c(0.05, 0.01, 0), keep='mw')
stargazer(list(accident_result, car_result), type='text', no.space = T, omit.stat = c("rsq","ser"),
          star.cutoffs = c(0.05, 0.01, 0), omit='trend')
(death = round(mean(accident$accidental_death, na.rm=T), 1))
(car_death = round(mean(car$car_death, na.rm=T), 1))
vars = c('ln(Minimum wage)', 
         'ln(Income per capita) (in 1,000 yen)', 'Share of nonregular workers',
         'Unemployed rate', 'Average monthly hours worked',
         'Percent employed within the prefecture',
         'Households receiving public assistance',
         'Percent college graduate', 'Percent married', 
         "Percent living alone", "New admissions in psychiatric hospitals") # 
stargazer(list(accident_result, car_result),
          out = 'Table6Accident.html', type = 'html', omit = 'trend',
          digits = 3, omit.stat = c("rsq","ser"), star.cutoffs = c(0.05, 0.01, 0),
          dep.var.caption = 'Dependent variable: Deaths (per 100,000)',
          dep.var.labels.include = FALSE, no.space = T, 
          covariate.labels = vars, 
          column.labels   = c("Accidental deaths", "Traffic deaths"), column.separate = c(2, 2),
          notes = paste('Significance levels: * p<0.05, ** p<0.01.',
                        'Standard errors clustered at the prefecture level are shown in parentheses.',
                        'All models include prefecture and year-by-region fixed effects.'),
          notes.align = "l",notes.append = F, notes.label = '', 
          title = 'Table 6. Regression results using accidental and traffic deaths',
          add.lines = list(c('Prefecture-specific linear trend', c(rep(c('no', 'yes'), 2))),
                           c('Mean deaths (per 100,000)', c(rep(death,2), rep(car_death,2)))))

# heterogeneous effects; nonregular
X = '+ (age20mw + age30mw + age40mw) + (age20n+age30n+age40n) +welfare +log(income) + unemp + hour + stayer + college + married + alone + mental|'
(nonregular = as.formula(paste0('rate ~ nonregular:(age20mw + age30mw + age40mw)', X, fe, '| 0 | pref')))
hetero_nonregular = list(felm(nonregular, data = men, weights = men$pop),
                         felm(nonregular, data = women, weights = women$pop))
stargazer(hetero_nonregular, type='text', no.space = T, omit.stat = c("rsq","ser"),
          star.cutoffs = c(0.05, 0.01, 0), keep = 'age')
stargazer(hetero_nonregular, out = 'Table7NonRegular.html', type = 'text', keep = 'age',
          digits = 3, omit.stat = c("rsq","ser"), 
          dep.var.caption = 'Dependent variable: Suicide rate (per 100,000)',
          dep.var.labels.include = FALSE, no.space = T, star.cutoffs = c(0.05, 0.01, 0),
          covariate.labels = c('ln(Minimum wage) × Age 20-29', 'ln(Minimum wage) × Age 30-39', 
                               'ln(Minimum wage) × Age 40-49', 
                               'Share of nonregular workers × Age 20-29', 'Share of nonregular workers × Age 30-39', 
                               'Share of nonregular workers × Age 40-49',
                               'ln(Minimum wage) × Share of nonregular workers × Age 20-29',
                               'ln(Minimum wage) × Share of nonregular workers × Age 30-39', 
                               'ln(Minimum wage) × Share of nonregular workers × Age 40-49'), 
          column.labels   = c("Men", "Women"), column.separate = c(1,1),
          notes = paste('Significance levels: * p<0.05, ** p<0.01.',
                        'Standard errors clustered at the prefecture level are shown in parentheses.',
                        'All models include prefecture-by-age, year-by-age, year-by-region fixed effects.'),
          notes.align = "l",notes.append = F, notes.label = '', 
          title = 'Table 7. Regression results')
# heterogeneous effect; welfare
X = '+ (age20mw + age30mw + age40mw) + (age20w+age30w+age40w) + nonregular + log(income) + unemp + hour + stayer + college + married + alone + mental|'
(welfare = as.formula(paste0('rate ~ welfare:(age20mw + age30mw + age40mw)', X, fe, '| 0 | pref')))
hetero_welfare = list(felm(welfare, data = men, weights = men$pop),
                      felm(welfare, data = women, weights = women$pop))
stargazer(hetero_welfare, type='text', no.space = T, omit.stat = c("rsq","ser"),
          star.cutoffs = c(0.05, 0.01, 0), keep = 'age')
stargazer(hetero_welfare, out = 'Table8Welfare.html', type = 'text', keep = 'age',
          digits = 3, omit.stat = c("rsq","ser"), 
          dep.var.caption = 'Dependent variable: Suicide rate (per 100,000)',
          dep.var.labels.include = FALSE, no.space = T, star.cutoffs = c(0.05, 0.01, 0),
          covariate.labels = c('ln(Minimum wage) × Age 20-29', 'ln(Minimum wage) × Age 30-39', 
                               'ln(Minimum wage) × Age 40-49', 
                               'Share of nonregular workers × Age 20-29', 'Share of nonregular workers × Age 30-39', 
                               'Share of nonregular workers × Age 40-49',
                               'ln(Minimum wage) × Households receiving public assistance × Age 20-29',
                               'ln(Minimum wage) × Households receiving public assistance × Age 30-39', 
                               'ln(Minimum wage) × Households receiving public assistance × Age 40-49'), 
          column.labels   = c("Men", "Women"), column.separate = c(1,1),
          notes = paste('Significance levels: * p<0.05, ** p<0.01.',
                        'Standard errors clustered at the prefecture level are shown in parentheses.',
                        'All models include prefecture-by-age, year-by-age, year-by-region fixed effects.'),
          notes.align = "l",notes.append = F, notes.label = '', 
          title = 'Table 8. Regression results')
#### pre and post pandemic
pre = 2009:2019
post = 2020:2024
men1 = men %>% filter(year %in% pre)
men2 = men %>% filter(year %in% post)
women1 = women %>% filter(year %in% pre)
women2 = women %>% filter(year %in% post)
X = '+ log(income) + nonregular + unemp + hour + stayer + welfare + college + married + alone + mental |'
(spec1 = as.formula(paste0('rate ~ age20mw + age30mw + age40mw', X, fe, '| 0 | pref')))
results = list(felm(spec1, data = men1, weights = men1$pop),
               felm(spec1, data = women1, weights = women1$pop),
               felm(spec1, data = men2, weights = men2$pop),
               felm(spec1, data = women2, weights = women2$pop))
stargazer(results, type='text', no.space = T, omit.stat = c("rsq","ser"),
          star.cutoffs = c(0.05, 0.01, 0), keep = 'mw')
(m1 = round(aggregate(rate~age, data = men1, mean)[1, 2], 1))
(m2 = round(aggregate(rate~age, data = men1, mean)[2, 2], 1))
(m3 = round(aggregate(rate~age, data = men1, mean)[3, 2], 1))
(f1 = round(aggregate(rate~age, data = women1, mean)[1, 2], 1))
(f2 = round(aggregate(rate~age, data = women1, mean)[2, 2], 1))
(f3 = round(aggregate(rate~age, data = women1, mean)[3, 2], 1))
(m4 = round(aggregate(rate~age, data = men2, mean)[1, 2], 1))
(m5 = round(aggregate(rate~age, data = men2, mean)[2, 2], 1))
(m6 = round(aggregate(rate~age, data = men2, mean)[3, 2], 1))
(f4 = round(aggregate(rate~age, data = women2, mean)[1, 2], 1))
(f5 = round(aggregate(rate~age, data = women2, mean)[2, 2], 1))
(f6 = round(aggregate(rate~age, data = women2, mean)[3, 2], 1))
stargazer(results, out = 'Table9age.html', type = 'text', keep = 'mw',
          digits = 3, omit.stat = c("rsq","ser"), 
          dep.var.caption = 'Dependent variable: Suicide rate (per 100,000)',
          dep.var.labels.include = FALSE, no.space = T, star.cutoffs = c(0.05, 0.01, 0),
          covariate.labels = c('ln(Minimum wage) × Age 20-29',
                               'ln(Minimum wage) × Age 30-39', 'ln(Minimum wage) × Age 40-49'),
          column.labels   = c("Men", "Women", "Men", "Women"),
          notes = paste('Significance levels: * p<0.05, ** p<0.01.',
                        'Standard errors clustered at the prefecture level are shown in parentheses.',
                        'All models include prefecture-by-age, year-by-age, year-by-region fixed effects.'),
          notes.align = "l",notes.append = F, notes.label = '', 
          title = 'Table 9. Regression results',
          add.lines = list(c('Time period', c('2009-2019', '2020-2024', '2009-2019', '2020-2024')),
                           c('Mean suicide rate (Age 20-29)', c(rep(m1, 2), rep(f1, 2))),
                           c('Mean suicide rate (Age 30-39)', c(rep(m2, 2), rep(f2, 2))),
                           c('Mean suicide rate (Age 40-49)', c(rep(m3, 2), rep(f3, 2)))))


############################
# regression for reasons
load('suicideReason.RDa'); head(data)
reason = subset(data, select = c(pref, year, gender, Family, Health, Money, Work, nojob), pref != '全国'); summary(reason)
load('suicideMW.RDa'); table(data$year, data$gender); colnames(data)
if(F){# create lagged cpi and real minimum wage
subset(data, pref== '全国', select=c(year, cpi))
cpi1 = data %>% filter(gender=='male' & age == '20-29' & year != 2024) %>% select(pref, year, cpi) %>%
  mutate(year=year+1, lagcpi = cpi) %>% select(-cpi); tail(cpi1)
cpi = data %>% filter(gender=='male' & age == '20-29' & year == 2009) %>% mutate(lagcpi = cpi) %>%
  select(pref, year, lagcpi) %>% bind_rows(cpi1); table(cpi$year); head(cpi)
data = data %>% left_join(cpi, by = c('pref', 'year')) %>% mutate(realmw = 100 * lagmw / lagcpi)
}
data = data %>% mutate(realmw = 100 * mw / cpi)
data = subset(data, pref != '全国'); summary(data)
data = data %>% filter(pref != '全国' & age == '20-29') %>% select(-pop, -suicide, -rate, -age)  
unique(data$pref); unique(reason$pref)
reason = reason %>% mutate(pref = gsub('県|府', '', pref), pref = gsub("東京都", "東京", pref))
# merge 
data = data %>% left_join(reason, by = c('pref', 'year', 'gender')); summary(data)
data$year_region = paste0(data$year, data$region); unique(data$year_region)
data$trend = data$year - 2009; table(data$trend)
data$trend2 = data$trend^2; table(data$trend2)
# create suicide rates
men = data %>% filter(gender == 'male') %>% rename('unemp' = 'unemployedMen', 'hour' = 'hourMen') %>% 
  mutate(nojob_rate = 100000*nojob/Men1564, money_rate = 100000*Money/Men15)
men %>% arrange(desc(nojob_rate)) %>% select(pref, year, nojob_rate) %>% head(15)
women = data %>% filter(gender == 'female') %>% rename('unemp' = 'unemployedWomen', 'hour' = 'hourWomen') %>% 
  mutate(nojob_rate = 100000*nojob/Women1564, money_rate = 100000*Money/Women15)
X = '+ log(income) + nonregular + unemp + hour  + stayer + welfare + college + married + alone + mental |'
fe = 'pref + year_region'
vars = c('ln(Minimum wage)', 
         'ln(Income per capita) (in 1,000 yen)', 'Share of nonregular workers',
         'Unemployed rate', 'Average monthly hours worked',
         'Percent employed within the prefecture',
         'Households receiving public assistance',
         'Percent college graduate', 'Percent married', 
         "Percent living alone", "New admissions in psychiatric hospitals") # 
# suicide among unemployed
(spec1 = as.formula(paste0('nojob_rate ~ log(realmw)', X, fe, '| 0 | pref')))
(spec2 = as.formula(paste0('nojob_rate ~ log(realmw) + pref:trend', X, fe, '| 0 | pref')))
resultMen = lapply(c(spec1, spec2), function(x) felm(x, data = men, weights = men$pop))
resultWomen = lapply(c(spec1, spec2), function(x) felm(x, data = women, weights = women$pop))
stargazer(list(resultMen, resultWomen), type='text', no.space = T, omit.stat = c("rsq","ser"),
          star.cutoffs = c(0.05, 0.01, 0), keep='mw')
(mm = round(mean(men$nojob_rate), 1))
(ff = round(mean(women$nojob_rate), 1))
stargazer(list(resultMen, resultWomen),
          out = 'Table3unemployed.html', type = 'text', omit = 'trend',
          digits = 3, omit.stat = c("rsq","ser"),
          dep.var.caption = 'Dependent variable: Suicide rate (per 100,000)',
          dep.var.labels.include = FALSE, no.space = T, 
          covariate.labels = vars, star.cutoffs = c(0.05, 0.01, 0),
          column.labels   = c("Men", "Women"), column.separate = c(2, 2),
          notes = paste('Significance levels: * p<0.05, ** p<0.01.',
                        'Standard errors clustered at the prefecture level are shown in parentheses.',
                        'All models include prefecture and year-by-region fixed effects.'),
          notes.align = "l",notes.append = F, notes.label = '', 
          title = 'Table 3. Regression results for suicide by unemployed people',
          add.lines = list(c('Prefecture-specific linear trend', c(rep(c('no', 'yes'), 2))),
                           c('Mean suicide rate (per 100,000)', c(rep(mm,2), rep(ff,2)))))

# suicide due to financial difficulties
(spec1 = as.formula(paste0('money_rate ~ log(realmw)', X, fe, '| 0 | pref')))
(spec2 = as.formula(paste0('money_rate ~ log(realmw) + pref:trend', X, fe, '| 0 | pref')))
resultMen = lapply(c(spec1, spec2), function(x) felm(x, data = men, weights = men$pop))
resultWomen = lapply(c(spec1, spec2), function(x) felm(x, data = women, weights = women$pop))
stargazer(list(resultMen, resultWomen), type='text', no.space = T, omit.stat = c("rsq","ser"),
          star.cutoffs = c(0.05, 0.01, 0), keep='mw')
(mm = round(mean(men$money_rate), 1))
(ff = round(mean(women$money_rate), 1))
stargazer(list(resultMen, resultWomen),
          out = 'Table4financial.html', type = 'text', omit = 'trend',
          digits = 3, omit.stat = c("rsq","ser"),
          dep.var.caption = 'Dependent variable: Suicide rate (per 100,000)',
          dep.var.labels.include = FALSE, no.space = T, 
          covariate.labels = vars, star.cutoffs = c(0.05, 0.01, 0),
          column.labels   = c("Men", "Women"), column.separate = c(2,2),
          notes = paste('Significance levels: * p<0.05, ** p<0.01.',
                        'Standard errors clustered at the prefecture level are shown in parentheses.',
                        'All models include prefecture and year-by-region fixed effects.'),
          notes.align = "l",notes.append = F, notes.label = '', 
          title = 'Table 4. Regression results for suicide caused by financial difficulties',
          add.lines = list(c('Prefecture-specific linear trend', c(rep(c('no', 'yes'), 2))),
                           c('Mean suicide rate (per 100,000)', c(rep(mm,2), rep(ff,2)))))

## 2 year lag minimum wage for a reviewer
setwd('C:/Users/masan/Documents/suicide')
packages = c('stargazer', 'lfe', 'fixest', 'dplyr', 'tidyr'); sapply(packages, library, character.only = TRUE)
load('suicideMW.RDa'); table(data$year); summary(data)
# create lagged cpi and real minimum wage
subset(data, pref== '全国', select=c(year, cpi))
cpi1 = data %>% filter(gender=='male' & age == '20-29' & year < 2023) %>% select(pref, year, cpi) %>%
  mutate(year=year+2, lagcpi = cpi) %>% select(-cpi); tail(cpi1); table(cpi1$year)
cpi = data %>% filter(gender=='male' & age == '20-29' & year %in% c(2009, 2010)) %>% mutate(lagcpi = cpi) %>%
  select(pref, year, lagcpi) %>% bind_rows(cpi1); table(cpi$year); head(cpi)
data = data %>% left_join(cpi, by = c('pref', 'year')) %>% mutate(realmw = 100 * lag2mw / lagcpi)
# fixed effects interactions
data$age_year = paste0(data$age, data$year); unique(data$age_year)
data$age_pref = paste0(data$age, data$pref); unique(data$age_pref)
data$year_region = paste0(data$year, data$region); unique(data$year_region)
data$trend = data$year - 2009; table(data$trend)
data$trend2 = data$trend^2; table(data$trend2)
fe = 'age_year + age_pref + year_region'
young = c('20-29', '30-39', '40-49')
data = data %>% mutate(age20 = ifelse(age=='20-29', 1, 0), 
                       age30 = ifelse(age=='30-39', 1, 0),
                       age40 = ifelse(age=='40-49', 1, 0),
                       age20mw = age20*log(realmw), age30mw = age30*log(realmw), age40mw = age40*log(realmw),
                       age20n = age20*nonregular, age30n = age30*nonregular, age40n = age40*nonregular,
                       age20w = age20*welfare, age30w = age30*welfare, age40w = age40*welfare,
                       age20mwn = age20*log(realmw)*nonregular, 
                       age30mwn = age30*log(realmw)*nonregular,
                       age40mwn = age40*log(realmw)*nonregular,
                       age20mww = age20*log(realmw)*welfare, 
                       age30mww = age30*log(realmw)*welfare,
                       age40mww = age40*log(realmw)*welfare)
men = data %>% filter(gender == 'male' & age %in% young ) %>% rename('unemp' = 'unemployedMen', 'hour' = 'hourMen')
women = data %>% filter(gender == 'female' & age %in% young) %>% rename('unemp' = 'unemployedWomen', 'hour' = 'hourWomen')
# age-specific regression
X = '+ log(income) + nonregular + unemp + hour + stayer + welfare + college + married + alone + mental |'
(spec1 = as.formula(paste0('rate ~ age20mw + age30mw + age40mw', X, fe, '| 0 | pref')))
(spec2 = as.formula(paste0('rate ~ age20mw + age30mw + age40mw + pref:trend', X, fe, '| 0 | pref')))
resultMen = lapply(c(spec1, spec2), function(x) felm(x, data = men, weights = men$pop))
resultWomen = lapply(c(spec1, spec2), function(x) felm(x, data = women, weights = women$pop))
stargazer(list(resultMen, resultWomen), type='text', no.space = T, omit.stat = c("rsq","ser"), report=('vc*p'),keep = 'mw')

# including age 50, for a reviewer
setwd('C:/Users/masan/Documents/suicide')
packages = c('stargazer', 'lfe', 'fixest', 'dplyr', 'tidyr'); sapply(packages, library, character.only = TRUE)
load('suicideMW.RDa'); table(data$year); summary(data)
# create lagged cpi and real minimum wage
subset(data, pref== '全国', select=c(year, cpi))
cpi1 = data %>% filter(gender=='male' & age == '20-29' & year != 2024) %>% select(pref, year, cpi) %>%
  mutate(year=year+1, lagcpi = cpi) %>% select(-cpi); tail(cpi1)
cpi = data %>% filter(gender=='male' & age == '20-29' & year == 2009) %>% mutate(lagcpi = cpi) %>%
  select(pref, year, lagcpi) %>% bind_rows(cpi1); table(cpi$year); head(cpi)
data = data %>% left_join(cpi, by = c('pref', 'year')) %>% mutate(realmw = 100 * lagmw / lagcpi)
data = subset(data, pref != '全国'); summary(data)
# fixed effects interactions
data$age_year = paste0(data$age, data$year); unique(data$age_year)
data$age_pref = paste0(data$age, data$pref); unique(data$age_pref)
data$year_region = paste0(data$year, data$region); unique(data$year_region)
data$trend = data$year - 2009; table(data$trend)
data$trend2 = data$trend^2; table(data$trend2)
fe = 'age_year + age_pref + year_region'
young = c('20-29', '30-39', '40-49', '50-59')
data = data %>% mutate(age20 = ifelse(age=='20-29', 1, 0), 
                       age30 = ifelse(age=='30-39', 1, 0),
                       age40 = ifelse(age=='40-49', 1, 0),
                       age50 = ifelse(age=='50-59', 1, 0),
                       age20mw = age20*log(realmw), age30mw = age30*log(realmw),
                       age40mw = age40*log(realmw), age50mw = age50*log(realmw),
                       age20n = age20*nonregular, age30n = age30*nonregular, age40n = age40*nonregular,
                       age20w = age20*welfare, age30w = age30*welfare, age40w = age40*welfare,
                       age20mwn = age20*log(realmw)*nonregular, 
                       age30mwn = age30*log(realmw)*nonregular,
                       age40mwn = age40*log(realmw)*nonregular,
                       age20mww = age20*log(realmw)*welfare, 
                       age30mww = age30*log(realmw)*welfare,
                       age40mww = age40*log(realmw)*welfare)
men = data %>% filter(gender == 'male' & age %in% young ) %>% rename('unemp' = 'unemployedMen', 'hour' = 'hourMen')
women = data %>% filter(gender == 'female' & age %in% young) %>% rename('unemp' = 'unemployedWomen', 'hour' = 'hourWomen')
table(men$year, men$age)