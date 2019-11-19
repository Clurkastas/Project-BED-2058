rm(list = c())

### Data
library(WDI) #connection 2 or world bank
library(gapminder) #data from gapminder
#Analysis 
library(psych) # for factor analysis
library(Hmisc) # rcorr()
library(PerformanceAnalytics)
#tidy
library(tidyverse) #basic cleaning/ organising functions



# Data Sources

### WORLD BANK ###
#- load table with all available variables
WDI_datasets <- as_tibble(WDIcache()$series)
temp <- stringr::str_detect(WDI_datasets[["name"]], "population dynamic")
temp2 <- WDI_datasets[temp,]
temp3 <- WDI_datasets$sourceOrganization %>% table()
temp3 <- cbind(temp3,temp3)


#temp2[["description"]]
#load datasets:
# HEALTH #######################
WDI_ch_mor <- WDI(indicator = "SP.DYN.IMRT.IN") #Child mortality source 1
WDI_ch_mor2 <- WDI(indicator = "HD.HCI.MORT") #Child mortality source 2
WDI_surv_65f <- WDI(indicator = "SP.DYN.TO65.FE.ZS") #survival to 65, female
WDI_surv_65m <- WDI(indicator = "SP.DYN.TO65.MA.ZS") #survival to 65, male
WDI_mor_hyg <- WDI(indicator = "SH.STA.WASH.P5") #deaths bcs of unclean water & bad hygience
#water is dichotom and has almost no variance
WDI_water2 <- WDI(indicator="SG.H2O.PRMS.HH.ZS") #water access in households
WDI_cook <- WDI(indicator = "EG.CFT.ACCS.ZS	") # access for clean cooking
WDI_mortality <- WDI(indicator = "SH.STA.AIRP.P5") #mortality rate attributed to household and ambient air pollution
# too much missing data
# EDUCATION ##################### Lucas
WDI_pri_Ed <- WDI(indicator = "UIS.NERT.1") # % in primary education
WDI_sec_Ed <- WDI(indicator = "UIS.NERA.3") # % in upper secondary education
WDI_pri_mon <- WDI(indicator = "fin37.t.a.5") # %Gov. transfers money for pri. ed.
WDI_sec_mon <- WDI(indicator = "fin37.t.a.6") # % Gov. transfers money for sec. ed.
WDI_sec_f <- WDI(indicator = "UIS.GTVP.23.GPV.F")
WDI_ed_exp <- WDI(indicator = "UIS.XPUBP.UK")#gov. exp. on ed. %of total exp.
# SAFETY ###################
WDI_road <- WDI(indicator = "SH.STA.TRAF.P5") # traffic deaths per 1000
WDI_pois <- WDI(indicator = "SH.STA.POIS.P5") # poison deaths per 1000
WDI_murder <- WDI(indicator = "VC.IHR.PSRC.P5") #intentional homicide per 100,000
# ECONOMICS ################## Mattia
WDI_gini <- WDI(indicator = "SI.POV.GINI") # Gini Index
#WDI_unemp <- WDI(indicator = "SL.UEM.LTRM.ZS") # % of total Unemployment
WDI_y_unemp <- WDI(indicator = "SL.UEM.1524.NE.ZS	") # % youth unemployment rate 
WDI_ad_unemp <- WDI(indicator = "SL.UEM.ADVN.ZS") # % unemployment rate with advance education 
WDI_int_unemp <- WDI(indicator = "SL.UEM.INTM.ZS") # % unemployment rate with intermediate education
WDI_bas_unemp <- WDI(indicator = "SL.UEM.BASC.ZS") # % unemployment rate with basic education 
WDI_gdp <- WDI(indicator = "6.0.GDPpc_constant") # per capita gdp
WDI_gdpgr <- WDI(indicator = "6.0.GDP_growth") #gdp growth

# ENVIRONMENT #################### Kirill
WDI_water_stress <- WDI(indicator = "ER.H2O.FWST.ZS") #Level of water stress
WDI_renewable_water <- WDI(indicator = "ER.H2O.INTR.PC") # Renewable internal freshwater resources per capita 
WDI_CO2 <- WDI(indicator = "EN.ATM.CO2E.KT") # CO2 emissions (kt)
WDI_air <- WDI(indicator = "EN.ATM.PM25.MC.M3") # Air pollution

WDI_road %>% group_by(country) %>% 
  summarise(mean = mean(.[[3]], na.rm=T)) %>%
  is.na() %>% summary() #TRUEs = missings for country

### 5 Gapminder ###
#- 6 variables: country, continent, year, lifeExp, pop, gdpPercap
summary(gapminder)
summary(country_codes) #can be added to gapminder data for country codes
col_gap <- cbind(country_colors,continent_colors, labels(country_colors)) %>% as_tibble()
names(col_gap) <- c("cntry_col","cntnt_col","country")
dat_gap <- left_join(gapminder, country_codes)
dat_gap <- left_join(dat_gap, col_gap)

### JOINING DATA ###
#join by country and year
# what?: a lot of WDI_variables and the dat_gap

WDI_list <- list(WDI_ch_mor, 
                 #WDI_ch_mor2, WDI_water2, #thrown out due missings
                 WDI_surv_65f,  WDI_surv_65m, WDI_cook, 
                 WDI_mortality, WDI_mor_hyg, #thrown out due missings
                 WDI_pri_Ed, WDI_sec_Ed,
                 WDI_pri_mon,   WDI_sec_mon, #thrown out due missings
                 WDI_sec_f, 
                 WDI_road,   #thrown out due missings
                 WDI_pois ,   WDI_murder,
                 WDI_gini,   WDI_y_unemp, WDI_ad_unemp,  WDI_int_unemp,
                 WDI_bas_unemp,
                 #WDI_gdp,    WDI_gdpgr,   #thrown out due only missing values
                 WDI_renewable_water, WDI_water_stress, #not useable, weird values  
                 WDI_CO2,       WDI_air)

#merge data from WDI
dat <- plyr::join_all(dfs = WDI_list, by = c("country","year","iso2c"))
dat$country[dat$country=="Russian Federation"] <- "Russia"
dat$country[dat$country=="Iran, Islamic Rep."] <- "Iran"
dat$country[dat$country=="Egypt, Arab Rep."]   <- "Egypt"
dat$country[dat$country=="Syrian Arab Republic"]   <- "Syria"
dat$country[dat$country=="Venezuela, RB"]   <- "Venezuela"
dat$country[dat$country=="Gambia, The"]   <- "Gambia"
dat$country[dat$country=="Micronesia, Fed. Sts."]   <- "Micronesia"
dat$country[dat$country=="Bahamas, The"]   <- "Bahamas"
dat$country[dat$country=="Yemen, Rep."] <- "Yemen"

dat <- full_join(dat_gap[,c(1,3:6)], dat, by=c("country","year"))
dat <- full_join(dat_gap[,c(1:2,7:length(dat_gap))], dat, by="country")#
names(dat) <- c("country","continent","iso_alpha","iso_num",
                "cntry_col","cntnt_col","year","lifeExp",
                "pop","gdpPercap","iso2c",
                "child_mor","surv_65f","surv_65m","cook",
                "mortality", "mor_hyg", #thornw out
                "pri_ed","sec_ed",
                "pri_mon",   "sec_mon", #thrown out
                "sec_f",
                "road",  #thrown out
                "poison","murder", "gini",
                "y_unemp","ad_unemp","int_unemp","bas_unemp",
                "renew_wat","water_stress", #not useable
                "CO2","air")

#C: make dataset smaller
#C1: delete countries without continent (like Europe, Virgin Islands, ...)
europe <- "Andorra|Belarus|Cyprus|Greenland|Latvia|Liechtenstein|Lithuania|Luxembourg|Malta|Moldova|Monaco|Russia|Estonia|Kosovo|Ukraine|North Macedonia"
dat$continent[stringr::str_detect(dat$country,europe)==T] <- "Europe"
africa <- "Eswatini|Egypt|South Sudan|Gambia|Seychelles|Cabo Verde"
dat$continent[stringr::str_detect(dat$country,africa)==T] <- "Africa"
americas <- "Belize|Grenada|Venezuela|Antigua and Barbuda|Bahamas|St. Kitts and Nevis|St. Lucia|Barbados|Suriname|Guyana|St. Vincent and the Grenadines"
dat$continent[stringr::str_detect(dat$country,americas)==T] <- "Americas"
asia <- "United Arab Emirates|Brunei Darussalam|Armenia|Timor-Leste|Azerbaijan|Uzbekistan|Bhutan|Tajikistan|Georgia|Syria|Kazakhstan|Maldives|Turkmenistan|Qatar"
dat$continent[stringr::str_detect(dat$country,asia)==T] <- "Asia"
oceania <- "Papua New Guinea|Solomon Islands|Fiji|Vanuatu|Samoa|Micronesia|Kiribati|Tonga|Marshall Islands|Palau|Tuvalu|Nauru"
dat$continent[stringr::str_detect(dat$country,oceania)==T] <- "Oceania"
NAcont <- dat[is.na(dat$continent),]$country %>% unique()
NAcont <- NAcont[48:length(NAcont)]
paste0(NAcont, collapse="         ")


dat <- dat[!is.na(dat$continent),]
#C2: which columns do have little data?
colNA <- map(dat,~table(is.na(.x))) %>%
  map_int(~.x["FALSE"])
colNA
# RESULT:
# "6.0.GDPpc_constant" doesnt have any data --> throw WDI_gdp out
# "6.0.GDP_growth" doesnt have any data --> throw WDI_gdpgr out
# "ER.H2O.FWST.ZS" has way less data than other vars --> throw WDI_water_stress out
# "SH.STA.AIRP.P5" copy above --> throw WDI_mortality out
# "SH.STA.WASH.P5" copy above --> throw WDI_mor_hyg out
# "fin37.t.a.6" copy above --> throw WDI_sec_mon out
# "fin37.t.a.5§ copy above --> throw WDI_pri_mon out
# "HD.HCI.MORT" copy above --> throw WDI_ch_mor2 out
# "SG.H2O.PRMS.HH.ZS" copy above --> throw WDI_water2 out
# "SH.STA.TRAF.P5" copy above --> throw WDI_road out

#C3: which rows do have little data?
# the answer: many, and every one at least some
# so instead of deleting those with missings, I'd like to shrink years to decades

#let's try this: take value of highest year available
dat <- dat %>% arrange(-year) #arrange descending by year

dat_long <- tidyr::gather(dat, variable, measurement, 
                          c(lifeExp:gdpPercap, child_mor:air))
#delete rows with missings
dat_long <- dat_long[is.na(dat_long$measurement)==F,] %>%
  group_by(country, variable)  %>%
  slice(1) %>% #take first available entry per group 
  ungroup()

#most entries are year=>2000, delete the ones before
dat_long <- dat_long[dat_long$year>=2000,]

#check: any countries with little available variables?
low_data <- dat_long %>%
  group_by(country) %>%
  count() %>% arrange(n)
#ten countries have way less (=3) data available than others(>=13)
temp <- low_data[low_data$n>14,]$country
keep.list <- paste(temp, collapse = '|')
dat_long <- dat_long %>% 
  filter(grepl(keep.list, country))

#dat_long -> dat_wide
dat_wide <- dat_long %>%
  select(-c("year")) %>%  #year must be dropped, can be attached later
  spread(variable, measurement)


#Open Questions:
#maybe scalable as web application?,
# include size of country as support quality variable

# Want:
#- A plot which shows multiple placements from multiple variables to see 
#which countries show always better values and which never do. 
#(+ To see variance) => z-standardize and compare


#look at variables
hist(dat_wide$air)
hist(dat_wide$CO2) #?wtf -> needs to by adjusted by population
dat_wide$CO2 <- dat_wide$CO2/dat_wide$pop
hist(dat_wide$CO2) 


cor(dat_wide)
cortab <- rcorr(as.matrix(dat_wide[,8:length(dat_wide)]))
library(corrplot)
corrplot(cortab$r, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
#check out groups of variables
envir <- c("air","CO2","renew_wat","water_stress")
health <- c("surv_65m","surv_65f","mortality","lifeExp","cook","child_mor")
econ <- c("gdpPercap","gini","y_unemp","ad_unemp","int_unemp","bas_unemp")
educ <- c("pri_ed","sec_ed","pri_mon","sec_mon","sec_f")
secu <- c("mor_hyg","road","poison","murder")
topics <- list(envir,health,econ,educ,secu)

chart.Correlation(as.matrix(dat_wide[,envir]), histogram=TRUE, pch=19)
#renew_wat and water_stress have weird extreme values
#--> do:
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}
dat_wide$renew_wat <- remove_outliers(dat_wide$renew_wat)
dat_wide$water_stress <- remove_outliers(dat_wide$water_stress)
dat_wide$CO2 <- remove_outliers(dat_wide$CO2)
chart.Correlation(as.matrix(dat_wide[,health]), histogram=TRUE, pch=19)
#health looks mostly good
chart.Correlation(as.matrix(dat_wide[,econ]), histogram=TRUE, pch=19)
#unemployment variables are similar
chart.Correlation(as.matrix(dat_wide[,educ]), histogram=TRUE, pch=19)

chart.Correlation(as.matrix(dat_wide[,secu]), histogram=TRUE, pch=19)



# test topic groups alone
fa <- topics %>%
  map(~psych::fa(dat_wide[,.x],rotate = "varimax",fm="ml"))

#education: sec_f not good, rest ok
educ <- c("pri_ed","sec_ed","pri_mon","sec_mon")
#econ: unemp is too similar, make rowMeans (different levels of education)
dat_wide <- dat_wide %>% 
  mutate(unemp =  rowMeans(cbind(ad_unemp, bas_unemp, int_unemp), na.rm=T))
econ <- c("gdpPercap","gini","unemp")
#health: is perfect, but "mortality" doesnt fit (has to do with air pollution)
health <- c("surv_65m","surv_65f","lifeExp","cook","child_mor")
#security: murder doesnt fit, rest is good
secu <- c("mor_hyg","road","poison") #redone :)
#environment: nur CO2 und air geht
envir <- c("CO2","air")
topics <- list(envir,health,econ,educ,secu)

#run final cfa's
fa <- topics %>%
  map(~psych::fa(dat_wide[,.x],rotate = "varimax",fm="ml"))
fa[[1]]


#get factor scores
fa_scores <- fa %>% map2(.y = topics, 
                         .f= ~factor.scores(x=dat_wide[,.y], f=.x, 
                                            impute = "median"))
fa_scores <- map(.x=fa_scores, ~.x[["scores"]])
fa_scores <- matrix(flatten(fa_scores), ncol = 5) %>% as_tibble()
names(fa_scores) <- c("envir","health","econ","educ","secu")
country <- dat_wide$country
fs <- cbind(country, fa_scores)
dat_wide2 <- left_join(dat_wide, fs, by="country")

#which do need to be recoded/ negated?
dat_wide <- dat_wide2 %>%
  mutate(envir = -as.numeric(envir), #environment was wrong, now correct
         health = as.numeric(health),
         econ = as.numeric(econ),
         educ = as.numeric(educ),
         secu = -as.numeric(secu))  #security was wrong, now correct
# now every dimension has this interpretation:
# the higher the value, the better the conditions

#make longformat again
dat_long <- gather(dat_wide, variable, measurement, ad_unemp:secu)
dat_long <- dat_long[is.na(dat_long$measurement)==F,]
dat_raw <- dat

#clean workspace except for necessary data
write.csv(dat_raw, "dat_raw.csv")
write.csv(dat_wide, "dat_wide.csv")
write.csv(dat_long, "dat_long.csv")


