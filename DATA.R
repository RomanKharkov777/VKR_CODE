    #ВКР: Влияние инвестиций в ИКТ на экономический рост
                    #Харьков Роман
                        #ЭФ МГУ

            ##Приводим данные по странам в нужный вид##

library(readxl)
data <- read_xlsx("DATA_COUNTRIES.xlsx")
data <- DATA_COUNTRIES
data$t <- NA

## делаем трехлетние средние ##

for (i in 1:length(data$TIME)) {
  if (data$TIME[i]==2000 | data$TIME[i]==2001 | data$TIME[i]==2002) {
    data$t[i] <- 1
  }
  
  if (data$TIME[i]==2003 | data$TIME[i]==2004 | data$TIME[i]==2005) {
    data$t[i] <- 2
  }
  
  if (data$TIME[i]==2006 | data$TIME[i]==2007 | data$TIME[i]==2008) {
    data$t[i] <- 3
  }
  
  if (data$TIME[i]==2009 | data$TIME[i]==2010 | data$TIME[i]==2011) {
    data$t[i] <- 4
  }
  
  if (data$TIME[i]==2012 | data$TIME[i]==2013 | data$TIME[i]==2014) {
    data$t[i] <- 5
  }
  
  
  if (data$TIME[i]==2015 | data$TIME[i]==2016 | data$TIME[i]==2017) {
    data$t[i] <- 6
  }
  
  if (data$TIME[i]==2018 | data$TIME[i]==2019 | data$TIME[i]==2020) {
    data$t[i] <- 7
  }
  if (data$TIME[i]==2021) {
    data$t[i] <- 8
  }
}

library(dplyr)

data1 <- data %>%
  group_by(CODE, t) %>%
  summarise(INV=exp(mean(log(INF/100+1), na.rm=TRUE)), 
            GDP_growth=exp(mean(log(GDP_growth/100+1), na.rm=TRUE)), 
            GDPPC=exp(mean(log(GDPPC/100+1), na.rm=TRUE)), 
            INF=exp(mean(log(INF/100+1), na.rm=TRUE)), 
            TRA=mean(TRA, na.rm=TRUE), 
            MU=mean(MU, na.rm = TRUE),
            UI=mean(UI, na.rm=TRUE), 
            GOV=exp(mean(log(GOV/100+1), na.rm=TRUE)), 
            ICT_INV_REAL=mean(ICT_INV_REAL, na.rm=TRUE), 
            DEVELOPE=mean(DEVELOPE, na.rm=TRUE),                                                                                                                LF_PC=mean(LF_PC, na.rm=TRUE),
            INF_CP=exp(mean(log(INF_CP/100+1), na.rm=TRUE)))

## INF & GDP_growth & GDPPC & INV & GOV взято среднее геометрическое соответственно ##

library(foreign)
write.dta(data1, "data1.dta")


                ##Приводим данные по регионам России в нужный вид##


library(readxl)
data <- read_xlsx("DATA_RUSSIA.xlsx")
data <- DATA_RUSSIA
data$t <- NA

## делаем трехлетние средние ##

for (i in 1:length(data$TIME)) {
  if (data$TIME[i]==2010 | data$TIME[i]==2011 | data$TIME[i]==2012) {
    data$t[i] <- 1
  }
  
  if (data$TIME[i]==2013 | data$TIME[i]==2014 | data$TIME[i]==2015) {
    data$t[i] <- 2
  }
  
  if (data$TIME[i]==2016 | data$TIME[i]==2017 | data$TIME[i]==2018) {
    data$t[i] <- 3
  }
  
  if (data$TIME[i]==2019 | data$TIME[i]==2020 | data$TIME[i]==2021) {
    data$t[i] <- 4
  }
}

library(dplyr)

data2 <- data %>%
  group_by(REGION, t) %>%
  summarise(GRP_IND=exp(mean(log(GRP_IND/100+1), na.rm=TRUE)), 
            GRP_IND_PC=exp(mean(log(GRP_IND_PC/100+1), na.rm=TRUE)), 
            K=mean(K, na.rm = TRUE),
            L=mean(L, na.rm=TRUE), 
            ICT=mean(ICT, na.rm=TRUE), 
            MU=mean(MU, na.rm=TRUE))

## GDP_IND & GDP_IND_PC взято среднее геометрическое соответственно ##

library(foreign)
write.dta(data2, "data2.dta")


## Так как получилось 4 периода, делаем тот же процесс для двухлетних средних ##

for (i in 1:length(data$TIME)) {
  if (data$TIME[i]==2010 | data$TIME[i]==2011) {
    data$t[i] <- 1
  }
  
  if (data$TIME[i]==2012 | data$TIME[i]==2013) {
    data$t[i] <- 2
  }
  
  if (data$TIME[i]==2014 | data$TIME[i]==2015) {
    data$t[i] <- 3
  }
  
  if (data$TIME[i]==2016 | data$TIME[i]==2017) {
    data$t[i] <- 4
    
  }
  if (data$TIME[i]==2018 | data$TIME[i]==2019) {
    data$t[i] <- 5
  
  }
  if (data$TIME[i]==2020 | data$TIME[i]==2021) {
    data$t[i] <- 6
  
  }
}

library(dplyr)

data3 <- data %>%
  group_by(REGION, t) %>%
  summarise(GRP_IND=exp(mean(log(GRP_IND/100+1), na.rm=TRUE)), 
            GRP_IND_PC=exp(mean(log(GRP_IND_PC/100+1), na.rm=TRUE)), 
            K=mean(K, na.rm = TRUE),
            L=mean(L, na.rm=TRUE), 
            ICT=mean(ICT, na.rm=TRUE), 
            MU=mean(MU, na.rm=TRUE))

## GDP_IND & GDP_IND_PC взято среднее геометрическое соответственно ##

library(foreign)
write.dta(data3, "data3.dta")






