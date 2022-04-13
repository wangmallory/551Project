#' 1. Data Cleaning 
#' As part of Stats 551 Final Project

# Library
library(tidyverse)

# Data
us_alldata = read.csv("us_alldata.csv", header = FALSE)
us_ind = read.csv("us_indicator.csv")

# Indicator Search 
# CO2 emissions (kt): EN.ATM.CO2E.KT
# CO2 emissions (metric ton per capita): EN.ATM.CO2E.PC
# Total Population: SP.POP.TOTL
# GDP in USD: NY.GDP.MKTP.CD
# GDP growth: NY.GDP.MKTP.KD.ZG 
# Gross Domestic Income: NY.GDY.TOTL.KN
# Net primary income: BN.GSR.FCTY.CD
# Population in urban agglomeration of more than 1 mil (% of total population): EN.URB.MCTY.TL.ZS
# Energy use (kg of oil equivalent per capita): EG.USE.PCAP.KG.OE
# Net energy import (% of total energy used): EG.IMP.CONS.ZS
# Electric power consumption (kWh per capita): EG.USE.ELEC.KH.PC

## Clean US data
us_alldata_clean = us_alldata[3:1446,]
names(us_alldata_clean) <- us_alldata_clean[1,]
us_alldata_clean <- us_alldata_clean[-1,]

# Generate US data sets
yr = colnames(us_alldata_clean[5:66])

var = c("co2","pop","gdp","gdpg","gdi","inc","urban","energy","eng_imp","elec")
code = c("EN.ATM.CO2E.KT","SP.POP.TOTL","NY.GDP.MKTP.CD","NY.GDP.MKTP.KD.ZG","NY.GDY.TOTL.KN","BN.GSR.FCTY.CD",
         "EN.URB.MCTY.TL.ZS","EG.USE.PCAP.KG.OE","EG.IMP.CONS.ZS","EG.USE.ELEC.KH.PC")

df = list()
for (i in 1:length(var)) {
  temp = data.frame(year = yr, 
                    co2 = as.numeric(us_alldata_clean[which(us_alldata_clean$`Indicator Code` == code[i]),5:66]))
  df = append(df, assign(paste0("us_",var[i]), temp))
}

# Data Cleaning
# us_co2 missing 2018 and after
# us_gdpg missing 1960
# us_gdi is missing data before 1970
# us_inc is missing data before 1970
# us_energy is missing data after 2015
# us_en_imp is missing data after 2015
# us_elec is missing data after 2014

us_co2 = drop_na(us_co2)
us_gdpg = drop_na(us_gdpg)
us_gdi = drop_na(us_gdi)
us_inc = drop_na(us_inc)
us_energy = drop_na(us_energy)
us_eng_imp = drop_na(us_eng_imp)
us_elec = drop_na(us_elec)

