getwd()

setwd("C:/Users/Noah/Documents/Class/Winter 2022/STATS 551/Project")

path <- "C:/Users/Noah/Documents/Class/Winter 2022/STATS 551/Project/National_Carbon_Emissions.xlsx"

library(tidyverse)
library(openxlsx)

# getting data from sheets
sheets <- openxlsx::getSheetNames(path)
data <- lapply(sheets, openxlsx::read.xlsx, xlsxFile=path)

# assigning names to data frame
names(data) <- sheets
data <- data[-5]

# printing the data
# print (data)
'%!in%' <- function(x,y)!('%in%'(x,y))

summary(data)

g20 <- c("ARGENTINA", "AUSTRALIA","BRAZIL", "CANADA", "CHINA.(MAINLAND)", "FRANCE.(INCLUDING.MONACO)", "GERMANY", 'INDIA', 'INDONESIA', 'ITALY.(INCLUDING.SAN.MARINO)', 'JAPAN',
         'DEMOCRATIC.PEOPLE.S.REPUBLIC.OF.KOREA', 'MEXICO', 'RUSSIAN.FEDERATION', 'SAUDI.ARABIA', 'SOUTH.AFRICA', 'TURKEY', 'UNITED.KINGDOM', 'UNITED.STATES.OF.AMERICA',
         'AUSTRIA', 'BELGIUM', 'BULGARIA', 'CROATIA','CYPRUS', 'CZECH.REPUBLIC', 'DENMARK', 'ESTONIA','FINLAND', 'FRANCE.(INCLUDING.MONACO)', 'GERMANY', 'GREECE',
         'HUNGARY', 'IRELAND','ITALY.(INCLUDING.SAN.MARINO)', 'LATVIA', 'LITHUANIA','LUXEMBOURG','MALTA', 'NETHERLANDS','POLAND', 'PORTUGAL', 'ROMANIA', 'SLOVAKIA',
         'SLOVENIA', 'SPAIN', 'SWEDEN')

# removing first row
data <- lapply(data, function(x){x <- slice(x, -1); return(x)})


myfun <- function(my_object) {
  deparse(match.call()$my_object)
}


data <- lapply(data, function(x){x <- pivot_longer(x, cols = colnames(x)[-1], 
                                           names_to = "country"); return(x)})


data <- left_join(data$`Territorial Emissions CDIAC`, data$`Territorial Emissions UNFCCC`, by = c("X1", "country")) %>%
        left_join(., data$`Consumption Emissions UNFCCC`, by=  c("X1", "country")) %>%
        left_join(. , data$`Emissions Transfers UNFCCC`, by =c("X1", "country"))

colnames(data) <- c("year", "country", "emissions_CDIAC", "emissions_UNFCCC", "consum_emissions_UNFCCC", "transfers_UNFCCC")

data <- data %>% filter(country %in% g20)

head(data)

write.csv(data, "cleaned_emissions.csv")
