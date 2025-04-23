library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(stringi)
library("sp")
library(data.table)

setwd("E:/Shishir/FieldData/Analysis/Connectivity/SHP_Connectivity/")

precip = read.csv("GoogleEE/Kar_basins_1981_2023_meanPrecip.csv",header=T)

precip$date = stri_sub(precip$date,from = 1,to = 10)
precip$date = ymd(precip$date)
precip$year = year(precip$date)
precip$month = lubridate::month(precip$date)

#Summarize the taluk wise rainfall by month, year
yearPrecip = as.data.frame(precip %>%
                             dplyr::group_by(Basn_nm, year) %>%
                             dplyr::summarize(Rain_mm = sum(mean, na.rm=TRUE)))

AvgAnnualPrecip1 = yearPrecip %>% dplyr::group_by(Basn_nm) %>% dplyr::summarize(Mean_Annual_Rain_mm = mean(Rain_mm, na.rm=TRUE))
AvgAnnualPrecip1 = left_join(AvgAnnualPrecip1, 
                            yearPrecip %>% dplyr::group_by(Basn_nm) %>% dplyr::summarize(sd = sd(Rain_mm, na.rm=TRUE)))




precip = read.csv("GoogleEE/Kar_basins_1981_2023_medianPrecip.csv",header=T) # median daily rainfall over the basin
head(precip)
precip$date = stri_sub(precip$date,from = 1,to = 10)
precip$date = ymd(precip$date)
precip$year = year(precip$date)
precip$month = lubridate::month(precip$date)

#Summarize the taluk wise rainfall by month, year
yearPrecip = as.data.frame(precip %>%
                             dplyr::group_by(Basn_nm, year) %>%
                             dplyr::summarize(Rain_mm = sum(median, na.rm=TRUE)))  

AvgAnnualPrecip2 = yearPrecip %>% dplyr::group_by(Basn_nm) %>% dplyr::summarize(Mean_Annual_Rain_mm = mean(Rain_mm, na.rm=TRUE))
AvgAnnualPrecip2 = left_join(AvgAnnualPrecip2, 
                             yearPrecip %>% dplyr::group_by(Basn_nm) %>% dplyr::summarize(sd = sd(Rain_mm, na.rm=TRUE)))


AvgAnnualPrecip2$Mean_Annual_Rain_mm = stri_paste(round(as.numeric(AvgAnnualPrecip2$Mean_Annual_Rain_mm),2),
                                                  " ± ",round(as.numeric(AvgAnnualPrecip2$sd),2))

AvgAnnualPrecip2 = AvgAnnualPrecip2 %>% select(Basn_nm, Mean_Annual_Rain_mm)

#### get elevation ranges

elev = read.csv("GoogleEE/Kar_basins_medianElev.csv",header=T)
head(elev)
names(elev) = c("Basn_nm", "Max_elev_m", "Min_elev_m")

AvgAnnualPrecip2 = left_join(AvgAnnualPrecip2,elev)
