library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(stringi)
library("sp")
library(data.table)

setwd("E:/Shishir/FieldData/Analysis/Connectivity/SHP_Connectivity/")

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

elev$Min_elev_m[elev$Min_elev_m<0] = 0  

AvgAnnualPrecip2 = left_join(AvgAnnualPrecip2,elev)
names(AvgAnnualPrecip2)


AvgAnnualPrecip2$Mean_Annual_Rain_mm = as.character(AvgAnnualPrecip2$Mean_Annual_Rain_mm)
AvgAnnualPrecip2$elev_range = stri_paste(AvgAnnualPrecip2$Max_elev_m,' ','-',' ',AvgAnnualPrecip2$Min_elev_m)
AvgAnnualPrecip2 = AvgAnnualPrecip2 %>% select(Basn_nm,Mean_Annual_Rain_mm,elev_range)

class(AvgAnnualPrecip2$Mean_Annual_Rain_mm)
class(AvgAnnualPrecip2$elev_range)
  
### execute the dam elevs code and then merge the two datasets here
dam_table = left_join(AvgAnnualPrecip2,dam_summary)

dam_table$Direction = "West"
dam_table$Direction[dam_table$Basn_nm == "Bhima" |
                      dam_table$Basn_nm == "Kaveri" |
                      dam_table$Basn_nm == "Krishna" |
                      dam_table$Basn_nm == "Tunga" |
                      dam_table$Basn_nm == "Palar" |
                      dam_table$Basn_nm == "NorthPennar" |
                      dam_table$Basn_nm == "SouthPennar"] = "East"

dam_table[is.na(dam_table)] = "-"

###Last thing. Get watershed areas 

Areas = read.csv("Basins/StudyBasins.csv",header=T)
Areas = Areas %>% select(Basn_nm,Area)

dam_table = left_join(Areas,dam_table)

### now reorder the columns the rename before printing it out
names(dam_table)

dam_table = dam_table %>% select(Basn_nm,Area,Mean_Annual_Rain_mm,elev_range,
                                 large_existing_no,large_existing_elev_range,
                                 small_existing_no,small_existing_elev_range,
                                 small_proposed_no,small_proposed_elev_range,Direction)

names(dam_table) = c("Basin name","Basin area (Sq km)", "Average annual rainfall (mm)",	"Elevation range (m)",
                     "# of existing large dams",	"Elevation range of existing large dams",
                     "# of existing small dams",	"Elevation range of existing small dams",
                     "# of proposed small dams",	"Elevation range of proposed small dams","Direction")

dam_table_east = dam_table[dam_table$Direction == "East",] %>% select(-Direction)
rownames(dam_table_east) = seq(1:nrow(dam_table_east))

#write.csv(dam_table_east,"E:/Shishir/FieldData/Analysis/Connectivity/SHP_Connectivity/Basins/dam_table_east_4.csv")

dam_table_west = dam_table[dam_table$Direction == "West",] %>% select(-Direction)
rownames(dam_table_west) = seq(1:nrow(dam_table_west))
#write.csv(dam_table_west,"E:/Shishir/FieldData/Analysis/Connectivity/SHP_Connectivity/Basins/dam_table_west_2.csv")
