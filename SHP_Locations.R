library(dplyr)
library(tidyr)
library("sf")
library("sp")


com = read.csv("E:/Shishir/FieldData/RTI/SHP RTI data/Download_KREDL/comdhydro.csv",header=T)

com$Lat_W = com$Lat.W.
com$Long_W = com$Long.W.

com$Lat_PH = com$Lat.PH.
com$Lat_PH = com$Long.PH.

com = com %>% separate(Lat.W.,c("Lat_W_Deg","Lat_W_Min","Lat_W_Sec")," ") 
com = com %>% separate(Long.W.,c("Long_W_Deg","Long_W_Min","Long_W_Sec")," ")
com = com %>% separate(Lat.PH.,c("Lat_PH_Deg","Lat_PH_Min","Lat_PH_Sec")," ")
com = com %>% separate(Long.PH.,c("Long_PH_Deg","Long_PH_Min","Long_PH_Sec")," ")

com$Lat_W = as.numeric(com$Lat_W_Deg) + as.numeric(com$Lat_W_Min)/60 + as.numeric(com$Lat_W_Sec)/3600
com$Long_W = as.numeric(com$Long_W_Deg) + as.numeric(com$Long_W_Min)/60 + as.numeric(com$Long_W_Sec)/3600
com$Lat_PH = as.numeric(com$Lat_PH_Deg) + as.numeric(com$Lat_PH_Min)/60 + as.numeric(com$Lat_PH_Sec)/3600
com$Long_PH = as.numeric(com$Long_PH_Deg) + as.numeric(com$Long_PH_Min)/60 + as.numeric(com$Long_PH_Sec)/3600

names(com)
com = com %>% select(-Lat_W_Deg,-Lat_W_Min,-Lat_W_Sec,-Long_W_Deg,-Long_W_Min,-Long_W_Sec,
                     -Lat_PH_Deg,-Lat_PH_Min,-Lat_PH_Sec,-Long_PH_Deg,-Long_PH_Min,-Long_PH_Sec)

#write.csv(com,"E:/Shishir/FieldData/RTI/SHP RTI data/Download_KREDL/comdhydro_1.csv")

#Now read the data from Suman and join the two datasets

all_com = st_read("E:/Shishir/FieldData/SHP/SHPfiles_Shishir/SHPfiles_Shishir/All commissioned.shp")
all_com = as.data.frame(all_com)

names(com)

names(all_com)[1] = "SL.No."
names(all_com)[2] = "Company"
names(all_com)[7] = "Commissioned.Capacity..MW."

class(all_com$Commissioned.Capacity..MW.)

com$SLNo_comment = com$SL.No.
com$SL.No. = as.numeric(com$SL.No.)


com = left_join(com,all_com,by = c("SL.No.","Company","Commissioned.Capacity..MW."))
names(com)

#write.csv(com,"E:/Shishir/FieldData/RTI/SHP RTI data/Download_KREDL/comdhydro_2.csv")


#### let us look at cancelled SHPs ###
canc = read.csv("E:/Shishir/FieldData/RTI/SHP RTI data/Download_KREDL/canclhydro.csv",header=T)
names(canc)

canc$Lat_W = canc$Lat.W.
canc$Long_W = canc$Long.W.

canc$Lat_PH = canc$Lat.PH.
canc$Lat_PH = canc$Long.PH.

canc = canc %>% separate(Lat.W.,c("Lat_W_Deg","Lat_W_Min","Lat_W_Sec")," ") 
canc = canc %>% separate(Long.W.,c("Long_W_Deg","Long_W_Min","Long_W_Sec")," ")
canc = canc %>% separate(Lat.PH.,c("Lat_PH_Deg","Lat_PH_Min","Lat_PH_Sec")," ")
canc = canc %>% separate(Long.PH.,c("Long_PH_Deg","Long_PH_Min","Long_PH_Sec")," ")

canc$Lat_W = as.numeric(canc$Lat_W_Deg) + as.numeric(canc$Lat_W_Min)/60 + as.numeric(canc$Lat_W_Sec)/3600
canc$Long_W = as.numeric(canc$Long_W_Deg) + as.numeric(canc$Long_W_Min)/60 + as.numeric(canc$Long_W_Sec)/3600
canc$Lat_PH = as.numeric(canc$Lat_PH_Deg) + as.numeric(canc$Lat_PH_Min)/60 + as.numeric(canc$Lat_PH_Sec)/3600
canc$Long_PH = as.numeric(canc$Long_PH_Deg) + as.numeric(canc$Long_PH_Min)/60 + as.numeric(canc$Long_PH_Sec)/3600

