library(dplyr)
library(tidyr)
library("sf")
library("sp")
library("stringr")


com = read.csv("E:/Shishir/FieldData/RTI/SHP RTI data/Download_KREDL/comdhydro.csv",header=T)

com$Lat_W = com$Lat.W.
com$Long_W = com$Long.W.

com$Lat_PH = com$Lat.PH.
com$Lat_PH = com$Long.PH.

com = com %>% separate(Lat.W.,c("Lat_W_Deg","Lat_W_Min","Lat_W_Sec")," ") 
com = com %>% separate(Long.W.,c("Long_W_Deg","Long_W_Min","Long_W_Sec")," ")
com = com %>% separate(Lat.PH.,c("Lat_PH_Deg","Lat_PH_Min","Lat_PH_Sec")," ")
com = com %>% separate(Long.PH.,c("Long_PH_Deg","Long_PH_Min","Long_PH_Sec")," ")

com$Lat_W = as.double(com$Lat_W_Deg) + as.double(com$Lat_W_Min)/60 + as.double(com$Lat_W_Sec)/3600
com$Long_W = as.double(com$Long_W_Deg) + as.double(com$Long_W_Min)/60 + as.double(com$Long_W_Sec)/3600
com$Lat_PH = as.double(com$Lat_PH_Deg) + as.double(com$Lat_PH_Min)/60 + as.double(com$Lat_PH_Sec)/3600
com$Long_PH = as.double(com$Long_PH_Deg) + as.double(com$Long_PH_Min)/60 + as.double(com$Long_PH_Sec)/3600

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
com$SL.No. = as.double(com$SL.No.)


com = left_join(com,all_com,by = c("SL.No.","Company","Commissioned.Capacity..MW."))
names(com)

#write.csv(com,"E:/Shishir/FieldData/RTI/SHP RTI data/Download_KREDL/comdhydro_2.csv")


#### let us look at cancelled SHPs ###
canc = read.csv("E:/Shishir/FieldData/RTI/SHP RTI data/Download_KREDL/canclhydro.csv",header=T)
names(canc)

canc$Lat_W = canc$Lat.W.
canc$Long_W = canc$Long.W.

canc$Lat_PH = canc$Lat.PH.
canc$Long_PH = canc$Long.PH.

# first split the columns with multiple, comma separated lat or long values
canc = separate(canc, 'Lat.W.', paste("Lat.W.", 1:4, sep = c("_","to")), sep=", ", extra="drop")
canc$Lat.W. = canc$Lat.W._1
# now split the columns that have "to" in between co-ordinates
canc = separate(canc, 'Lat.W.', paste("Lat.W.", 5:6, sep="_"), sep="to ", extra="drop")
canc$Lat.W._1[str_detect(canc$Lat.W._1, "to")] = canc$Lat.W._5[str_detect(canc$Lat.W._1, "to")]
canc$Lat.W._2[!is.na(canc$Lat.W._6)] = canc$Lat.W._6[!is.na(canc$Lat.W._6)] 

canc = separate(canc, 'Long.W.', paste("Long.W.", 1:4, sep = c("_","to")), sep=", ", extra="drop")
canc$Long.W. = canc$Long.W._1
canc = separate(canc, 'Long.W.', paste("Long.W.", 5:6, sep="_"), sep="to ", extra="drop")
canc$Long.W._1[str_detect(canc$Long.W._1, "to")] = canc$Long.W._5[str_detect(canc$Long.W._1, "to")]
canc$Long.W._2[!is.na(canc$Long.W._6)] = canc$Long.W._6[!is.na(canc$Long.W._6)] 

canc = separate(canc, 'Lat.PH.', paste("Lat.PH.", 1:4, sep = c("_","to")), sep=", ", extra="drop")
canc$Lat.PH. = canc$Lat.PH._1
canc = separate(canc, 'Lat.PH.', paste("Lat.PH.", 5:6, sep="_"), sep="to ", extra="drop")
canc$Lat.PH._1[str_detect(canc$Lat.PH._1, "to")] = canc$Lat.PH._5[str_detect(canc$Lat.PH._1, "to")]
canc$Lat.PH._2[!is.na(canc$Lat.PH._6)] = canc$Lat.PH._6[!is.na(canc$Lat.PH._6)] 

canc = separate(canc, 'Long.PH.', paste("Long.PH.", 1:4, sep = c("_","to")), sep=", ", extra="drop")
canc$Long.PH. = canc$Long.PH._1
canc = separate(canc, 'Long.PH.', paste("Long.PH.", 5:6, sep="_"), sep="to ", extra="drop")
canc$Long.PH._1[str_detect(canc$Long.PH._1, "to")] = canc$Long.PH._5[str_detect(canc$Long.PH._1, "to")]
canc$Long.PH._2[!is.na(canc$Long.PH._6)] = canc$Long.PH._6[!is.na(canc$Long.PH._6)] 


# now separate the deg, min and sec columns
canc = canc %>% separate(Lat.W._1,c("Lat_W_1_Deg","Lat_W_1_Min","Lat_W_1_Sec")," ") 
canc = canc %>% separate(Lat.W._2,c("Lat_W_2_Deg","Lat_W_2_Min","Lat_W_2_Sec")," ") 
canc = canc %>% separate(Lat.W._3,c("Lat_W_3_Deg","Lat_W_3_Min","Lat_W_3_Sec")," ") 
canc = canc %>% separate(Lat.W._4,c("Lat_W_4_Deg","Lat_W_4_Min","Lat_W_4_Sec")," ") 

canc = canc %>% separate(Lat.PH._1,c("Lat_PH_1_Deg","Lat_PH_1_Min","Lat_PH_1_Sec")," ") 
canc = canc %>% separate(Lat.PH._2,c("Lat_PH_2_Deg","Lat_PH_2_Min","Lat_PH_2_Sec")," ") 
canc = canc %>% separate(Lat.PH._3,c("Lat_PH_3_Deg","Lat_PH_3_Min","Lat_PH_3_Sec")," ") 
canc = canc %>% separate(Lat.PH._4,c("Lat_PH_4_Deg","Lat_PH_4_Min","Lat_PH_4_Sec")," ") 

canc = canc %>% separate(Long.W._1,c("Long_W_1_Deg","Long_W_1_Min","Long_W_1_Sec")," ")
canc = canc %>% separate(Long.W._2,c("Long_W_2_Deg","Long_W_2_Min","Long_W_2_Sec")," ")
canc = canc %>% separate(Long.W._3,c("Long_W_3_Deg","Long_W_3_Min","Long_W_3_Sec")," ")
canc = canc %>% separate(Long.W._4,c("Long_W_4_Deg","Long_W_4_Min","Long_W_4_Sec")," ")


canc = canc %>% separate(Long.PH._1,c("Long_PH_1_Deg","Long_PH_1_Min","Long_PH_1_Sec")," ")
canc = canc %>% separate(Long.PH._2,c("Long_PH_2_Deg","Long_PH_2_Min","Long_PH_2_Sec")," ")
canc = canc %>% separate(Long.PH._3,c("Long_PH_3_Deg","Long_PH_3_Min","Long_PH_3_Sec")," ")
canc = canc %>% separate(Long.PH._4,c("Long_PH_4_Deg","Long_PH_4_Min","Long_PH_4_Sec")," ")


temp = canc %>% select(SL._No., Company, Long_W,Long_W_1, Long_W_1_Deg,Long_W_1_Min,Long_W_1_Sec)  

## if sec values are NA, replace them with zero
canc$Lat_W_1_Sec[is.na(canc$Lat_W_1_Sec) | canc$Lat_W_1_Sec == ""] = as.double(0)
canc$Lat_W_2_Sec[is.na(canc$Lat_W_2_Sec) | canc$Lat_W_2_Sec == ""] = as.double(0)
canc$Lat_W_3_Sec[is.na(canc$Lat_W_3_Sec) | canc$Lat_W_3_Sec == ""] = as.double(0) 
canc$Lat_W_4_Sec[is.na(canc$Lat_W_4_Sec) | canc$Lat_W_4_Sec == ""] = as.double(0) 


canc$Lat_PH_1_Sec[is.na(canc$Lat_PH_1_Sec) | canc$Lat_PH_1_Sec == ""] = as.double(0) 
canc$Lat_PH_2_Sec[is.na(canc$Lat_PH_2_Sec) | canc$Lat_PH_2_Sec == ""] = as.double(0) 
canc$Lat_PH_3_Sec[is.na(canc$Lat_PH_3_Sec) | canc$Lat_PH_3_Sec == ""] = as.double(0)
canc$Lat_PH_4_Sec[is.na(canc$Lat_PH_4_Sec) | canc$Lat_PH_4_Sec == ""] = as.double(0)


canc$Long_W_1_Sec[is.na(canc$Long_W_1_Sec) | canc$Long_W_1_Sec == ""] = as.double(0) 
canc$Long_W_2_Sec[is.na(canc$Long_W_2_Sec) | canc$Long_W_2_Sec == ""] = as.double(0) 
canc$Long_W_3_Sec[is.na(canc$Long_W_3_Sec) | canc$Long_W_3_Sec == ""] = as.double(0)
canc$Long_W_4_Sec[is.na(canc$Long_W_4_Sec) | canc$Long_W_4_Sec == ""] = as.double(0)


canc$Long_PH_1_Sec[is.na(canc$Long_PH_1_Sec) | canc$Long_PH_1_Sec == ""] = as.double(0)
canc$Long_PH_2_Sec[is.na(canc$Long_PH_2_Sec) | canc$Long_PH_2_Sec == ""] = as.double(0)
canc$Long_PH_3_Sec[is.na(canc$Long_PH_3_Sec) | canc$Long_PH_3_Sec == ""] = as.double(0)
canc$Long_PH_4_Sec[is.na(canc$Long_PH_4_Sec) | canc$Long_PH_4_Sec == ""] = as.double(0)

#convert from hms to decimal
canc$Lat_W_1 = as.double(canc$Lat_W_1_Deg) + as.double(canc$Lat_W_1_Min)/60 + as.double(canc$Lat_W_1_Sec)/3600
canc$Lat_W_2 = as.double(canc$Lat_W_2_Deg) + as.double(canc$Lat_W_2_Min)/60 + as.double(canc$Lat_W_2_Sec)/3600
canc$Lat_W_3 = as.double(canc$Lat_W_3_Deg) + as.double(canc$Lat_W_3_Min)/60 + as.double(canc$Lat_W_3_Sec)/3600
canc$Lat_W_4 = as.double(canc$Lat_W_4_Deg) + as.double(canc$Lat_W_4_Min)/60 + as.double(canc$Lat_W_4_Sec)/3600

as.double(canc$Long_W_1_Deg)[178] + as.double(canc$Long_W_1_Min)[178]/60 + as.double(canc$Long_W_1_Sec)[178]/3600

canc$Long_W_1 = as.double(canc$Long_W_1_Deg) + as.double(canc$Long_W_1_Min)/60 + as.double(canc$Long_W_1_Sec)/3600
canc$Long_W_2 = as.double(canc$Long_W_2_Deg) + as.double(canc$Long_W_2_Min)/60 + as.double(canc$Long_W_2_Sec)/3600
canc$Long_W_3 = as.double(canc$Long_W_3_Deg) + as.double(canc$Long_W_3_Min)/60 + as.double(canc$Long_W_3_Sec)/3600
canc$Long_W_4 = as.double(canc$Long_W_4_Deg) + as.double(canc$Long_W_4_Min)/60 + as.double(canc$Long_W_4_Sec)/3600


canc$Lat_PH_1 = as.double(canc$Lat_PH_1_Deg) + as.double(canc$Lat_PH_1_Min)/60 + as.double(canc$Lat_PH_1_Sec)/3600
canc$Lat_PH_2 = as.double(canc$Lat_PH_2_Deg) + as.double(canc$Lat_PH_2_Min)/60 + as.double(canc$Lat_PH_2_Sec)/3600
canc$Lat_PH_3 = as.double(canc$Lat_PH_3_Deg) + as.double(canc$Lat_PH_3_Min)/60 + as.double(canc$Lat_PH_3_Sec)/3600
canc$Lat_PH_4 = as.double(canc$Lat_PH_4_Deg) + as.double(canc$Lat_PH_4_Min)/60 + as.double(canc$Lat_PH_4_Sec)/3600


canc$Long_PH_1 = as.double(canc$Long_PH_1_Deg) + as.double(canc$Long_PH_1_Min)/60 + as.double(canc$Long_PH_1_Sec)/3600
canc$Long_PH_2 = as.double(canc$Long_PH_2_Deg) + as.double(canc$Long_PH_2_Min)/60 + as.double(canc$Long_PH_2_Sec)/3600
canc$Long_PH_3 = as.double(canc$Long_PH_3_Deg) + as.double(canc$Long_PH_3_Min)/60 + as.double(canc$Long_PH_3_Sec)/3600
canc$Long_PH_4 = as.double(canc$Long_PH_4_Deg) + as.double(canc$Long_PH_4_Min)/60 + as.double(canc$Long_PH_4_Sec)/3600

names(canc)

#remove extra columns
canc = canc %>% select(-Lat_W_1_Deg,-Lat_W_1_Min,-Lat_W_1_Sec,
                       -Lat_W_2_Deg,-Lat_W_2_Min,-Lat_W_2_Sec,
                       -Lat_W_3_Deg,-Lat_W_3_Min,-Lat_W_3_Sec,
                       -Lat_W_4_Deg,-Lat_W_4_Min,-Lat_W_4_Sec,
                       
                       -Lat_PH_1_Deg,-Lat_PH_1_Min,-Lat_PH_1_Sec,
                       -Lat_PH_2_Deg,-Lat_PH_2_Min,-Lat_PH_2_Sec,
                       -Lat_PH_3_Deg,-Lat_PH_3_Min,-Lat_PH_3_Sec,
                       
                       -Long_W_1_Deg,-Long_W_1_Min,-Long_W_1_Sec,
                       -Long_W_2_Deg,-Long_W_2_Min,-Long_W_2_Sec,
                       -Long_W_3_Deg,-Long_W_3_Min,-Long_W_3_Sec,
                       
                       -Long_PH_1_Deg,-Long_PH_1_Min,-Long_PH_1_Sec,
                       -Long_PH_2_Deg,-Long_PH_2_Min,-Long_PH_2_Sec,
                       -Long_PH_3_Deg,-Long_PH_3_Min,-Long_PH_3_Sec,
                       
                       -Lat.W._5,-Lat.W._6,
                       -Lat.PH._5,-Lat.PH._6,
                       -Long.W._5,-Long.W._6,
                       -Long.PH._5,-Long.PH._6)
                       
temp = canc %>% select(SL._No., Company, Lat_W, Lat_W_1, Long_W, Long_W_1)  
                       
#write.csv(canc,"E:/Shishir/FieldData/RTI/SHP RTI data/Download_KREDL/canchydro_2.csv")

