setwd("E:/Shishir/FieldData/Analysis/Connectivity/SHP_Connectivity/")

prop_old_Gurupura = st_read("Gurupura/Gurupura_SHPs_new_2010.shp")
prop_new_Gurupura = st_read("Gurupura/Gurupura_SHPs_new.shp")
old_full = read.csv("E:/Shishir/FieldData/SHP/SHPfiles_Shishir/SHPfiles_Shishir/SHPs_commisioned_proposed_karnataka.csv",header=T)

names(prop_old_Gurupura) = c("Sl.no","Company", "District", "Capacity", "kredl_lat",  "kredl_long", "final.long", "final.lat","geometry")
names(prop_old_Gurupura)
prop_old_Gurupura = as.data.frame(prop_old_Gurupura)

names(old_full)
class(old_full$kredl_lat)

prop_old_Suvarna = st_read("Suvarna/Suvarna_SHPs_new_2010.shp")
prop_new_Suvarna = st_read("Suvarna/Suvarna_SHPs_new.shp")


prop_new_Suvarna$Company 

prop_old_Suvarna$Company [(which(prop_old_Suvarna$Company %in% prop_new_Suvarna$Company== FALSE))]

