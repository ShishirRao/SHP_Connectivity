setwd("E:/Shishir/FieldData/Analysis/Connectivity/SHP_Connectivity/")

prop_old_Gurupura = st_read("Gurupura/Gurupura_SHPs_new_2010.shp")
prop_new_Gurupura = st_read("Gurupura/Gurupura_SHPs_new.shp")
old_full = read.csv("E:/Shishir/FieldData/SHP/SHPfiles_Shishir/SHPfiles_Shishir/SHPs_commisioned_proposed_karnataka.csv",header=T)

names(prop_old_Gurupura) = c("Sl.no","Company", "District", "Capacity", "kredl_lat",  "kredl_long", "final.long", "final.lat","geometry")
names(prop_old_Gurupura)
prop_old_Gurupura = as.data.frame(prop_old_Gurupura)

names(old_full)
class(old_full$kredl_lat)

prop_old_Gurupura$kredl_lat = as.character(prop_old_Gurupura$kredl_lat)
prop_old_Gurupura$kredl_long = as.character(prop_old_Gurupura$kredl_long)

old_full$kredl_lat = as.character(old_full$kredl_lat)
old_full$kredl_long =as.character(old_full$kredl_long)



temp = left_join(prop_old_Gurupura,old_full,join_by(Company,District,Capacity,kredl_lat,kredl_long))

