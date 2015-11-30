# extract data from the DistIni.mdb file

library(Hmisc)

param_str <- mdb.get("DistIni.mdb","ProjectSettings")
param_str <- param_str[param_str$Section == "PrmDesc",]


param_str$Section <- NULL

print(param_str)


