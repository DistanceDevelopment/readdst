# extract data from the DistIni.mdb file

library(Hmisc)

# get the parameter codes and descriptions
param_str <- mdb.get("DistIni.mdb","ProjectSettings")
param_str <- param_str[param_str$Section == "PrmDesc",]
param_str$Section <- NULL
print(param_str)


# Unit conversions
unitc <- mdb.get("DistIni.mdb","ProjectSettingsNumber")
unitc <- unitc[unitc$Section %in% c("LinUnit", "AreaUnit", "AngUnit"),]
unitc$Section <- NULL
print(unitc)



