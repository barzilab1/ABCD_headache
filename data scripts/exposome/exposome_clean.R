
source("config.R")
source("utility_fun.R")
library(dplyr)
########### Discrimination ###########
ydmes01 = load_instrument("abcd_ydmes01",abcd_files_path)

ydmes01[ydmes01 == 777 | ydmes01 == 999] = NA
ydmes01 = droplevels(ydmes01)
ydmes01 <- ydmes01[, grepl("src|interview|sex|event|yesno", colnames(ydmes01))]

########### Youth Neighborhood Safety/Crime ###########
nsc01 = load_instrument("abcd_nsc01",abcd_files_path)


########### Parent Neighborhood Safety/Crime ###########
pnsc01 = load_instrument("abcd_pnsc01",abcd_files_path)
pnsc01 = pnsc01[, !(colnames(pnsc01) %in% c("nei_p_select_language___1"))]



########### Parent Family History Summary Scores ###########
fhxssp01 = load_instrument("abcd_fhxssp01",abcd_files_path)
fhxssp01 = fhxssp01[, grepl("src|interview|event|sex|(fath|moth|momdad)_.*?(alc|dg|dprs)_p", colnames(fhxssp01))]

# Remove information of baseline --> carry one the history to later time points
fhxssp01 <- fhxssp01[, !(names(fhxssp01) %in% c("eventname", "interview_date", "interview_age", "sex"))]

# -1 or -2 (missing father/mother) will be NA
fhxssp01[fhxssp01 == -1 | fhxssp01 == -2] <- NA

#TODO check for each time point
#remove columns with more than 20% NA
# fhxssp01 = fhxssp01[,-which(colSums(is.na(fhxssp01)) >= 0.2*dim(fhxssp01)[1])]

########### Youth Life Events ###########
yle01 = load_instrument("abcd_yle01",abcd_files_path)
# Choose the variables
yle01 = yle01[, grepl("src|interview|event|sex|(_fu_y)$|(e[d|p]|u[r|d|e]|m[e|h]|i[m|b|l]|a[r|w|y|l]|o[b|l]|nd|ll|st|ve)_y$", colnames(yle01))]
# 6 and 7 will be NA
yle01[yle01 == 6 | yle01 == 7] <- NA


########### family relationship section ###########
acspsw03 = load_instrument("acspsw03",abcd_files_path)
acspsw03 = acspsw03[acspsw03$eventname == "baseline_year_1_arm_1",grepl("src|inter|^sex|event|fam", colnames(acspsw03))]


########### Cyber Bully ###########
cb = load_instrument("abcd_cb01",abcd_files_path)
cb[cb == 777 | cb == 999] = NA

cb <- cb[, grepl("src|interview|sex|event|(_harm)$", colnames(cb))]

########### Developmental History ###########
dhx01 = load_instrument("dhx01",abcd_files_path)

dhx01[dhx01 == 999 | dhx01 == -1] = NA
dhx01$accult_select_language = NULL

#remove empty columns
dhx01 = dhx01[,colSums(is.na(dhx01)) != nrow(dhx01)]
dhx01$devhx_1_p = NULL

dhx01 <- dhx01[, grepl("src|interview|sex|event|devhx_9_(to|al)", colnames(dhx01))]



########### merge all tables
exposome_set = merge(ydmes01, nsc01, all.y = T)
exposome_set = merge(exposome_set, pnsc01, all.x = T)
exposome_set = merge(exposome_set, fhxssp01, all.x = T)
exposome_set = merge(exposome_set, yle01, all.x = T)
exposome_set = merge(exposome_set, cb, all.x = T)
exposome_set = merge(exposome_set, dhx01, all.x = T)

write.csv(file = "outputs/family_id.csv",x = acspsw03, row.names=F, na = "")
write.csv(file = "outputs/exposome_set.csv",x = exposome_set, row.names=F, na = "")



