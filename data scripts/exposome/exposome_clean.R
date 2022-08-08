
source("config.R")
source("utility_fun.R")
library(dplyr)
########### Discrimination ###########
ydmes01 = load_instrument("abcd_ydmes01",abcd_files_path)

ydmes01[ydmes01 == 777 | ydmes01 == 999] = NA
ydmes01 = droplevels(ydmes01)

summary(ydmes01[ydmes01$eventname == "1_year_follow_up_y_arm_1",])

#check collinearity
library("psych")
matrix_names = colnames(ydmes01[ ,grep("_matrix_", colnames(ydmes01)) ])
ydmes01[,matrix_names] = apply(ydmes01[,matrix_names], 2, function(x) {as.numeric(as.character(x))})
xcor <- polychoric(ydmes01[,matrix_names])$rho
VSS.scree(xcor)
eigen(xcor)$values[1]/eigen(xcor)$values[2]


########### Youth Neighborhood Safety/Crime ###########
nsc01 = load_instrument("abcd_nsc01",abcd_files_path)


########### Parent Neighborhood Safety/Crime ###########
pnsc01 = load_instrument("abcd_pnsc01",abcd_files_path)
pnsc01 = pnsc01[, !(colnames(pnsc01) %in% c("nei_p_select_language___1"))]



########### Parent Family History Summary Scores ###########
fhxssp01 = load_instrument("abcd_fhxssp01",abcd_files_path)
fhxssp01 = fhxssp01[, grepl("src|interview|event|sex|(fath|moth|parent)_.*?(alc|dg|dprs)_p", colnames(fhxssp01))]

# Remove information of baseline --> carry one the history to later time points
fhxssp01 <- fhxssp01[, !(names(fhxssp01) %in% c("eventname", "interview_date", "interview_age", "sex"))]

# -1 or -2 (missing father/mother) will be NA
fhxssp01[fhxssp01 == -1 | fhxssp01 == -2] <- NA

#TODO check for each time point
#remove columns with more than 20% NA
# fhxssp01 = fhxssp01[,-which(colSums(is.na(fhxssp01)) >= 0.2*dim(fhxssp01)[1])]

########### Youth Life Events ###########
yle01 = load_instrument("abcd_yle01",abcd_files_path)

summary(droplevels(yle01[yle01$eventname == "1_year_follow_up_y_arm_1",]))


########### family relationship section ###########
acspsw03 = load_instrument("acspsw03",abcd_files_path)
acspsw03 = acspsw03[acspsw03$eventname == "baseline_year_1_arm_1",grepl("src|inter|sex|event|fam", colnames(acspsw03))]

summary(acspsw03)


########### Longitudinal Parent Sports and Activities Involvement Questionnaire ###########
lpsaiq01 = load_instrument("abcd_lpsaiq01",abcd_files_path)

lpsaiq01[lpsaiq01 == 999] = NA
lpsaiq01 = lpsaiq01[, !(colnames(lpsaiq01) %in% c("sai_l_p_select_language___1"))]

# Total activities
lpsaiq01$sai_total_activities = rowSums(lpsaiq01[,grepl("sai_p_activities_l___[0-28]",colnames(lpsaiq01))])

# select variables
lpsaiq01 <- lpsaiq01 %>% dplyr::select(src_subject_id, eventname, contains("perwk"), sai_total_activities)


########### ABCD Parent Sports and Activities Involvement Questionnaire (SAIQ) ###########
saiq02 = load_instrument("abcd_saiq02",abcd_files_path)
saiq02$sai_total_activities = rowSums(saiq02[,grepl("sai_p_activities___[0-28]",colnames(saiq02))])

saiq02 <- saiq02 %>% dplyr::select(src_subject_id, eventname, sai_total_activities)

########### Cyber Bully ###########
cb = load_instrument("abcd_cb01",abcd_files_path)
cb[cb == 777 | cb == 999] = NA

########### merge all tables
exposome_set = merge(ydmes01, nsc01, all.y = T)
exposome_set = merge(exposome_set, pnsc01, all.x = T)
exposome_set = merge(exposome_set, fhxssp01, all.x = T)
exposome_set = merge(exposome_set, yle01, all.x = T)
exposome_set = merge(exposome_set, lpsaiq01, all.x = T)
exposome_set = merge(exposome_set, saiq02, all.x = T)
exposome_set = merge(exposome_set, cb, all.x = T)

write.csv(file = "outputs/family_id.csv",x = acspsw03, row.names=F, na = "")
write.csv(file = "outputs/exposome_set.csv",x = exposome_set, row.names=F, na = "")



