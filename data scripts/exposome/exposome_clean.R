
source("config.R")
source("utility_fun.R")

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
fhxssp = load_instrument("abcd_fhxssp01",abcd_files_path)

#TODO check for each time point
#remove columns with more than 20% NA
# fhxssp = fhxssp[,-which(colSums(is.na(fhxssp)) >= 0.2*dim(fhxssp)[1])]

summary(fhxssp)

########### Youth Life Events ###########
yle01 = load_instrument("abcd_yle01",abcd_files_path)

summary(droplevels(yle01[yle01$eventname == "1_year_follow_up_y_arm_1",]))


########### family relationship section ###########
acspsw03 = load_instrument("acspsw03",abcd_files_path)
acspsw03 = acspsw03[acspsw03$eventname == "baseline_year_1_arm_1",grepl("src|inter|sex|event|fam", colnames(acspsw03))]

summary(acspsw03)


########### Longitudinal Parent Sports and Activities Involvement Questionnaire ###########
lpsaiq = load_instrument("abcd_lpsaiq01",abcd_files_path)

lpsaiq[lpsaiq == 999] = NA
lpsaiq = lpsaiq[, !(colnames(lpsaiq) %in% c("sai_l_p_select_language___1"))]

#TODO check for each time point
#remove columns with more than 20% NA
# lpsaiq = lpsaiq[,-which(colSums(is.na(lpsaiq)) >= 0.2*dim(lpsaiq)[1])]
summary(droplevels(lpsaiq))


########### merge all tables
exposome_set = merge(abcd_ydmes01, abcd_nsc01)
exposome_set = merge(exposome_set,abcd_pnsc01)
exposome_set = merge(exposome_set,abcd_fhxssp01)
exposome_set = merge(exposome_set,abcd_yle01)
exposome_set = merge(exposome_set,abcd_lpsaiq01)


write.csv(file = "outputs/family_id.csv",x = acspsw03, row.names=F, na = "")
write.csv(file = "outputs/exposome_set.csv",x = exposome_set, row.names=F, na = "")



