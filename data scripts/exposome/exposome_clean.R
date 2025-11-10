
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
nsc01 = nsc01 %>% mutate(neighborh_notsafe_y = case_when(neighborhood_crime_y <= 2 ~ 1, neighborhood_crime_y > 2 ~ 0, TRUE ~ as.numeric(neighborhood_crime_y)))

########### Parent Neighborhood Safety/Crime ###########
pnsc01 = load_instrument("abcd_pnsc01",abcd_files_path)
pnsc01 = pnsc01[, !(colnames(pnsc01) %in% c("nei_p_select_language___1"))]
# Create binary variables
## 5_ my neighborhood is safe from crime: if <=2: then code as 1 --- if >2: then code as 0
# For sensitivity analysis 2 (P=0.5)
## 9-neighborhood2r_p
pnsc01 <- pnsc01 %>%
    mutate(
        neighborh_notsafe = case_when(neighborhood3r_p <= 2 ~ 1, neighborhood3r_p > 2 ~ 0, TRUE ~ as.numeric(neighborhood3r_p)),
        neighborh_violence = case_when(neighborhood2r_p <= 2 ~ 1, neighborhood2r_p > 2 ~ 0, TRUE ~ as.numeric(neighborhood2r_p))
    )

########### Parent Family History Summary Scores ###########
fhxssp01 = load_instrument("abcd_fhxssp01",abcd_files_path)
fhxssp01 = fhxssp01[, grepl("src|interview|event|sex|(fath|moth|momdad)_.*?(alc|dg|dprs)_p", colnames(fhxssp01))]

# Remove information of baseline --> carry on the history to later time points
fhxssp01 <- fhxssp01[, !(names(fhxssp01) %in% c("eventname", "interview_date", "interview_age", "sex"))]


########### Youth Life Events ###########
yle01 = load_instrument("abcd_yle01",abcd_files_path)
# Choose the variables
yle01 = yle01[, grepl("src|interview|event|sex|(_fu_y)$|(e[d|p]|u[r|d|e]|m[e|h]|i[m|b|l]|a[r|w|y|l]|o[b|l]|nd|ll|st|ve)_y$", colnames(yle01))]
# 6 and 7 will be NA
yle01[yle01 == 6 | yle01 == 7] <- NA

# Create bad(yes, no) life events
yle01 <- yle01 %>% mutate(across(contains("fu_"), ~case_when(.x == 2 ~ 1, .x == 1 ~ 0, TRUE ~ NA_real_), .names = "{col}_bad"))

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

# dhx01 <- dhx01[, grepl("src|interview|sex|event|devhx_9_(to|al)", colnames(dhx01))]
dhx01 <- dhx01[, grepl("src|sex|devhx_9_(to|al)", colnames(dhx01))]



########### merge all tables
exposome_set = merge(ydmes01, nsc01, all.y = T)
exposome_set = merge(exposome_set, pnsc01, all.x = T)
exposome_set = merge(exposome_set, fhxssp01, all.x = T)
exposome_set = merge(exposome_set, yle01, all.x = T)
exposome_set = merge(exposome_set, cb, all.x = T)
exposome_set = merge(exposome_set, dhx01, all.x = T)

write.csv(file = "data/family_id.csv",x = acspsw03, row.names=F, na = "")
write.csv(file = "data/exposome_set.csv",x = exposome_set, row.names=F, na = "")



