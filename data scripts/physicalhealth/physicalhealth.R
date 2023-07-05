source("config.R")
source("utility_fun.R")
library(dplyr)

########### Parent Medical History Questionnaire (MHX) ###########
mx01 = load_instrument("abcd_mx01",abcd_files_path)
mx01 = mx01[, grepl("src|interview|event|(2(h|q)|6(i|j))$", colnames(mx01))]
# Rename columns to bind with lpmh01
mx01 = mx01 %>% rename_with(., ~ paste(.x, "_l", sep = ""), .cols = contains("medhx"))

########### Longitudinal Parent Medical History Questionnaire ###########
lpmh01 = load_instrument("abcd_lpmh01",abcd_files_path)
lpmh01 = lpmh01[, grepl("src|interview|event|(2(h|q)|6(i|j))_l$", colnames(lpmh01))]


########### ABCD Parent Ohio State Traumatic Brain Injury Screen-Short Modified (OTBI) ###########
otbi01 = load_instrument("abcd_otbi01", abcd_files_path)
otbi01 = otbi01[,grepl("src|event|tbi_[1-5]$|tbi_7a",colnames(otbi01))]

########### ABCD Longitudinal Parent Ohio State Traumatic Brain Injury Screen-Short Modified (OTBI) ###########
lpohstbi01 = load_instrument("abcd_lpohstbi01", abcd_files_path)
lpohstbi01$tbi_l_select_language___1 = NULL

lpohstbi01 = lpohstbi01[,grepl("src|event|tbi_[1-5]_|tbi_7a_", colnames(lpohstbi01))]

### combine the 2 instruments
colnames(lpohstbi01) = sub("_l$", "", colnames(lpohstbi01))

physicalhealth = bind_rows(mx01, lpmh01)
tbi = bind_rows(otbi01, lpohstbi01)
physicalhealth = merge(physicalhealth, tbi, all.x = T)


write.csv(file = "data/physicalhealth.csv", x = physicalhealth, row.names = F, na = "")
