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


########### ABCD Parent Pubertal Development Scale and Menstrual Cycle Survey History (PDMS) ###########

# ppdms = load_instrument("abcd_ppdms01",abcd_files_path)
#
# #"Don't know" will be treated as NA
# ppdms[ppdms == "999"] = NA
#
# #remove empty col
# ppdms = ppdms[, !colSums(is.na(ppdms)) == nrow(ppdms)]
# ppdms$pds_select_language___1 = NULL
# #
# # #fix scale
# ppdms$pds_f5b_p = ppdms$pds_f5b_p - 1
# ppdms$pds_f5b_p[ppdms$pds_f5b_p == 3] = 1
#
# ppdms$pds_f6_p[ppdms$pds_f6_p >= 99] = NA
# ppdms$menstrualcycle2_p[ppdms$menstrualcycle2_p >= 400] = NA
# ppdms$menstrualcycle2_p[ppdms$menstrualcycle2_p <= 3] = NA
#
# describe(ppdms)


########### ABCD Youth Pubertal Development Scale and Menstrual Cycle Survey History (PDMS) ###########

# ypdms = load_instrument("abcd_ypdms01",abcd_files_path)

#"Don't know" and "Decline to answer" will be treated as NA
# ypdms[ypdms == "777" | ypdms == "999"] = NA
# ypdms$pds_device = NULL
#
# #remove empty col
# ypdms = ypdms[, !colSums(is.na(ypdms)) == nrow(ypdms)]
#
# #fix scale
# ypdms$pds_f5_y = ypdms$pds_f5_y - 1
# ypdms$pds_f5_y[ypdms$pds_f5_y == 3] = 1
#
# ypdms$menstrualcycle4_y[ypdms$menstrualcycle4_y >= 2] = NA
#
# describe(ypdms)


########### ABCD Youth Youth Risk Behavior Survey Exercise Physical Activity (YRB) ###########

# yrb = load_instrument("abcd_yrb01",abcd_files_path)
#
# #select variables
# yrb = yrb[,grepl("src|interview|event|sex|physical_activity(1|2)_y",colnames(yrb))]
#
# #change scale
# yrb$physical_activity2_y = as.numeric(as.character(yrb$physical_activity2_y)) - 1


########### Pain Questionnaire ###########
# pq01 = load_instrument("abcd_pq01",abcd_files_path)
# pq01 = pq01[,grepl("src|interview|event|__b0[1-4]|__f0[1-2]",colnames(pq01))]
# pq01$head_pain = Reduce("|", pq01[,grep("__b0[3-4]", colnames(pq01),value = T)])*1
# pq01$head_cranium_pain = Reduce("|", pq01[,grep("__(b|f)", colnames(pq01),value = T)])*1




########### ABCD Parent Sleep Disturbance Scale for Children (SDS) ###########
# sds01 = load_instrument("abcd_sds01",abcd_files_path)
# sds01 = sds01[,grepl("src|interview|event|b1_p|b4_p|b15_p|b23_p|b25_p",colnames(sds01))]


########## ABCD Developmental History Questionnaire ##########
# dhx01 = load_instrument("dhx01",abcd_files_path)
## Not include eventname so that the devhx will be carried over next time points
# dhx01 = dhx01[,grepl("src|_9_(tobacco|alcohol|marijuana|coc_crack|her_morph|oxycont|other_drugs)$|caffeine_11$|_10[a-m]3_p$|_12a_p$|_14[a-h]3_p$",colnames(dhx01))]
# dhx01[dhx01 == 999] <- NA

########### ABCD Parent Ohio State Traumatic Brain Injury Screen-Short Modified (OTBI) ###########
otbi01 = load_instrument("abcd_otbi01", abcd_files_path)
otbi01 = otbi01[,grepl("src|event|tbi_[1-5]$|tbi_7a",colnames(otbi01))]


physicalhealth = bind_rows(mx01, lpmh01)
# physicalhealth = merge(physicalhealth, pq01, all.x = T)
# physicalhealth = merge(physicalhealth, sds01, all.x = T)
# physicalhealth = merge(physicalhealth, dhx01, all.x = T)
physicalhealth = merge(physicalhealth, otbi01, all.x = T)


write.csv(file = "outputs/physicalhealth.csv",x = physicalhealth, row.names = F, na = "")


