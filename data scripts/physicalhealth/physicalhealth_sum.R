library(dplyr)

source("config.R")
source("utility_fun.R")

########### Summary Scores Medical History ###########
medhxss01 = load_instrument("abcd_medhxss01", abcd_files_path)
medhxss01 = medhxss01[,grepl("src|interview|event|medhx_ss_6[i|j|p]_times_p",colnames(medhxss01))]
names(medhxss01)[names(medhxss01) == "medhx_ss_6i_times_p"] <- "medhx_ss_6i_times_p_l"
names(medhxss01)[names(medhxss01) == "medhx_ss_6j_times_p"] <- "medhx_ss_6j_times_p_l"
names(medhxss01)[names(medhxss01) == "medhx_ss_6p_times_p"] <- "medhx_ss_6p_times_p_l"


########### Longitudinal Summary Scores Medical History ###########

lssmh01 = load_instrument("abcd_lssmh01", abcd_files_path)
lssmh01 = lssmh01[,grepl("src|interview|event|medhx_ss_6[i|j|p]_times_p_l",colnames(lssmh01))]


########### ABCD Sum Scores Traumatic Brain Injury ###########
tbi01 = load_instrument("abcd_tbi01", abcd_files_path)
tbi01 = tbi01[,!grepl("_nm",colnames(tbi01))]
tbi01 = tbi01 %>% rename_with(., ~ paste(.x, "_l", sep = ""), .cols = contains(c("tbi_ss_nt", "agefirst", "overall")))

########### ABCD Longitudinal Summary Scores Traumatic Brain Injury  ###########
lsstbi01 = load_instrument("abcd_lsstbi01", abcd_files_path)
lsstbi01 = lsstbi01[,!grepl("_nm",colnames(lsstbi01))]


abcd_lpohstbi01
physicalhealth_sum = bind_rows(medhxss01, lssmh01)
tbi = bind_rows(tbi01, lsstbi01)
physicalhealth_sum = merge(physicalhealth_sum, tbi)

# Create binary variables
## 2_Number of knocked unconscious: if >0: then code as 1 --- if =0: then code as 0
## 3_Number of head injuries: if >0: then code as 1 --- if =0: then code as 0
## 4_ worst injury overall: if >1: then code as 1 --- if =1: then code as 0
physicalhealth_sum <- physicalhealth_sum %>%
    mutate(
        knocked_unconscious = case_when(medhx_ss_6j_times_p_l > 0 ~ 1, TRUE ~ as.numeric(medhx_ss_6j_times_p_l)),
        head_injuries = case_when(medhx_ss_6i_times_p_l > 0 ~ 1, TRUE ~ as.numeric(medhx_ss_6i_times_p_l)),
        tbi_worst_overall = case_when(tbi_ss_worst_overall_l == 1 ~ 0, tbi_ss_worst_overall_l > 1 ~ 1, TRUE ~ as.numeric(tbi_ss_worst_overall_l)))

write.csv(file = "data/physicalhealth_sum.csv", x = physicalhealth_sum, row.names = F, na = "")






