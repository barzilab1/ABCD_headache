library(dplyr)

source("config.R")
source("utility_fun.R")

########## ABCD Sum Scores Physical Health Parent ###########

ssphp01 = load_instrument("abcd_ssphp01", abcd_files_path)

#select variables
ssphp01 = ssphp01[,!grepl("_(nm|nt|dims|da|swtd|does|shy|total)$",colnames(ssphp01))]

ssphp01$male_p_late_or_post_puberty = ifelse( ssphp01$pds_p_ss_male_category_2 >= 4 ,1,0)
ssphp01$female_p_late_or_post_puberty = ifelse( ssphp01$pds_p_ss_female_category_2 >= 4 ,1, 0)

ssphp01$late_or_post_puberty_both_sexes_p <- ifelse(is.na(ssphp01$male_p_late_or_post_puberty), ssphp01$female_p_late_or_post_puberty, ssphp01$male_p_late_or_post_puberty )
ssphp01$puberty_both_sexes_p <- ifelse(is.na(ssphp01$pds_p_ss_male_category_2), ssphp01$pds_p_ss_female_category_2, ssphp01$pds_p_ss_male_category_2)

########## ABCD Sum Scores Physical Health Youth ###########

ssphy01 = load_instrument("abcd_ssphy01", abcd_files_path)

#select variables
ssphy01 = ssphy01[,!grepl("_(nm|nt)$",colnames(ssphy01))]

ssphy01$male_y_late_or_post_puberty = ifelse(ssphy01$pds_y_ss_male_cat_2 >= 4, 1,0)
ssphy01$female_y_late_or_post_puberty = ifelse(ssphy01$pds_y_ss_female_category_2 >=4, 1,0)
ssphy01$late_or_post_puberty_both_sexes = ifelse(is.na(ssphy01$male_y_late_or_post_puberty), ssphy01$female_y_late_or_post_puberty, ssphy01$male_y_late_or_post_puberty )


which(ssphy01$pds_y_ss_male_cat_2 & ssphy01$pds_y_ss_female_category_2)
ssphy01$puberty_both_sexes = ifelse(is.na(ssphy01$pds_y_ss_male_cat_2), ssphy01$pds_y_ss_female_category_2, ssphy01$pds_y_ss_male_cat_2)


########### Summary Scores Medical History ###########
medhxss01 = load_instrument("abcd_medhxss01",abcd_files_path)
medhxss01 = medhxss01[,grepl("src|interview|event|medhx_ss_6[i|j|p]_times_p",colnames(medhxss01))]
names(medhxss01)[names(medhxss01) == "medhx_ss_6i_times_p"] <- "medhx_ss_6i_times_p_l"
names(medhxss01)[names(medhxss01) == "medhx_ss_6j_times_p"] <- "medhx_ss_6j_times_p_l"
names(medhxss01)[names(medhxss01) == "medhx_ss_6p_times_p"] <- "medhx_ss_6p_times_p_l"


########### Longitudinal Summary Scores Medical History ###########

lssmh01 = load_instrument("abcd_lssmh01",abcd_files_path)
lssmh01 = lssmh01[,grepl("src|interview|event|medhx_ss_6[i|j|p]_times_p_l",colnames(lssmh01))]
#medhx_ss_6i_times_p_l has one value of 20. outlier?







########### ABCD Sum Scores Traumatic Brain Injury ###########
tbi01 = load_instrument("abcd_tbi01", abcd_files_path)
tbi01 = tbi01[,!grepl("_nm",colnames(tbi01))]
tbi01 = tbi01 %>% rename_with(., ~ paste(.x, "_l", sep = ""), .cols = contains(c("tbi_ss_nt", "agefirst", "overall")))

########### ABCD Longitudinal Summary Scores Traumatic Brain Injury  ###########
lsstbi01 = load_instrument("abcd_lsstbi01", abcd_files_path)
lsstbi01 = lsstbi01[,!grepl("_nm",colnames(lsstbi01))]




physicalhealth_sum = bind_rows(medhxss01, lssmh01)
tbi = bind_rows(tbi01, lsstbi01)
physicalhealth_sum = merge(physicalhealth_sum, ssphp01)
physicalhealth_sum = merge(physicalhealth_sum, ssphy01)
physicalhealth_sum = merge(physicalhealth_sum, tbi)
write.csv(file = "outputs/physicalhealth_sum.csv",x = physicalhealth_sum, row.names = F, na = "")






