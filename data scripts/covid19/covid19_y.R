library(psych)
library(plyr)
library(psych)

source("config.R")
source("utility_fun.R")


covidy_r1 = load_instrument("yabcdcovid19questionnaire01",covid19_1_3_files_path)
covidy_r2 = load_instrument("yabcdcovid19questionnaire01",covid19_4_6_files_path)

covidy = rbind.fill(covidy_r1, covidy_r2)
covidy[covidy == 777 | covidy == 999] = NA

#  new variable to use in reshape from long to wide format
covidy$timepoint = regmatches(covidy$eventname, regexpr("cv[1-6]", covidy$eventname))


####################################
###### outputs
####################################

###### 1. Sadness Scale
sadness_scale = covidy[,grep("src_|timepoint|felt|sad(.*)tot", colnames(covidy))]
sadness_scale$felt_angry_cv = NULL
sadness_scale$felt_nervous_cv = NULL
sadness_scale$felt_scared_cv = NULL

# sum only if there are 8 answers 
sadness_scale$felt_sad_cv_raw_tot_bar = rowSums(sadness_scale[,grep("felt", colnames(sadness_scale))])
sadness_scale$nih_sad_cv_raw_tot = NULL

# no missing data
sadness_scale = sadness_scale[rowSums(is.na(sadness_scale)) == 0,]

sadness_scale_wide = reshape(sadness_scale, direction = "wide", idvar = "src_subject_id", timevar = "timepoint", sep = "_")

# include kids with at least 2 time points
sadness_scale_wide = sadness_scale_wide[rowSums(!is.na(sadness_scale_wide)) > 18,]



###### 2. Substance
substance = covidy[,grep("src_|timepoint|su_(a|l|m)", colnames(covidy))]
substance$su_mj_edible_cv = NULL
substance$su_mj_oils_cv = NULL

substance = substance[rowSums(is.na(substance)) == 0,]

# convert scale to 0;1
substance$su_alcohol_binary = ifelse(substance$su_alcohol_cv > 1, 1, 0)
substance$su_liquids_binary = ifelse(substance$su_liquids_cv > 1, 1, 0)
substance$su_medication_binary = ifelse(substance$su_medication_cv > 1, 1, 0)
substance$su_meth_binary = ifelse(substance$su_meth_cv > 1, 1, 0)

# check if the variables can be combined
xcor <- polychoric(substance[ ,grep("binary|mj", colnames(substance), value = T) ])$rho
VSS.scree(xcor)
eigen(xcor)$values[1]/eigen(xcor)$values[2]

substance$su_total_cv = rowSums(substance[ ,grep("binary|mj", colnames(substance), value = T) ])

substance_wide = reshape(substance, direction = "wide", idvar = "src_subject_id", timevar = "timepoint", sep = "_")

# include kids with at least 3 time points
substance_wide = substance_wide[(rowSums(!is.na(substance_wide)) > 30),]



###### 3. Perceived Stress Scale
perceived_stress = covidy[,grep("src_|timepoint|pstr", colnames(covidy))]
perceived_stress$pstr_cv_raw_tot_bar = 10 - perceived_stress$pstr_confidence_p_cv - perceived_stress$pstr_way_p_cv + perceived_stress$pstr_overcome_p_cv + perceived_stress$pstr_unable_control_cv
perceived_stress$pstr_cv_raw_tot = NULL
perceived_stress = perceived_stress[rowSums(is.na(perceived_stress)) == 0,]

perceived_stress_wide = reshape(perceived_stress, direction = "wide", idvar = "src_subject_id", timevar = "timepoint", sep = "_")

# include kids with at least 3 time points
perceived_stress_wide = perceived_stress_wide[(rowSums(!is.na(perceived_stress_wide)) > 15),]


###### 4. Mental health
mental_health = covidy[,grep("src_|timepoint|mental", colnames(covidy))]
mental_health = mental_health[rowSums(is.na(mental_health))  == 0,]

mental_health_wide = reshape(mental_health, direction = "wide", idvar = "src_subject_id", timevar = "timepoint", sep = "_")

# include kids with at least 2 time points
mental_health_wide = mental_health_wide[(rowSums(!is.na(mental_health_wide)) > 2),]


###### merge
cv_age = covidy[,grep("src_|timepoint|_age", colnames(covidy))]
cv_age = reshape(cv_age, direction = "wide", idvar = "src_subject_id", timevar = "timepoint", sep = "_")



covid_outcomes_2 = merge(sadness_scale_wide, mental_health_wide, all = T)
covid_outcomes_2 = merge(covid_outcomes_2, cv_age, all.x = T)


covid_outcomes_3 = merge(substance_wide, perceived_stress_wide, all = T)
covid_outcomes_3 = merge(covid_outcomes_3, cv_age, all.x = T)




##################

temp1 = covidy[,c("src_subject_id", "timepoint", "witness_rac_disc_cv")]
temp2 = covidy[,c("src_subject_id", "timepoint", "exp_rac_disc_cv" )]

temp1 = reshape(temp1, direction = "wide", idvar = "src_subject_id", timevar = "timepoint", sep = "_")
temp2 = reshape(temp2, direction = "wide", idvar = "src_subject_id", timevar = "timepoint", sep = "_")

temp1 = temp1[rowSums(is.na(temp1)) < 4,]
temp2 = temp2[rowSums(is.na(temp2)) < 4,]

temp1$witness_rac_disc_mean = rowMeans(temp1[,grep("witness", colnames(temp1))], na.rm = T)
temp2$exp_rac_disc_mean = rowMeans(temp2[,grep("exp", colnames(temp2))], na.rm = T)


temp = merge(temp1[,c("src_subject_id", "witness_rac_disc_mean")], temp2[,c("src_subject_id", "exp_rac_disc_mean")], all =T)
##################


####################################
###### exposures
####################################

