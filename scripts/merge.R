library(readr)
library(plyr)
library(dplyr)
source("config.R")

demographics_baseline <- read_csv("outputs/demographics_baseline.csv")
demographics_long <- read_csv("outputs/demographics_long.csv")
medications <- read_csv("outputs/medications.csv", col_types = cols(.default = "d", src_subject_id = "c", eventname = "c",))
physicalhealth_sum <- read_csv("outputs/physicalhealth_sum.csv")
physicalhealth <- read_csv("outputs/physicalhealth.csv") %>% dplyr::select(-sex)
# hormone_saliva <- read_csv("outputs/hormone_saliva.csv")
family_id <- read_csv("outputs/family_id.csv") %>% select(src_subject_id, rel_family_id)
exposome_set <- read_csv("outputs/exposome_set.csv")
exposome_sum_set <- read_csv("outputs/exposome_sum_set.csv")
# ABCD_BMI <- read_csv("outputs/ABCD_BMI.csv")
# psychopathology <- read_csv("outputs/psychopathology.csv")
psychopathology_sum_scores <- read_csv("outputs/psychopathology_sum_scores.csv")
site <- read_csv("outputs/site.csv")
geo_data <- read_csv("outputs/geo_data.csv")
e_factor <- read_csv(paste0(e_factor_files_path, "ABCD_Exposome_bifactor_scores_16March2021.csv"))
e_factor$src_subject_id = paste0("NDAR_", e_factor$ID)
e_factor$ID <- NULL
genetics <- read_csv(paste0(genetic_files_path, "genetic.csv")) %>% dplyr::select(src_subject_id, migraine_PRS, genetic_afr)
# ksad_y_diagnosis <- read_csv("outputs/ksad_y_diagnosis.csv")

# suicide_set <- read_csv("outputs/suicide_set.csv")

# combine demographics of all time points
demo_race = demographics_baseline[,grep("src|race|hisp", colnames(demographics_baseline))]

demographics_long = merge(demographics_long, demo_race)
demographics_long = demographics_long[demographics_long$eventname != "baseline_year_1_arm_1",]

demographics = rbind.fill(demographics_baseline, demographics_long)


# define headaches medications
medications = medications[,grep("src|inter|event|Migraine|Daily.Preventive|Rescue.Medications", colnames(medications))]
medications$any_migraine_med_2w = Reduce("|",medications[,c("Migraine.Medications_2w", "Daily.Preventive.medications_2w", "Rescue.Medications_2w")])*1
medications$any_migraine_med_1yr = Reduce("|",medications[,c("Migraine.Medications_1yr", "Daily.Preventive.medications_1yr", "Rescue.Medications_1yr")])*1



dataset = merge(demographics, medications, all.x = T)
dataset = merge(dataset, physicalhealth_sum, all.x = T)
dataset = merge(dataset, physicalhealth, all.x = T)
# dataset = merge(dataset, hormone_saliva, all.x = T)
dataset = merge(dataset, family_id, all.x = T)
dataset = merge(dataset, exposome_set, all.x = T)
dataset = merge(dataset, exposome_sum_set, all.x = T)
# dataset = merge(dataset, ABCD_BMI, all.x = T)
# dataset = merge(dataset, psychopathology, all.x = T)
dataset = merge(dataset, psychopathology_sum_scores, all.x = T)
# dataset = merge(dataset, ksad_y_diagnosis, all.x = T)
dataset = merge(dataset, e_factor, all.x = T)
dataset = merge(dataset, genetics, all.x = T)
dataset = merge(dataset, site, all.x = T)
dataset = merge(dataset, geo_data, all.x = T)
# dataset = merge(dataset, suicide_set)
dataset = janitor::remove_empty(dataset, "cols")
# Create new variables
dataset <- dataset %>%
    mutate(
        # headache_severity_cat = case_when(
        #     medhx_2q_l == 0 & any_migraine_med_2w == 0 ~ 0,
        #     medhx_2q_l == 1 & any_migraine_med_2w == 0 ~ 1,
        #     medhx_2q_l == 1 & Rescue.Medications_2w == 1 ~ 2,
        #     medhx_2q_l == 1 & Migraine.Medications_2w == 1 ~ 3,
        #     medhx_2q_l == 1 & Daily.Preventive.medications_2w == 1 ~ 4,
        #     TRUE ~ NA_real_
        # ),
        age_years = interview_age/12
    ) %>%
    janitor::remove_empty("cols")

# Only use data at baseline, 1-year and 2-year
dataset <- dataset %>%
    filter(eventname != "3_year_follow_up_y_arm_1")

dataset <- dataset %>%
    mutate(across(contains("fu_"), ~case_when(.x == 2 ~ 1, .x == 1 ~ 0, TRUE ~ NA_real_), .names = "{col}_bad")) %>%
    mutate(migraine_PRS_EUR = case_when(genetic_afr == 0 ~ migraine_PRS, TRUE ~ NA_real_)) %>%
    filter(!is.na(medhx_2q_l))


# Dichotomize those significant variables to calculate exposome scores
# Create binary variables
## 1_Household income if<$25k; then code as 1 --- if>$25k; then code as 0
## 2_Number of nocked unconscious: if >0: then code as 1 --- if =0: then code as 0
## 3_Number of head injuries: if >0: then code as 1 --- if =0: then code as 0
## 4_ worst injury overall: if >1: then code as 1 --- if =1: then code as 0
## 5_ my neighborhood is safe from crime: if <=2: then code as 1 --- if >2: then code as 0
## 6_ discrimination measure: if >1: then code as 1 --- if =1: then code as 0
## 7_total life events: if >=90th percentile; then =1 --- if <90th percentile; then =0
## 8_area deprivation: if <=10th percentile; then =1 --- if >10th percentile; then =0

dataset <- dataset %>%
    mutate(fam_under_poverty_line = case_when(household_income <= 4 ~ 1, household_income >= 6 ~ 0, TRUE ~ NA_real_), # group 5: 1/2 above, 1/2 below poverty line --> NA
           knocked_unconscious = case_when(medhx_ss_6j_times_p_l > 0 ~ 1, TRUE ~ as.numeric(medhx_ss_6j_times_p_l)),
           head_injuries = case_when(medhx_ss_6i_times_p_l > 0 ~ 1, TRUE ~ as.numeric(medhx_ss_6i_times_p_l)),
           tbi_worst_overall = case_when(tbi_ss_worst_overall_l == 1 ~ 0, tbi_ss_worst_overall_l > 1 ~ 1, TRUE ~ as.numeric(tbi_ss_worst_overall_l)),
           neighborh_notsafe = case_when(neighborhood3r_p <= 2 ~ 1, neighborhood3r_p > 2 ~ 0, TRUE ~ as.numeric(neighborhood3r_p)),
           discrimination = case_when(dim_y_ss_mean == 1 ~ 0, dim_y_ss_mean > 1 ~ 1, TRUE ~ as.numeric(dim_y_ss_mean)),
           # ple_total_bad_90perc =
           #     case_when(ple_y_ss_total_bad >= (quantile(dataset$ple_y_ss_total_bad, probs = 0.9, na.rm = T)) ~ 1,
           #               ple_y_ss_total_bad < (quantile(dataset$ple_y_ss_total_bad, probs = 0.9, na.rm = T)) ~ 0,
           #               TRUE ~ NA_real_), # risk # TOP 10% ~ 1
           ADI_10perc =
               case_when(reshist_addr1_adi_perc <= (quantile(dataset$reshist_addr1_adi_perc, probs = 0.1, na.rm = T)) ~ 1,
                         reshist_addr1_adi_perc > (quantile(dataset$reshist_addr1_adi_perc, probs = 0.1, na.rm = T)) ~ 0,
                         TRUE ~ NA_real_) # protective # BOTTOM 10% ~ 1
    )


write.csv(file = "outputs/dataset_long.csv", x = dataset, row.names = F, na = "")



########## DESCRIPTIVE ##########

#################### table 1
library(tableone)
binary_vars = c("sex_br","separated_or_divorced", "parents_married", grep("race|hisp|(2w|1yr)$|medhx_[^s]|head", colnames(dataset), value = T))
all_vars = c("age", "demo_fam_poverty" ,binary_vars, grep("medhx_ss|total$", colnames(dataset), value = T))


tab <- CreateTableOne(vars = all_vars, data = dataset, factorVars = binary_vars , strata = "eventname", addOverall = T)
table1 <- print(tab, quote = FALSE, noSpaces = TRUE, printToggle = FALSE, missing = T)
write.csv(table1, file = "results/Table1.csv")




#################### check overlaps
setDT(dataset)
dataset[eventname == "baseline_year_1_arm_1", table(Migraine.Medications_2w, medhx_2q)]
dataset[eventname == "baseline_year_1_arm_1", table(Daily.Preventive.medications_2w, medhx_2q)]
dataset[eventname == "baseline_year_1_arm_1", table(Rescue.Medications_2w, medhx_2q)]
dataset[eventname == "baseline_year_1_arm_1", table(any_migraine_med_2w , medhx_2q)]


dataset[eventname == "1_year_follow_up_y_arm_1", table(Migraine.Medications_2w, medhx_2q_l)]
dataset[eventname == "1_year_follow_up_y_arm_1", table(Daily.Preventive.medications_2w, medhx_2q_l)]
dataset[eventname == "1_year_follow_up_y_arm_1", table(Rescue.Medications_2w, medhx_2q_l)]
dataset[eventname == "1_year_follow_up_y_arm_1", table(any_migraine_med_2w , medhx_2q_l)]

dataset[eventname == "2_year_follow_up_y_arm_1", table(Migraine.Medications_2w, medhx_2q_l)]
dataset[eventname == "2_year_follow_up_y_arm_1", table(Daily.Preventive.medications_2w, medhx_2q_l)]
dataset[eventname == "2_year_follow_up_y_arm_1", table(Rescue.Medications_2w, medhx_2q_l)]
dataset[eventname == "2_year_follow_up_y_arm_1", table(any_migraine_med_2w , medhx_2q_l)]

dataset[eventname == "2_year_follow_up_y_arm_1", table(Migraine.Medications_2w, head_cranium_pain)]
dataset[eventname == "2_year_follow_up_y_arm_1", table(Daily.Preventive.medications_2w, head_cranium_pain)]
dataset[eventname == "2_year_follow_up_y_arm_1", table(Rescue.Medications_2w, head_cranium_pain)]
dataset[eventname == "2_year_follow_up_y_arm_1", table(any_migraine_med_2w , head_cranium_pain)]



#################### ever (baseline -> 2 year)

dataset_ever = dataset[eventname != "3_year_follow_up_y_arm_1", ]
dataset_ever[, time := sub("_year.*", "", eventname)]
dataset_ever[, eventname := NULL]

meds_1_year = grep("_1yr", colnames(dataset_ever), value = T)
dataset_ever[, (meds_1_year) := NULL ]
dataset_ever_wide = dcast(dataset_ever,
                          src_subject_id + race_white + race_black ~ time,
                          value.var = c(grep("medhx|2w", colnames(dataset), value = T), "separated_or_divorced", "parents_married", "age", "demo_fam_poverty"))


dataset_ever_wide[, Migraine.Medications_2w_ever := (Migraine.Medications_2w_baseline | Migraine.Medications_2w_1 | Migraine.Medications_2w_2)*1]
dataset_ever_wide[, Daily.Preventive.medications_2w_ever := (Daily.Preventive.medications_2w_baseline | Daily.Preventive.medications_2w_1 | Daily.Preventive.medications_2w_2)*1]
dataset_ever_wide[, Rescue.Medications_2w_ever := (Rescue.Medications_2w_baseline | Rescue.Medications_2w_1 | Rescue.Medications_2w_2)*1]
dataset_ever_wide[, any_migraine_med_2w_ever := (any_migraine_med_2w_baseline | any_migraine_med_2w_1 | any_migraine_med_2w_2)*1]
dataset_ever_wide[, medhx_2q_ever := (medhx_2q_baseline | medhx_2q_l_1 | medhx_2q_l_2)*1 ]



vars = grep("ever", colnames(dataset_ever_wide), value = T)
tab2 = CreateTableOne(vars = vars, data = dataset_ever_wide, factorVars = vars )
table2 = print(tab2, quote = FALSE, noSpaces = TRUE, printToggle = FALSE, missing = T)
write.csv(table2, file = "results/Table2.csv")





