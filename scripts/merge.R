library(readr)
library(dplyr)
library(janitor)
source("config.R")

demographics_baseline <- read_csv("outputs/demographics_baseline.csv")
demographics_long <- read_csv("outputs/demographics_long.csv")
medications <- read_csv("outputs/medications.csv", col_types = cols(.default = "d", src_subject_id = "c", eventname = "c",))
physicalhealth_sum <- read_csv("outputs/physicalhealth_sum.csv")
physicalhealth <- read_csv("outputs/physicalhealth.csv") # %>% dplyr::select(-sex)
family_id <- read_csv("outputs/family_id.csv") %>% select(src_subject_id, rel_family_id)
exposome_set <- read_csv("outputs/exposome_set.csv")
exposome_sum_set <- read_csv("outputs/exposome_sum_set.csv")
psychopathology_sum_scores <- read_csv("outputs/psychopathology_sum_scores.csv")
site <- read_csv("outputs/site.csv")
geo_data <- read_csv("outputs/geo_data.csv")
e_factor <- read_csv(file.path(e_factor_files_path, "ABCD_Exposome_bifactor_scores_16March2021.csv")) %>%
    mutate(src_subject_id = paste0("NDAR_", ID)) %>%
    select(-ID)
genetics <- read_csv(file.path(genetic_files_path, "genetic.csv")) %>% dplyr::select(src_subject_id, migraine_PRS, genetic_afr)

# combine demographics of all time points
demo_race = demographics_baseline[,grep("src|race|hisp", colnames(demographics_baseline))]

demographics_long <- merge(demographics_long, demo_race)
demographics_long <- demographics_long %>% filter(eventname != "baseline_year_1_arm_1")

demographics = bind_rows(demographics_baseline, demographics_long)

# define headaches medications
medications = medications[,grep("src|inter|event|Migraine|Daily.Preventive|Rescue.Medications", colnames(medications))]
medications$any_migraine_med_2w = Reduce("|",medications[,c("Migraine.Medications_2w", "Daily.Preventive.medications_2w", "Rescue.Medications_2w")])*1
medications$any_migraine_med_1yr = Reduce("|",medications[,c("Migraine.Medications_1yr", "Daily.Preventive.medications_1yr", "Rescue.Medications_1yr")])*1

# Merge data
dataset = merge(demographics, medications, all.x = T)
dataset = merge(dataset, physicalhealth_sum, all.x = T)
dataset = merge(dataset, physicalhealth, all.x = T)
dataset = merge(dataset, family_id, all.x = T)
dataset = merge(dataset, exposome_set, all.x = T)
dataset = merge(dataset, exposome_sum_set, all.x = T)
dataset = merge(dataset, psychopathology_sum_scores, all.x = T)
dataset = merge(dataset, e_factor, all.x = T)
dataset = merge(dataset, genetics, all.x = T)
dataset = merge(dataset, site, all.x = T)
dataset = merge(dataset, geo_data, all.x = T)

# Create new variables
dataset <- dataset %>%
    # Age in years
    mutate(age_years = interview_age/12) %>%

    # Create bad(yes, no) life events
    mutate(across(contains("fu_"), ~case_when(.x == 2 ~ 1, .x == 1 ~ 0, TRUE ~ NA_real_), .names = "{col}_bad")) %>%

    # Create migraine PRS among only European ancestry (for convenience)
    mutate(migraine_PRS_EUR = case_when(genetic_afr == 0 ~ migraine_PRS, TRUE ~ NA_real_))

# Create binary variables
## 1_Household income if<$25k; then code as 1 --- if>$25k; then code as 0
## 2_Number of nocked unconscious: if >0: then code as 1 --- if =0: then code as 0
## 3_Number of head injuries: if >0: then code as 1 --- if =0: then code as 0
## 4_ worst injury overall: if >1: then code as 1 --- if =1: then code as 0
## 5_ my neighborhood is safe from crime: if <=2: then code as 1 --- if >2: then code as 0
## 6_ discrimination measure: if >1: then code as 1 --- if =1: then code as 0
## 7_area deprivation: if <=10th percentile; then =1 --- if >10th percentile; then =0
# For sensitivity analysis 2 (P=0.5)
## 8-neighborhood_crime_y
## 9-neighborhood2r_p
dataset <- dataset %>%
    mutate(
        fam_under_poverty_line = case_when(household_income <= 4 ~ 1, household_income >= 6 ~ 0, TRUE ~ NA_real_), # group 5: 1/2 above, 1/2 below poverty line --> NA
        knocked_unconscious = case_when(medhx_ss_6j_times_p_l > 0 ~ 1, TRUE ~ as.numeric(medhx_ss_6j_times_p_l)),
        head_injuries = case_when(medhx_ss_6i_times_p_l > 0 ~ 1, TRUE ~ as.numeric(medhx_ss_6i_times_p_l)),
        tbi_worst_overall = case_when(tbi_ss_worst_overall_l == 1 ~ 0, tbi_ss_worst_overall_l > 1 ~ 1, TRUE ~ as.numeric(tbi_ss_worst_overall_l)),
        neighborh_notsafe = case_when(neighborhood3r_p <= 2 ~ 1, neighborhood3r_p > 2 ~ 0, TRUE ~ as.numeric(neighborhood3r_p)),
        neighborh_notsafe_y = case_when(neighborhood_crime_y <= 2 ~ 1, neighborhood_crime_y > 2 ~ 0, TRUE ~ as.numeric(neighborhood_crime_y)),
        neighborh_violence = case_when(neighborhood2r_p <= 2 ~ 1, neighborhood2r_p > 2 ~ 0, TRUE ~ as.numeric(neighborhood2r_p)),
        discrimination = case_when(dim_y_ss_mean == 1 ~ 0, dim_y_ss_mean > 1 ~ 1, TRUE ~ as.numeric(dim_y_ss_mean)),
        ADI_10perc =
            case_when(reshist_addr1_adi_perc <= (quantile(dataset$reshist_addr1_adi_perc, probs = 0.1, na.rm = T)) ~ 1,
                      reshist_addr1_adi_perc > (quantile(dataset$reshist_addr1_adi_perc, probs = 0.1, na.rm = T)) ~ 0,
                      TRUE ~ NA_real_) # protective # BOTTOM 10% ~ 1
    )

# Remove empty rows and columns
dataset <- dataset %>% remove_empty(c("rows","cols")) %>%
    # Remove rows with missing values of headache
    filter(!is.na(medhx_2q_l),

           # Only use data at baseline, 1-year and 2-year
           eventname != "3_year_follow_up_y_arm_1")

write.csv(file = "outputs/dataset_long.csv", x = dataset, row.names = F, na = "")





