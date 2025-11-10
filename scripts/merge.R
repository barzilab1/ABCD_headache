library(readr)
library(dplyr)
library(janitor)
library(lubridate)
library(stringr)
source("config.R")

demographics_all <- read_csv("data/demographics_all.csv") %>% mutate(interview_date = mdy(interview_date))
demo_race <- read_csv("data/demo_race.csv")
demographics <- merge(demographics_all, demo_race)

medications <- read_csv("data/medications.csv", col_types = cols(.default = "d", src_subject_id = "c", eventname = "c",))
physicalhealth_sum <- read_csv("data/physicalhealth_sum.csv") %>% mutate(interview_date = mdy(interview_date))
physicalhealth <- read_csv("data/physicalhealth.csv") %>% mutate(interview_date = mdy(interview_date))
family_id <- read_csv("data/family_id.csv") %>% select(src_subject_id, rel_family_id)
exposome_set <- read_csv("data/exposome_set.csv") %>% mutate(interview_date = mdy(interview_date))
exposome_sum_set <- read_csv("data/exposome_sum_set.csv") %>% mutate(interview_date = mdy(interview_date))
psychopathology_sum_scores <- read_csv("data/psychopathology_sum_scores.csv") %>% mutate(interview_date = mdy(interview_date))
site <- read_csv("data/site.csv") %>% mutate(interview_date = mdy(interview_date))
geo_data <- read_csv("data/geo_data.csv") %>% mutate(interview_date = mdy(interview_date))
e_factor <- read_csv(file.path(e_factor_files_path, "ABCD_Exposome_bifactor_scores_16March2021.csv")) %>%
    mutate(src_subject_id = paste0("NDAR_", ID)) %>%
    select(-ID)
genetics <- read_csv(file.path(abcd_genetics_path, "genetic.csv")) %>% dplyr::select(src_subject_id, migraine_PRS, genetic_afr)

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
    mutate(age_years = interview_age/12,
    # Create migraine PRS among only European ancestry (for convenience)
    migraine_PRS_EUR = case_when(genetic_afr == 0 ~ migraine_PRS, TRUE ~ NA_real_))

# Remove empty rows and columns
dataset <- dataset %>% remove_empty(c("rows","cols")) %>%
    # Remove rows with missing values of headache
    filter(!is.na(medhx_2q_l), eventname != "3_year_follow_up_y_arm_1")

write.csv(file = "data/dataset_long.csv", x = dataset, row.names = F, na = "")

# Extract list of 98 exposures
vars <- dataset %>%
    dplyr::select(matches(c("d_inc|famhx_ss_mom|dim_y|harm(2?)$|bully|hood(_?)[^_P]|nsc_|adi.*_[irmp]|pmq|fes_y.*_fc$|crpbi|srpf|macv_y.*fs|ple.*[^fu]_(y|fu_y_bad)$|hx.*[26][ij]|tbi.*_l$|tbi_[123457]|cat$|alcohol$|tobacco$"))) %>% names() # 98 vars

saveRDS(vars, "outputs/vars_98.rds")


# Create discovery and testing sets
matched_data <- read.table(file = file.path(abcd_partition_path, "participants.tsv"), header = TRUE)
matched_data <- matched_data %>%
    mutate(participant_id = str_replace_all(participant_id, "sub-", ""),
           participant_id = str_replace_all(participant_id, "NDAR", "NDAR_"))

# Get IDs of participants in group 1
id_gr1 <- matched_data %>% filter(matched_group == 1) %>% pull(participant_id)
id_gr2 <- matched_data %>% filter(matched_group == 2) %>% pull(participant_id)

# Create ABCD dataset 1 & 2
abcd_gr1 <- dataset %>% filter(src_subject_id %in% id_gr1)
abcd_gr2 <- dataset %>% filter(src_subject_id %in% id_gr2)

# Write data for sensitivity analysis
saveRDS(abcd_gr1, file = "outputs/abcd_gr1.rds")
saveRDS(abcd_gr2, file = "outputs/abcd_gr2.rds")



