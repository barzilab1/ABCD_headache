library(dplyr)
library(readr)

## organize demographics ####
demographics_baseline <- read_csv("data/demographics_baseline.csv")
demographics_long <- read_csv("data/demographics_long.csv") %>% rename(demo_roster_v2 = "demo_roster_v2_l")

demo_race = demographics_baseline[,grep("src|sex|race|hisp|born_in_usa", colnames(demographics_baseline))] #|roster because also have longitudinal roster
demographics_baseline = demographics_baseline[, grep("race|hisp|born_in_usa", colnames(demographics_baseline), invert= T)]

demographics_long = demographics_long[demographics_long$eventname != "baseline_year_1_arm_1",]
demographics_long = bind_rows(demographics_baseline, demographics_long)

# Use this data for table 1 as in data_long (in merge.R), missing data of headache was removed and it causes a lot of missing data for household income in wide data
demo_income_age_2y = demographics_long %>% filter(eventname == "2_year_follow_up_y_arm_1") %>% select(src_subject_id, household_income, interview_age) %>%
    mutate(age_years = interview_age/12)


write.csv(file = "data/demographics_all.csv", x = demographics_long, row.names=F, na = "")
write.csv(file = "data/demo_race.csv", x = demo_race, row.names=F, na = "")
write.csv(file = "data/demo_income_age_2y.csv", x = demo_income_age_2y, row.names=F, na = "")






















