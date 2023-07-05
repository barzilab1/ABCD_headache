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

# SES was estimated using the income-to-needs ratio (INR).
# The INR was calculated by dividing reported household income by the federal poverty threshold for a given household size.
# A lower INR ratio indicated higher SES.
# Income was reported in bins and was adjusted to the median for each income bin.
# We used the 2017 federal poverty level for the corresponding household sizes
# The U.S. Census Bureau defines “deep poverty” as living in a household with a total cash income below 50 percent of its poverty threshold.


# demographics_long <- demographics_long %>%
#     mutate(median_income = case_when(
#         household_income == 1 ~ median(seq(1, 4999, 1)),
#         household_income == 2 ~ median(seq(5000, 11999, 1)),
#         household_income == 3 ~ median(seq(12000, 15999, 1)),
#         household_income == 4 ~ median(seq(16000, 24999, 1)),
#         household_income == 5 ~ median(seq(25000, 34999, 1)),
#         household_income == 6 ~ median(seq(35000, 49999, 1)),
#         household_income == 7 ~ median(seq(50000, 74999, 1)),
#         household_income == 8 ~ median(seq(75000, 99999, 1)),
#         household_income == 9 ~ median(seq(100000, 199999, 1)),
#         household_income == 10 ~ median(seq(200000, 299999, 1))
#         # 200000 is higher than the threshold of a household of 20 people (maximum number of household members in ABCD study is 19) (41320 + 12*4180 = 91480)
#         # assume the max of group 10 is 299999 so that median is 249999.5 (higher than 200k
#     ))
#
# demographics_long <- demographics_long %>%
#     # add poverty threshold according to the number of hh members
#     # https://aspe.hhs.gov/topics/poverty-economic-mobility/poverty-guidelines/prior-hhs-poverty-guidelines-federal-register-references/2017-poverty-guidelines
#     mutate(pov_threshold = case_when(
#         demo_roster_v2 == 1 ~ 12060,
#         demo_roster_v2 == 2 ~ 16240,
#         demo_roster_v2 == 3 ~ 20420,
#         demo_roster_v2 == 4 ~ 24600,
#         demo_roster_v2 == 5 ~ 28780,
#         demo_roster_v2 == 6 ~ 32960,
#         demo_roster_v2 == 7 ~ 37140,
#         demo_roster_v2 == 8 ~ 41320,
#         demo_roster_v2 == 9 ~ (41320 + 4180),
#         demo_roster_v2 == 10 ~ (41320 + 2*4180),
#         demo_roster_v2 == 11 ~ (41320 + 3*4180),
#         demo_roster_v2 == 12 ~ (41320 + 4*4180),
#         demo_roster_v2 == 13 ~ (41320 + 5*4180),
#         demo_roster_v2 == 14 ~ (41320 + 6*4180),
#         demo_roster_v2 == 15 ~ (41320 + 7*4180),
#         demo_roster_v2 == 16 ~ (41320 + 8*4180),
#         demo_roster_v2 == 17 ~ (41320 + 9*4180),
#         demo_roster_v2 == 18 ~ (41320 + 10*4180),
#         demo_roster_v2 == 19 ~ (41320 + 11*4180),
#         demo_roster_v2 == 20 ~ (41320 + 12*4180),
#         TRUE ~ NA_real_
#     ))
#
# demographics_long <- demographics_long %>%
#     mutate(income_to_needs_ratio = median_income/pov_threshold,
#
#            fam_under_poverty_line = case_when(
#                median_income < pov_threshold ~ 1,
#                median_income >= pov_threshold ~ 0,
#                TRUE ~ NA_real_
#            ))



write.csv(file = "data/demographics_all.csv", x = demographics_long, row.names=F, na = "")
write.csv(file = "data/demo_race.csv", x = demo_race, row.names=F, na = "")
write.csv(file = "data/demo_income_age_2y.csv", x = demo_income_age_2y, row.names=F, na = "")






















