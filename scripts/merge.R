library(readr)
library(plyr)


demographics_baseline <- read_csv("outputs/demographics_baseline.csv")
demographics_long <- read_csv("outputs/demographics_long.csv")
medications <- read_csv("outputs/medications.csv",col_types = cols(.default = "d", src_subject_id = "c", eventname = "c",))
physicalhealth_sum <- read_csv("outputs/physicalhealth_sum.csv")
physicalhealth <- read_csv("outputs/physicalhealth.csv")


# combine demographics of all time points
demo_race = demographics_baseline[,grep("src|race|hisp", colnames(demographics_baseline))]

demographics_long = merge(demographics_long, demo_race)
demographics_long = demographics_long[demographics_long$eventname != "baseline_year_1_arm_1",] 

demographics = rbind.fill(demographics_baseline, demographics_long)


# define headaches medications
medications = medications[,grep("src|inter|event|Migraine|Daily.Preventive|Rescue.Medications", colnames(medications))]
medications$any_migraine_med_2w = Reduce("|",medications[,c("Migraine.Medications_2w", "Daily.Preventive.medications_2w", "Rescue.Medications_2w")])*1
medications$any_migraine_med_1yr = Reduce("|",medications[,c("Migraine.Medications_1yr", "Daily.Preventive.medications_1yr", "Rescue.Medications_1yr")])*1

describe(medications)


dataset = merge(demographics, medications)
dataset = merge(dataset, physicalhealth_sum)
dataset = merge(dataset, physicalhealth)



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





