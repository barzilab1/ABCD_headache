#' ############################################################
#' Here should be all the merging of the different datasets. 
#' as well as creation of additional variables that are specific to this project
#' #############################################################

library(readr)




#' example of merging:
#' 1. first read the datasets from the outputs folder 
demographics_baseline <- read_csv("outputs/demographics_baseline.csv")
family_set_baseline <- read_csv("outputs/family_set_baseline.csv")

#' 2. merge the datasets (make sure you merge datasets from the same time points)
baseline_set = merge(demographics_baseline,family_set_baseline)

#' optional:
#' 3. create any new required variables
#' 4. get only the relevant time point
baseline_set = baseline_set[baseline_set$eventname == "baseline_year_1_arm_1",]

#' 5. export the dataset to csv 
write.csv(file = "outputs/baseline_set.csv",x = baseline_set, row.names = F, na = "")










