
source("config.R")
source("utility_fun.R")

medsy01 = load_instrument("medsy01", abcd_files_path)

#remove odd names
ind = which(grepl("<span style=", medsy01$med1_rxnorm_p))
medsy01$med1_rxnorm_p[ind] = NA
ind = which(grepl("<span style=", medsy01$med_otc_1_rxnorm_p))
medsy01$med_otc_1_rxnorm_p[ind] = NA
medsy01 = droplevels(medsy01)

#remove empty col
medsy01 = medsy01[, colSums(is.na(medsy01)) != nrow(medsy01)]


#################### create the medication table according to the tagging  ####################
med_dataset = medsy01#[,grepl("(src|interview_a|gender|event|brou|_rxnorm_p)", colnames(medsy01))]

# Update 4.0 - medication IDs + medication names --> remove medication names
library(dplyr)
med_dataset <- med_dataset %>%
    # remove medication names
    mutate(across(contains("_rxnorm_p"), ~ vapply(strsplit(.," "), `[`, 1, FUN.VALUE=character(1))))


# tagged_med = read_csv(paste0(additional_files_path, "20210224_coded_medication.csv"))
tagged_med = read_csv("~/Documents/KateTran_Github/ABCD_headache/materials/Headachecoded_medication_Marissa_02122022.csv")

#split the med name to get the numbers
tagged_med$med_number = vapply(strsplit(tagged_med$med," "), `[`, 1, FUN.VALUE=character(1))


#add medication category to each child according to tagging
for(i in 2:19){

    #get med category
    colname = colnames(tagged_med)[i]

    #get the medications in the category
    meds = unique(tagged_med$med_number[which(tagged_med[,i] == 1)])

    #bug while reading the txt file: add ".0" to all meds numbers as it may appear in any of the two versions in the med_dataset
    meds = union(meds, paste0(meds,".0"))

    #mark the kids
    med_dataset[,colname] = 0
    med_dataset[apply(med_dataset[,grepl("_rxnorm_",colnames(med_dataset))], 1, function(r) any(r %in% meds)), colname] = 1

}

med_dataset[apply(med_dataset[,grepl("_rxnorm_",colnames(med_dataset))], 1, function(r) any(r %in% meds)), colname] = 1


#fix the tagging for parents that refused to answer (brought_medications == 2) or NA
med_dataset[(med_dataset$brought_medications %in% c(2,NA)),colnames(tagged_med)[2:19]] = NA


#number of asthma meds
asthma_meds = unique(tagged_med$med_number[which(tagged_med$Asthma_medications == 1)])

med_dataset$Asthma_number_meds = apply(med_dataset[,grepl("_rxnorm_",colnames(med_dataset))], 1, function(r) sum(unique(as.numeric(r)) %in% asthma_meds))
med_dataset[(med_dataset$brought_medications %in% c(2,NA)),"Asthma_number_meds"] = NA


#remove empty columns
# med_dataset = med_dataset[med_dataset$eventname == "baseline_year_1_arm_1",]
# med_dataset = med_dataset[,colSums(is.na(med_dataset)) != nrow(med_dataset)]


write.csv(file = paste0("outputs/medications.csv"),x = med_dataset ,row.names=F, na = "")


