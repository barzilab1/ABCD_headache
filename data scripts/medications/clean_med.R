
source("config.R")
source("utility_fun.R")

medsy01 = load_instrument("medsy01",medication_files_path)

#remove odd names
ind = which(grepl("<span style=", medsy01$med1_rxnorm_p))
medsy01$med1_rxnorm_p[ind] = NA
ind = which(grepl("<span style=", medsy01$med_otc_1_rxnorm_p))
medsy01$med_otc_1_rxnorm_p[ind] = NA
medsy01 = droplevels(medsy01)

#remove empty col
medsy01 = medsy01[,!colSums(is.na(medsy01)) == nrow(medsy01)]


#################### create the medication table according to the tagging  ####################
med_dataset = medsy01[,grepl("(src|interview_a|gender|event|brou|_rxnorm_p)", colnames(medsy01))]
tagged_med = read_csv(paste0(main_abcd_path, abcd_box_path, "medication/20210224_coded_medication.csv"))

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



#fix the tagging for parents that refused to answer (brought_medications == 2) or NA
med_dataset[(med_dataset$brought_medications %in% c(2,NA)),colnames(tagged_med)[2:19]] = NA

write.csv(file = paste0("outputs/medications.csv"),x = med_dataset ,row.names=F, na = "")


