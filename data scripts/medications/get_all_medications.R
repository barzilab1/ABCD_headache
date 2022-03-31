#### get all medications


source("config.R")
source("utility_fun.R")

medsy01 = load_instrument("medsy01",medication_files_path)


#remove odd names
ind = which(grepl("<span style=", medsy01$med1_rxnorm_p))
medsy01$med1_rxnorm_p[ind] = NA
ind = which(grepl("<span style=", medsy01$med_otc_1_rxnorm_p))
medsy01$med_otc_1_rxnorm_p[ind] = NA
medsy01 = droplevels(medsy01)


#split the data according to baseline and follow_up
medsy01_base = medsy01[which(grepl("baseline", medsy01$eventname)),]
medsy01_follow_up = medsy01[which(grepl("follow_up", medsy01$eventname)),]

medsy01_base = droplevels(medsy01_base)
medsy01_follow_up = droplevels(medsy01_follow_up)


############# get a list of all meds in the dataset, according to the time point for tagging ###########
meds_base = meds_follow_up = otc_base = otc_follow_up = list()

for(i in 1:15){
  colname = paste0("med",i,"_rxnorm_p")
  colname_otc = paste0("med_otc_",i,"_rxnorm_p")
  
  #in 2.01 -> the name of the med was included
  #in 3.01 -> only the med number includded
  meds_base[[i]] =  levels(medsy01_base[,colname])
  meds_follow_up[[i]] =  levels(medsy01_follow_up[,colname])
  otc_base[[i]] = levels(medsy01_base[,colname_otc])
  otc_follow_up[[i]] =  levels(medsy01_follow_up[,colname_otc])
  
}

meds_base = unique(unlist(meds_base))
meds_follow_up = unique(unlist(meds_follow_up))
otc_base = unique(unlist(otc_base))
otc_follow_up = unique(unlist(otc_follow_up))

#bug while reading the txt file: fix numbers that include ".0" at the end of the number
meds_base = unique(ifelse(is.na(as.numeric(meds_base)), meds_base, as.numeric(meds_base)))
meds_follow_up = unique(ifelse(is.na(as.numeric(meds_follow_up)), meds_follow_up, as.numeric(meds_follow_up)))
otc_base = unique(ifelse(is.na(as.numeric(otc_base)), otc_base, as.numeric(otc_base)))
otc_follow_up = unique(ifelse(is.na(as.numeric(otc_follow_up)), otc_follow_up, as.numeric(otc_follow_up)))

##meds
med_only_base = setdiff(meds_base, meds_follow_up)
med_only_follow_up = setdiff(meds_follow_up ,meds_base)
med_both = intersect(meds_base, meds_follow_up)

#otc
otc_only_base = setdiff(otc_base, otc_follow_up)
otc_only_follow_up = setdiff(otc_follow_up ,otc_base)
otc_both = intersect(otc_base, otc_follow_up)


#create the meds list for tagging 
all_meds = cbind(med_both, rep(1,length(med_both)), rep(1,length(med_both)), rep("med",length(med_both)))
all_meds = rbind(all_meds, cbind(med_only_base, rep(1,length(med_only_base)), rep(0,length(med_only_base)), rep("med",length(med_only_base))))
all_meds = rbind(all_meds, cbind(med_only_follow_up, rep(0,length(med_only_follow_up)), rep(1,length(med_only_follow_up)), rep("med",length(med_only_follow_up))))

all_meds = rbind(all_meds, cbind(otc_both, rep(1,length(otc_both)), rep(1,length(otc_both)), rep("otc",length(otc_both))))
all_meds = rbind(all_meds, cbind(otc_only_base, rep(1,length(otc_only_base)), rep(0,length(otc_only_base)), rep("otc",length(otc_only_base))))
all_meds = rbind(all_meds, cbind(otc_only_follow_up, rep(0,length(otc_only_follow_up)), rep(1,length(otc_only_follow_up)), rep("otc",length(otc_only_follow_up))))

colnames(all_meds) = c("med", "baseline", "follow_up","type")





tagged_med = read_csv(paste0(main_abcd_path, abcd_box_path, "medication/20210224_coded_medication.csv"))

#split the med name to get the numbers 
tagged_med$med_number = vapply(strsplit(tagged_med$med," "), `[`, 1, FUN.VALUE=character(1))

#check that all meds are tagged
all_meds = as.data.frame(all_meds)
all_meds$med = as.character(all_meds$med)
setdiff(all_meds$med[all_meds$follow_up == 1], tagged_med$med_number) 


# write.csv(file="outputs/medication_summary.csv",na.omit(all_meds),row.names=F)


