
source("config.R")
source("utility_fun.R")

########### Sum Scores Culture & Environment Youth ###########
sscey01 = load_instrument("abcd_sscey01",abcd_files_path)

# sscey01 = sscey01[, grepl("^(src|interview|event|sex|pmq|fes|crpbi|srpf|dim)", colnames(sscey01))]

#remove nt (Number Total Questions) and nm (Number Missing Answers) and na (Number Answered)
sscey01 = sscey01[,!grepl("_(nm|nt|na|answered)$",colnames(sscey01))] #pr

# sscey01$school_protective_factors = as.numeric(as.character(sscey01$srpf_y_ss_ses)) + as.numeric(as.character(sscey01$srpf_y_ss_iiss))

summary(droplevels(sscey01))


########### Sum Scores Culture & Environment Parent ###########
# sscep = load_instrument("abcd_sscep01",abcd_files_path)

#remove nt (Number Total Questions) and nm (Number Missing Answers) and na (Number Answered)
# sscep = sscep[,!grepl("_(nm|nt|na|answered)$",colnames(sscep))]


########### Sum Scores Mobil Tech Youth ###########
ssmty = load_instrument("abcd_ssmty01",abcd_files_path)

ssmty = ssmty[, grepl("(src|interview|event|sex)|_(weekend|weekday)$", colnames(ssmty))]

summary(ssmty)



########### Longitudinal Summary Scores Sports Activity ###########
lsssa = load_instrument("abcd_lsssa01", abcd_files_path)
lsssa[lsssa == 999] = NA

#remove empty columns
lsssa = lsssa[,colSums(is.na(lsssa)) != dim(lsssa)[1]]
summary(droplevels(lsssa))




########### merge all tables
exposome_sum_set = merge(sscey01, ssmty)
exposome_sum_set = merge(exposome_sum_set, lsssa, all.x = T)

write.csv(file = "outputs/exposome_sum_set.csv",x = exposome_sum_set, row.names = F, na = "")




