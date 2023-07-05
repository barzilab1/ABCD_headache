
source("config.R")
source("utility_fun.R")

########### Sum Scores Culture & Environment Youth ###########
sscey01 = load_instrument("abcd_sscey01", abcd_files_path)
## 6_ discrimination measure: if >1: then code as 1 --- if =1: then code as 0
sscey01 <- sscey01 %>%
    mutate(discrimination = case_when(dim_y_ss_mean == 1 ~ 0, dim_y_ss_mean > 1 ~ 1, TRUE ~ as.numeric(dim_y_ss_mean)))

#remove nt (Number Total Questions) and nm (Number Missing Answers) and na (Number Answered)
sscey01 = sscey01[,!grepl("_(nm|nt|na|answered)$", colnames(sscey01))]

########### Sum Scores Culture & Environment Parent ###########
sscep = load_instrument("abcd_sscep01", abcd_files_path)

#remove nt (Number Total Questions) and nm (Number Missing Answers) and na (Number Answered)
sscep = sscep[, !grepl("_(nm|nt|na|answered)$", colnames(sscep))]



########### merge all tables
exposome_sum_set = merge(sscey01, sscep)

write.csv(file = "data/exposome_sum_set.csv",x = exposome_sum_set, row.names = F, na = "")




