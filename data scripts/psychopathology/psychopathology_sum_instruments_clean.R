library(data.table)
source("config.R")
source("utility_fun.R")


################### Sum Scores Mental Health Youth ###################
mhy = load_instrument("abcd_mhy02", abcd_files_path)

# mhy = mhy[,grepl("^(src|interview|event|sex|ple_y|pps_)",colnames(mhy))]

#remove nt (Number Total Questions) and nm (Number Missing Answers)
mhy = mhy[, grepl("src|interview|event|sex|total_bad$|^peq",colnames(mhy))]
mhy = mhy[,!grepl("_(nm|nt)$",colnames(mhy))]


setDT(mhy)
mhy[,bully_vic:= peq_ss_relational_victim +peq_ss_reputation_victim +peq_ss_overt_victim]
mhy[,bully_aggs:= peq_ss_relational_aggs+peq_ss_reputation_aggs+peq_ss_overt_aggression]

mhy[,bully_vic_90_q:= {
    prs_90_q = quantile(bully_vic, prob = seq(0, 1, length = 11), na.rm = T)["90%"]
    fcase(
        bully_vic > prs_90_q, 1,
        bully_vic <= prs_90_q, 0,
        default = NA)
}]

mhy[,bully_aggs_90_q:= {
    prs_90_q = quantile(bully_aggs, prob = seq(0, 1, length = 11), na.rm = T)["90%"]
    fcase(
        bully_aggs > prs_90_q, 1,
        bully_aggs <= prs_90_q, 0,
        default = NA)
}]

mhy[,summary(.SD), .SDcols =c("bully_vic", "bully_aggs", "bully_vic_90_q", "bully_aggs_90_q")]

################### Sum Scores Mental Health Parent ###################
mhp02 = load_instrument("abcd_mhp02", abcd_files_path)
mhp02 = mhp02[,grepl("^(src|interview|event|sex)|(score)$",colnames(mhp02))]


psychopathology_sum_scores = merge(mhy,mhp02)


write.csv(file = "data/psychopathology_sum_scores.csv", x = psychopathology_sum_scores, row.names = F, na = "")

