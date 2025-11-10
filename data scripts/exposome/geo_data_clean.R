
source("config.R")
source("utility_fun.R")


########### School Risk and Protective Factors ###########
rhds01 = load_instrument("abcd_rhds01", abcd_files_path)

rhds01 = rhds01[, grepl("^(src|interview|event|sex)|addr1_(valid|status|years|walk|grnd|p1tot|p1vl|drugtot|elevation|adi_(income|pov|wsum|perc|rent|mortg))", colnames(rhds01))]
## 7_area deprivation: if <=10th percentile; then =1 --- if >10th percentile; then =0
rhds01 <- rhds01 %>%
    mutate(ADI_10perc =
            case_when(reshist_addr1_adi_perc <= (quantile(rhds01$reshist_addr1_adi_perc, probs = 0.1, na.rm = T)) ~ 1,
                      reshist_addr1_adi_perc > (quantile(rhds01$reshist_addr1_adi_perc, probs = 0.1, na.rm = T)) ~ 0,
                      TRUE ~ NA_real_)) # protective # BOTTOM 10% ~ 1
write.csv(file = "data/geo_data.csv",x = rhds01, row.names = F, na = "")

