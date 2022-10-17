
source("config.R")
source("utility_fun.R")


########### School Risk and Protective Factors ###########
rhds01 = load_instrument("abcd_rhds01", abcd_files_path)

rhds01 = rhds01[, grepl("^(src|interview|event|sex)|addr1_(valid|status|years|walk|grnd|p1tot|p1vl|drugtot|elevation|adi_(income|pov|wsum|perc|rent|mortg))", colnames(rhds01))]

write.csv(file = "outputs/geo_data.csv",x = rhds01, row.names = F, na = "")

