
source("config.R")
source("utility_fun.R")

########### Hormone Saliva Salimetric Scores ###########

hsss01 = load_instrument("abcd_hsss01",abcd_files_path)
hsss01 = hsss01[,grepl("src|age|event|sex|mean",colnames(hsss01))]

summary(hsss01)
write.csv(file = "outputs/hormone_saliva.csv", x = hsss01, row.names = F, na = "")
