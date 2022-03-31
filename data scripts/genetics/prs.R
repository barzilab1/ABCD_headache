library(plyr)

source("config.R")


load_psr <- function(input_list) {
  
  tables = list()
  for(file_name in input_list){
    print(file_name)
    
    instrument = read.csv(file = file_name, sep = '\t',header = TRUE, 	row.names=NULL, na.string = c("","NA"), check.names=FALSE)
    
    #get the new name for Residual_PRS from the file name 
    new_col_name = regmatches(file_name, gregexpr("(?<=(AFR|EUR)_).*?(?=.txt)", file_name, perl=T))[[1]]
    top_10_col_name = paste0(new_col_name,"_top10")
    
    #create binary variable for top 10%
    prs_90_q = quantile(instrument$Residual_PRS, prob = seq(0, 1, length = 11), na.rm = T)["90%"]
    instrument[[top_10_col_name]] = ifelse(instrument$Residual_PRS >= prs_90_q, 1,0)
    
    #rename Residual_PRS col
    colnames(instrument)[which(colnames(instrument) == "Residual_PRS")] = new_col_name
    
    #fix sex range
    instrument$SEX = instrument$SEX - 1
    #fix ids
    instrument$src_subject_id = paste0("NDAR_" , instrument$IID)
    
    tables[[file_name]] = instrument[,c("src_subject_id", "FID", "SEX", new_col_name,top_10_col_name)]
  }
  
  for(i in 2:length(tables)){
    tables[[1]] = merge(tables[[1]], tables[[i]])
  }
  
  return(tables[[1]])
}


#collect all of the ABCD txt files
input_list_afr = Sys.glob(paths = c(paste(prs_files_path,"*ABCD_AFR*.txt",sep="")))
input_list_eur = Sys.glob(paths = c(paste(prs_files_path,"*ABCD_EUR*.txt",sep="")))

afr_data = load_psr(input_list_afr)
eur_data = load_psr(input_list_eur)

write.csv(file = "outputs/prs_afr.csv",x = afr_data,row.names=F, na = "")
write.csv(file = "outputs/prs_eur.csv",x = eur_data,row.names=F, na = "")



afr_data$genetic_afr = 1
eur_data$genetic_afr = 0

genetic = rbind.fill(eur_data,afr_data)

write.csv(file = "outputs/genetic.csv",x = genetic, row.names=F, na = "")

