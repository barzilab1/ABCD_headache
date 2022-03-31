install.packages(c("httr", "jsonlite"))


medication_coding <- read_csv(paste0(main_abcd_path, abcd_box_path, "medication/20210224_coded_medication.csv"))



library(httr)
library(jsonlite)

#' 1. get a list of all medications numbers
#' 2. send a get request 
res <- GET("https://rxnav.nlm.nih.gov/REST/rxclass/class/byRxcui.json?rxcui=1429984&relaSource=ATC")
#' wait?
#' 3. get response:
data = fromJSON(rawToChar(res$content))
#' 4. get relevant data
View(data$rxclassDrugInfoList$rxclassDrugInfo$rxclassMinConceptItem)
#' 5. for each class, get hierarchy
res2 <- GET("https://rxnav.nlm.nih.gov/REST/rxclass/classContext.json?classId=A01AC&relaSource=ATC") #get the hierarchy
data2 = fromJSON(rawToChar(res2$content))
View(data2$classPathList$classPath$rxclassMinConcept[[1]])



