library(readr)
library(dplyr)
library(lme4)
library(broom.mixed)
library(qgraph)
library(corrplot)
library(Hmisc)

load_instrument <- function(file_name, file_path) {

  instrument = read.csv(file = paste0(file_path,file_name,".txt"), sep = '\t',header = TRUE,
                        row.names=NULL, na.string = c("","NA"), check.names=FALSE)

  #remove details line
  instrument=instrument[-1,]

  #drop columns introduced by NDA, they are not required in the instruments.
  instrument = instrument[,!(names(instrument) %in% c(paste0(file_name,"_id"), "collection_id", "collection_title", "promoted_subjectkey","subjectkey" ,"study_cohort_name", "dataset_id"))]

  #if visit was used instead of eventname, rename
  if ("visit" %in% names(instrument) ){
    ind = which(names(instrument) == "visit")
    names(instrument)[ind] = "eventname"
    print("eventname replaced visit")
  }

  #remove empty columns (and print their names)
  instrument = instrument[,colSums(is.na(instrument)) != nrow(instrument)]

  instrument = droplevels(instrument)


  #convert to numeric
  for (i in 1:ncol(instrument)) {

    tryCatch({
      if(typeof(instrument[,i]) == "character"){
        instrument[,i] = as.numeric(instrument[,i])
      }else if (typeof(instrument[,i]) == "factor"){
        instrument[,i] = as.numeric(as.character(instrument[,i]))
      }
    }, error = function(e) {
      print(colnames(instrument)[i])
      print(e)
    }, warning = function(e){
      print(colnames(instrument)[i])
      print(e)
    })

  }


  return(instrument)
}


create_ever_var <- function(data, search_term, new_col_name) {
    data <- data %>%
        mutate(!!new_col_name := apply(data[, grepl(search_term, colnames(data))], 1, function(x) {any(x == 1)*1}))
    data <- data %>%
        mutate(!!new_col_name := ifelse((is.na(get(new_col_name)) &
                                              (apply(data[, which(grepl(search_term, colnames(data)))], 1, function(x) {any(x == 0)}))), 0, get(new_col_name)))
    return(data)
}

models <- function(outcome, predictor, variables, var_added) {
    if (is.null(var_added)) {
        model <- as.formula(paste0(outcome, " ~", paste0(c(
            predictor, variables
        ), collapse = " + "), sep = ""))
    } else {
        model <- as.formula(paste0(outcome, " ~", paste0(
            c(predictor, variables, var_added), collapse = " + "
        ), sep = ""))
    }
    return(model)
}


# get_est <- function(outcome = "medhx_2q_l", predictor, variables = var_headache_0, var_added = NULL, data, estimate, output) {
get_est <- function(outcome, predictor, variables, var_added = NULL, data, estimate, output) {

    estimate <- match.arg(estimate, c("coef", "OR"))


    model <- tryCatch(glmer(models(outcome = outcome, predictor = predictor, variables = variables, var_added = NULL),
                            family = binomial, data = data, nAGQ = 0), error = function(e) "Notrun")
    p_value <- c()
    coef <- c()
    std.error <- c()
    low_ci <- c()
    high_ci <- c()

    mixed_eff <- tryCatch(tidy(model, effects = "fixed", conf.int = T, conf.level = 0.90) %>% select(-effect), error = function(e) "NA")
    p_value <- tryCatch(mixed_eff[2,5], error = function(e) 9999999)
    coef <- tryCatch(mixed_eff[2,2], error = function(e) 9999999)
    std.error <- tryCatch(mixed_eff[2,3], error = function(e) 9999999)
    low_ci <- tryCatch(mixed_eff[2,6], error = function(e) 9999999)
    high_ci <- tryCatch(mixed_eff[2,7], error = function(e) 9999999)


    output[,1] <- tryCatch(predictor, error = function(e) "NA")
    output[,3] <- tryCatch(round(p_value, 5), error = function(e) 9999999)

    if(estimate == "OR") {
        output[,2] <- tryCatch(exp(coef), error = function(e) 9999999)
        output[,4] <- tryCatch(round(std.error, 5), error = function(e) 9999999)
        output[,5] <- tryCatch(round(exp(low_ci), 2), error = function(e) 9999999)
        output[,6] <- tryCatch(round(exp(high_ci), 2), error = function(e) 9999999)
    } else {
        output[,2] <- tryCatch(coef, error = function(e) 9999999)
        output[,4] <- tryCatch(round(std.error, 5), error = function(e) 9999999)
        output[,5] <- tryCatch(round(low_ci, 5), error = function(e) 9999999)
        output[,6] <- tryCatch(round(high_ci, 5), error = function(e) 9999999)
    }

    return(output)
}

cor_plot <- function(vars) {
    cor = cor_auto(vars)
    testRes = cor.mtest(vars, conf.level = 0.95)
    plot <- corrplot(cor, p.mat = testRes$p, method = 'color', diag = FALSE, type = 'upper', #col = col,
                     sig.level = c(0.001, 0.01, 0.05), pch.cex = 0.4,
                     insig = 'label_sig', pch.col = 'grey20', order = 'original', tl.col = "black", tl.srt = 45, tl.cex = 0.35, cl.cex = 0.4, cl.ratio = 0.4)
    return(plot)
}
