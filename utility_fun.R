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



get_formula <- function(outcome, predictor, random_eff, var_added = NULL) {
    if (is.null(var_added)) {
        model_formula <- as.formula(paste0(outcome, " ~", paste0(c(
            predictor, random_eff
        ), collapse = " + "), sep = ""))
    } else {
        model_formula <- as.formula(paste0(outcome, " ~", paste0(
            c(predictor, random_eff, var_added), collapse = " + "
        ), sep = ""))
    }
    return(model_formula)
}

get_est <- function(outcome, predictor, random_eff = "(1 | site_id_l_br/rel_family_id/src_subject_id)", var_added = NULL, data, binary_DV = T, conf_int = 0.9) { #output,

    # Turn predictor to numeric (in case predictor is binary) - otherwise models will show results predictor == 1
    if(is.factor(predictor)) data <- data %>% mutate(predictor = as.numeric(as.character(predictor)))

    p_value <- c()
    coef <- c()
    OR <- c()
    std_error <- c()
    t_value <- c()
    low_ci <- c()
    high_ci <- c()

    if(!binary_DV) {
        # Continuous outcome
        model <- tryCatch(lmer(get_formula(outcome = outcome, predictor = predictor, random_eff = random_eff, var_added = var_added),
                               data = data,
                               control=lmerControl(check.nobs.vs.nlev = "ignore",
                                                   check.nobs.vs.rankZ = "ignore",
                                                   check.nobs.vs.nRE = "ignore",
                                                   optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))), error = function(e) "Notrun")

        output <- data.frame("variable" = NA, "coef" = NA, "p_value" = NA, "std_error" = NA, "t_value" = NA,"low_ci" = NA, "high_ci" = NA)
    }

    if(binary_DV) {
        # Binary outcome
        model <- tryCatch(glmer(get_formula(outcome = outcome, predictor = predictor, random_eff = random_eff, var_added = var_added),
                                family = binomial, data = data, nAGQ = 0), error = function(e) "Notrun")

        output <- data.frame("variable" = NA, "coef" = NA, "OR" = NA, "p_value" = NA, "std_error" = NA, "t_value" = NA, "low_ci" = NA, "high_ci" = NA)
    }

    mixed_eff <- tryCatch(tidy(model, effects = "fixed", conf.int = T, conf.level = conf_int), error = function(e) "NA")
    p_value <- tryCatch(parameters::p_value(model) %>% filter(Parameter == predictor) %>% pull(p), error = function(e) 9999999) # as tidy function does not give p-values for the lmer model
    coef <- tryCatch(mixed_eff %>% filter(term == predictor) %>% pull(estimate), error = function(e) 9999999)
    std_error <- tryCatch(mixed_eff %>% filter(term == predictor) %>% pull(std.error), error = function(e) 9999999)
    t_value <- tryCatch(mixed_eff %>% filter(term == predictor) %>% pull(statistic), error = function(e) 9999999)
    low_ci <- tryCatch(mixed_eff %>% filter(term == predictor) %>% pull(conf.low), error = function(e) 9999999)
    high_ci <- tryCatch(mixed_eff %>% filter(term == predictor) %>% pull(conf.high), error = function(e) 9999999)

    output[, "variable"] <- tryCatch(predictor, error = function(e) "NA")
    output[, "coef"] <- tryCatch(coef, error = function(e) 9999999)
    output[, "p_value"] <- tryCatch(p_value, error = function(e) 9999999)
    output[, "std_error"] <- tryCatch(round(std_error, 3), error = function(e) 9999999)
    output[, "t_value"] <- tryCatch(round(t_value, 3), error = function(e) 9999999)
    output[, "low_ci"] <- tryCatch(round(low_ci, 3), error = function(e) 9999999)
    output[, "high_ci"] <- tryCatch(round(high_ci, 3), error = function(e) 9999999)

    if (binary_DV) {
        output[, "OR"] <- tryCatch(round(exp(coef), 3), error = function(e) 9999999)
    }

    return(output)
}


exwas <- function(data_train, vars, outcome, data_test = data_test_scale, binary_DV = T, conf_int = 0.9, p_cutoff = 0.1) {
    # Run all univariate models
    univariate_models <- vars %>%
        map_dfr(~get_est(outcome = outcome, predictor = .x,
                         var_added = NULL, #c("interview_age", "sex_br", "race_black", "race_white", "ethnicity_hisp"), no covariates for headache project
                         data = data_train,  binary_DV = binary_DV, conf_int = conf_int)) %>%
        # mutate(p_fdr = p.adjust(p_value, method = "fdr")) %>%
        mutate(significant_p0.1 = case_when(p_value <= p_cutoff ~ 1, TRUE ~ NA_real_)) # for Headache project, p-value cut-off is 0.1, and use p-values, not adjusted

    # Extract significant variables
    univariate_models_sig <- univariate_models %>% filter(significant_p0.1 == 1)

    # Get weights in order
    weight <- univariate_models_sig %>% arrange(coef) %>% select(coef) %>% pull()

    # Calculate weighted exposome score
    data_test <- transform(data_test, weighted_exposome =
                               rowSums(sweep(data_test %>%
                                                 select(all_of(univariate_models_sig %>%
                                                                   arrange(coef) %>%
                                                                   select(variable) %>%
                                                                   pull())), 2, weight, `*`), na.rm = T))

    return(list(univariate_models_dat = univariate_models, univariate_models_sig = univariate_models_sig, data_test_exposome = data_test))
}
