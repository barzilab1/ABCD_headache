library(readr)
library(dplyr)
library(lme4)
library(broom.mixed)
# library(qgraph)
# library(corrplot)
# library(Hmisc)

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


check_test_type <- function(data, var_list, reference_var) {
    chisq_vars <- c()
    fisher_vars <- c()

    for (var in var_list) {
        # Create contingency table
        cont_table <- data %>% xtabs(~ get(reference_var) + get(var), data = .)

        # Print contingency table
        cat("Contingency table for", var, "vs", reference_var, ":\n")
        print(cont_table)

        # Check expected cell counts
        exp_counts <- as.vector(cont_table)

        # Determine appropriate test
        if (min(exp_counts) < 5) {
            # Use Fisher's exact test
            fisher_vars <- c(fisher_vars, var)
            cat("Using Fisher's exact test for", var, "\n\n")
        } else {
            # Use chi-square test
            chisq_vars <- c(chisq_vars, var)
            cat("Using Chi-square test for", var, "\n\n")
        }
    }

    results <- list(chi_square_vars = chisq_vars, fisher_exact_vars = fisher_vars)
    return(results)
}


get_formula <- function(outcome, predictor, random_eff, var_added = NULL) {
    if (is.null(var_added)) {
        model_formula <- reformulate(c(predictor, random_eff), response = outcome)
    } else {
        model_formula <- reformulate(c(predictor, var_added, random_eff), response = outcome)
    }
    return(model_formula)
}


random_effects <- "(1 | site_id_l_br/rel_family_id/src_subject_id)"
get_est <- function(outcome, predictor, data, random_eff = random_effects, var_added = NULL, binary_DV = T, conf_int = 0.9) {

    output <- data.frame("variable" = predictor, "coef" = NA, "OR" = NA, "p_value" = NA, "std_error" = NA, "t_value" = NA, "low_ci" = NA, "high_ci" = NA)

    # Binary outcome
    model <- tryCatch(glmer(get_formula(outcome = outcome, predictor = predictor, random_eff = random_eff, var_added = var_added),
                            data = data,
                            family = binomial, nAGQ = 0), error = \(x) return(NULL))


    if(!is_null(model)){

        mixed_eff <- tidy(model, effects = "fixed", conf.int = T, conf.level = conf_int)
        output[, "p_value"] <- parameters::p_value(model) %>% filter(Parameter == predictor) %>% pull(p) # as tidy function does not give p-values for the lmer model
        output[, "coef"] <- mixed_eff %>% filter(term == predictor) %>% pull(estimate)
        output[, "std_error"] <- mixed_eff %>% filter(term == predictor) %>% pull(std.error) %>% round(., 3)
        output[, "t_value"] <- mixed_eff %>% filter(term == predictor) %>% pull(statistic) %>% round(., 3)
        output[, "low_ci"] <- mixed_eff %>% filter(term == predictor) %>% pull(conf.low) %>% round(., 3)
        output[, "high_ci"] <- mixed_eff %>% filter(term == predictor) %>% pull(conf.high) %>% round(., 3)
        output[, "OR"] <- round(exp(mixed_eff %>% filter(term == predictor) %>% pull(estimate)), 3)

      }

    return(output)
}


exwas <- function(data_train, vars, outcome, data_test = data_test_scale, binary_DV = T, conf_int = 0.9, p_cutoff = 0.1) {
    # Run all univariate models
    univariate_models <- vars %>%
        map_dfr(~get_est(outcome = outcome, predictor = .x,
                         var_added = NULL, #no covariates for headache project
                         data = data_train,  binary_DV = binary_DV, conf_int = conf_int)) %>%
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


# Calculate RERI for complex mixed models (at least 2 random effect terms)
# Adapted from epiR package
get_reri <- function (model, coef, param = "product", conf.level = 0.95) {
    N. <- 1 - ((1 - conf.level)/2)
    z <- qnorm(N., mean = 0, sd = 1)

    theta1 <- as.numeric(summary(model)$coefficients[coef[1]])
    theta2 <- as.numeric(summary(model)$coefficients[coef[2]])
    theta3 <- as.numeric(summary(model)$coefficients[coef[3]])
    theta1.se <- as.numeric(summary(model)$coefficients[coef[1], 2])
    theta2.se <- as.numeric(summary(model)$coefficients[coef[2], 2])
    theta3.se <- as.numeric(summary(model)$coefficients[coef[3], 2])

    cov.mat <- vcov(model)
    h1 <- exp(theta1 + theta2 + theta3) - exp(theta1)
    h2 <- exp(theta1 + theta2 + theta3) - exp(theta2)
    h3 <- exp(theta1 + theta2 + theta3)
    reri.var <- (h1^2 * theta1.se^2) + (h2^2 * theta2.se^2) +
        (h3^2 * theta3.se^2) + (2 * h1 * h2 * cov.mat[coef[1],
                                                      coef[2]]) + (2 * h1 * h3 * cov.mat[coef[1], coef[3]]) +
        (2 * h2 * h3 * cov.mat[coef[2], coef[3]])
    reri.se <- sqrt(reri.var)
    reri.p <- exp(theta1 + theta2 + theta3) - exp(theta1) -
        exp(theta2) + 1
    reri.l <- reri.p - (z * reri.se)
    reri.u <- reri.p + (z * reri.se)
    reri <- data.frame(est = reri.p, lower = reri.l, upper = reri.u)
    mult.p <- as.numeric(exp(theta3))

    # Get 95% CI
    mult.ci <- broom.mixed::tidy(model, conf.int = TRUE, conf.level = 0.95)[coef[3],8:9]
    # Extract high and low CI
    mult.l <- as.numeric(exp(mult.ci[1]))
    mult.u <- as.numeric(exp(mult.ci[2]))

    multiplicative <- data.frame(est = mult.p, lower = mult.l,
                                 upper = mult.u)
    cov.mat <- vcov(model)
    h1 <- ((exp(theta1 + theta2 + theta3) - exp(theta1))/(exp(theta1 +
                                                                  theta2 + theta3))) - ((exp(theta1 + theta2 + theta3) -
                                                                                             exp(theta1) - exp(theta2) + 1)/(exp(theta1 + theta2 +
                                                                                                                                     theta3)))
    h2 <- ((exp(theta1 + theta2 + theta3) - exp(theta2))/(exp(theta1 +
                                                                  theta2 + theta3))) - ((exp(theta1 + theta2 + theta3) -
                                                                                             exp(theta1) - exp(theta2) + 1)/(exp(theta1 + theta2 +
                                                                                                                                     theta3)))
    h3 <- 1 - ((exp(theta1 + theta2 + theta3) - exp(theta1) -
                    exp(theta2) + 1)/exp(theta1 + theta2 + theta3))
    apab.var <- (h1^2 * theta1.se^2) + (h2^2 * theta2.se^2) +
        (h3^2 * theta3.se^2) + (2 * h1 * h2 * cov.mat[coef[1],
                                                      coef[2]]) + (2 * h1 * h3 * cov.mat[coef[1], coef[3]]) +
        (2 * h2 * h3 * cov.mat[coef[2], coef[3]])
    apab.se <- sqrt(apab.var)
    apab.p <- (exp(theta1 + theta2 + theta3) - exp(theta1) -
                   exp(theta2) + 1)/exp(theta1 + theta2 + theta3)
    apab.l <- apab.p - (z * apab.se)
    apab.u <- apab.p + (z * apab.se)
    apab <- data.frame(est = apab.p, lower = apab.l, upper = apab.u)
    s.p <- (exp(theta1 + theta2 + theta3) - 1)/(exp(theta1) +
                                                    exp(theta2) - 2)
    cov.mat <- vcov(model)
    if (class(model)[1] == "glmerMod" & s.p < 0) {
        warning(paste("Point estimate of synergy index (S) is less than zero (",
                      round(s.p, digits = 2), ").\n  Confidence intervals cannot be calculated using the delta method. Consider re-parameterising as linear odds model.",
                      sep = ""))
    }
    h1 <- ((exp(theta1 + theta2 + theta3))/(exp(theta1 +
                                                    theta2 + theta3) - 1)) - (exp(theta1)/(exp(theta1) +
                                                                                               exp(theta2) - 2))
    h2 <- ((exp(theta1 + theta2 + theta3))/(exp(theta1 +
                                                    theta2 + theta3) - 1)) - (exp(theta2)/(exp(theta1) +
                                                                                               exp(theta2) - 2))
    h3 <- exp(theta1 + theta2 + theta3)/(exp(theta1 + theta2 +
                                                 theta3) - 1)
    lns.var <- h1^2 * theta1.se^2 + h2^2 * theta2.se^2 +
        h3^2 * theta3.se^2 + (2 * h1 * h2 * cov.mat[coef[2],
                                                    coef[1]]) + (2 * h1 * h3 * cov.mat[coef[3], coef[1]]) +
        (2 * h2 * h3 * cov.mat[coef[3], coef[2]])
    lns.se <- sqrt(lns.var)
    lns.p <- log(s.p)
    lns.l <- lns.p - (z * lns.se)
    lns.u <- lns.p + (z * lns.se)
    s.l <- exp(lns.l)
    s.u <- exp(lns.u)
    s <- data.frame(est = s.p, lower = s.l, upper = s.u)
    rval <- list(reri = reri, apab = apab, s = s, multiplicative = multiplicative)

    return(rval)
}


# Extract the results for cross validation
get_cv_results <- function(cv_mod) {

    # Number of clusters
    n_clusters <- cv_mod$`n clusters`

    # Coefficients
    coefs <- as.data.frame(cv_mod$details$coefficients) %>%
        bind_cols(as.data.frame(cv_mod$coefficients) %>% setNames("average_10folds"))

    # Get results
    name_cri <- toupper(cv_mod$criterion)
    criterion_by_fold <- as.data.frame(cv_mod$details$criterion) %>% setNames(name_cri)
    criterion_average <- as.data.frame(cv_mod$`CV crit`[1]) %>% setNames(paste("Cross-Validation ", name_cri))
        # bind_cols(as.data.frame(cv_mod$`adj CV crit`[1]) %>% setNames(paste("Bias-Adjusted Cross-Validation ", name_cri))) %>%
        # bind_cols(as_tibble(cv_mod$`confint`[1]) %>% setNames(paste("Lower 95% CI Bias-Adjusted Cross-Validation ", name_cri))) %>%
        # bind_cols(as_tibble(cv_mod$`confint`[2]) %>% setNames(paste("Upper 95% CI Bias-Adjusted Cross-Validation ", name_cri)))

    return(list(n_clusters = n_clusters, coefficients = coefs, criterion_by_fold = criterion_by_fold, criterion_average = criterion_average))

}



# Combine cv results for all models
get_n_clusters <- function(list_mod) {
    n_clusters <- map(list_mod, ~ get_cv_results(.x)$n_clusters)

    n_clusters_df <- tibble::tibble(
        Model = paste("Model", seq_along(n_clusters), sep = " "),
        n_clusters = unlist(n_clusters)
    )

    return(n_clusters_df)
}

# Extract, transform to ORs, and combine coefficients into a single data frame
get_cv_ORs <- function(list_mod) {
    results_df <- map_df(seq_along(list_mod), ~ {
        model <- list_mod[[.x]]
        ORs <- get_cv_results(model)$coefficients %>% exp() %>% round(., 2) %>% as.data.frame() %>%
            tibble::rownames_to_column(var = "Coefficient") %>%
            mutate(Model = paste("Model", .x, sep = "_"))

        return(ORs)
    })

    return(results_df)
}

# Extract and combine criterion_by_fold and criterion_average from each model
get_criteria <- function(list_mod) {
    # criterion_by_fold
    criterion_by_fold_df <- map2(list_mod, seq_along(list_mod), ~ {
        criterion_by_fold <- get_cv_results(.x)$criterion_by_fold %>% round(., 3)
        setNames(criterion_by_fold, paste("Model", .y))
    })
    criterion_by_fold_ext <- bind_cols(criterion_by_fold_df)

    # criterion_average
    criterion_average_ext <- map2_df(list_mod, seq_along(list_mod), ~ {
        criterion_average <- get_cv_results(.x)$criterion_average %>% round(., 3)
        criterion_averages_df <- bind_cols(criterion_average, data.frame(Model = paste("Model", .y)))
    })

    return(list(criterion_by_fold = criterion_by_fold_ext, criterion_averages = criterion_average_ext))
}

