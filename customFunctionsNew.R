# Source this file when you need to use any of its functions
# ------------------------------------------------------------------------------
# Author: K.Taskova
# Description: custom functions that can be used across projects
# Created: June 2018 for US projects
# Major update: June 2019
# Minor update: July 2019
# ------------------------------------------------------------------------------


suppressPackageStartupMessages(library(doParallel))
suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(pROC))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(glmnet))
suppressPackageStartupMessages(library(ranger))
suppressPackageStartupMessages(library(xgboost))
suppressPackageStartupMessages(library(naniar))
suppressPackageStartupMessages(library(MLmetrics))
suppressPackageStartupMessages(library(stargazer))
suppressPackageStartupMessages(library(rlang))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(tictoc))

# ------------------------------------------------------------------------------
# Create folder if does not exist
# ------------------------------------------------------------------------------
createFolder <- function(path_folder){
  if (!dir.exists(path_folder)) dir.create(path_folder)
}

# nonEmptyList
# returns logical
nonEmptyList <- function(arg){
  is.list(arg) & length(arg)
}

# getThisFunctionName
# returns character, the name of the parent function
getThisFunctionName <- function(){
  deparse(as.list(sys.call(sys.nframe() - 1))[[1]])
}

#printDistribution
# percentage of 1s for a binary variable 
printDistribution <- function(vector, text = NULL, percent = FALSE){
  if (!is.null(text)) print(text)
  if (percent) {
    print(prop.table(table(vector, useNA = "ifany")))
  } else {
    print(addmargins(table(vector)))
  }
  
}

# ------------------------------------------------------------------------------
# getDataFromCSV
# Load data, prints summary of missing values
# Returns tibble
#------------------------------------------------------------------------------
getDataFromCSV <- function(path_data, col_types = NULL, verbose = TRUE,
                           comment_starts_with = "#") {
  
  this_func <- getThisFunctionName()
  
  if (tools::file_ext(path_data) != "csv")
    message(this_func, "\n - File extension is not csv")
  
  # read data
  data <- readr::read_delim(path_data, delim = ",", col_types = col_types,
                            comment = comment_starts_with)
  
  # report problems and exit with NULL
  if (nrow(problems(data)) > 0) {
    message(this_func, "\n - Check file, some problems while reading it:\n")
    message(problems(data))
    return(NULL)
  }
  
  if (verbose) {
    message(sprintf("%s \n - Read rows = %d cols = %d",
                    this_func, nrow(data), ncol(data)))
    if (any(is.na(data))) {
      message("Detected missing values,",
              "ordered by the most missing-s in each variable.")
      print(naniar::miss_var_summary(data)[1:min(20, ncol(data)), ])
    }
  }
  data
}

# ------------------------------------------------------------------------------
# readExcelFormatFile
# Load data from excel file, prints summary of missing values
# Returns list of tibbles, one for each sheet.
# The name of the list element is the name of the sheet
#------------------------------------------------------------------------------
readExcelFormatFile <- function(excel_file_path, skip_first_rows = 0,
                                verbose = TRUE){
  
  this_func <- getThisFunctionName()
  
  sheets <- readxl::excel_sheets(excel_file_path)
  names(sheets) <- sheets
  out <- lapply(sheets,
                function(x) {
                  tmp <- readxl::read_excel(excel_file_path,
                                            sheet = x, skip = skip_first_rows)
                  if (verbose) {
                    message(sprintf("%s\n - Read sheet %s with rows = %d cols = %d",
                                    this_func, x, nrow(data), ncol(data)))
                    if (any(is.na(tmp))) {
                      message(this_func, sprintf("- Detected missing values in sheet %s,", x),
                              "ordered by the most missing-s in each variable.")
                      print(naniar::miss_var_summary(tmp)[1:min(20, ncol(tmp)),])
                    }
                  }
                  tmp
                  
                })
  out
}

#-------------------------------------------------------------------------------
# subsetDatabyDate
# Performs filtering based on the given input values of from & to
# (that should be given in year-month-date format )
#-------------------------------------------------------------------------------
subsetDataByDate <- function(data,
                             time_var = "FECHA_NAC",
                             from = "2007-01-01",
                             to = "2010-12-31",
                             verbose = TRUE) {
  this_func <- getThisFunctionName()
  if (!tibble::is_tibble(data)) data <- as_tibble(data)
  if (!time_var %in% colnames(data)) {
    warning(sprintf("No column named %s in 'data'.", time_var))
    return(data)
  }
  if (!is.Date(pull(data, time_var)))
    stop(sprintf("Column %s is not formatted as Date: Cannot perform date filtering",
                 time_var))
  
  data <- data %>%
    dplyr::filter(!!rlang::sym(time_var) >= ymd(from)
                  & !!rlang::sym(time_var) <= ymd(to)
    ) 
  # Log
  if (verbose) {
    if (nrow(data) == 0) message(this_func,
                                 " - Subseting resulted with empty dataset")
    else {
      message(sprintf("%s \n - Subset with rows = %d, columns = %d",
                      this_func, nrow(data), ncol(data)))
      print(table(lubridate::year(pull(data, time_var))))
    }
  }
  data
}

#-------------------------------------------------------------------------------
# makeDataFeatureSummary
# data: tibble
# sel_features:  a character with specif pattern that will be passed to grep
#                or NULL in which case it will
# write_dir:     absolute path to the folder where we want to save the output
# file_basename: a character with the name of the output file (without a
#                file extension and parent directory)
#
# verbose:       flag for controlling prints # not used at the moment
# return tibble
#-------------------------------------------------------------------------------
getDataFeatureSummary <- function(data, sel_feature_pattern = "^PRI_|^PRO_",
                                  write_dir = NULL,
                                  file_basename = NULL,
                                  verbose = TRUE){
  # Select features
  sel_col <- colnames(data)
  if (!is.null(sel_feature_pattern))
    sel_col <- grep(sel_feature_pattern, sel_col, value = TRUE)
  data <- data %>% dplyr::select(sel_col)
  
  # Summarize
  data_sum <- data %>%
    dplyr::summarise_if(is.numeric,
                        funs("_N" = sum(!is.na(.)),
                             "_Min" = min(., na.rm = TRUE),
                             "_Percentile 25" = quantile(., 0.25, na.rm = TRUE,
                                                         names = FALSE),
                             "_Median" = median(., na.rm = TRUE),
                             "_Percentile 75" = quantile(., 0.75, na.rm = TRUE,
                                                         names = FALSE),
                             "_Max" = max(., na.rm = TRUE),
                             "_Mean" = mean(.,na.rm = TRUE),
                             "_St. Dev." = sd(., na.rm = TRUE),
                             "_Missing count" = sum(is.na(.)),
                             "_Zero count" = sum(. == 0, na.rm = TRUE),
                             "_One count" = sum(. == 1, na.rm = TRUE),
                             "_Missing count %" = 100*sum(is.na(.))/n(),
                             "_Zero count %" = 100*sum(. == 0, na.rm = TRUE)/n(),
                             "_One count %" = 100*sum(. == 1, na.rm = TRUE)/n()
                        )) %>%
    round(digits = 5)
  
  # Reshape the wide data output from the previous command with tidyr functions
  data_sum_tidy <- data_sum %>% tidyr::gather(stat, val) %>%
    tidyr::separate(stat, into = c("Feature", "Statistic"), sep = "__") %>%
    tidyr::spread(Statistic, val) %>%
    # reorder columns
    dplyr::select(Feature, N, Min, "Percentile 25", Median,
                  "Percentile 75", Max, Mean, "St. Dev.",
                  #"Missing portion %", "Min portion %", "Max portion %"
                  "Missing count", "Zero count", "One count",
                  "Missing count %", "Zero count %", "One count %")
  
  
  data_sum_tidy <- data_sum_tidy %>% dplyr::arrange(Feature)
  # Write files
  if (!is.null(write_dir)) {
    # Create the folder if does not exist
    createFolder(write_dir)
    # Create file name
    file_name <- ifelse(is.null(file_basename), "data_summary", file_basename)
    # write csv
    file_name_csv <- paste0(file_name, ".csv")
    readr::write_csv(data_sum_tidy,
                     path = file.path(write_dir, file_name_csv))
    # write txt (with formated table for printing)
    file_name_txt <- paste0(file_name, ".txt")
    stargazer::stargazer(as.data.frame(data), type = "text",
                         summary.stat = c("n", "min", "p25", "median",
                                          "p75", "max", "mean", "sd"),
                         out = file.path(write_dir, file_name_txt)
    )
  }
  
  return(data_sum_tidy)
}


# ------------------------------------------------------------------------------
# Setup project code structure
# The main project folder path should be given
# Creates several path names and returns them in a list
# ------------------------------------------------------------------------------
setupFolderStructure <- function(path_projectdir){
  path_list = list()
  path_list[["log"]] <- file.path(path_projectdir, "log")
  path_list[["processed_data"]] <- file.path(path_projectdir, "processed_data")
  path_list[["raw_data"]] <- file.path(path_projectdir, "raw_data")
  path_list[["r"]] <- file.path(path_projectdir, "R")
  
  sapply(path_list, createFolder)
  
  path_list
}

# ------------------------------------------------------------------------------
# MODELING RELATED FUNCTIONS
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Extend the RF (from ranger) integrated in caret to allow
# number of trees to be be a tunable parameter.
# Follows the caret framework for the model object structure and function
# Check the ranger package for more information on RF method and its parameters
# ------------------------------------------------------------------------------

defineExtendedRF <- function(num.threads = NULL, importance_type = "impurity"){
  
  rf_ext <- getModelInfo("ranger", regex = FALSE)[[1]]
  
  rf_ext$parameters <- data.frame(parameter = c("mtry", "splitrule",
                                                "min.node.size", "num.trees"),
                                  class = c("numeric", "character",
                                            "numeric", "numeric"),
                                  label = c("#Randomly Selected Predictors",
                                            "Splitting Rule",
                                            "Minimal Node Size",
                                            "Number of trees"))
  
  rf_ext$grid <- function(x, y, len = NULL, search = "grid") {
    if (search == "grid") {
      srule <- if (is.factor(y))
        "gini"
      else
        "variance"
      out <- expand.grid(mtry = caret::var_seq(p = ncol(x),
                                               classification = is.factor(y),
                                               len = len),
                         splitrule = c(srule, "extratrees"),
                         min.node.size = ifelse(is.factor(y), 1, 5),
                         num.trees = 500)
    } else {
      srules <- if (is.factor(y))
        c("gini", "extratrees")
      else
        c("variance", "extratrees", "maxstat")
      out <-
        data.frame(
          mtry = sample(1:ncol(x), size = len, replace = TRUE),
          splitrule = sample(srules, size = len, replace = TRUE),
          min.node.size = sample(1:(min(20,nrow(x))), size = len, replace = TRUE),
          num.trees = sample(seq(500, 10000, by = 500), size = len, replace = TRUE)
        )
    }
    out
  }
  
  rf_ext$fit <- function(x, y, wts, param, lev, last, classProbs, ...) {
    if ((!is.data.frame(x)) || dplyr::is.tbl(x)) x <- as.data.frame(x)
    x$.outcome <- y
    
    if (!is.null(wts)) {
      out <- ranger::ranger(dependent.variable.name = ".outcome",
                            data = x,
                            num.trees = param$num.trees,
                            mtry = min(param$mtry, ncol(x)),
                            min.node.size = param$min.node.size,
                            splitrule = as.character(param$splitrule),
                            write.forest = TRUE,
                            probability = classProbs,
                            case.weights = wts, # pass the weights
                            num.threads = num.threads,
                            importance = importance_type,
                            ...)
    } else {
      out <- ranger::ranger(dependent.variable.name = ".outcome",
                            data = x,
                            num.trees = param$num.trees,
                            mtry = min(param$mtry, ncol(x)),
                            min.node.size = param$min.node.size,
                            splitrule = as.character(param$splitrule),
                            write.forest = TRUE,
                            probability = classProbs,
                            num.threads = num.threads,
                            importance = importance_type,
                            ...)
    }
    
    ## in case the resampling method is "oob"
    if (!last) out$y <- y
    out
  }
  
  return(rf_ext)
}

# ------------------------------------------------------------------------------
# Modify the XGboost model for binary classification (already integrated in caret)
# to correctly set the class weights, via the argument scale_pos_weight
# Check the xgboost package for more information on the method and its parameters
# ------------------------------------------------------------------------------
defineBinaryClassXgbTree <- function(scale_pos_weight=1){
  
  ext <- getModelInfo("xgbTree", regex = FALSE)[[1]]
  ext$fit = function(x, y, wts, param, lev, last, classProbs, ...) {
    if (!inherits(x, "xgb.DMatrix"))
      x <- as.matrix(x)
    
    if (is.factor(y) && (length(lev) == 2)) {
      
      y <- ifelse(y == lev[1], 1, 0)
      
      if (!inherits(x, "xgb.DMatrix"))
        x <- xgboost::xgb.DMatrix(x, label = y, missing = NA) else
          xgboost::setinfo(x, "label", y)
      
      if (!is.null(wts))
        xgboost::setinfo(x, 'weight', wts)
      
      out <- xgboost::xgb.train(list(eta = param$eta,
                                     max_depth = param$max_depth,
                                     gamma = param$gamma,
                                     colsample_bytree =
                                       param$colsample_bytree,
                                     min_child_weight =
                                       param$min_child_weight,
                                     subsample = param$subsample,
                                     scale_pos_weight =
                                       scale_pos_weight, # pass weights
                                     max_delta_step =
                                       ifelse(scale_pos_weight == 1, 1, 0)
      ),
      data = x,
      nrounds = param$nrounds,
      watchlist = list(eval = x, train = x),
      objective = "binary:logistic",
      ...)
    } else{
      stop("Expected binary class. Exit defineBinaryClassXgbTree()")
    }
    out
  }
  
  return(ext)
}

# ------------------------------------------------------------------------------
# Customized version of Linear SVM model that takes into account class weights
# Check the e1071 package for more information on the SVM method and its parameters
# ------------------------------------------------------------------------------
defineLinearSVM <- function(){
  modelInfo <-
    list(label = "Linear Support Vector Machines with Class Weights",
         library = "e1071",
         type = c("Classification"),
         parameters = data.frame(parameter = c('cost'),
                                 class = c("numeric"),
                                 label = c("Cost")),
         
         grid = function(x, y, len = NULL, search = "grid") {
           if (search == "grid") {
             out <- expand.grid(cost = 2^((1:len) - 3))
           } else {
             out <- data.frame(cost = 2^runif(len, min = -5, max = 10))
           }
           
           out
           
         },
         
         loop = NULL,
         
         fit = function(x, y, wts, param, lev, last, classProbs, ...) {
           
           if (!is.null(wts)) {
             
             wts <- wts[levels(y)]
             if (length(levels(y)) != length(wts) ||
                length(wts) != 2 )
               stop("Currently implemented for 2-class problems, should be named two element vector")
             
             if (length(setdiff(names(wts), levels(y))) > 0)
               stop("Currently implemented for 2-class problems, different names from target levels")
           }
           
           out <- e1071::svm(x = as.matrix(x), y = y,
                             kernel = "linear",
                             cost = param$cost,
                             probability = classProbs,
                             class.weights = wts,
                             ...)
           
           out
           
         },
         
         predict = function(modelFit, newdata, submodels = NULL) {
           
           predict(modelFit, newdata)
           
         },
         
         prob = function(modelFit, newdata, submodels = NULL) {
           
           out <- predict(modelFit, newdata, probability = TRUE) 
           attr(out, "probabilities")
           
         },
         
         predictors = function(x, ...){
           
           out <- if (!is.null(x$terms)) predictors.terms(x$terms) else x$xNames
           if (is.null(out)) out <- names(attr(x, "scaling")$x.scale$`scaled:center`)
           if (is.null(out)) out <- NA
           
           out
           
         },
         
         tags = c("Kernel Method", "Support Vector Machines","Linear Classifier",
                  "Robust Methods", "Cost Sensitive Learning", "Two Class Only"),
         
         levels = function(x) x$levels,
         
         sort = function(x) {
           
           # If the cost is high, the decision boundary will work hard to
           # adapt.
           x[order(x$cost),]
           
         })
  
  return(modelInfo)
}

# ------------------------------------------------------------------------------
# Custom function to extract the area under the ROC curve (AUC)
# from a given caret model object. Also prints other evaluation metrics
# across different thresholds for the predicted outcome.
# Check the PROC package for more information on AUC/other metrics
# implementation
#     model - a model object 
#     data  - the dataset used to build the given model
#     predictor_names - a vector with names of the columns that code for the PRIs 
#                     used to build the model 
#     outcome_name - the name of the column that codes the outcome 
# ------------------------------------------------------------------------------

get_roc <- function(model, data, predictor_names, outcome_name) {
  # Predicted outcome for the given input data
  predictions <- predict(object = model, data[, predictor_names],
                         type = "prob")
  # Observed outcome
  obs <- data[, outcome_name]
  
  # AUC calculation
  roc_obj <- pROC::roc(obs, predictions[,"yes"])
  
  print(pROC::auc(roc_obj))
  print(paste("Area under the Precison-Recall curve (PRAUC):",
              MLmetrics::PRAUC(y_pred = predictions[,"yes"], y_true = obs)))
  print("Performance Metrics for different probability thresholds")
  coord <- pROC::coords(roc_obj, x = seq(0, 1, 0.1),
                        ret = c("threshold", "specificity", "sensitivity",
                                "accuracy", "tn", "tp", "fn", "fp", "npv", "ppv"))
  print(round(coord, digits = 2))
  
}

# ------------------------------------------------------------------------------
# Function to train and validate models (including parameter tuning)
# Depends on caret's train function for model training.
#     train_data - the train datset 
#     test_data  - the test dataset
#     predictor_names - a vector with names of columns that code for the PRIs 
#                     used to build the model 
#     outcome_name - name of the column that codes the outcome 
#     params - a list of  parameters that control the model training process
#     alg_parms - a list of tunable paramters specifc for the selected algorithm
#     alg - name of the selected algorithm
# ------------------------------------------------------------------------------

doModelTraining <- function(train_data, test_data, predictor_names,
                            outcome_name, params, alg_params, alg
) {
  
  # FOR debuging purposes:REMOVE
  # train_data.y <- ifelse(train_data[, outcome_name] == 1, "yes","no")
  # cv_model_w <- cv.glmnet(as.matrix(train_data[, predictor_names]), 
  #                       as.matrix(train_data.y), weights =  weights_vector,
  #                       family = "binomial", alpha = 1, type.measure = "auc")
  
  # Re-code the outcome
  train_data.y <- ifelse(train_data[, outcome_name] == 1,"yes","no")
  
  # Reformat the outcome as factor
  # Note: for glmnet-Lasso this should not be done as it expects 
  # the second level to be the postive class; while caret takes the first level as the positive class
  # So when looking at the (caret object) resampled/tunning results from cross-validation 
  # for glmnet-Lasso the Sensitivity and Specifity are reversed. 
  # We select parmaters by AUC so there is no difference;
  # But if we want to select by other factor might matter 
  if (alg != "Lasso")
    train_data.y <- factor(train_data.y, levels = c("yes", "no"))
  
  
  # Calculate weights from the train data
  # Format/define the weights w.r.t. to the selected algorithm
  tab_counts <- table(train_data.y)
  weights_vector <- NULL
  if (params[["weight"]]) {
    if (grepl("SVM", alg)) {
      weights_vector <- as.vector(tab_counts/sum(tab_counts))
      names(weights_vector) <- names(tab_counts)
    } else {
      ## in glmnet weights should sum up to 1
      weights_vector <- ifelse(train_data.y == "yes",
                               (1/tab_counts)["yes"] * 0.5,
                               (1/tab_counts)["no"] * 0.5)
    }
    # In the case of XGBoost
    scale_pos_weight <- as.numeric(tab_counts["no"]/tab_counts["yes"])
    
  }
  
  # Set the random seed, in the case of params[["folds"]] is NULL
  set.seed(params[["seed"]])
  
  # Set control parameters for caret's train function via
  # caret's trainControl function
  fitControl <- trainControl(method = params[["method"]],
                             number = params[["num_folds"]],
                             repeats = params[["num_repeats"]],
                             index = params[["folds"]],
                             # Estimate class probabilities
                             classProbs = TRUE,
                             verboseIter = TRUE,
                             returnResamp = "all",
                             # Evaluate performance using
                             # the following function
                             summaryFunction =  params[["summaryFunction"]],
                             selectionFunction = params[["selectionFunction"]],
                             allowParallel = TRUE)
  
  
  # run training in parallel
  cl <- makeCluster(WORKERS, outfile =  file.path(OTHER_PATHS[["log"]], "log_cluster.txt"))
  on.exit(stopCluster(cl))
  registerDoParallel(cl)
  
  # train a model
  train_arguments <- 
    list(x = train_data[, predictor_names],
         y = train_data.y,
         method = alg_params[[alg]][["method"]],
         preProcess = alg_params[[alg]][["preProcess"]],
         #weights = weights_vector,
         metric = params[["metric"]],
         trControl = fitControl,
         tuneGrid = alg_params[[alg]][["paramGrid"]])
  
  if (alg == "XGB") {
    # Note1: we use the parameter scale_pos_weight or max_dealta_step to
    # address the class imbalance, and do not pass any values to
    # the weights argument
    # Note2: set the threads to 1, according to the author of caret, 
    # when using allowParallel = TRUE  - still XGB is quite slow when used from caret
    train_arguments$nthread = 1
  }
  else if (alg == "LR") {
    train_arguments$family = binomial()
    train_arguments$weights = weights_vector
  } else{
    train_arguments$weights = weights_vector
  }
  
  fitModel <- do.call("train", args = train_arguments)
  
  # Print area under the ROC curve and other metrics
  for (type_data in c("Train", "Test")) {
    print(paste(type_data,"performance AUC-ROC"))
    if (type_data == "Test")
      get_roc(fitModel, data = test_data, predictor_names, outcome_name)
    else
      get_roc(fitModel, data = train_data, predictor_names, outcome_name)
  }
  
  print("Final model trained with parameters (best found)")
  print(fitModel$bestTune)
  
  return(fitModel)
}

# ------------------------------------------------------------------------------
# Function that filters dataset row-wise and column-wise
# Filtering rows is only possible by using binary (dummy) type columns
#     data - a dataset that needs to be filtered by rows or columns 
#     filter_by_dummy - the name of a dummy column used for filtering, 
#                       default NULL whish means no filtering by rows
#     keep_features - a vector of names of columns that we want to keep, 
#                     default NULL which means keep all columns
#     as.dataframe - a flag that if TRUE returns the dataset as a data frame 
# ------------------------------------------------------------------------------
selectData <- function(data,
                       filter_by_dummy = NULL,
                       keep_features = NULL,
                       as.dataframe = TRUE) {
  data_out <- data
  
  if (is.null(keep_features))
    keep_features <- colnames(data)
  else
    keep_features <- intersect(keep_features, colnames(data))
  
  if (!is.null(filter_by_dummy))
    data_out <- data %>%
      filter((!!as.symbol(filter_by_dummy)) == 1)
  
  
  data_out <- data_out %>% select(keep_features)
  
  if (as.dataframe)
    data_out <- as.data.frame(data_out)
  
  data_out
}

# ------------------------------------------------------------------------------
# FUNCTION performSingleModelingTask
# Performs a single modeling task, as decribed in the arguments 
#     data_split - list of train set, test set, and sublist of specific features 
#     task - a vector of pametetes describing the modelling task 
#     train_parameters - a list of parameters controlling the training process
#     alg_paramters - a list of parameters controlling the the selected algorithm
#     right_closed - controls how the borders of the groups are defined,
#                   i.e. if the quantile is included in the left or the right group -
#                   more details in the R base function cut(). A flag which has 
#                   the same meaning as INTERVAL_RIGHT_CLOSED in the TrainAndValidate.R
# ------------------------------------------------------------------------------

performSingleModelingTask <- function(task, data_split, 
                                      train_params, alg_params,
                                      right_closed = FALSE){
  
  # 1. Unlist task parameters
  weight_flag <- task['weight_flag']
  alg <- task['alg']
  what_data <- task['what_data']
  outcome_name <- task['outcome_name']
  
  prob_name <- sprintf("%s%s-%s-%s%s%s",
                       what_data,
                       ifelse(!is.null(RACE_FILTER), paste0("_Race", RACE_FILTER),""),
                       outcome_name, alg,
                       ifelse(weight_flag, "-Weighted",  ""),
                       ifelse(CV_BLOCKED, "-SiblingBlocked",""))
  
  # Set the name of the model file (to be produced if models is build)
  output_rds_file <- file.path(res_dir, paste0(prob_name, ".rds"))
  
  # Log
  message(sprintf("\n%s %s %s\n", strrep("#",10), prob_name, strrep("#", 10)))
  
  if (FIT_MODELS) {
    tic(paste("Total time with", WORKERS, "workers for", prob_name))
    
    # 2.Control check for any not-handled missing values.
    # Remove columns with missing values as glmnet can't handle them or
    # we will have to remove data instances - na.action=na.omit in train()
    predictor_names <- setdiff(data_split$features[[what_data]],
                               data_split$features[["withNA"]])
    
    
    predictor_names <- setdiff(predictor_names,
                               getNoInfoColumns(data_split$train[, predictor_names]))
    
    
    # 3.Pull only the relevant features
    train_data <- selectData(data_split$train,
                             filter_by_dummy = NULL,
                             keep_features = c(predictor_names,
                                               BLOCKING_VAR,
                                               outcome_name),
                             as.dataframe = TRUE)
    test_data <- selectData(data_split$test,
                            filter_by_dummy = NULL,
                            keep_features = c(predictor_names,
                                              BLOCKING_VAR,
                                              outcome_name),
                            as.dataframe = TRUE)
    
    message(sprintf("\nTrain data %d x %d, outcome rate = %.4f\n",
                    nrow(train_data), ncol(train_data),
                    sum(train_data[, outcome_name])/nrow(train_data)))
    message(sprintf("\nTest data %d x %d, outcome rate = %.4f\n",
                    nrow(test_data), ncol(test_data),
                    sum(test_data[, outcome_name])/nrow(test_data)))
    
    
    # 4. Set the modeling parameters with NULL values and make the CV folds
    # As the outcomes might have different distribution,
    # we will create a new list of folds for each outcome, when using blocking.
    
    train_params[["weight"]] <- weight_flag
    
    if (CV_BLOCKED) {
      # Group data by the blocking variable
      train_data %>%
        group_by(!!rlang::sym(BLOCKING_VAR)) %>%
        summarise(mean = mean(!!rlang::sym(outcome_name))) %>%
        ungroup() -> groups
      
      # Create the stratified folds based on the grouped data
      folds <- createMultiFolds(groups$mean,
                                train_params[["num_folds"]],
                                train_params[["num_repeats"]] )
      
      # Back join train_data to the group data and take the train_data row id's
      # this give us list of row ids
      folds <- lapply(folds,
                      function(i){
                        groups %>%
                          dplyr::slice(i) %>%
                          select(!!rlang::sym(BLOCKING_VAR)) %>%
                          left_join(train_data %>% rowid_to_column(),
                                    by = BLOCKING_VAR ) %>%
                          pull(rowid)
                      })
      
      train_params[["folds"]] <- folds
      
    }
    
    
    # 5. Train the models
    
    dtime <- system.time(fitModel <- doModelTraining(train_data, test_data,
                                                     predictor_names,
                                                     outcome_name,
                                                     params = train_params,
                                                     alg_params = alg_params,
                                                     alg = alg))
    
    
    time_tasks <- tibble(model_name = prob_name,
                         set_rows = nrow(train_data),
                         set_cols = ncol(train_data),
                         threads = WORKERS,
                         train.user_min = dtime[1]/60,
                         train.sys_min = dtime[2]/60,
                         train.tot_min = dtime[3]/60)
    
    dtime <- system.time(write_rds(fitModel, output_rds_file))
    time_tasks <- time_tasks %>%
      mutate(save.user_min = dtime[1]/60,
             save.sys_min = dtime[2]/60,
             save.tot_min = dtime[3]/60)
    
    message("\nTime in minutes for training (tuning) and saving the model into a file")
    print(time_tasks %>% select(-model_name) %>% as.data.frame)
    out <- time_tasks
    
    toc()
    
  }else{
    out <- NULL
    fitModel <- NULL
  }
  
  tic(paste("Total time for post-modeling analysis and saving results", prob_name))
  # 6. Save model predictions, scores and coefficients  
  if (!FIT_MODELS & file.exists(output_rds_file)) {
    fitModel <-  readRDS(file = output_rds_file)
  }
  
  if (!is.null(fitModel)) {
    getModelPredictionsScoresCoefficients(fitModel,
                                          data_split,
                                          outcome_name,
                                          prob_name,
                                          RACE_FILTER,
                                          res_dir,
                                          right_closed)
    
  }
  
  toc()
  
  out
  
}


# ------------------------------------------------------------------------------
# FUNCTION getQuantiles
# Get quantiles (at specified positions given in probs)
# for a given numeric vector (num_vec) that can be ranked
# First the num_vector is ranked and ties resolved by "average" method
# ------------------------------------------------------------------------------
getQuantiles <- function(num_vector, probs, dorank=TRUE){
  if (dorank)
    ranks <- rank(num_vector, ties.method = "average")
  else
    ranks <- num_vector
  quantiles <- quantile(ranks, probs = probs)
  list(ranks = ranks, quantiles = quantiles)
}

# ------------------------------------------------------------------------------
# FUNCTION getRankCategory
# For a given list of numeric values pred and the number of groups n,
# it assigns each numeric value to one of n groups.
# The groups are defined by the n quantiles (0, 1/n , 2/n ....n-1/n) defined over pred.
# right_closed - controls how the borders of the groups are defined,
# i.e. if the quantile is included in the left or the right group -
# more details in the R base function cut()
# ------------------------------------------------------------------------------
getRankCategory <- function(pred, text, n = 20,
                            right_closed = FALSE,
                            extra_pred = NULL){
  
  res_list <- getQuantiles(pred, probs = 0:n/n,
                           dorank = ifelse(is.null(extra_pred), TRUE, FALSE))
  breaks <- res_list$quantiles
  labels <- FALSE
  if (sum(duplicated(breaks))) {
    labels <- which(!duplicated(breaks))
    new_labels <-
      sapply(seq_along(labels),
             function(i){
               if (labels[i] == (n + 1)) {
                 out <- NULL
               } else {
                 current_label <- labels[i]
                 next_label <- labels[i + 1]
                 diff_label <- next_label - current_label
                 out <- ifelse(diff_label == 1,
                               current_label,
                               sprintf("%d-%d",
                                       current_label,
                                       next_label - 1))
               }
             })
    breaks <- breaks[!duplicated(breaks)]
    labels <- unlist(new_labels)# removes the NULL
    stopifnot(length(breaks) == (length(labels) + 1))
  }
  
  vec <- res_list$ranks
  if (!is.null(extra_pred)) vec <- extra_pred
  
  rank_cat <- cut(vec,
                  breaks = breaks,
                  include.lowest = TRUE,
                  right = right_closed,
                  labels = labels,
                  ordered_result = TRUE)
  rank_cat <- as.character(rank_cat)
  print(paste("rank categories for", text, "in", n, "categories"))
  print(table(rank_cat, useNA = "ifany"))
  return(rank_cat)
}

# ------------------------------------------------------------------------------
# FUNCTION getScoreGroupsBounds
# Calculates the probability intervals for each score group
# pred_scores - a data frame with model predictions in a column named Predicted
# score_var - the name of the column with score groups
# right_closed - controls how the borders of the groups are defined,
# i.e. if the quantile is included in the left or the right group -
# more details in the R base function cut()
# ------------------------------------------------------------------------------

getScoreGroupsBounds <- function(pred_scores, 
                                 score_var = "PredictedRiskCategory", 
                                 right_closed = FALSE){
  
  pred_scores <- pred_scores %>% arrange(Predicted)
  
  unique_scores <- unique(unlist(pred_scores[, score_var]))
  ng <- length(unique_scores)
  
  tmp <- data.frame(RiskGroup = 1:ng,
                    RiskGroupName = unique_scores,
                    Rule = NA,
                    Lower = NA, Upper = NA,
                    op_lower = NA, op_upper = NA,
                    stringsAsFactors = FALSE)
  
  for (g in tmp$RiskGroup) {
    
    name_group = tmp[g, "RiskGroupName"]
    data_group <- pred_scores %>%
      filter(!!rlang::sym(score_var) == name_group)
    min_pred <- min(pull(data_group, Predicted))
    max_pred <- max(pull(data_group, Predicted))
    
    if (g == 1)
    {
      tmp$Lower[g] <- 0
      tmp$Upper[g] <- max_pred
      tmp$op_lower[g] <- ">="
      tmp$op_upper[g] <- ifelse(right_closed, "<", "<=")
    } else if (g > 1 & g < ng) {
      tmp$Lower[g] <- min_pred
      tmp$Upper[g] <- max_pred
      tmp$op_lower[g] <- ifelse(right_closed, ">=", ">")
      tmp$op_upper[g] <- ifelse(right_closed, "<", "<=")
    } else if (g == ng) {
      tmp$Lower[g] <- min_pred
      tmp$Upper[g] <- 1
      tmp$op_lower[g] <- ifelse(right_closed, ">=", ">")
      tmp$op_upper[g] <- "<="
    }
    
  }
  
  # Go over once again to fix the borders based on the right_closed value
  # and generate the rules for score assignment
  
  for (g in tmp$RiskGroup){
    
    if (right_closed) {
      if (g > 1)
        tmp$Upper[g-1] <- tmp$Lower[g]
    } else{
      if (g < ng)
        tmp$Lower[g+1] <-  tmp$Upper[g]
    }
    
    tmp$Rule[g]  <-
      sprintf("Score group = %s, if Predicted %s %.8f and Predicted %s %.8f", 
              tmp$RiskGroupName[g],
              tmp$op_lower[g], tmp$Lower[g],
              tmp$op_upper[g], tmp$Upper[g])
  }
  
  tmp
}

# ------------------------------------------------------------------------------
# FUNCTION getScores
# For a given vector of predictions, calculates the corresponding scores,
# using the provided set of n-based group score rules in a data frame format
# ------------------------------------------------------------------------------
getScores <- function(predictions, score_rules){
  ng <- nrow(score_rules)
  out <- sapply(predictions,
                function(value){
                  for (g in 1:ng) {
                    if (do.call(score_rules$op_upper[g],
                                list(value, score_rules$Upper[g])))
                    {
                      group <- g
                      break
                    }
                    
                  }
                  score_rules$RiskGroupName[group]
                })
  out
}

# ------------------------------------------------------------------------------
# FUNCTION getPerformanceMetrics
# data_pred - data frame con las predicciones
# cutoffs - umbrales de corte
# right_closed - controls how the borders of the groups are defined,
# i.e. if the quantile is included in the left or the right group -
# more details in the R base function cut()
# ------------------------------------------------------------------------------

getPerformanceMetrics <- function(data_pred, # data frame with all predictions
                                  cutoffs = c("standard" = 0.5), # probability thresholds
                                  right_closed = FALSE,
                                  list_groups = c("All"), # vector of column names that can be used for group-ing ,
                                  race_filter # if model was trained on given race only, then the race name would be here 
){
  
  # as race missing data is the same for several race group idnicators, 
  # (expect for the hispanic onem which has its own indicator for missing info) 
  # we will only need to calculate stats for RaceMissing group once
  race_groups <- grep("MomWhite|MomBlack|MomAsian|MomNative", list_groups, value = T)
  race_miss_duplication = FALSE
  
  out <- 
    lapply(list_groups,
           function(x) {
             
             #print(x)
             
             if (!x %in% colnames(data_pred)) return(NULL)
             
             data_pred <- data_pred %>% 
               mutate(Group = !!rlang::sym(x))
             
             if (x %in% race_groups & race_miss_duplication) {
               data_pred <- filter(data_pred, Group != "RaceMissing")
             }
             
             df_auc <- data_pred %>%
               group_by(Group, Partition) %>%
               summarise(n_dist = n_distinct(Observed),
                         AUC = ifelse(n_dist != 2, NA, 
                                      pROC::auc(pROC::roc(Observed, Predicted)))) %>%
               select(-n_dist)
             
             df <- 
               lapply(cutoffs,
                      function(cutoff){
                        data_pred %>%
                          ## the cutoffs are predfined, not influened by the group
                          #DBP - previous code was weirdly at the group level, but mutate is being applied at the row level
                          #mutate(PredictedDummy = if_else(right_closed,
                                                          #Predicted >= cutoff,
                                                          #Predicted > cutoff) %>% as.numeric) %>%
                          #DBP (10/17/21) - original version
                          mutate(PredictedDummy = if_else(rep(right_closed, nrow(.)),
                                                          Predicted >= cutoff,
                                                          Predicted > cutoff) %>% as.numeric) %>%
                          group_by(Group, Partition) %>%
                          summarize(CutoffValue = cutoff,
                                    SetSize = n(),
                                    TP = sum(PredictedDummy == 1 & Observed == 1),
                                    FP = sum(PredictedDummy == 1 & Observed == 0),
                                    TN = sum(PredictedDummy == 0 & Observed == 0),
                                    FN = sum(PredictedDummy == 0 & Observed == 1 ),
                                    TPR = TP / (TP + FN), # true positive rate (sensitivity)
                                    TNR = TN / (TN + FP), # true negative rate (specificity)
                                    PPV = TP / (TP + FP), # positive predictive value (precision)
                                    NPV = TN / (TN + FN), # negative predictive value
                                    FPR = 1 - TNR, # false positive rate
                                    FNR = 1 - TPR, # false negative rate
                                    FOR = 1 - NPV, # false omission rate
                                    FDR = 1 - PPV, # false discovery
                                    ACC = (TP + TN) / (TP + FN + TN + FP)#,
                                    #DB (10/17/21): Kappa not necessary and too expensive to calculate
                                    #Kappa = unlist(e1071::classAgreement(table(Observed, PredictedDummy)))[c("kappa")]
                          )
                        
                      }) %>% bind_rows(.id = "CutoffName")
             
             out_df <- left_join(df, df_auc, by = c("Partition", "Group"))
             
             if (x %in% race_groups & !race_miss_duplication) {
               # set to FALSE so next time we do not do duplicated calculations
               race_miss_duplication <<- TRUE
             }
             
             out_df
           }) 
  
  out <- out %>% bind_rows
  
  if (!is.null(race_filter)) {
    
    all_race_groups <- c("All","Black", "NonBlack", 
                         "NativeAM", "NonNativeAM", 
                         "Hispanic", "NonHispanic", 
                         "White", "NonWhite", 
                         "Asian", "NonAsian",
                         "RaceMissing", "HispanicMissing")
    
    out <- out %>% 
      filter(Group %in% 
               c(setdiff(unique(Group), all_race_groups), race_filter))
  }
  
  return(out)
}


# ------------------------------------------------------------------------------
# FUNCTION getNoInfoColumns
# ------------------------------------------------------------------------------
getNoInfoColumns <- function(data_in, lin_comb = TRUE, verbose = TRUE){
  
  res1 <- nearZeroVar(data_in, saveMetrics = T, allowParallel = TRUE)
  # Columns with zero variance 
  zv_list <- res1[res1$zeroVar == TRUE, ] %>% rownames(.)
  
  keep_list <- res1[res1$zeroVar == FALSE, ] %>% rownames(.)
  
  
  if (lin_comb) {
    data_in_sub <- data_in[, keep_list]
    res2 <- findLinearCombos(data_in_sub)
    
    ## This code only will select the columns that 
    ## are equal to other columns in the dataset 
    ## but will keep the ones that are linear combination of two or more columns
    ## We will keep the one-hot encoding variables (although we know that 
    ## redundancy in var coding can influence linear models)
    lin_comb_list <- 
      lapply(res2[[1]],  
             function(ids) { 
               tmp_ids <- colnames(data_in_sub)[ids] 
               
               
               lapply(tmp_ids[2:length(tmp_ids)],
                      function(x){
                        if (all(data_in_sub[, x] == data_in_sub[, tmp_ids[1]]))
                          tmp_ids[1]
                        else
                          NULL
                      }) %>% unlist()
               
             })  %>% 
      unlist() # to remove the NULLs 
  }
  
  if (verbose) {
    if (length(zv_list)) {
      print(sprintf("The following %d variables have zero variance and will be removed",
                    length(zv_list)))
      print(paste(zv_list, collapse = ","))
    }
    
    if (lin_comb & length(lin_comb_list)) {
      print(sprintf("The following %d variables are redundant and will be removed",
                    length(lin_comb_list)))
      print(paste(lin_comb_list, collapse = ","))
    }
  }
  
  return(c(zv_list, lin_comb_list))
}

# ------------------------------------------------------------------------------
# FUNCTION calculateRiskRatiosForFeatures
# Calculates relative risk - RR for given set of factors. 
# RR(factor) = mean(factor, score == top_score)/ mean(factor, score =/= top_score)
# score_data - data frame with the model predictions and risk scores 
# sel_features - set of factor names
# race_filter - the race group
# cutoffs -  named vector of prob.cutoffs for the top score group 
# partition - default is c("train", "test"). RR are calculated for each set separately 
# 
# This is a data-specific function
# ------------------------------------------------------------------------------

calculateRiskRatiosForFeatures <- function(score_data,
                                           sel_features,
                                           race_filter,
                                           cutoffs, 
                                           right_closed = FALSE,
                                           partition =  c("train+test", "train", "test")){
  
  if (is.null(race_filter))
    race_group <- c("All","Black", "NonBlack", 
                    "NativeAM", "NonNativeAM", 
                    "Hispanic", "NonHispanic", 
                    "White", "NonWhite", 
                    "Asian", "NonAsian")
  else
    race_group <- race_filter
  
  options_df <-
    expand.grid(
      race_group = race_group,
      cutoff_name = names(cutoffs),
      partition =  partition,
      feature = sel_features,
      stringsAsFactors = FALSE) %>%
    mutate(cutoff_value = cutoffs[cutoff_name])
  
  out <- 
    apply(options_df, 1, 
          function(item){
            dat <- score_data 
            
            # filter by partition 
            if (item['partition'] != "train+test") {
              dat <- dat %>% 
                filter(Partition == item['partition']) 
            }
            
            dat <- dat %>%
              mutate(RiskCategory = ifelse(if_else(rep(right_closed, nrow(.)),
                                                   Predicted >= item['cutoff_value'],
                                                   Predicted > item['cutoff_value']), 
                                           "Top", "Rest"),
                     RiskCatgeory = factor(RiskCategory, levels = c("Rest", "Top")))
            
            
            # filter by race
            dat <- filterRows(dat, item['race_group']) 
            
            # filter on the block-missing indicator
            dat <- filterRows(dat, filter_value = item['feature'])
            
            # add riskratios
            if (nrow(dat) == 0) {
              return(NULL)
            }
            
            dat.tab <- table(dat %>% select(RiskCategory, !!rlang::sym(item['feature'])))
            
            if ((nrow(dat.tab) == 1 & ncol(dat.tab) == 1 ) | 
                (nrow(dat.tab) != ncol(dat.tab))) {
              return(NULL)
            }
            
            message_text <- tryCatch(epitools::riskratio(dat.tab, verbose = TRUE,
                                                         replicates = 100000,
                                                         correction = TRUE),
                                     error = function(e) e,
                                     warning = function(w) w)
            
            if (is(message_text, "warning")) {
              
              rr <-  epitools::riskratio(dat.tab, verbose = TRUE, method = "small",
                                         replicates = 100000,
                                         correction = TRUE)
            }
            else {
              rr <- epitools::riskratio(dat.tab, verbose = TRUE, method = "wald",
                                        replicates = 100000,
                                        correction = TRUE)
            }
            
            rr_ci <- rr$measure[grep("Top", rownames(rr$measure), value = T), ]
            tmp_rr <- data.frame(race_group = item['race_group'],
                                 cutoff_name <- item['cutoff_name'],
                                 partition <- item['partition'], 
                                 feature = item['feature'], 
                                 matrix(rr_ci, nrow = 1),
                                 # the p-value if for test of independence 
                                 # (for the predicted risk, given the feature) 
                                 pval <- rr$p.value["Top", "midp.exact"],
                                 method <- attributes(rr)[["method"]],
                                 stringsAsFactors = FALSE)
            
            
            colnames(tmp_rr) <- c("Race_group", "Cutoff_name", "Partition", 
                                  "Feature", "RR_estimated", 
                                  "RR_95CIlower", "RR_95CIupper",
                                  "Independence_pvalue", "RR_method")
            
            # some more stats per group 
            tmp_stat <- dat %>% 
              group_by(RiskCategory) %>%
              summarize(N = sum(!is.na(!!rlang::sym(item['feature']))),
                        Mean = mean(!!rlang::sym(item['feature']),na.rm = TRUE),
                        StDev = sd(!!rlang::sym(item['feature']), na.rm = TRUE)) %>%
              tidyr::gather(stat, val, -RiskCategory) %>% 
              tidyr::unite(col = StatGroup, RiskCategory, stat) %>% 
              tidyr::spread(StatGroup, val)
            
            # combine stats and riskratios
            bind_cols(tmp_rr, tmp_stat)
            
            
          }) %>% bind_rows(.)
}


# ------------------------------------------------------------------------------
# FUNCTION getModelPredictionsScoresCoefficients
# Calculates and saves model predictions for both train and test data, 
# risk scores, the model performance metrics and the model coefficients 
# (where appropriate). 
# fitModel - the model object
# data_split - a list of two datsets, train and test sets 
# outcome_name - the name of the column in data_split sets that codes the outcome 
# model_name - the name of the model
# write_dir - the name of the directory where results will be saved
# right_closed  - controls how the borders of the groups are defined,
# i.e. if the quantile is included in the left or the right group -
# more details in the R base function cut()
# ------------------------------------------------------------------------------

getModelPredictionsScoresCoefficients <- function(fitModel, 
                                                  data_split,
                                                  outcome_name,
                                                  model_name,
                                                  race_filter,
                                                  write_dir,
                                                  right_closed = FALSE){
  
  
  # 1.Combine provided data list (both train and test) but this has only the default columns
  full_data <-  bind_rows(list(train = data_split$train,
                               test = data_split$test),
                          .id = "Partition")
  
  train_data <- fitModel$trainingData
  
  # If provided partitioning (the train, specifically) has different size then exit with error
  try(if (nrow(train_data) != nrow(data_split$train))
    stop(paste("Model was trained on a different partitioning of the dataset.",
               "Check that data_split is loaded from the right file")))
  
  predictor_names <- setdiff(colnames(train_data), c(".outcome",  ".weights"))
  keep_features <- c( data_split[["features"]][["ids"]],
                      data_split[["features"]][["race"]],
                      data_split[["features"]][["prfs"]]) %>% unique()
  
  score_data <- full_data[, c("Partition", keep_features)]
  
  # Save the true observed outcome
  score_data$Observed <- pull(full_data, outcome_name)
  
  # 2. Get predictions and risk scores
  score_data <- score_data %>%
    mutate(Predicted = NA,
           PredictedRiskCategory = NA)
  
  #  Calculate predictions for the whole set
  pred <- predict(
    # the model
    object = fitModel,
    # the data set has to have the features specified in predictor_names
    select(full_data, predictor_names),
    type = "prob")
  score_data$Predicted <- pred[, "yes"]
  
  # Get risk score for the whole set.
  # We want to partition the whole set in 20 risk groups.
  # The boundaries for each score groups as calculated using the predicted
  # probability of all instances (train + test), with cutoffs set at the
  # 5th, 10th ... 95th percentile.
  #Problem found with this call, using another simpler version - DBP May 15th 2020
  # score_data <- score_data %>%
  #   dplyr::mutate(PredictedRiskCategory = getRankCategory(Predicted,"all", 20,
  #                                                  right_closed = right_closed),
  #          PredictedRiskCategory100 = getRankCategory(Predicted,"all", 100,
  #                                                     right_closed = right_closed)
  #   )
  score_data$PredictedRiskCategory <- getRankCategory(score_data$Predicted,"all", 20,
                                                          right_closed = right_closed)
  score_data$PredictedRiskCategory100 <- getRankCategory(score_data$Predicted,"all", 100,
                                                      right_closed = right_closed)
  # Generate the risk score bounds - including the rules in textual format
  score_rules <- 
    lapply(list("Partition in 20" = "PredictedRiskCategory",
                "Partition in 100" = "PredictedRiskCategory100"
    ),
    function(x)
      getScoreGroupsBounds(score_data, 
                           score_var = x,
                           right_closed  = right_closed)) %>%
    bind_rows(.id = "RuleGroup")
  
  # 3. Get the performance metrics (including confusion matrix) in a table format
  #    with one row per probability cutoff. We choose the standard cutoff of 0.5,
  #    the one for top 5% (score = 20) and top10% (score 19 or 20)
  
  cutoff_set <- c("standard" = 0.5,
                  "top 1% on train+test" = 
                    score_rules$Lower[score_rules$RuleGroup == "Partition in 100" & 
                                        score_rules$RiskGroupName == "100"],
                  "top 5% on train+test" = 
                    score_rules$Lower[score_rules$RuleGroup == "Partition in 20" & 
                                        score_rules$RiskGroupName == "20"],
                  "top 10% on train+test" = 
                    score_rules$Lower[score_rules$RuleGroup == "Partition in 20" & 
                                        score_rules$RiskGroupName == "19"])
  
  # data-specifc part, so we can easily get in one go all performance stats
  # add new columns that define groups of interest
  score_data_tmp <- addNewColumns(score_data) 
  # get the name of these group-realted new columns
  list_groups = setdiff(colnames(score_data_tmp), colnames(score_data))
  
  perf_df <- getPerformanceMetrics(score_data_tmp, cutoffs = cutoff_set, 
                                   right_closed, list_groups, race_filter)
                          
  # save the groups for external use
  score_data <- score_data_tmp
  
  # 4. Get the coefficients when possible (like in the case of LASSO, LR)
  
  if (grepl("-Lasso", model_name))
  {
    model_coef <- coef(fitModel$finalModel, s =
                         fitModel$bestTune$lambda)
    fvalues <- model_coef[, 1]
    fnames <- names(model_coef[, 1])
    
  }else if (grepl("-LR", model_name))
  {
    model_coef <- summary(fitModel)$coef
    fvalues <- model_coef[, 1]
    fnames <- names(model_coef[, 1])
  }else if (grepl("-RF|-XGB", model_name))
  { 
    # the feature importance values (unscaled) are returned 
    # as coefficients 
    varimp <- varImp(fitModel, scale = FALSE)
    fnames <- rownames(varimp$importance)
    fvalues <- varimp$importance$Overall
  }else{
    fnames <- fvalues <- NULL
  }
  
  model_coef <- data.frame(Name = fnames,
                           Value = fvalues,
                           row.names = NULL,
                           stringsAsFactors = FALSE)
  model_coef <- left_join(model_coef, data_split$info_features,
                          by = c("Name" = "Variable"))
  
  # 5. Calculate riskratios (aka rel.risks) for given set of factors (in add_data)
  #    in the group predicted at high risk (top 5%) vs the rest (bottom 95%)
  #    We will do this by partition
  # if (length(data_split[["features"]][["prfs"]])) {
  #   
  #   prfs <- data_split[["features"]][["prfs"]]
  #   rr_df <- calculateRiskRatiosForFeatures(score_data,
  #                                           prfs,
  #                                           race_filter,
  #                                           cutoffs = cutoff_set[2:3],
  #                                           right_closed = right_closed)
  #   if (nrow(rr_df)) {
  #     write_csv(rr_df,
  #               path = file.path(write_dir, paste0(model_name, "_model_factors_relrisk.csv")),
  #               col_names = TRUE)
  #   }
  #   
  # }
  
  # 6. Write the results in separate files 
  
  if (nrow(model_coef)) {
    write_csv(x = model_coef,
              path = file.path(write_dir,
                               paste0(model_name, "_model_coef.csv")))
  }
  
  write_csv(score_data,
            path = file.path(write_dir, paste0(model_name, "_model_scores.csv")),
            col_names = TRUE)
  
  write_csv(score_rules,
            path = file.path(write_dir, paste0(model_name, "_model_score_rules.csv")),
            col_names = TRUE)
  
  write_csv(perf_df,
            path = file.path(write_dir, paste0(model_name, "_model_conf_matrix.csv")),
            col_names = TRUE)
  
  
}

