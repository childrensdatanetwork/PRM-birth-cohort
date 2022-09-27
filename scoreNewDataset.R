# ------------------------------------------------------------------------------ 
# Autor: K.Taskova
# Revised by Eunhye Ahn for distribution (May 2022)
# Descripcion: Generate scores on a new set given model and score rules
# Creado: June 2019 
# ------------------------------------------------------------------------------
# Correr desde la linea de comando en linux:
# Rscript --vanilla --slave R/scoreNewDataset.R >> log_score_dataset.txt 2>&1
# Assumptions:
# 1) You are located in the working directory where folder 'R' with this script
#    exists
# 2) In the same directory, the models and data files are located. If this is not the case, then
#    set the corresponding variables in the script to point to the right file paths


library("readstata13")
library("ggplot2")
library("magrittr")
library("dplyr")
library("knitr")
library("kableExtra")
library("hrbrthemes")
library("here")

# Load custom functions
# Save the file "customFunctionsNew.R" in the same folder
setwd("working directory")
options(width = 200)
source("customFunctionsNew.R")
RAND_SEED <- 12

# ------------------------------------------------------------------------------
# FUNCTIONS
# ------------------------------------------------------------------------------
# Given model name returns the modeled outcome name
getOutcomeNamefromModelName <- function(name){
  after_pro_string <- unlist(strsplit(name, "PRO_"))[2]
  outcome_name <- sprintf("PRO_%s", unlist(strsplit(after_pro_string, "-"))[1])
  outcome_name
}

getPerformanceMetrics <- function(data_pred, # data frame with all predictions
         cutoffs = c("standard" = 0.5), # probability thresholds
         right_closed = FALSE,
         list_groups = c("All"), # vector of column names that can be used for group-ing ,
         race_filter = FALSE # if model was trained on given race only, then the race name would be here 
){
  
  # as race missing data is the same for several race group indicators, 
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
  
  # if (!is.null(race_filter)) {
  #   
  #   all_race_groups <- c("All","Black", "NonBlack", 
  #                        "NativeAM", "NonNativeAM", 
  #                        "Hispanic", "NonHispanic", 
  #                        "White", "NonWhite", 
  #                        "Asian", "NonAsian",
  #                        "RaceMissing", "HispanicMissing")
  #   
  #   out <- out %>% 
  #     filter(Group %in% 
  #              c(setdiff(unique(Group), all_race_groups), race_filter))
  # }
  
  return(out)
}



# ------------------------------------------------------------------------------
# MAIN CODE
# ------------------------------------------------------------------------------

#- 1. Initialization and loading model and data --------------------------------

# Set the full path to the model file
#set option: race or no race model
option <- "birthNoRace"
model_dir <- getwd()
model_file_name <- paste0(option,"-PRO_PLACED_3YEARS-Lasso-Weighted-SiblingBlocked_reduced.rds")
model_rdata_file <- file.path(model_dir, model_file_name)

model_object <- readRDS(model_rdata_file)


# Set the full path to the model-specific score rules files
# This will be automatically set assuming
# - no changes have been  done to the naming process
# - the file is in he same folder as the model file
model_name <- gsub(".rds", "", model_file_name)


# Set the name of the outcome - the one that was modeled.
# This is dependent on the specific model used and can be extracted
# from the model name, assuming no changes have been done to the naming process
# outcome_name <- "PRO_PLACED_3years"
outcome_name <- getOutcomeNamefromModelName(model_name)


# Set the full path to the rds file with the new dataset that needs to be scored.
#
# Note 1: this dataset should have the same format as the spine dataset saved in
# "data_pri_pro*.rds". At least it should have the PRIs that are
# used by the loaded model to make the predictions for the modeled outcome.
# As in practice the new data will not have values for the outcome, we do not
# expect the corresponding PRO column in the new dataset.
#
# Note 2: it is your responsibility to ensure that the selected model makes sense
# to be applied to the new dataset.

data_dir <- getwd()
data_file_name <- "birth_placements_pris_labeled.csv"
data_rdata_file <- file.path(getwd(), data_file_name)
new_data <- read.csv(data_rdata_file)
data_name <- gsub(".csv", "", data_file_name)


#change features names to upper-case in case
colnames(new_data) <- toupper(colnames(new_data))

# Create folder for the results
# Note: create a name that will be unique and will reflect the specific dataset,
# to avoid overwriting past results.

setwd("working directory")

res_dir <- file.path(paste0(getwd()),
                     sprintf("scoring_results_%s", data_name))
createFolder(res_dir)


#fix PRI names
colnames(new_data)[colnames(new_data) == "PRI_CHILD_APGAR_SCORE_5MIN_MODER_ABNML"] <- "PRI_CHILD_APGAR_SCORE_5MIN_MODER"
colnames(new_data)[colnames(new_data) == "PRI_CHILD_APGAR_SCORE_5MIN_REASSURING"] <- "PRI_CHILD_APGAR_SCORE_5MIN_REASS"

#- 2. Calculation of predictions and scores for the new data set  --------------

message("\nCalculation of predictions and scores for the new data set\n")

train_data <- model_object$trainingData
predictor_names <- setdiff(train_data, c(".outcome",  ".weights"))

if (length(setdiff(predictor_names, colnames(new_data))) > 0)
  stop("the new data set does not have all the PRIs that the model train set has.")

# create features that do not exist in the birth data and give value=0
feature_list <- setdiff(predictor_names, colnames(new_data))
for(feature in feature_list)
{
  new_data[,feature]<-0
}


keep_features <- c("BID","PRI_MOM_FIRSTBORN",
                   "PRI_MOM_FIRSTBORN_MISSING", "PRI_CHILD_FEMALE_OR_MISS", toupper(outcome_name),
                   grep("PRI.*_RACE", colnames(new_data), value = TRUE))

score_df <- new_data %>%
  select(all_of(keep_features)) %>%
  mutate(Partition = data_name)

#  Get the predictions
score_df <- score_df %>%
  mutate(Predicted = NA,
         PredictedRiskCategory = NA)

pred <- predict(object = model_object,
                select(new_data, predictor_names),
                type = "prob")
score_df$Predicted <- pred[, "yes"]

# Calculate the scores, based on a set of score rules 
# if there are more sets of rules we select the one based on 20 groups

# score_df$PredictedRiskCategory2 <- 
#   getScores(pull(score_df, Predicted), 
#             rules_df %>% filter(RuleGroup == "Partition in 20"))

score_df$PredictedRiskCategory <-getRankCategory(score_df$Predicted, "all", 20, right_closed = TRUE)
rules_df2 <- getScoreGroupsBounds(score_df, "PredictedRiskCategory", right_closed = TRUE)


# Save the scores
if (nrow(score_df) > 0)
  write_csv(score_df,
            path = file.path(res_dir, paste0(model_name, "_scores.csv")),
            col_names = TRUE)

#- 3. (Optional) Model performance evaluation on the new data set ---------------

# Check if the dataset has a valid PRO column corresponding to the modeled outcome
# then set the flag for calculating performance metrics using the observed values
# for the outcome.
# Valid PRO column means that there are no missing values, and the PRO column has
# only 0s and 1s.
#
# Note: this analysis makes sense when the size of the new dataset is
# large enough so we can have enough cases present across different score groups.

valid_outcome  <-
  ifelse(toupper(outcome_name) %in% colnames(new_data) &
           (new_data %>% pull(toupper(outcome_name)) %>% is.na %>% any) == FALSE,
         TRUE,
         FALSE)



if (valid_outcome) {
  message("\nModel performance evaluation on the new data set\n")
  
  # pull the observed outcome values
  tmp_df <- tibble(BID = new_data %>% pull(BID),
                   Observed = new_data %>% pull(toupper(outcome_name)))
  score_df <- left_join(score_df, tmp_df, by = "BID")
  
  # The same cutoffs defined when the model was built and validated
  # based on the 20-groups partitioning
  cutoff_set <- c("standard" = 0.5,
                  "top 5% test" = rules_df2$Lower[rules_df2$RiskGroupName == "20"],
                  "top 10% test" = rules_df2$Lower[rules_df2$RiskGroupName == "19"],
                  "top 20% test" = rules_df2$Lower[rules_df2$RiskGroupName == "17"],
                  "top 30% test" = rules_df2$Lower[rules_df2$RiskGroupName == "15"],
                  "top 40% test" = rules_df2$Lower[rules_df2$RiskGroupName == "13"],
                  "top 50% test" = rules_df2$Lower[rules_df2$RiskGroupName == "11"],
                  "top 60% test" = rules_df2$Lower[rules_df2$RiskGroupName == "9"],
                  "top 70% test" = rules_df2$Lower[rules_df2$RiskGroupName == "7"],
                  "top 80% test" = rules_df2$Lower[rules_df2$RiskGroupName == "5"],
                  "top 90% test" = rules_df2$Lower[rules_df2$RiskGroupName == "3"],
                  "top 100% test" = rules_df2$Lower[rules_df2$RiskGroupName == "1"])
  
  # This flag should be the same as when the model was built,
  # and we extract it from the score rules
  right_closed <- ifelse(rules_df2$op_upper[1] == "<", TRUE, FALSE)
  
  # Calculate the metrics
  # If we want to get the metrics for some groups of interest we need to 
  # code this information first (make sure that if you need some variables 
  # to get that information then these variables have to be saved in keep_features)
  # The default setting keeps the race variables and few more 
  # and we will use the same groups as defined in function addNewColumns() 
  # from trainAndValidate.R
  score_df_tmp <- score_df %>%
    mutate(MomBlack = ifelse(PRI_MOM_RACE_MISS, 
                             "RaceMissing", 
                             ifelse((PRI_MOM_RACE_BLACK_AA==1 & PRI_MOM_RACE_HISPANIC==0), "Black", "NonBlack")),
           MomNative = ifelse(PRI_MOM_RACE_MISS , 
                              "RaceMissing", 
                              ifelse((PRI_MOM_RACE_NATIVE_AM==1 & PRI_MOM_RACE_HISPANIC==0), "NativeAM", "NonNativeAM")),
           MomHispanic = ifelse(PRI_MOM_RACE_HISPANIC_MISS , 
                                "HispanicMissing", 
                                ifelse(PRI_MOM_RACE_HISPANIC, "Hispanic", "NonHispanic")),
           MomWhite = ifelse(PRI_MOM_RACE_MISS, 
                             "RaceMissing", 
                             ifelse((PRI_MOM_RACE_WHITE==1 & PRI_MOM_RACE_HISPANIC==0), "White", "NonWhite")),
           MomAsian = ifelse(PRI_MOM_RACE_MISS, 
                             "RaceMissing", 
                             ifelse((PRI_MOM_RACE_ASIAN==1 & PRI_MOM_RACE_HISPANIC==0), "Asian", "NonAsian")),
           ChildGender = ifelse(PRI_CHILD_FEMALE_OR_MISS, "FemaleOrMissing", "Male"),
           MomFirstborn = ifelse(PRI_MOM_FIRSTBORN_MISSING, 
                                 "FirstbornMissing", 
                                 ifelse(PRI_MOM_FIRSTBORN, "Firstborn", "NonFirstborn")),
           All = "All")
  
  list_groups <- c("All", "MomBlack", "MomNative", "MomHispanic", 
                   "MomWhite", "MomAsian", "ChildGender", "MomFirstborn")
  perf_df <- getPerformanceMetrics(score_df_tmp, cutoffs = cutoff_set,
                                   right_closed = right_closed, list_groups = list_groups)
  # Save the results
  if (nrow(perf_df) > 0)
    write_csv(perf_df,
              path = file.path(res_dir, paste0(model_name, "_conf_matrix.csv")),
              col_names = TRUE)
}


# Distribution charts

score_df$Observed2 <- as.factor(score_df$Observed)
p <- ggplot(data=score_df, aes(x=Predicted)) + 
  geom_density(fill="lightgray") + 
  labs(title="Density Plot: Predicted Risk Scores",x="Scores", y = "Density")
p2 <- p+geom_vline(aes(xintercept=cutoff_set[3]),
                   color="blue", linetype="dashed", size=0.5) 
p3 <- ggplot(data=score_df, aes(x=Predicted, group=Observed2, fill=Observed2)) +
  geom_vline(aes(xintercept=cutoff_set[3]), color="blue", linetype="dashed", size=0.5) +
  geom_density(adjust=1.5, alpha=.4) + scale_fill_discrete(name = "Placed in 3 years", labels = c("No", "Yes")) + 
  labs(title="Density Plot: Predicted Risk Scores",x="Scores", y = "Density") +
  theme(legend.position="top")

ggsave(here("density_plot1.pdf"), p, height = 6, width = 7)
ggsave(here("density_plot2.pdf"), p2, height = 6, width = 7)
ggsave(here("density_plot3.pdf"), p3, height = 6, width = 7)
