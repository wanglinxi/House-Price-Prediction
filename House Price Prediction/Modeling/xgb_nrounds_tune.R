####################################### Tune number of Rounds Xgboost ###############################
library(ggplot2)
library(xgboost)

source("load_ames_data.R")
source("utils/quick_preprocessing.R") # naive preprecessing

# Function to find the optimal number of iteration (nrounds) for xgboost. Function evaluates each number of nround using
# k-fold cross-validation and stop after n iteration (specified by early_stop) without improvements. This is done 
# repeated times. In the end the final number of iteration for further tuning holding can be determine be taking the mean or
# median of the t-results. Results are save as csv-files under "Modeling/Results/xgboost/norunds/xgb_nrounds_train_t_.csv"
# make sure that your working directory is set to SPL16!!!
# 
# input: xgb_parameters - a list of arbitrary parameters that are holded fixed while experimenting with nrounds 
#        feature_matrix - the training data without labels!!
#        labels - labels of the corresponding training data (SalePrice) 
#        max_rounds - maximum number of iteration to be tested (default 1000)
#        k - number of fold in k-fold cv (default 10)
#        repetitions - how often to repeate the procedure (the higher the more stable the median/mean result should be) (default 10)
#        early_stop - number of iteration without improvement the algorithm should stop
#
# output: a vector of the repetition length containing the best nround in each repetition
tune_nrounds <- function(xgb_parameters, feature_matrix, labels, max_nrounds = 1000, k = 10, repetitions = 10, early_stop = 20){
  optimal_rounds <- rep(0,repetitions)
  result_path <- "Modeling/Results/xgboost/nrounds/xgb_nrounds_train"
  for(t in 1:repetitions){
    # cross-validate xgboost to get the accurate measure of error
    xgb_cv = xgb.cv(params = xgb_parameters, # previously specified parameters
                    data = feature_matrix, # training data without labels!
                    label = labels, # labels (y)
                    nrounds = max_nrounds, # maximum iteration 
                    nfold = k,         # number of folds in K-fold
                    prediction = TRUE,  # return the prediction using the final model 
                    showsd = TRUE,      # standard deviation of loss across folds
                    verbose = TRUE,
                    print.every.n = 1,  # print each round
                    early.stop.round = early_stop # stop after n rounds without improvement 
    )
    # get the optimal number of nrounds
    optimal_rounds[t] <- nrow(xgb_cv$dt) - early_stop
    # save all training results as csv file xgb_nrounds_train_repetition.csv
    write.csv(as.data.frame(xgb_cv$dt), file = paste(result_path, t,".csv", sep="_"))
  }#end repeatitions
  # return whole result
  return(optimal_rounds)
}

### usage
#prepare the data for xgb
train_frame=data.matrix(train[,-c(1,ncol(train))], rownames.force = NA) 
train_labels=data.matrix(y, rownames.force = NA)
train <- as(train_frame, "dgCMatrix")
Y <- as(train_labels, "dgCMatrix")

# determine arbitrary xgboost parameters in a list
xgb_parameters = list(                                              
  eta = 0.1,               # learning rate                                                                
  max.depth = 6,           # max nodes of a tree                                                       
  eval_metric = "rmse",    # error metric
  gamma = 0.1,             # minimal improvement per iteration
  colsample_bytree=0.8,    # fraction of variable to consider per tree (similar to mtry in rf)
  subsample = 0.8          # fraction of the whole sample that the bootstrap sample should consist of (like ntree in rf)
)
# takes ~30min with repetition = 100
optimal_nrounds <- tune_nrounds(xgb_parameters=xgb_parameters, feature_matrix=train, labels = Y, repetitions = 100)  
median(optimal_nrounds) # 109.5 
mean(optimal_nrounds)   # 116.25
sd(optimal_nrounds) # 28.92
final_rounds = mean(optimal_nrounds) + sd(optimal_nrounds)  # ~145



############# plot test and training error of one result to get an intuition ##############################
# cross-validate xgboost to get the accurate measure of error
xgb_cv = xgb.cv(params = xgb_parameters,
                data = train_frame,
                label = train_labels,
                nrounds = 1000, 
                nfold = 10,         # number of folds in K-fold
                prediction = TRUE,  # return the prediction using the final model 
                showsd = TRUE,      # standard deviation of loss across folds
                verbose = TRUE,
                print.every.n = 1,  # print each round
                early.stop.round = 50 # stop after n rounds without improvement 
)
# Plot train and test error
res_cv <- as.data.frame(xgb_cv$dt)
res_cv$nrounds <- 1:nrow(res_cv)

ggplot() + 
  geom_line(aes(nrounds, test.rmse.mean, colour="red"), res_cv) +  
  geom_line(aes(nrounds, train.rmse.mean, colour="blue"), res_cv) +
  ggtitle("Training vs. Test Error (Number of Iterations)") +
  xlab("nrounds") + ylab("RMSE") 