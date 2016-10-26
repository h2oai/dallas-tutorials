## Load H2O library
library(h2o)

## Connect to H2O cluster
h2o.init(nthreads = -1)

## Define data paths
loans_path = normalizePath("~/Desktop/H2OTOUR/Datasets/LoanStats/")

## Task 1: Import Data
loan_stats <- h2o.importFile(path = loans_path, parse = F)

## Parse with user imposed schema which changes the column types of column:
## Change the column types in the parse setup
## Force 'int_rate', 'revol_util', 'emp_length', and 'term' to String instead of Enum
## Force 'mths_since_last_delinq', 'mths_since_last_record', and 'mths_since_last_major_derog' to Numeric instead of Enum
parse_setup <- h2o.parseSetup(loan_stats)
col_types <- parse_setup$column_types

col_names <- c( "int_rate", "revol_util", "emp_length", "term")
col_indices <- which ( parse_setup$column_names %in% col_names)
col_types[col_indices] <- "String"

col_names <- c("mths_since_last_delinq", "mths_since_last_record", "mths_since_last_major_derog")
col_indices <- which ( parse_setup$column_names %in% col_names)
col_types[col_indices] <- "Numeric"

loan_stats <- h2o.parseRaw(data = loan_stats, col.types = col_types, destination_frame = "loan_stats")


## Task 2: Look at the levels in the response column loan_status
## Hint: Use h2o.table function on the response column, use as.data.frame to return the table to R
loan_status_table = as.data.frame(h2o.table(loan_stats$loan_status))
loan_status_table = loan_status_table[order(loan_status_table$Count),]
## Plot the count of the different levels in the response column
par(mai=c(1,2,1,0.5))
barplot(loan_status_table$Count, names.arg = loan_status_table$loan_status, las = 2, main = "Loan Statuses", horiz = T)


## Task 3: Filter out all loans that are completed, aka subset data
## Hint: "Current", "In Grace Period", "Late (16-30 days)", "Late (31-120 days)" are ongoing loans
loan_stats <- loan_stats[!(loan_stats$loan_status %in% c("Current", "In Grace Period", "Late (16-30 days)", "Late (31-120 days)")), ]


## Task 4: Bin the response variable to good/bad loans only, use your best judgment for what is a good/bad loan
## Create new column called bad_loan which should be a binary variable
## Hint: You can turn the bad_loan column into factor using as.factor
loan_stats$bad_loan <- !( loan_stats$loan_status == "Fully Paid")
loan_stats$bad_loan <- as.factor(loan_stats$bad_loan)
## Run h2o.table to see the distribution of bad vs good loans
as.data.frame(h2o.table(loan_stats$bad_loan))


## Task 5: String munging to clean string columns before converting to numeric
## Hint: Columns that need munging includes "int_rate", "revol_util", "emp_length", "term"

## Example for int_rate using h2o.strsplit, trim, as.numeric
loan_stats$int_rate <- h2o.strsplit(loan_stats$int_rate, split = "%")
loan_stats$int_rate <- h2o.trim(loan_stats$int_rate)
loan_stats$int_rate <- as.numeric(loan_stats$int_rate)

## Now do the same for revol_util
loan_stats$revol_util <- h2o.strsplit(loan_stats$revol_util, split = "%")
loan_stats$revol_util <- h2o.trim(loan_stats$revol_util)
loan_stats$revol_util <- as.numeric(loan_stats$revol_util)
loan_stats$revol_util <- loan_stats$revol_util/100

## Now we're going to clean up emp_length.
## Use h2o.sub to remove " year" and " years", also translate n/a to ""
loan_stats$emp_length <- h2o.sub(x = loan_stats$emp_length, pattern = "([ ]*+[a-zA-Z].*)|(n/a)", replacement = "")
## Use h2o.trim to remove any trailing spaces
loan_stats$emp_length <- h2o.trim(loan_stats$emp_length)
## Use h2o.sub to convert < 1 to 0 years and do the same for 10 + to 10 
## Hint: Be mindful of spaces between characters
loan_stats$emp_length <- h2o.sub(x = loan_stats$emp_length, pattern = "< 1", replacement = "0")
loan_stats$emp_length <- h2o.sub(x = loan_stats$emp_length, pattern = "10\\+", replacement = "10")
loan_stats$emp_length <- as.numeric(loan_stats$emp_length)
h2o.hist(loan_stats$emp_length)

## Using h2o.sub you can remove the string 'months' from the term column
loan_stats$term <- h2o.gsub(pattern = " months", replacement = "", x = loan_stats$term)
loan_stats$term <- h2o.trim(loan_stats$term)
loan_stats$term <- as.numeric(loan_stats$term)


## Task 6: Create new feature called "credit_length_in_years"
## Hint: Use the date columns "earliest_cr_line" and "issue_d" and use h2o.year to show date in years
loan_stats$credit_length_in_years <- h2o.year(loan_stats$issue_d) - h2o.year(loan_stats$earliest_cr_line)


## Task 7: Use h2o.sub to create two levels for column "verification_status" ie "Verified" and "Not Verified"
## Hint: Use h2o.table to examine levels within "verification_status"
loan_stats$verification_status <- ifelse(loan_stats$verification_status == "not verified", "not verified", "verified")
h2o.table(loan_stats$verification_status)


## Task 8: Create a few new features using available ratios

## Take the log of annual_inc 
summary(loan_stats$annual_inc, exact_quantiles = T)
loan_stats$annual_inc_log <- ifelse(loan_stats$annual_inc ==0, 0, log(loan_stats$annual_inc) )

## Calculate the users' total available credit limit across all credit lines
loan_stats$avail_credit <- ifelse(loan_stats$revol_util == 0, NA, loan_stats$revol_bal/loan_stats$revol_util)
loan_stats$avail_credit_log <- ifelse(loan_stats$avail_credit == 0, 0, log(loan_stats$avail_credit))

## Calculate the users' monthly debt vs monthly income and the increase in monthly debt with the new loan
loan_stats$monthly_debt <- (loan_stats$annual_inc/12 )*( loan_stats$dti)
loan_stats$monthly_inc <- loan_stats$annual_inc/12 
loan_stats$new_monthly_debt <- loan_stats$monthly_debt + loan_stats$installment

## Calculate the new debt to income ratio with the new debt, as well as the change in the dti ratio
loan_stats$dti2 <- loan_stats$new_monthly_debt/loan_stats$monthly_inc
loan_stats$dti_delta <- loan_stats$dti2 - loan_stats$dti

## Calculate ratio of available credit to income
loan_stats$avail_credit_util <- loan_stats$avail_credit/loan_stats$annual_inc
loan_stats$avail_credit_util_log <- ifelse( loan_stats$avail_credit_util == 0, 0, log(loan_stats$avail_credit_util))

## Task 9: Find out how many loans are bad in each year in the dataset
loan_stats <- loan_stats[!is.na(loan_stats[,1]), ]
loan_stats$issue_d_year <- h2o.year(loan_stats$issue_d) + 1900
loans_by_year  <- as.data.frame(h2o.group_by(data = loan_stats, by = "issue_d_year", nrow("bad_loan"), sum("bad_loan")))
loans_by_year2 <- t(loans_by_year[,2:3])
colnames(loans_by_year2) <- loans_by_year$issue_d_year
## Plot flights over the years using user defined R function
barplot(loans_by_year2, beside = T, col = c("dark blue", "red"))


## Task 10: Define your response and predictor variables
myY <- "bad_loan"
myX <- c("term","home_ownership", "verification_status",
         "purpose","loan_amnt", "emp_length", "annual_inc",
         "dti", "delinq_2yrs","inq_last_6mths", "mths_since_last_delinq",
         "mths_since_last_record", "collections_12_mths_ex_med", 
         "mths_since_last_major_derog", "open_acc", "pub_rec", "revol_bal",
         "revol_util","total_acc", "credit_length_in_years", 
         "avail_credit", "avail_credit_log", "monthly_debt", "monthly_inc", 
         "new_monthly_debt", "dti2", "dti_delta", "annual_inc_log",
         "avail_credit_util", "avail_credit_util_log")


## Task 11: Do a test-train split (80-20)
## Hint: Use h2o.splitFrame ONLY once
split <- h2o.splitFrame(loan_stats, ratios = 0.8)
train <- split[[1]]
valid <- split[[2]]

## Task 12: Build model predicting good/bad loan 
## Note: Use any of the classification methods available including GLM, GBM, Random Forest, and Deep Learning
glm_model <- h2o.glm(x = myX, y = myY, training_frame = train, validation_frame = valid,
                     family = "binomial", model_id = "GLM_BadLoan")
gbm_model <- h2o.gbm(x = myX, y = myY, training_frame = train, validation_frame = valid, 
                     learn_rate = 0.05, score_each_iteration = T, ntrees = 100, model_id = "GBM_BadLoan")

## Task 13: Plot the scoring history to make sure you're not overfitting
## Hint: Use plot function on the model object
par(mai=c(1,1,1,0.5))
plot(gbm_model)

## Task 14: Plot the ROC curve for the binomial models and get auc using h2o.auc
## Hint: Use h2o.performance and plot to grab the modelmetrics and then plotting the modelmetrics
gbm_perf <- h2o.performance(gbm_model)
plot(gbm_perf)

## Report AUC on preliminary glm and gbm models
auc_table <- data.frame(GLM_AUC = c(h2o.auc(glm_model, train = T), h2o.auc(glm_model, valid = T)),
                       GBM_AUC = c(h2o.auc(gbm_model, train = T), h2o.auc(gbm_model, valid = T)))


## Task 15: Check the variable importance and generate confusion matrix for max F1 threshold
## Hint: Use h2o.varimp for non-GLM model and use h2o.confusionMatrix
glm_varimp <- glm_model@model$standardized_coefficient_magnitudes
gbm_varimp <- h2o.varimp(gbm_model)
h2o.confusionMatrix(glm_model, valid = T)
h2o.confusionMatrix(gbm_model, valid = T)

par(mai=c(1,2.2,1,0.5))
barplot(height = glm_varimp[1:10, "coefficients"], 
        names.arg = glm_varimp[1:10, "names"],
        col = ifelse(glm_varimp[1:10, "sign"] == "POS", "green", "red"), 
        las = 2,
        horiz = T)


## Task 16: Check partial dependence of features in GBM model for a few features
pdp <- h2o.partialPlot(object = gbm_model,
                       data = valid, 
                       cols = c("inq_last_6mths", "term", "dti_delta",
                                "revol_util", "annual_inc_log", "purpose",
                                "verification_status", "credit_length_in_years",
                                "home_ownership", "avail_credit_log", "emp_length"),
                       destination_key = "GBM_PartialPlots")


## Task 17: Score the entire data set using the model
## Hint: Use h2o.predict.
pred_gbm <- h2o.predict(gbm_model, valid)
pred_glm <- h2o.predict(glm_model, valid)


## Extra: Calculate the money gain/loss if model is implemented
## Calculate the total amount of money earned or lost per loan
valid$expected_earned <- valid$term * valid$installment - valid$loan_amnt
valid$earned <- valid$total_pymnt - valid$loan_amnt

## Calculate how much money will be lost to false negative, vs how much will be saved due to true positives
pred <- pred_glm
valid$pred <- pred[,1]
net <- as.data.frame(h2o.group_by(data = valid, by = c("bad_loan", "pred"), gb.control = list(na.methods = "ignore"), sum("earned")))
n1  <- net[ net$bad_loan == 0 & net$pred == 0, 3]
n2  <- net[ net$bad_loan == 0 & net$pred == 1, 3]
n3  <- net[ net$bad_loan == 1 & net$pred == 1, 3]
n4  <- net[ net$bad_loan == 1 & net$pred == 0, 3]


## Function defined to pretty print numerics as dollars
printMoney <- function(x){
  x <- round(x,2)
  format(x, digits=10, nsmall=2, decimal.mark=".", big.mark=",")
}

## Calculate the amount of earned
print(paste0("Total amount of profit still earned using the model : $", printMoney(n1) , ""))
print(paste0("Total amount of profit forfeitted using the model : $", printMoney(n2) , ""))
print(paste0("Total amount of loss that could have been prevented : $", printMoney(n3) , ""))
print(paste0("Total amount of loss that still would've accrued : $", printMoney(n4) , ""))
## Calculate Net Earned
print(paste0("Total amount earned without model : $", printMoney(sum(valid$earned))))
print(paste0("Total amount earned with model : $", printMoney( n1 + n4)))
print(paste0("Total profit : $", printMoney( -n3-n2)))

