
# coding: utf-8


# Import necessary packages
import os
import h2o
from h2o.estimators.gbm import H2OGradientBoostingEstimator
from h2o.estimators.glm import H2OGeneralizedLinearEstimator



# Initialize instance of H2O
h2o.init(nthreads = -1)



#Set file path
path = os.path.abspath("~/Desktop/H2OTour/Datasets/LoanStats/")



#Parse with user imposed schema which changes the column types;
#Change the column types in the parse setup
#Change "int_rate," "revol_util," "emp_length," "verification_status" to String instead of Enum
col_types = {"int_rate": "string", "revol_util":"string", "emp_length":"string", "term":"string",
            "mths_since_last_delinq":"numeric", "mths_since_last_record":"numeric", 
             "mths_since_last_major_derog":"numeric"}



#Import data and set column types to those specified above
loan_stats = h2o.import_file(path = path, col_types = col_types)



# Look at the levels int he response column, "loan_status"
# Hint: Use .table() function on the response column
loan_stats["loan_status"].table()



# Drop all loans that are still in progess and therefore cannot be deemed good/bad loans
# Hint: "Current", "In Grace Period", "Late (16-30 days)", "Late (31-120 days)" are ongoing loans
loan_stats = loan_stats[~loan_stats["loan_status"].isin(["Current", "In Grace Period", "Late (16-30 days)", 
                                                         "Late (31-120 days)"]), :]



# Bin the response variable to good/bad oans only, use your best judgement for what qualifies as a good/bad loan
# Create a new column called "bad_loan" which should be a binary variable
# Hint: You can turn the bad_loan columm into a factor using .asfactor()

loan_stats["bad_loan"] = ~(loan_stats["loan_status"] == "Fully Paid")
loan_stats["bad_loan"] = loan_stats["bad_loan"].asfactor()



## String munging to clean string columns before converting to numeric
## Hint: Columns that need munging includes "int_rate", "revol_util", "emp_length", "term"

## Example for int_rate using h2o.strsplit, trim, as.numeric
loan_stats["int_rate"] = loan_stats["int_rate"].gsub(pattern = "%", replacement = "") # strip %
loan_stats[ "int_rate"] = loan_stats["int_rate"].trim() # trim ws
loan_stats["int_rate"] = loan_stats["int_rate"].asnumeric() #change to a numeric 
loan_stats["int_rate"].show()



# Now try for revol_util yourself
loan_stats["revol_util"] = loan_stats["revol_util"].gsub(pattern = "%", replacement = "")
loan_stats["revol_util"] = loan_stats["revol_util"].trim() 
loan_stats["revol_util"] = loan_stats["revol_util"].asnumeric()
loan_stats["revol_util"] = loan_stats["revol_util"]/100
loan_stats["revol_util"].show()



# Now we're going to clean up emp_length
# Use gsub to remove " year" and " years" also translate n/a to "" 
loan_stats ["emp_length"] = loan_stats["emp_length"].gsub(pattern = "([ ]*+[a-zA-Z].*)|(n/a)", 
                                              replacement = "")
# Use trim to remove any trailing spaces 
loan_stats ["emp_length"] = loan_stats["emp_length"].trim()
# Use sub to convert < 1 to 0 years and do the same for 10 + to 10
# Hint: Be mindful of spaces between characters
loan_stats ["emp_length"] = loan_stats["emp_length"].gsub(pattern = "< 1", replacement = "0")
loan_stats ["emp_length"] = loan_stats["emp_length"].gsub(pattern = "10\\+", replacement = "10")
loan_stats ["emp_length"] = loan_stats["emp_length"].asnumeric()
loan_stats ["emp_length"].show()



# Convert the term column to a numeric from a string 
loan_stats["term"] = loan_stats["term"].gsub(pattern = " months", replacement = "")
loan_stats["term"] = loan_stats["term"].trim()
loan_stats["term"] = loan_stats["term"].asnumeric()



# Create a new column called "credit_length_in_years" by subtracting the values in "earliest_cr_line" from "issue_d"
# Hint: Convert earliest_cr_line and issue_d using .year() function before completing calculation
loan_stats["credit_length_in_years"] = loan_stats["issue_d"].year() - loan_stats["earliest_cr_line"].year()



# Use the sub function to create two levels from the verification_status column. 
#Ie "verified" and "not verified"
loan_stats["verification_status"] = (loan_stats["verification_status"] == "not verified").ifelse("not verified", 
                                                                                              "verified")
loan_stats["verification_status"].table()



# Create a few new features using available ratios 

# Take the log of annual_inc
loan_stats["annual_inc_log"] = (loan_stats["annual_inc"] ==  0).ifelse(0, loan_stats["annual_inc"].log())



# Calculate the users' total available credit limit across all credit lines
loan_stats["avail_credit"] = (loan_stats["revol_util"] == 0).ifelse(float('nan'), loan_stats["revol_bal"]/loan_stats["revol_util"])
loan_stats["avail_credit_log"] = (loan_stats["avail_credit"] == 0).ifelse(0, loan_stats["avail_credit"].log())



get_ipython().magic(u'matplotlib inline')
loan_stats["avail_credit_log"].hist()



# Calculate the users' monthly debt vs monthly income and the increase in monthly debt with the new loan
loan_stats["monthly_debt"] = ((loan_stats["annual_inc"]/12)*(loan_stats["dti"]))
loan_stats["monthly_inc"] = loan_stats["annual_inc"]/12
loan_stats["new_monthly_debt"] = loan_stats["monthly_debt"] + loan_stats["installment"]



# Calculate the new debt to income ratio with the new debt, as well as the change in the dti ratio
loan_stats["dti2"] = loan_stats["new_monthly_debt"]/loan_stats["monthly_inc"]
loan_stats["dti_delta"] = loan_stats["dti2"] - loan_stats["dti"]



# Calculate ratio of available credit to income
loan_stats["avail_credit_util"] = loan_stats["avail_credit"]/loan_stats["annual_inc"]
loan_stats["avail_credit_util_log"] = (loan_stats["avail_credit_util"] == 0).ifelse(0,
                                                                        loan_stats["avail_credit_util"].log())



# Find out how many loans are bad in each year in the dataset
loan_stats = loan_stats[~(loan_stats["member_id"].isna()),:]
loan_stats["issue_d_year"] = loan_stats["issue_d"].year()

grouped = loan_stats.group_by(["issue_d_year"])
loans_by_year = grouped.count().sum("bad_loan").get_frame()
loans_by_year_df = loans_by_year.as_data_frame(use_pandas = True)



# Plot number of actual and predicted bad loans
import matplotlib.pyplot as plt

fig, ax = plt.subplots()
width = 0.35 
rects1 = ax.bar(loans_by_year_df["issue_d_year"] + width, loans_by_year_df["nrow_id"], width, color='b')
rects2 = ax.bar(loans_by_year_df["issue_d_year"], loans_by_year_df["sum_bad_loan"], width, color='r')

ax.set_ylabel('Number of Bad Loans')
ax.set_xlabel('Year')

ax.set_title('Bad Loans by Year')
ax.set_xticks(loans_by_year_df["issue_d_year"] + width)
ax.set_xticklabels(loans_by_year_df["issue_d_year"])

ax.legend((rects1[0], rects2[0]), ('Total Loans', 'Total Bad Loans'))



# Do a test-train split (80-20)
s = loan_stats["int_rate"].runif()
train = loan_stats[s <= 0.80]
valid  = loan_stats[s > 0.80]



# Define your response and predictor variables
myY = "bad_loan"
myX = ["term","home_ownership", "verification_status",
         "purpose","loan_amnt", "emp_length", "annual_inc",
         "dti", "delinq_2yrs","inq_last_6mths", "mths_since_last_delinq",
         "mths_since_last_record", "collections_12_mths_ex_med", 
         "mths_since_last_major_derog", "open_acc", "pub_rec", "revol_bal",
         "revol_util","total_acc", "credit_length_in_years", 
         "avail_credit", "avail_credit_log", "monthly_debt", "monthly_inc", 
         "new_monthly_debt", "dti2", "dti_delta", "annual_inc_log",
         "avail_credit_util", "avail_credit_util_log"]



# Set parameters for GBM model 
from h2o.estimators.gbm import H2OGradientBoostingEstimator
gbm_model = H2OGradientBoostingEstimator(model_id="GBM_BadLoan",
                                       score_each_iteration=True,
                                       ntrees=100,
                                       learn_rate=0.05)
# Build GBM model
gbm_model.train(x=myX, y=myY, training_frame=train, validation_frame=valid)



# Set parameters for GLM model
from h2o.estimators.glm import H2OGeneralizedLinearEstimator
glm_model = H2OGeneralizedLinearEstimator(model_id = "GLM_BadLoan",
                                         family = "binomial")
# Build GLM model
glm_model.train(x = myX, y=myY, training_frame=train, validation_frame= valid)



print "GLM AUC on training = " + str(glm_model.auc(train = True)) + " and GLM AUC on validation = " + str(glm_model.auc(valid = True))

print "GBM AUC on training = " + str(gbm_model.auc(train = True)) + " and GBM AUC on validation = " + str(gbm_model.auc(valid = True))



# GLM variable coefficients plot
glm_model.std_coef_plot()



# GLM variable coefficients plot
gbm_model.varimp_plot()



# GLM confusion matrix
print(glm_model.confusion_matrix(valid = True))



# GBM confusion matrix
print(gbm_model.confusion_matrix(valid = True))



# Score the entire data set using the model
pred_gbm = gbm_model.predict(valid)
pred_glm = glm_model.predict(valid)



# Calculate the total amount of money earned or lost per loan
valid["expected_earned"] = valid["term"] * valid["installment"] - valid["loan_amnt"]
valid["earned"] = valid["total_pymnt"] - valid["loan_amnt"]



# Calculate how much money will be lost to false negative, vs how much will be saved due to true positives
valid["pred"] = pred_gbm["predict"]

grouped = valid.group_by(["bad_loan","pred"])
net = grouped.sum(col = "earned").get_frame()

n1 = net[(net["bad_loan"] == "0") & (net["pred"] == "0")]["sum_earned"].round(digits = 0).max()
n2 = net[(net["bad_loan"] == "0") & (net["pred"] == "1")]["sum_earned"].round(digits = 0).max()
n3 = (-1)*net[(net["bad_loan"] == "1") & (net["pred"] == "1")]["sum_earned"].round(digits = 0).max()
n4 = (-1)*net[(net["bad_loan"] == "1") & (net["pred"] == "0")]["sum_earned"].round(digits = 0).max()

# Calculate the amount of earned
print "Total amount of profit still earned using the model : %s" %'${:0,.0f}'.format(n1)
print "Total amount of profit forfeitted using the model : %s" %'${:0,.0f}'.format(n2)
print "Total amount of loss that could have been prevented : %s" %'${:0,.0f}'.format(n3)
print "Total amount of loss that still would've accrued : %s" %'${:0,.0f}'.format(n4)



h2o.shutdown()

