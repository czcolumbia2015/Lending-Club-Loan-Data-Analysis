##Remove all columns with large number of missing value
NA_cols = c('earliest_cr_line','mths_since_last_delinq','mths_since_last_record','mths_since_last_major_derog',
           'annual_inc_joint','dti_joint','open_acc_6m','open_il_6m','open_il_12m','open_il_24m','mths_since_rcnt_il',
           'total_bal_il','il_util','open_rv_12m','open_rv_24m','max_bal_bc','all_util','inq_fi','total_cu_tl',
           'inq_last_12m')


New_LoanData = LoanData[, !colnames(LoanData) %in% NA_cols]

str(New_LoanData)

# with the rest of colums with relative small missing value and also numeric colume, use mean value to replace these missing value.
New_LoanData$annual_inc<-ifelse(is.na(New_LoanData$annual_inc),mean(New_LoanData$annual_inc,na.rm = TRUE),New_LoanData$annual_inc)
New_LoanData$delinq_2yrs<-ifelse(is.na(New_LoanData$delinq_2yrs),mean(New_LoanData$delinq_2yrs,na.rm = TRUE),New_LoanData$delinq_2yrs)
New_LoanData$inq_last_6mths<-ifelse(is.na(New_LoanData$inq_last_6mths),mean(New_LoanData$inq_last_6mths,na.rm = TRUE),New_LoanData$inq_last_6mths)
New_LoanData$open_acc<-ifelse(is.na(New_LoanData$open_acc),mean(New_LoanData$open_acc,na.rm = TRUE),New_LoanData$open_acc)
New_LoanData$pub_rec<-ifelse(is.na(New_LoanData$pub_rec),mean(New_LoanData$pub_rec,na.rm = TRUE),New_LoanData$pub_rec)
New_LoanData$revol_util<-ifelse(is.na(New_LoanData$revol_util),mean(New_LoanData$revol_util,na.rm = TRUE),New_LoanData$revol_util)
New_LoanData$total_acc<-ifelse(is.na(New_LoanData$total_acc),mean(New_LoanData$total_acc,na.rm = TRUE),New_LoanData$total_acc)
New_LoanData$collections_12_mths_ex_med<-ifelse(is.na(New_LoanData$collections_12_mths_ex_med),mean(New_LoanData$collections_12_mths_ex_med,na.rm = TRUE),New_LoanData$collections_12_mths_ex_med)
New_LoanData$acc_now_delinq<-ifelse(is.na(New_LoanData$acc_now_delinq),mean(New_LoanData$acc_now_delinq,na.rm = TRUE),New_LoanData$acc_now_delinq)
New_LoanData$tot_coll_amt<-ifelse(is.na(New_LoanData$tot_coll_amt),mean(New_LoanData$tot_coll_amt,na.rm = TRUE),New_LoanData$tot_coll_amt)
New_LoanData$tot_cur_bal<-ifelse(is.na(New_LoanData$tot_cur_bal),mean(New_LoanData$tot_cur_bal,na.rm = TRUE),New_LoanData$tot_cur_bal)
New_LoanData$total_rev_hi_lim<-ifelse(is.na(New_LoanData$total_rev_hi_lim),mean(New_LoanData$total_rev_hi_lim,na.rm = TRUE),New_LoanData$total_rev_hi_lim)


New_LoanData$id<-factor(New_LoanData$id)
New_LoanData$member_id<-factor(New_LoanData$member_id)
New_LoanData$policy_code<-factor(New_LoanData$policy_code)

# dataset with numeric columns

numeric_cols = sapply(New_LoanData, is.numeric)
LoanData_numeric = New_LoanData[, numeric_cols]
corr = cor(LoanData_numeric)

# check the density distribution 
library("reshape2")

# 'bad' statuses
bad_indicators <- c("Charged Off ",
                    "Default",
                    "Does not meet the credit policy. Status:Charged Off",
                    "In Grace Period", 
                    "Default Receiver", 
                    "Late (16-30 days)",
                    "Late (31-120 days)")

# assign certain statuses to a 'bad' ('0') group
LoanData_numeric$is_bad <- ifelse(LoanData$loan_status %in% bad_indicators, 0,
                                  ifelse(LoanData$loan_status=="", NA, 1)
)


loanbook.lng <- melt(LoanData_numeric, id="is_bad")

# plot the distribution for 'bad' and 'good' for each numeric variable
# create the plot to check if there are any good variables that can be used in predictive models
p <- ggplot(aes(x = value, group = is_bad, colour = factor(is_bad)), 
            data = loanbook.lng) + geom_density() + facet_wrap(~variable, scales="free")
p


cat_cols = sapply(New_LoanData, is.factor)
LoanData_cat = New_LoanData[, cat_cols]

multicollinear_cols = c("funded_amnt", "total_pymnt_inv", "funded_amnt_inv")
unwanted_cols = c("id", "member_id", "url", "desc", "policy_code", "last_pymnt_d", "next_pymnt_d", 
                  "last_credit_pull_d", "zip_code", "issue_d", "emp_title", "title", "sub_grade")

New_LoanData = New_LoanData[, !colnames(New_LoanData) %in% multicollinear_cols]
New_LoanData = New_LoanData[, !colnames(New_LoanData) %in% unwanted_cols]


New_LoanData$addr_state = as.character(New_LoanData$addr_state)
