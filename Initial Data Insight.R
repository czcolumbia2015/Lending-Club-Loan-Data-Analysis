library("readr")
library("dplyr")
library("ggplot2")
library("lubridate")
library("choroplethr")

path = "/Users/chenzheng/Downloads/Lending Club Loan Data/loan.csv"
LoanData = read.csv(path)

head(LoanData, nrow = 10)
sprintf("# of Rows in Dataframe: %s", nrow(LoanData))
sprintf("Dataframe Size: %s", format(object.size(LoanData), units = "MB"))

unique(LoanData$grade)
summary(LoanData)
##check for duplicates
length(unique(LoanData$member_id)) == nrow(LoanData)
length(unique(LoanData$id))
str(LoanData)


LoanData$issue_date = as.Date(gsub("^", '01-', LoanData$issue_d), format = "%d-%b-%Y")
Loan_daily = LoanData %>% group_by(issue_date) %>% summarize(daily_amount = sum(loan_amnt)/1000000)

head(Loan_daily, nrow=10)

ggplot(Loan_daily, aes(x=issue_date, y=daily_amount)) + geom_line(color = "red", size = 1) +
  labs(x="year", y="Loan Amount", title = "Loan Amount(million) (2007 - 2015)") +
  ##geom_smooth(color = '#1A1A1A') +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")


###there are some daily variation between year 2014 and 2016. let's break it down by month of year in year of 2014 and 2015.

LoanData$month = month(LoanData$issue_date)

Loan_monthly = LoanData %>% filter(year(LoanData$issue_date) %in% c("2014", "2015")) %>%  
  group_by(month) %>% summarise(monthly_amount = sum(loan_amnt)/1000)

ggplot(Loan_monthly, aes(x = month, y = monthly_amount)) + geom_bar(stat = "identity", aes(fill=monthly_amount)) +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)) +
  labs(x="year", y="Loan Amount", title = "Loan Amount (thousand) (2014 & 2015)") +
  theme(plot.title = element_text(hjust = 0.5))


Loan_by_interestrate = LoanData %>% group_by(int_rate) %>% summarise(loan_amount = sum(loan_amnt)/1000)
ggplot(Loan_by_interestrate, aes(x = int_rate, y=loan_amount, color = loan_amount)) + 
  geom_point(shape = 16, size = 3) + scale_x_continuous(breaks = seq(0, 30, by=2))

Loan_by_income = LoanData %>% group_by(annual_inc) %>% summarise(loan_amount = sum(loan_amnt, na.rm = TRUE)/1000000)
ggplot(Loan_by_income, aes(x = annual_inc/100, y=loan_amount, color = loan_amount)) + 
  geom_point(shape = 16, size = 3) +
  labs(x="annual income", y="Loan Amount", title = "Loan Amount (million)") +
  theme(plot.title = element_text(hjust = 0.5))

## Loan amount by loan purpose

Loan_by_purpose = LoanData %>% group_by(purpose) %>% summarise(loan_amount = sum(loan_amnt)) %>%
  arrange(desc(loan_amount))

ggplot(Loan_by_purpose, aes(x = purpose, y=loan_amount)) + 
  geom_boxplot(aes(fill=purpose)) + theme(axis.title.x = element_blank())
  
 
## Loan amount by state
state_mapping = read.csv("/Users/chenzheng/Downloads/Lending Club Loan Data/state_mapping.csv")
LoanData = merge(LoanData, state_mapping, by = "addr_state", all.x = TRUE)

Loan_by_state = LoanData %>% group_by(state) %>% summarise(loan_amount = sum(loan_amnt)/1000000)
names(Loan_by_state) = c("region", "value")
state_choropleth(Loan_by_state, title = "Loan Amount by State")

## Loan amount by home ownership 
Loan_by_ownership = LoanData %>% group_by(issue_date, home_ownership) %>% summarise(loan_amount = sum(loan_amnt)/1000000)
ggplot(Loan_by_ownership, aes(x = issue_date, y = loan_amount, colour = home_ownership)) + geom_line() +
  labs(x="issued year", y="Loan Amount", title = "Loan Amount (million)") +
  theme(plot.title = element_text(hjust = 0.5))
  
LoanData[LoanData$home_ownership == "ANY",] # three records
LoanData[LoanData$home_ownership == "NONE",] # 50 number of records all before the end of 2012
LoanData[LoanData$home_ownership == "OTHER",]

## Loan Amount by term
Loan_by_term = LoanData %>% group_by(issue_date, term) %>% summarise(loan_amount = sum(loan_amnt)/1000000)
ggplot(Loan_by_term, aes(x = issue_date, y = loan_amount, colour = term)) + geom_line() +
  labs(x="issued year", y="Loan Amount", title = "Loan Amount (million)") +
  theme(plot.title = element_text(hjust = 0.5))


## Loan amount by grade
Loan_by_grade = LoanData %>% group_by(issue_date, grade) %>% summarise(loan_amount = sum(loan_amnt)/1000)
ggplot(Loan_by_grade, aes(x=issue_date, y = loan_amount)) + geom_area(aes(fill=grade)) + 
  labs(x="issued year", y="Loan Amount", title = "Loan Amount (thousand)") +
  theme(plot.title = element_text(hjust = 0.5))




numeric_cols = sapply(LoanData, is.numeric)
install.packages("reshape2")
library("reshape2")

#turn the data into long format
LoanData.lng = melt(LoanData[,numeric_cols], id = (c("id")))
#create the plot to check if there are any good variables that can be user
ggplot(LoanData.lng, aes(x = value/1000000), colour = id) + geom_density() + facet_wrap(~variable)


