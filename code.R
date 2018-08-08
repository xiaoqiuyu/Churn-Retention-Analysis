library(dplyr)
library(lubridate)

data=read.csv("data.csv")
data$revenue=data$unit_price*data$quantity
str(data)
sum(is.na(data))

data$invoice_date=mdy_hm(data$invoice_date)
data$invoice_date=mdy(data$invoice_date)
data$invoice_month=format(as.Date(data$invoice_date), "%Y-%m")

### Retention/churn analysis
customer2=customer[, c(7, 9)]
customer3=unique(customer2)

customer4=
customer3 %>%
  group_by(customer_id) %>%
  arrange(customer_id, invoice_month) %>%
  mutate(lag.month = dplyr::lag(invoice_date, n = 1, default = NA),
         lead.month = dplyr::lead(invoice_date, n = 1, default = NA))

customer4$invoice_date=as.Date(paste(customer4$invoice_month,"-01",sep=""))
# lag
lag.cord=!is.na(customer4$lag.month)
customer4$lag.date=NA
customer4$lag.date[lag.cord]=
  as.Date(paste(customer4$lag.month[lag.cord],"-01",sep=""))
customer4$lag.date=ymd(customer4$lag.date)
# lead
lead.cord=!is.na(customer4$lead.month)
customer4$lead.date=NA
customer4$lead.date[lead.cord]=
  paste(customer4$lead.month[lead.cord],"-01", sep="")
customer4$lead.date=as.Date(customer4$lead.date)
customer4$lead.date=ymd(customer4$lead.date)
# lag size, lead size
monnb <- function(d) { lt <- as.POSIXlt(as.Date(d, origin="1900-01-01")); 
lt$year*12 + lt$mon } 
# compute a month difference as a difference between two monnb's
mondf <- function(d1, d2) { monnb(d2) - monnb(d1) }
customer4$lag.size= mondf(customer4$lag.date, customer4$invoice_date)
customer4$lead.size= mondf(customer4$invoice_date, customer4$lead.date)

# determine retention / churn status
customer4=
customer4 %>% 
  mutate(this.month.value = case_when( is.na(.$lag.size) ~ "new",
                        .$lag.size == 1  ~ "retained",
                        .$lag.size > 1 ~ "resurrecting"))
customer4=
  customer4 %>% 
  mutate(next.month.churn = case_when( lead.size>1 | is.na(lead.size) ~ 'churn'
                                       ))

status_count=
  customer4 %>%
  group_by(invoice_month, this.month.value) %>%
  summarise(count=n()) %>%
  arrange(invoice_month, this.month.value)

churn_count=
  customer4 %>%
  filter(next.month.churn== 'churn') %>%
  group_by(invoice_month) %>%
  summarise(count=n())
churn_count$invoice_month= as.Date(paste(churn_count$invoice_month,"-01",sep=""))
churn_count$invoice_month= ymd(churn_count$invoice_month)
churn_count2= churn_count
churn_count2$invoice_month= churn_count2$invoice_month + months(1)
churn_count2= churn_count2[1:12, ]
library(tibble)
churn_count3= add_column(churn_count2, this.month.value = 'churn', .after = "invoice_month")
churn_count3$invoice_month= format(as.Date(churn_count3$invoice_month), "%Y-%m")

monthly_status= bind_rows(status_count, churn_count3)
monthly_status =
  monthly_status %>%
  arrange(invoice_month, this.month.value)

data %>%
  filter(invoice_month!= '2011-12') %>%
  group_by(invoice_month) %>%
  summarise(customer= n_distinct(customer_id)) %>%
  ggplot(aes(x= invoice_month, y= customer, group= 1)) +
  geom_line(color= "dodgerblue4") +
  theme_minimal() +
  xlab('Month') +
  ylab('Number of Customers') +
  ggtitle('Monthly Customers') +
  geom_text(aes(label = customer), vjust=-0.8)

monthly_status %>%
  filter(invoice_month!='2011-12') %>%
  ggplot(aes(x = invoice_month, y = count, 
                           colour = this.month.value, group = this.month.value)) +
  geom_line() +
  theme_minimal() +
  labs(color='Customer Type') + 
  xlab('Month') +
  ggtitle("Customer Type by Month")

### Cohort Analysis
# How would you define weekly or monthly cohorts of new customers?# 
# What do cohorts of new customers look like over time? 
# Is cohort size increasing or decreasing? 
# What about LTV? Is it getting better or worse for new cohorts over time? 
# How about cohortized churn rates?
length(unique(data$customer_id))
first_month=
  data %>%
  group_by(customer_id) %>%
  summarise(first_month= min(invoice_month))
first_month$first_month= as.Date(paste(first_month$first_month,"-01",sep=""))
first_month$first_month= ymd(first_month$first_month)

cohort_size=
  first_month %>%
  group_by(first_month) %>%
  summarise(new_users=n())
sum(cohort_size$new_users)

customer3$invoice_month= as.Date(paste(customer3$invoice_month,"-01",sep=""))
customer3$invoice_month= ymd(customer3$invoice_month)

library(sqldf)
a<- sqldf("select * 
          from first_month as f left outer join customer3 as c
          on f.customer_id = c.customer_id
          and first_month < invoice_month")
a$retention_month=mondf(a$first_month, a$invoice_month)
colnames(a)[1]='Customer_id'
b= a %>%
  arrange(first_month, retention_month)
b %>%
  filter(first_month== '2010-12-01' & retention_month== 1) 
b %>%
  filter(first_month== '2010-12-01' & retention_month== 2)

cohort_retention=
a %>%
  group_by(first_month, retention_month) %>%
  summarise(retained= n_distinct(Customer_id))

cohort_retention2= 
  cohort_retention %>%
  filter(!is.na(retention_month))
cohort_retention2$first_month= format(as.Date(cohort_retention2$first_month), "%Y-%m")
cohort_full= merge(cohort_size, cohort_retention2, by='first_month', all= T)
cohort_full$retention_rate=cohort_full$retained/cohort_full$new_users
write.csv(cohort_full, "cohort.csv")

##### LTV #####
cust_monthly_rev=
  data %>%
  group_by(customer_id, invoice_month) %>%
  summarise(monthly.revenue= sum(revenue))

c<- sqldf("select * 
          from first_month as f left outer join customer3 as c
          on f.customer_id = c.customer_id
          and first_month <= invoice_month")
c= c[, c(1, 2, 4)]

remove(cust_cohort_ltv)
cust_monthly_rev$invoice_month= as.Date(paste(cust_monthly_rev$invoice_month,"-01",sep=""))
cc= sqldf("select * 
          from c left outer join cust_monthly_rev as mr
                       on c.Customer_id = mr.customer_id
                       and c.invoice_month = mr.invoice_month")
cc$rentention_month= mondf(cc$first_month, cc$invoice_month)
colnames(cc)[7]='retention_month'
colnames(cc)[3]='Invoice_month'
dd =
  cc %>%
  group_by(first_month, retention_month) %>%
  summarise(retained= n_distinct(Customer_id),
            total_revenue= sum(monthly.revenue)) 
dd$LTV= dd$total_revenue/dd$retained
write.csv(dd, 'cohort LTV.csv')

##### DISTRIBUTION #####
avg_monthly_rev=
  cust_monthly_rev %>%
  group_by(customer_id) %>%
  summarise(avg.monthly.revenue= sum(monthly.revenue)/n_distinct(invoice_month),
            total.revenue= sum(monthly.revenue)) %>%
  arrange(-(total.revenue))
# hist(avg_monthly_rev$avg.monthly.revenue)
# sum(avg_monthly_rev$avg.monthly.revenue)
# length(unique(data$invoice_month))
# sum(data$revenue)
# sum(avg_monthly_rev$total.revenue)
# hist(avg_monthly_rev$total.revenue, breaks = 50000)
# sum(avg_monthly_rev$total.revenue[1:6])/sum(avg_monthly_rev$total.revenue)
monthly_revenue=
  data %>%
  group_by(invoice_month) %>%
  summarise(revenue= sum(revenue))

monthly_revenue_cust=
  data %>%
  group_by(invoice_month, customer_id) %>%
  summarise(revenue=sum(revenue))

rnk=
  monthly_revenue_cust %>% 
  group_by(invoice_month) %>%
  arrange(-(revenue)) %>%
  mutate(rank = order(-(revenue)))
top5=
  rnk %>%
  filter(rank>= 1 & rank<= 5)
top5total=
  top5 %>%
  group_by(invoice_month) %>%
  summarise(top5_rev= sum(revenue))
top5ndtotal= 
  merge(top5total, monthly_revenue, by='invoice_month')
top5ndtotal$percent=top5ndtotal$top5_rev/top5ndtotal$revenue
library(reshape2)
library(ggplot2)
top5.m=melt(top5ndtotal, id.vars = "invoice_month")
ggplot(top5ndtotal, aes(x= invoice_month)) + 
  geom_bar(aes(y = revenue), stat="identity",position ="identity", alpha = 0.5) +
  geom_bar(aes(y = top5_rev), stat="identity",position ="identity", alpha = 0.5) +
  geom_text(aes(x= invoice_month, y=0, label = paste(percent*100, '%')))

top5ndtotal$rev0.95 <- top5ndtotal$revenue-top5ndtotal$top5_rev
top5ndtotal$percent= top5ndtotal$top5_rev/top5ndtotal$revenue
#data_long1 <- top5ndtotal %>% dplyr::select(invoice_month,top5_rev)
#data_long2<- top5ndtotal %>% dplyr::select(invoice_month,rev0.95)

library(reshape2)
library(tidyr)
#data_wide1 <- spread(data_long1, invoice_month,top5_rev)
#data_wide2 <- spread(data_long2, invoice_month,rev0.95)
#data_wide<-rbind(data_wide1,data_wide2)

n <- top5ndtotal%>% dplyr::select(invoice_month,top5_rev,rev0.95)
data_long <- melt(n, id.vars = "invoice_month",
                  variable.name = "rev", 
                  value.name = "values")
ggplot() + geom_bar(aes(y = values, x = invoice_month, fill = rev), data = data_long,
                    stat="identity")
write.csv(data_long, 'cust_size.csv')

