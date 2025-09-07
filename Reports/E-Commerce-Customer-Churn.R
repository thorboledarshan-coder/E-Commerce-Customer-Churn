#load libraries
library(tidyverse)
library(dplyr)
library(janitor)

#load datasets
churn <- read_excel("E Commerce Dataset.csv.xlsx")

#clean dataset
churn <- clean_names(churn)
churn <- churn %>% 
  drop_na()
churn <- churn %>% 
  distinct()
colnames(churn)
colSums(is.na(churn))
str(churn)
# Convert to factors
churn <- churn %>%
  mutate(
    gender = factor(gender),
    marital_status = factor(marital_status),
    preferred_payment_mode = factor(preferred_payment_mode),
    preferred_login_device = factor(preferred_login_device),
    prefered_order_cat = factor(prefered_order_cat),
    churn = factor(churn, levels = c(0,1), labels = c("Active","Churned"))
  )

#Add new metrics for analysis (RFM model).adds new 3 columns
churn <- churn %>%
  mutate(
    recency = day_since_last_order,
    frequency = order_count,
    monetary_value = order_amount_hike_fromlast_year + cashback_amount
  )
#save cleaned dataset
write_csv(churn, "cleaned_churn.csv")

#Churn Rate (Overall)This gives you the overall churn rate .(active=3143,83.3%)(churned=631,16.7%)
churn %>%
  count(churn) %>%
  mutate(percentage = round(n/sum(n)*100,2))

#Churn by Demographics This shows if certain groups (e.g., single customers, females, etc.) churn more.
#gender   churn     customers 
#Male     Active    300 
#Male     Churned   200 
#Female   Active    250 
#Female   Churned   250 
#Gender
churn %>%
  group_by(gender, churn) %>%
  summarise(customers = n(), .groups="drop") %>%
  mutate(percentage = round(customers/sum(customers)*100,2))

#Marital Status

churn %>%
  group_by(marital_status, churn) %>%
  summarise(customers = n(), .groups="drop") %>%
  mutate(percentage = round(customers/sum(customers)*100,2))

#Churn by Behavior

#By Preferred Payment Mode
churn %>%
  group_by(preferred_payment_mode, churn) %>%
  summarise(customers = n(), .groups="drop") %>%
  mutate(percentage = round(customers/sum(customers)*100,2))

#By Login Device
churn %>%
  group_by(preferred_login_device, churn) %>%
  summarise(customers = n(), .groups="drop") %>%
  mutate(percentage = round(customers/sum(customers)*100,2))

#Numerical Comparisons(This shows behavioral differences between churned and retained customers.)
#Average Values for Active vs Churned

churn %>%
  group_by(churn) %>%
  summarise(
    avg_tenure = mean(tenure, na.rm=TRUE),
    avg_order_count = mean(order_count, na.rm=TRUE),
    avg_coupon_used = mean(coupon_used, na.rm=TRUE),
    avg_complaints = mean(complain, na.rm=TRUE),
    avg_satisfaction = mean(satisfaction_score, na.rm=TRUE),
    avg_cashback = mean(cashback_amount, na.rm=TRUE)
  )




