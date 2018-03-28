library(lubridate)
# library(plyr)
library(dplyr)
# library(ade4)
# library(tidyr)
library(caret)

####################--Params--###########################################

subs_tenure_cutoff = 30
inactive_period_cutoff = 7 # filter out customer who has not been active since last 30 days 

####################--Data Load--########################################

registration <- read.csv("registration.csv")
transaction_history <- read.csv("transaction_history.csv")
rounds_info <- read.csv("rounds_info.csv")

registration <- registration %>%
  mutate(MobileVerified = as.factor(MobileVerified), EmailVerified = as.factor(EmailVerified)) %>%
  mutate(MobileVerified = recode(MobileVerified, "1" = "yes", "0" = "no")) %>%
  mutate(EmailVerified = recode(EmailVerified, "1" = "yes", "0" = "no"))

transaction_history <- transaction_history %>%
  mutate(transaction_date = ymd_hms(transaction_date))

rounds_info <- rounds_info %>%
  mutate(Match_start_time = ymd_hms(Match_start_time))

current_date = max(transaction_history$transaction_date)

#########################################################################

#####################--EDA and Data Prep--###############################

cust_trans_hist_dist <- transaction_history %>%
  group_by(User_id) %>%
  summarise(min_trans_date = min(transaction_date), max_trans_date = max(transaction_date), 
            max_gap_retured = max(as.numeric(max(diff(date(transaction_date)))),0),
            median_gap_retured = max(as.numeric(median(diff(date(transaction_date)))),0),
            last_active_before = as.numeric(date(current_date)-max(date(transaction_date))),
            max_gap_till_current_date = max(max(as.numeric(max(diff(date(transaction_date)))),0),
                                            (as.numeric(date(current_date)-max(date(transaction_date))))), 
            gap_above_90_count = sum(ifelse(diff(date(transaction_date))>=90,1,0))) %>% 
  mutate(subs_tenure = as.numeric(date(max_trans_date) - date(min_trans_date)))

#extract game played data ifno

game_played_data <- rounds_info %>%
  group_by(Game_played) %>%
  summarise(game_name = unique(as.character(Game_name)))

# data prep 
cust_churn_data <- transaction_history %>%
  left_join(game_played_data, by = "Game_played") %>%
  group_by(User_id) %>%
  mutate(date_diff = as.numeric(c(0,diff(date(transaction_date))))) %>%
  arrange(User_id,transaction_date) %>%
  filter(date(transaction_date) < ifelse(date_diff >= 90, 
                                         date(transaction_date[row_number(max(date_diff))]), 
                                         date(current_date))) %>%
  mutate(trans_cat_deposit = ifelse(transaction_category == "Deposit", 1, 0),
         trans_cat_join = ifelse(transaction_category == "Join", 1, 0),
         trans_cat_withdraw = ifelse(transaction_category == "Withdraw", 1, 0),
         trans_cat_other = ifelse(transaction_category == "other", 1, 0),
         transaction_category = NULL)

last_trans_date <- cust_churn_data %>%
  group_by(User_id) %>%
  summarise(last_trans_dt = max(date(transaction_date)))

cust_surv_data <- cust_trans_hist_dist %>%
  left_join(last_trans_date, by = "User_id") %>%
  mutate(last_active_date = ifelse(is.na(last_trans_dt), as.character(date(as.character(current_date))), 
                                   as.character(last_trans_dt))) %>%
  mutate(surv_age = ifelse(max_gap_till_current_date >= 90, 
                           as.numeric(date(last_trans_dt) - date(min_trans_date)), 
                           as.numeric(date(current_date) - date(min_trans_date))))

# selected customer to build the model 
model_cust_list <- cust_trans_hist_dist %>%
  filter(subs_tenure >= subs_tenure_cutoff) %>%
  filter(max_gap_till_current_date >= 90 | 
           (max_gap_till_current_date < 90 & last_active_before <inactive_period_cutoff)) %>%
  mutate(churn = as.factor(ifelse(max_gap_till_current_date >=90,"1","0")))

# final input data to the model 
churn_class_data <- cust_churn_data %>%
  filter(User_id %in% model_cust_list$User_id) %>%
  group_by(User_id) %>%
  summarise(
    # subs_tenure = as.numeric(date(max(transaction_date)) - date(min(transaction_date))),
    median_gap = (as.numeric(ifelse(length(transaction_date) >2,as.numeric(median(diff(date(transaction_date)))), 
                                    as.numeric(max(date(transaction_date)) - min(date(transaction_date)))))),
    tot_trans_amount = sum(trans_amount),
    trans_count = length(trans_amount),
    avg_balance = mean(balance),
    curr_balance = balance[length(balance)],
    tot_game_played = length(unique(Game_played)),
    tot_league_played = length(unique(League_played)),
    cricket_match_count = length(game_name[which(game_name == "cricket")]),
    football_match_count = length(game_name[which(game_name == "football")]),
    kabaddi_match_count = length(game_name[which(game_name == "Kabaddi")]),
    rugby_match_count = length(game_name[which(game_name == "rugby")]),
    trans_deposit_count = sum(trans_cat_deposit),
    trans_join_count = sum(trans_cat_join),
    trans_withdraw_count = sum(trans_cat_withdraw),
    trans_other_count = sum(trans_cat_other),
    trans_deposit_amount = sum(trans_amount[which(trans_cat_deposit == 1)]),
    trans_join_amount = sum(trans_amount[which(trans_cat_join == 1)]),
    trans_withdraw_amount = sum(trans_amount[which(trans_cat_withdraw == 1)]),
    trans_other_amount = sum(trans_amount[which(trans_cat_other == 1)]),
    anonymous_var2_sum = sum(anonymous_var2)) %>%
  left_join(registration, by = "User_id") %>%
  left_join(cust_surv_data[,c("User_id", "surv_age")], by = "User_id") %>%
  left_join(model_cust_list[,c("User_id", "churn")], by = "User_id") %>%
  mutate(churn = recode(churn, "1" = "yes", "0" = "no"))

########### Model building ################################################

model_input <- churn_class_data
model_input$User_id <- NULL

intrain <- createDataPartition(model_input$churn,p = 0.8,list = FALSE)
train <- model_input[intrain,]
test <- model_input[-intrain,]

# trainSplit <- SMOTE(Default_Flag ~ ., train, perc.over = 100, perc.under=300)
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           ## Estimate class probabilities
                           classProbs = TRUE,
                           ## Evaluate performance using 
                           ## the following function
                           summaryFunction = twoClassSummary)
gbmGrid <-  expand.grid(interaction.depth = c(3, 6),
                        n.trees = (5:30)*10, 
                        shrinkage = c(.1),
                        n.minobsinnode = 10)
model_gbm <- train(churn ~ ., data = train, 
                   method = "gbm", 
                   trControl = fitControl,
                   bag.fraction = 0.6,
                   verbose = FALSE,
                   tuneGrid = gbmGrid,
                   metric = "ROC")













