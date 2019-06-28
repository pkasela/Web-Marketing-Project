#### LIBRARIES ####
library(dplyr)
library(magrittr)
library(ggplot2)




#### IMPORTING DATA SETS ####

### set working directory ###
setwd("/home/pranav/Desktop/web marketing/datasets/")
### clients fidelity subscriptions ###
df_1_cli_fid <- read.csv2("raw_1_cli_fid.csv", na.strings = c("NA", ""))

### clients accounts details ###
df_2_cli_account <- read.csv2("raw_2_cli_account.csv", na.strings = c("NA", ""))
  
### clients addresses ###
df_3_cli_address <- read.csv2("raw_3_cli_address.csv", na.strings = c(""), stringsAsFactors = F)

### clients privacy ###
df_4_cli_privacy <- read.csv2("raw_4_cli_privacy.csv" , na.strings = c("NA", ""))

### email campaign characterization ###
df_5_camp_cat <- read.csv2("raw_5_camp_cat.csv" , na.strings = c("NA", ""))
  
### email event ###
df_6_camp_event <- read.csv2("raw_6_camp_event.csv" , na.strings = c("NA", ""))




#### DATA CLEANING ####

### df_1_cli_fid ###

## first look ##
str(df_1_cli_fid)
summary(df_1_cli_fid)

## cleaning ##
df_1_cli_fid_clean <- df_1_cli_fid

## formatting dates ##
df_1_cli_fid_clean <- df_1_cli_fid_clean %>%
  mutate(DT_ACTIVE = as.Date(DT_ACTIVE))

## formatting numerical categories as factor ##
df_1_cli_fid_clean <- df_1_cli_fid_clean %>%
  mutate(ID_NEG = as.factor(ID_NEG)) %>%
  mutate(TYP_CLI_FID = as.factor(TYP_CLI_FID)) %>%
  mutate(STATUS_FID = as.factor(STATUS_FID))

## (consistency control) number of fid per client ##
num_fid_x_cli <- df_1_cli_fid_clean %>%
  group_by(ID_CLI) %>%
  summarize(NUM_FIDs =  n_distinct(ID_FID), NUM_DATEs = n_distinct(DT_ACTIVE))

dist_num_fid_x_cli <- num_fid_x_cli %>%
  group_by(NUM_FIDs, NUM_DATEs) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI))

dist_num_fid_x_cli

# there are clients with multiple fid
# lets have a closer look
num_fid_x_cli %>% filter(NUM_DATEs == 3)

df_1_cli_fid %>% filter(ID_CLI == 621814)

## keep both first fid and last fid ##
# first --> registration date
# last --> features
df_1_cli_fid_first <- df_1_cli_fid_clean %>%
  group_by(ID_CLI) %>%
  filter(DT_ACTIVE == min(DT_ACTIVE)) %>%
  arrange(ID_FID) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  as.data.frame()

df_1_cli_fid_last <- df_1_cli_fid_clean %>%
  group_by(ID_CLI) %>%
  filter(DT_ACTIVE == max(DT_ACTIVE)) %>%
  arrange(desc(ID_FID)) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  as.data.frame()

df_1_cli_fid_clean <- df_1_cli_fid_last %>%
  left_join(df_1_cli_fid_first %>%
              select(ID_CLI, FIRST_ID_NEG = ID_NEG, FIRST_DT_ACTIVE = DT_ACTIVE)
            , by = 'ID_CLI') %>%
  left_join(num_fid_x_cli %>%
              select(ID_CLI, NUM_FIDs) %>%
              mutate(NUM_FIDs = as.factor(NUM_FIDs))
            , by = 'ID_CLI')

## lets review ##
str(df_1_cli_fid_clean)
summary(df_1_cli_fid_clean)

## explore distributions ##
# COD_FID
df_1_cli_fid_clean %>%
  group_by(COD_FID) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

ggplot(df_1_cli_fid_clean, aes(x=COD_FID)) + geom_bar()

# TYP_CLI_FID
df_1_cli_fid_clean %>%
  group_by(TYP_CLI_FID) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

ggplot(df_1_cli_fid_clean, aes(x=TYP_CLI_FID)) + geom_bar()

# STATUS_FID
df_1_cli_fid_clean %>%
  group_by(STATUS_FID) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

ggplot(df_1_cli_fid_clean, aes(x=STATUS_FID)) + geom_bar()

# ID_NEG
df_1_cli_fid_clean %>%
  group_by(ID_NEG) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs))  %>%
  arrange(desc(PERCENT))

ggplot(df_1_cli_fid_clean, aes(x=ID_NEG)) + geom_bar()



### df_2_cli_account ###

## first look ##
str(df_2_cli_account)
summary(df_2_cli_account)

## cleaning ##
df_2_cli_account_clean <- df_2_cli_account

## formatting boolean as factor ##
df_2_cli_account_clean <- df_2_cli_account_clean %>%
  mutate(W_PHONE = as.factor(W_PHONE))

## formatting numerical categories as factor ##
df_2_cli_account_clean <- df_2_cli_account_clean %>%
  mutate(TYP_CLI_ACCOUNT = as.factor(TYP_CLI_ACCOUNT))

## correct NA in categories ##
# we make use of the package forcats
library(forcats)

df_2_cli_account_clean <- df_2_cli_account_clean %>%
  mutate(W_PHONE = fct_explicit_na(W_PHONE, "0")) %>%
  mutate(EMAIL_PROVIDER = fct_explicit_na(EMAIL_PROVIDER, "(missing)")) %>%
  mutate(TYP_JOB = fct_explicit_na(TYP_JOB, "(missing)"))

## explore distributions ##
# COD_FID
df_2_cli_account_clean %>%
  group_by(EMAIL_PROVIDER) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

df_2_cli_account_clean %>%
  summarize(TOT_EMAIL_PROVIDER = n_distinct(EMAIL_PROVIDER))

# too many different values for EMAIL_PROVIDER to be an useful category

# W_PHONE
df_2_cli_account_clean %>%
  group_by(W_PHONE) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

ggplot(df_2_cli_account_clean, aes(x=W_PHONE)) + geom_bar()

# TYP_JOB
df_2_cli_account_clean %>%
  group_by(TYP_JOB) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

df_2_cli_account_clean %>%
  summarize(TOT_TYP_JOB = n_distinct(TYP_JOB))

ggplot(df_2_cli_account_clean, aes(x=TYP_JOB)) + geom_bar()

## lets review ##
str(df_2_cli_account_clean)
summary(df_2_cli_account_clean)

# too many missing values for EMAIL_PROVIDER to be an useful category
# keep the most frequent values and (missing) while changing the remaing into "OTHER"
freq_email_providers <- df_2_cli_account_clean %>%
  group_by(EMAIL_PROVIDER) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT)) %>%
  mutate(PERCENT_COVERED = cumsum(TOT_CLIs)/sum(TOT_CLIs))

head(freq_email_providers, 20)

clean_email_providers <- freq_email_providers %>%
  mutate(EMAIL_PROVIDER = as.character(EMAIL_PROVIDER)) %>%
  mutate(AUX = if_else(PERCENT_COVERED < 0.85 | (PERCENT_COVERED > 0.85 & lag(PERCENT_COVERED) < 0.85), 1,0)) %>%
  mutate(EMAIL_PROVIDER_CLEAN = if_else(AUX | EMAIL_PROVIDER == "(missing)", EMAIL_PROVIDER, "others"))

head(clean_email_providers, 20)

df_2_cli_account_clean <- df_2_cli_account %>%
  mutate(EMAIL_PROVIDER = as.character(EMAIL_PROVIDER)) %>%
  left_join(clean_email_providers %>%
              select(EMAIL_PROVIDER, EMAIL_PROVIDER_CLEAN)
            , by = "EMAIL_PROVIDER") %>%
  select(-EMAIL_PROVIDER) %>%
  mutate(EMAIL_PROVIDER_CLEAN = as.factor(EMAIL_PROVIDER_CLEAN))

## explore distributions ##
# EMAIL_PROVIDER_CLEAN
df_2_cli_account_clean %>%
  group_by(EMAIL_PROVIDER_CLEAN) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

ggplot(df_2_cli_account_clean, aes(x=EMAIL_PROVIDER_CLEAN)) + geom_bar()

## lets review ##
str(df_2_cli_account_clean)
summary(df_2_cli_account_clean)



### df_3_cli_address ###

## first look ##
str(df_3_cli_address)
summary(df_3_cli_address)

## cleaning ##
df_3_cli_address_clean <- df_3_cli_address

## convert PRV e REGION into factors ##
df_3_cli_address_clean <- df_3_cli_address_clean %>%
  mutate(PRV = as.factor(PRV)) %>%
  mutate(REGION = as.factor(REGION)) %>%
  distinct()

# closer look on df_3_cli_address
df_3_cli_address_clean %>%
  group_by(w_CAP = !is.na(CAP), w_PRV = !is.na(PRV), w_REGION = !is.na(REGION)) %>%
  summarize(TOT_ADDs = n_distinct(ID_ADDRESS))

df_3_cli_address_clean %>% select(PRV, REGION) %>% distinct() %>% arrange(desc(nchar(as.character(PRV))))

# drop the record without CAP - PRV - REGION
df_3_cli_address_clean <- df_3_cli_address_clean %>%
  filter(!is.na(CAP) & !is.na(PRV) & !is.na(REGION))

## explore distributions ##
# PRV
df_3_cli_address_clean %>%
  group_by(PRV) %>%
  summarize(TOT_ADDs = n_distinct(ID_ADDRESS)) %>%
  mutate(PERCENT = TOT_ADDs/sum(TOT_ADDs)) %>%
  arrange(desc(PERCENT))

ggplot(df_3_cli_address_clean, aes(x=PRV)) + geom_bar()

# REGION
df_3_cli_address_clean %>%
  group_by(REGION) %>%
  summarize(TOT_ADDs = n_distinct(ID_ADDRESS)) %>%
  mutate(PERCENT = TOT_ADDs/sum(TOT_ADDs)) %>%
  arrange(desc(PERCENT))

ggplot(df_3_cli_address_clean, aes(x=REGION)) + geom_bar()

## lets review ##
str(df_3_cli_address_clean)
summary(df_3_cli_address_clean)



### df_4_cli_privacy ###

## first look ##
str(df_4_cli_privacy)
summary(df_4_cli_privacy)

## cleaning ##
df_4_cli_privacy_clean <- df_4_cli_privacy

# formatting boolean into facotr
df_4_cli_privacy_clean <- df_4_cli_privacy_clean %>%
  mutate(FLAG_PRIVACY_1 = as.factor(FLAG_PRIVACY_1)) %>%
  mutate(FLAG_PRIVACY_2 = as.factor(FLAG_PRIVACY_2)) %>%
  mutate(FLAG_DIRECT_MKT = as.factor(FLAG_DIRECT_MKT))

## explore distributions ##
# FLAG_PRIVACY_1
df_4_cli_privacy_clean %>%
  group_by(FLAG_PRIVACY_1) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

ggplot(df_4_cli_privacy_clean, aes(x=FLAG_PRIVACY_1)) + geom_bar()

# FLAG_PRIVACY_2
df_4_cli_privacy_clean %>%
  group_by(FLAG_PRIVACY_2) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

ggplot(df_4_cli_privacy_clean, aes(x=FLAG_PRIVACY_2)) + geom_bar()

# FLAG_DIRECT_MKT
df_4_cli_privacy_clean %>%
  group_by(FLAG_DIRECT_MKT) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

ggplot(df_4_cli_privacy_clean, aes(x=FLAG_DIRECT_MKT)) + geom_bar()

df_4_cli_privacy_clean %>%
  group_by(FLAG_PRIVACY_1, FLAG_PRIVACY_2, FLAG_DIRECT_MKT) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

## lets review ##
str(df_4_cli_privacy_clean)
summary(df_4_cli_privacy_clean)


### df_5_camp_cat ###

## first look ##
str(df_5_camp_cat)
summary(df_5_camp_cat)

## cleaning ##
df_5_camp_cat_clean <- df_5_camp_cat

# the field CHANNEL_CAMP has one value <-- is not relevant
df_5_camp_cat_clean <- df_5_camp_cat_clean %>%
  select(-CHANNEL_CAMP)

## explore distributions ##
# FLAG_DIRECT_MKT
df_5_camp_cat_clean %>%
  group_by(TYP_CAMP) %>%
  summarize(TOT_CAMPs = n_distinct(ID_CAMP)) %>%
  mutate(PERCENT = TOT_CAMPs/sum(TOT_CAMPs)) %>%
  arrange(desc(PERCENT))

ggplot(df_5_camp_cat_clean, aes(x=TYP_CAMP)) + geom_bar()



### df_6_camp_event ###

## first look ##
str(df_6_camp_event)
summary(df_6_camp_event)

## cleaning ##
df_6_camp_event_clean <- df_6_camp_event

# despite the field EVENT_TIME is datetime, we just need the corresponding dates
df_6_camp_event_clean <- df_6_camp_event_clean %>%
  mutate(EVENT_DATE = as.Date(EVENT_DATE, format="%Y-%m-%dT%H:%M:%S"))

# for the purpose of the analysis we are delivering here it would make no difference distinguish "ERRORS" and "BOUNCE"
# lets combine them into a common category "FAILURE" with "F" as EVENT_CODE before changing the field to factor
df_6_camp_event_clean <- df_6_camp_event_clean %>%
  mutate(TYP_EVENT = as.factor(if_else(TYP_EVENT == "E" | TYP_EVENT == "B", "F", as.character(TYP_EVENT))))

## explore distributions ##
# type event
df_6_camp_event_clean %>%
  group_by(TYP_EVENT) %>%
  summarize(TOT_EVENTs = n_distinct(ID_EVENT), TOT_CLIs = n_distinct(ID_CLI), TOT_CAMPs = n_distinct(ID_CAMP)) %>%
  mutate(PERCENT_EVENT = TOT_EVENTs/sum(TOT_EVENTs), PERCENT_CLI = TOT_CLIs/sum(TOT_CLIs), PERCENT_CAMP = TOT_CAMPs/sum(TOT_CAMPs)) %>%
  arrange(desc(PERCENT_EVENT), desc(PERCENT_EVENT), desc(PERCENT_CAMP))

ggplot(df_6_camp_event_clean %>% select(TYP_EVENT, ID_EVENT) %>% distinct(), aes(x=TYP_EVENT)) + geom_bar()
ggplot(df_6_camp_event_clean %>% select(TYP_EVENT, ID_CLI) %>% distinct(), aes(x=TYP_EVENT)) + geom_bar()
ggplot(df_6_camp_event_clean %>% select(TYP_EVENT, ID_CAMP) %>% distinct(), aes(x=TYP_EVENT)) + geom_bar()

# min - max dates
df_6_camp_event_clean %>% summarize(MIN_DATE = min(EVENT_DATE), MAX_DATE = max(EVENT_DATE))

#### DATA PREPARATION ####
# the aim is to create what we need for our model.

# first we explore the distribution

df_6_camp_event_clean_w_type <- df_6_camp_event_clean %>%
  left_join(df_5_camp_cat_clean
            , by = "ID_CAMP")

# send
df_sents <- df_6_camp_event_clean_w_type %>%
  filter(TYP_EVENT == "S") %>%
  select(-TYP_EVENT) %>%
  select(ID_EVENT_S = ID_EVENT, ID_CLI, ID_CAMP, TYP_CAMP, ID_DELIVERY, SEND_DATE = EVENT_DATE)

# open
df_opens <- df_6_camp_event_clean_w_type %>%
  filter(TYP_EVENT == "V") %>%
  select(-TYP_EVENT) %>%
  select(ID_EVENT_O = ID_EVENT, ID_CLI, ID_CAMP, TYP_CAMP, ID_DELIVERY, OPEN_DATE = EVENT_DATE) %>%
  group_by(ID_CLI, ID_CAMP, ID_DELIVERY) %>%
  filter(OPEN_DATE == min(OPEN_DATE)) %>%
  filter(row_number() == 1) %>%
  ungroup()

# click
df_clicks <- df_6_camp_event_clean_w_type %>%
  filter(TYP_EVENT == "C") %>%
  select(-TYP_EVENT) %>%
  select(ID_EVENT_C = ID_EVENT, ID_CLI, ID_CAMP, TYP_CAMP, ID_DELIVERY, CLICK_DATE = EVENT_DATE) %>%
  group_by(ID_CLI, ID_CAMP, ID_DELIVERY) %>%
  filter(CLICK_DATE == min(CLICK_DATE)) %>%
  filter(row_number() == 1) %>%
  ungroup()

# failure
df_fails <- df_6_camp_event_clean_w_type %>%
  filter(TYP_EVENT == "F") %>%
  select(-TYP_EVENT) %>%
  select(ID_EVENT_F = ID_EVENT, ID_CLI, ID_CAMP, TYP_CAMP, ID_DELIVERY, FAIL_DATE = EVENT_DATE) %>%
  group_by(ID_CLI, ID_CAMP, ID_DELIVERY) %>%
  filter(FAIL_DATE == min(FAIL_DATE)) %>%
  filter(row_number() == 1) %>%
  ungroup()

# attach send to open
df_sents_w_open <- df_sents %>%
  left_join(df_opens
            , by = c("ID_CLI", "ID_CAMP", "ID_DELIVERY", "TYP_CAMP")
            ) %>%
  filter(is.na(OPEN_DATE) | SEND_DATE <= OPEN_DATE) %>%
  mutate(DIFF = as.integer(OPEN_DATE - SEND_DATE))

# number of sents without opens
df_sents_w_open %>%
  group_by(w_open = !is.na(DIFF)) %>%
  summarize(TOT_SENTs = n_distinct(ID_EVENT_S)) %>%
  mutate(PERCENT = TOT_SENTs/sum(TOT_SENTs)) %>%
  arrange(desc(PERCENT))

ggplot(df_sents_w_open, aes(x=!is.na(DIFF))) + geom_bar()

# distribution days opens
df_sents_w_open %>% filter(!is.na(DIFF)) %>%
  group_by(DIFF) %>%
  summarize(TOT_EVENTs = n_distinct(ID_EVENT_S)) %>%
  arrange(DIFF) %>%
  mutate(PERCENT_COVERED = cumsum(TOT_EVENTs)/sum(TOT_EVENTs))

ggplot(df_sents_w_open %>% filter(!is.na(DIFF)) %>%
         group_by(DIFF) %>%
         summarize(TOT_EVENTs = n_distinct(ID_EVENT_S)) %>%
         arrange(DIFF) %>%
         mutate(PERCENT_COVERED = cumsum(TOT_EVENTs)/sum(TOT_EVENTs)) %>%
         filter(DIFF <= 14)
       , aes(y=PERCENT_COVERED, x=DIFF)) + geom_line() + geom_point() + scale_x_continuous(breaks=seq(0,14,2), minor_breaks=0:14)

# we can choose as window function 2 day
window_days <- 2

### construction of the datamart ###

# our target variable will be if a send event is open within the timespan of the window days

target_event <- df_sents_w_open %>%
  mutate(TARGET = as.factor(if_else(!is.na(DIFF) & DIFF <= window_days, "1", "0"))) %>%
  select(ID_EVENT_S, ID_CLI, ID_CAMP, ID_DELIVERY, SEND_DATE, TARGET)

# some relavant variable we want to include are:
# - average open rate (within 14 days) of the communications received by the client in the 30 days before the sent
# - average click-through (within 14 days) rate of the communications received by the client in the 30 days before the sent

# in order to have comparable situation we are considering:
# - targeted sent made after the 2019-02-01 and window_days before 2019-04-30
# - targeted sent to clients registered by at least 30 days

rate_window <- 14
prev_window <- 30

dt_start <- as.Date("2019-02-01")
dt_end <- as.Date("2019-04-30") - window_days

relevant_event <- df_sents %>%
  left_join(df_opens
            , by = c("ID_CLI", "ID_CAMP", "ID_DELIVERY", "TYP_CAMP")
  ) %>%
  filter(is.na(OPEN_DATE) | SEND_DATE <= OPEN_DATE) %>%
  left_join(df_clicks
            , by = c("ID_CLI", "ID_CAMP", "ID_DELIVERY", "TYP_CAMP")
  ) %>%
  filter(is.na(CLICK_DATE) | SEND_DATE <= CLICK_DATE) %>%
  left_join(df_fails
            , by = c("ID_CLI", "ID_CAMP", "ID_DELIVERY", "TYP_CAMP")
  ) %>%
  filter(is.na(FAIL_DATE) | SEND_DATE <= FAIL_DATE) %>%
  mutate(DIFF_OPEN = as.integer(OPEN_DATE - SEND_DATE)) %>%
  mutate(DIFF_CLICK = as.integer(CLICK_DATE - SEND_DATE)) %>%
  filter(is.na(DIFF_OPEN) | DIFF_OPEN < rate_window) %>%
  filter(is.na(DIFF_CLICK) | DIFF_CLICK < rate_window)

names(relevant_event) <- sapply(names(relevant_event), paste0, "_PREV")

target_event_w_prev <- target_event %>% filter(SEND_DATE >= dt_start & SEND_DATE <= dt_end) %>%
  left_join(relevant_event
            , by = c("ID_CLI" = "ID_CLI_PREV")
            ) %>%
  filter(is.na(SEND_DATE_PREV) | (SEND_DATE_PREV < SEND_DATE & SEND_DATE <= SEND_DATE_PREV + prev_window)) %>%
  mutate(OPENED = if_else(OPEN_DATE_PREV <= SEND_DATE & SEND_DATE <= OPEN_DATE_PREV + prev_window, 1, 0)) %>%
  mutate(CLICKED = if_else(CLICK_DATE_PREV <= SEND_DATE & SEND_DATE <= CLICK_DATE_PREV + prev_window, 1, 0)) %>%
  mutate(FAILED = if_else(!is.na(ID_EVENT_F_PREV), 1, 0)) %>%
  group_by(ID_EVENT_S, ID_CLI, ID_CAMP, ID_DELIVERY, SEND_DATE,  TARGET) %>%
  summarize(NUM_SEND_PREV = n_distinct(ID_EVENT_S_PREV, na.rm = T)
            , NUM_OPEN_PREV = sum(OPENED, na.rm = T)
            , NUM_CLICK_PREV = sum(CLICKED, na.rm = T)
            , NUM_FAIL_PREV = sum(FAILED, na.rm = T)
            ) %>%
  ungroup() %>%
  mutate(OPEN_RATE_PREV = NUM_OPEN_PREV/NUM_SEND_PREV) %>%
  mutate(CLICK_RATE_PREV = NUM_CLICK_PREV/NUM_OPEN_PREV) %>%
  mutate(W_SEND_PREV = as.factor(NUM_SEND_PREV > 0)) %>%
  mutate(W_FAIL_PREV = as.factor(NUM_FAIL_PREV > 0)) %>%
  mutate(SEND_WEEKDAY = as.factor(weekdays(SEND_DATE))) %>%
  mutate(OPEN_RATE_PREV = if_else(is.na(OPEN_RATE_PREV), 0, OPEN_RATE_PREV)) %>%
  mutate(CLICK_RATE_PREV = if_else(is.na(CLICK_RATE_PREV), 0, CLICK_RATE_PREV))

# add client data

df_master <- target_event_w_prev %>%
  left_join(df_1_cli_fid_clean %>%
              select(ID_CLI, ID_NEG, TYP_CLI_FID, COD_FID, STATUS_FID, FIRST_DT_ACTIVE, NUM_FIDs)
            , by = "ID_CLI") %>%
  filter(FIRST_DT_ACTIVE <= SEND_DATE) %>%
  # filter(FIRST_DT_ACTIVE <= SEND_DATE - 30) %>%
  mutate(AGE_FID = as.integer(SEND_DATE - FIRST_DT_ACTIVE)) %>%
  left_join(df_2_cli_account_clean
            , by = "ID_CLI") %>%
  left_join(df_3_cli_address_clean %>%
              select(ID_ADDRESS, PRV, REGION)
            , by = "ID_ADDRESS") %>%
  left_join(df_4_cli_privacy_clean
            , by = "ID_CLI") %>%
  mutate(PRV = fct_explicit_na(PRV)) %>%
  mutate(REGION = fct_explicit_na(REGION)) %>%
  select(-ID_ADDRESS, -ID_CLI, -ID_CAMP, -ID_DELIVERY, -SEND_DATE, -FIRST_DT_ACTIVE)

# check there are not duplicates
df_master %>%
  group_by(ID_EVENT_S) %>% 
  summarize(num = n()) %>% 
  group_by(num) %>%
  count()

#### DATA ESPLORATION ####

# lets see the frequency of the event
df_master %>%
  group_by(TARGET) %>%
  summarize(NUM_EVENTs = n_distinct(ID_EVENT_S))

df_master %>%
  group_by(TARGET,  W_SEND_PREV) %>%
  summarize(NUM_EVENTs = n_distinct(ID_EVENT_S), mean_OR = mean(OPEN_RATE_PREV, na.rm = T))

str(df_master)
summary(df_master)


library(tidyr) # needed for the pivot function spread()

prepare_chisq <- function(df, x){
  y <- enquo(x)
  

  test_df <- df %>%
    mutate(KEY = if_else(TARGET == "1", "OK", "KO")) %>%
    select(UQ(y), KEY, ID_EVENT_S) %>%
    group_by(UQ(y), KEY) %>%
    summarize(n = n()) %>%
    spread(KEY, n) %>%
    ungroup() %>%
    as.data.frame()

  test_m <- test_df %>%
    select(OK, KO) %>%
    mutate(OK = if_else(is.na(OK), as.integer(0), OK)) %>%
    mutate(KO = if_else(is.na(KO), as.integer(0), KO)) %>%
    as.matrix() 
  row.names(test_m) <- as.character(test_df[,1])

  return(test_m)
}

plot_factor <- function(df, x, lab){
  y <- enquo(x)

  df_count_tot <- df %>%
  group_by(UQ(y)) %>%
  summarise(n_tot = n_distinct(ID_EVENT_S)) %>%
  ungroup()

  df_count <- df %>%
  group_by(UQ(y), TARGET) %>%
  summarise(n = n_distinct(ID_EVENT_S))

  df <- df_count %>%
  left_join(df_count_tot, by = lab) %>%
  mutate(frac = round(n / n_tot, 2))

  ggplot(data=df, aes(x=UQ(y), y=frac, fill=TARGET)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text(aes(x=UQ(y), y=frac, label = frac),
            position = position_dodge(width = 1),
            vjust = 2, size = 3, color = "white", fontface = "bold")
}