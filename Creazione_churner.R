require(tidyr)
require(dplyr)

set.seed(12345)

setwd("/home/pranav/Desktop/web marketing/datasets/")

df_1_cli_fid <- read.csv2("raw_1_cli_fid.csv", na.strings = c("NA", ""))
df_2_cli_account <- read.csv2("raw_2_cli_account.csv", na.strings = c("NA", ""))
df_3_cli_address <- read.csv2("raw_3_cli_address.csv", na.strings = c(""), stringsAsFactors = F)
df_4_cli_privacy <- read.csv2("raw_4_cli_privacy.csv" , na.strings = c("NA", ""))
df_7_scrontrini <- read.csv2("raw_7_tic.csv")

df_7_scrontrini$DATETIME <- as.Date(df_7_scrontrini$DATETIME)
df_7_scrontrini$IMPORTO_LORDO <- as.numeric(gsub(",",".",df_7_scrontrini$IMPORTO_LORDO))
head(df_7_scrontrini)
#data inizio raccolta: 2018-05-01, data fine raccolta: 2019-04-30

df_7_scrontrini %>%
  group_by(ID_CLI) %>%
  summarize(LAST_PURCHASE_DATE = max(DATETIME),
            TOTAL_PURCHASE = sum(IMPORTO_LORDO),
            NUMBER_OF_PURCHASE=n()) %>%
  mutate(CHURN = as.numeric(LAST_PURCHASE_DATE < as.Date("2019-02-16"))) %>%
  select(CHURN,ID_CLI,LAST_PURCHASE_DATE,TOTAL_PURCHASE,NUMBER_OF_PURCHASE)-> df_churn

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





df_2_cli_account_clean <- df_2_cli_account

## correct NA in categories ##
# we make use of the package forcats
require(forcats)

df_2_cli_account_clean <- df_2_cli_account_clean %>%
  mutate(EMAIL_PROVIDER = fct_explicit_na(EMAIL_PROVIDER, "(missing)"))


# too many missing values for EMAIL_PROVIDER to be an useful category
# keep the most frequent values and (missing) while changing the remaing into "OTHER"
freq_email_providers <- df_2_cli_account_clean %>%
  group_by(EMAIL_PROVIDER) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT)) %>%
  mutate(PERCENT_COVERED = cumsum(TOT_CLIs)/sum(TOT_CLIs))

clean_email_providers <- freq_email_providers %>%
  mutate(EMAIL_PROVIDER = as.character(EMAIL_PROVIDER)) %>%
  mutate(AUX = if_else(PERCENT_COVERED < 0.85 | (PERCENT_COVERED > 0.85 & lag(PERCENT_COVERED) < 0.85), 1,0)) %>%
  mutate(EMAIL_PROVIDER_CLEAN = if_else(AUX | EMAIL_PROVIDER == "(missing)", EMAIL_PROVIDER, "others"))

df_2_cli_account_clean <- df_2_cli_account %>%
  mutate(EMAIL_PROVIDER = as.character(EMAIL_PROVIDER)) %>%
  left_join(clean_email_providers %>%
              select(EMAIL_PROVIDER, EMAIL_PROVIDER_CLEAN)
            , by = "EMAIL_PROVIDER") %>%
  select(-EMAIL_PROVIDER,-TYP_JOB) %>%
  mutate(EMAIL_PROVIDER_CLEAN = as.factor(EMAIL_PROVIDER_CLEAN))%>%
  mutate(EMAIL_PROVIDER_CLEAN = fct_explicit_na(EMAIL_PROVIDER_CLEAN, "(missing)"))%>%
  mutate(W_PHONE = as.factor(W_PHONE)) %>%
  mutate(W_PHONE = fct_explicit_na(W_PHONE, "0"))


df_3_cli_address_clean <- df_3_cli_address

## convert PRV e REGION into factors ##
df_3_cli_address_clean <- df_3_cli_address_clean %>%
  mutate(PRV = as.factor(PRV)) %>%
  mutate(REGION = as.factor(REGION)) %>%
  distinct()

# drop the record without CAP - PRV - REGION
df_3_cli_address_clean <- df_3_cli_address_clean %>%
  filter(!is.na(CAP) & !is.na(PRV) & !is.na(REGION))



df_4_cli_privacy_clean <- df_4_cli_privacy

# formatting boolean into factor
df_4_cli_privacy_clean <- df_4_cli_privacy_clean %>%
  mutate(FLAG_PRIVACY_1 = as.factor(FLAG_PRIVACY_1)) %>%
  mutate(FLAG_PRIVACY_2 = as.factor(FLAG_PRIVACY_2)) %>%
  mutate(FLAG_DIRECT_MKT = as.factor(FLAG_DIRECT_MKT))


df_master <- df_1_cli_fid_clean %>%
  select(ID_CLI, ID_NEG, TYP_CLI_FID, COD_FID, STATUS_FID) %>%
  left_join(df_2_cli_account_clean
            , by = "ID_CLI") %>%
  left_join(df_3_cli_address_clean %>%
              select(ID_ADDRESS, PRV, REGION)
            , by = "ID_ADDRESS") %>%
  left_join(df_4_cli_privacy_clean
            , by = "ID_CLI") %>%
  select(-ID_ADDRESS)


#Questo è il dataset dei churn finale
df_master_2 <- df_churn %>%
  left_join(df_master, by="ID_CLI")%>%
  mutate(PRV = fct_explicit_na(PRV)) %>%
  mutate(REGION = fct_explicit_na(REGION))

write.csv(df_master_2,"df_master_churner.csv",row.names = FALSE)
