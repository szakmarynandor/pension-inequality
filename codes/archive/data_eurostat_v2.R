library(eurostat)
library(dplyr)
library(tidyr)
library(openxlsx)
library(tidyverse)
library(stringr)
library(readxl)
library(zoo)

#population structure (sex, age, region, 1990-2024)
pop_data <- get_eurostat(id = "demo_r_d2jan", time_format = "date")
pop_data <- pop_data %>% filter(
  sex %in% c("M", "F"),
  geo %in% c("HU1", "HU10", "HU11", "HU12", "HU21", "HU22", "HU23", "HU31", "HU32", "HU33"),
  age != "UNK" & age !="TOTAL") %>% 
  rename(count=values) %>% 
  mutate(cal_year = year(TIME_PERIOD)) %>% 
  select(cal_year, sex, geo, age, count)
pop_data$age[pop_data$age=="Y_LT1"] <- "Y0"
pop_data$age[pop_data$age=="Y_OPEN"] <- "Y100"
pop_data$geo[pop_data$geo=="HU1"] <- "HU10"
pop_data <- distinct(pop_data, .keep_all = T)
pop_data$birth_year <- pop_data$cal_year - as.numeric(str_extract(pop_data$age, "\\d+"))

# employment by sex age and nuts2 regions
emp_data <- get_eurostat(id = "lfst_r_lfe2emp", time_format = "date")

emp_data <- emp_data %>%
  filter(
  geo %in% c("HU1", "HU10", "HU11", "HU12", "HU21", "HU22", "HU23", "HU31", "HU32", "HU33"),
  sex %in% c("M", "F")) %>%
  rename(employed_ths_per=values) %>%
  mutate(cal_year = year(TIME_PERIOD)) %>% 
  pivot_wider(names_from = age, values_from = employed_ths_per) %>% 
  mutate(`Y20-24`=`Y20-64`-`Y25-64`, `Y65-74`=`Y15-74`-`Y15-64`,
         `Y_GE75`=Y_GE65-(`Y15-74`-`Y15-64`), `Y15-19`=`Y15-64`-`Y20-64`) %>%
  select(sex, geo, cal_year, `Y15-19`, `Y20-24`, `Y25-34`, `Y35-44`, `Y45-54`, `Y55-64`,
         `Y65-74`, Y_GE75)
emp_data$geo[emp_data$geo=="HU1"] <- "HU10"
emp_data <- distinct(emp_data, .keep_all = T)

emp_data_long <- emp_data %>% 
  pivot_longer(cols=c(`Y15-19`, `Y20-24`, `Y25-34`, `Y35-44`, `Y45-54`, `Y55-64`, `Y65-74`,
                      Y_GE75), names_to = "age", values_to = "employed_ths_per") %>% 
  mutate(employed=employed_ths_per*1000,
         age_start = as.numeric(str_extract(age, "\\d+")),
         age_end = as.numeric(substr(age, nchar(age)-1, nchar(age)))
        )

#koronkénti foglalkoztatottak száma
emp_data_long$mid_age <- round((emp_data_long$age_start + emp_data_long$age_end)/2, digits = 0)

# születési évekre áttérés
emp_data_long$birth_year <- emp_data_long$cal_year-emp_data_long$mid_age

# Employment rates by sex, age and NUTS 2 regions (%)
emp_rate_data <- get_eurostat(id = "lfst_r_lfe2emprt", time_format = "date")

emp_rate_data <- emp_rate_data %>%
  filter(
    geo %in% c("HU1", "HU10", "HU11", "HU12", "HU21", "HU22", "HU23", "HU31", "HU32", "HU33"),
    sex %in% c("M", "F")) %>% 
  mutate(cal_year = year(TIME_PERIOD),
         emp_rate = values/100,
         age_start = as.numeric(str_extract(age, "\\d+")),
         age_end = as.numeric(substr(age, nchar(age)-1, nchar(age)))) %>%
  filter(age %in% c("Y15-24", "Y25-34", "Y35-44", "Y45-54", "Y55-64", "Y_GE65")) %>% 
  select(sex, geo, cal_year, age, age_start, age_end, emp_rate)

#by age
emp_rate_data$mid_age <- round((emp_rate_data$age_start + emp_rate_data$age_end)/2, digits = 0)
# birth years
emp_rate_data$birth_year <- emp_rate_data$cal_year-emp_rate_data$mid_age
#regional modification
emp_rate_data$geo[emp_rate_data$geo=="HU1"] <- "HU10"
emp_rate_data <- emp_rate_data %>% 
  distinct(.keep_all = T) %>% 
  filter(cal_year!=2001, cal_year!=2011, cal_year!=2016, cal_year!=2022) %>% 
  select(cal_year, geo, birth_year, sex, emp_rate)
  
#seged <- emp_rate_data %>% filter(cal_year %in% c(2011, 2016, 2022), geo %in% c("HU1", "HU10"))

# Mean and median income by age and sex - EU-SILC and ECHP surveys
# inc_data <- get_eurostat(id = "ilc_di03", time_format = "date", filters = list(geo = "HU"))

# # Income of households by NUTS 2 regions 
# hhinc_data <- get_eurostat(id = "nama_10r_2hhinc", time_format = "date")
# 
# hhinc_data <- hhinc_data %>% filter(
#   unit %in% c("MIO_EUR", "MIO_NAC"),
#   geo %in% c("HU10", "HU11", "HU12", "HU21", "HU22", "HU23", "HU31", "HU32", "HU33"),
#   na_item %in% c("D1", "D63", "D5", "D61", "D62")
# )

#Income data from KSH
income_data_path <- "C:/Nandi/BPM/Szakdoga/adatok/KSH/Adki_kor_regio_nem_brutto.xlsx"
# List of sheet names
sheet_names <- c("2019", "2020", "2021", "2022", "2023")
inc_df_list <- list()
# Create data frames for each sheet
for (sheet_name in sheet_names) {
  # Read data from each sheet
  df <- read_excel(income_data_path, sheet = sheet_name)
  
  assign(paste0("inc_", sheet_name), df)
}

# Loop through each data frame and apply pivot_longer
for (sheet_name in sheet_names) {
  # Get the data frame
  df_name <- paste0("inc_", sheet_name)
  df <- get(df_name)
  
  # Apply pivot_longer
  df_long <- pivot_longer(df, cols = -c('cal_year', 'region', 'geo', 'sex'),
                              names_to = "birth_year", values_to = "income")
  
  inc_df_list[[sheet_name]] <- df_long
}

# Combine all data frames into a single data frame
income_df <- do.call(rbind, inc_df_list)
income_df <- income_df %>% select(cal_year, geo, birth_year, sex, income)
# rm(inc_2019, inc_2020, inc_2021, inc_2022, inc_2023, inc_df_list)

#Add estimated income data for years 2003-2018
income_2003_18 <- read_xlsx("C:/Nandi/BPM/Szakdoga/adatok/keresetek/income_by_age_region.xlsx", sheet = "regional") %>% 
  pivot_longer(cols = c('M', 'F'), names_to = 'sex', values_to = 'income') %>% 
  select(cal_year, geo, birth_year, sex, income)
if (sum(colnames(income_df)==colnames(income_2003_18))==ncol(income_df)) {
  income_df <- rbind(income_df, income_2003_18)
  # rm(income_2003_18)
}

# Inflation data for Hungary 2000-2023
inf_data <- read_xlsx("C:/Nandi/BPM/Szakdoga/adatok/inflation_HU.xlsx", sheet = "data")

# Pension data for Hungary 2000-2023
pension_data <- read_xlsx("C:/Nandi/BPM/Szakdoga/adatok/pension_by_region.xlsx")

# Import the masterdata and then fill it up with the available data
master <- read_xlsx("C:/Nandi/BPM/Szakdoga/adatok/thesis_pension.xlsx", sheet = "masterdata")
master <- master %>% 
  mutate(age = cal_year-birth_year) %>% 
  filter(age>=20 & age<=100, geo !="HU11" & geo != "HU12")

#cenzus data for employment
emp_cenzus <- read_xlsx("C:/Nandi/BPM/Szakdoga/adatok/thesis_pension.xlsx", sheet = "emp_rate")

emp_cenzus <- emp_cenzus %>%
  pivot_longer(cols = -c(1:4), names_to = "age", values_to = "emp_rate") %>% 
  mutate(birth_year = cal_year-as.numeric(age)) %>% 
  select(cal_year, geo, birth_year, sex, emp_rate, age)
#emp_cenzus$birth_year <- emp_cenzus$cal_year-as.numeric(emp_cenzus$age)

#Merge data_frames into masterdata
key_cols <- c("cal_year", "geo", "birth_year", "sex")

# Perform rbind for employment data

if (sum(colnames(emp_rate_data)==colnames(emp_cenzus[,-6]))==ncol(emp_rate_data)) {
  emp_rate_df <- rbind(emp_rate_data, emp_cenzus[,-6])
  # rm(income_2003_18)
}

# emp <- merge(emp_rate_data[, c(key_cols, "employment_rate")], emp_cenzus[, c(key_cols, "emp_rate")], by = key_cols, all.x = TRUE)
# emp <- emp %>%
#   mutate(employment_rate = ifelse(!is.na(emp_rate), emp_rate, employment_rate))
# emp$emp_rate <- NULL

merged <- merge(master, emp_rate_df, by = key_cols, all.x = TRUE)
#years with employment rate data
c(min(merged$cal_year[!is.na(merged$emp_rate)]), max(merged$cal_year[!is.na(merged$emp_rate)]))

merged <- merge(merged, income_df, by = key_cols, all.x = TRUE)
# merged <- merge(merged, pop_data[, c(key_cols, "count")], by = key_cols, all.x = TRUE)

# Interpolation of employment data with linear regression
# the estimation can be tried with larger dataset from emp_rate_df

proba <- merged %>%
  mutate(age=cal_year-birth_year) %>% 
  filter(cal_year<= 2022, geo !="HU11", geo != "HU12" & age>=20) %>%
  select(cal_year, geo, birth_year, sex, emp_rate, age) %>%
  arrange(cal_year)

proba$stages <- "1st stage"
proba$stages[proba$age>22] <- "2nd stage"
proba$stages[proba$age>30] <- "3rd stage"
proba$stages[proba$age>40] <- "4th stage"
proba$ls <- "1st"
proba$ls[proba$birth_year>1973] <- "2nd"
proba$ls[proba$birth_year>1983] <- "3rd"

proba[,c('geo','sex', 'stages', 'ls')] <- lapply(proba[,c('geo','sex', 'stages', 'ls')], as.factor)

model <- lm(emp_rate ~ cal_year + geo + sex + age + I(age^2) + sex*age +
              stages + ls + stages*sex*age + ls*cal_year + geo*cal_year, data = proba)
summary(model) #r-squared > 0.96


for (i in 1:nrow(proba)) {
  if (is.na(proba[i,'emp_rate'])) {
    proba[i,'emp_rate'] = min(max(predict(model, newdata = proba[i, c(1,2,4,6,7,8)]), 0.1),0.95)
    }
}

#plot employment rates for every birth year
proba %>% 
  filter(age<=65) %>% 
  ggplot() +
  facet_wrap(~birth_year) +
  geom_line(aes(x=cal_year, y=emp_rate, linetype=sex, color=geo)) + 
  geom_vline(xintercept = c(2001, 2011, 2016, 2022), linetype="dotted", color="blue")

# Avg. yearly relative changes of employment rates by ages, genders and regions
emp_fc_helper <- read_xlsx("C:/Nandi/BPM/Szakdoga/adatok/thesis_pension.xlsx", sheet = "emp_changes_regional_16_22")

# Fill up the database with estimated and partially forecasted employment data
for (i in 1:nrow(merged)) {
  if (merged[i, 'cal_year']<=2022) {
  if (is.na(merged[i, 'emp_rate'])) {
    merged[i, 'emp_rate'] = proba %>%
      filter(cal_year==merged[i, 'cal_year'],
             geo==merged[i, 'geo'],
             birth_year==merged[i, 'birth_year'],
             sex==merged[i, 'sex']) %>% 
      select(emp_rate)
  }
  }
  
  # Estimated employment rates for ages firstly occuring in the model
  else{
  if (merged[i, 'birth_year']==1970 & merged[i, 'cal_year']>=2023 & merged[i, 'cal_year']<=2035) {
    # Most recent factual data for emp rates from 2022
    er_2022 = emp_cenzus %>%
      filter(sex==merged[i, 'sex'],
             cal_year==2022,
             geo==merged[i, 'geo'],
             birth_year==1970) %>% 
      select(emp_rate) %>% as.numeric()
    
    #yearly development based on historical data
    helper = emp_fc_helper %>% 
      filter(sex==merged[i, 'sex'], 
             age==merged[i, 'age']) %>% 
      select(merged[i, 'geo']) %>% 
      as.numeric()
  
  merged[i, 'emp_rate'] = min(0.95, er_2022*helper)
  }
  }
}

#Forecasting future employment rates
sum(is.na(merged$emp_rate[merged$cal_year<2023]))

for (i in 1:nrow(merged)) {
  if (is.na(merged[i, 'emp_rate']) & merged[i, 'cal_year']<=2029) {
    #employment rate from previous calendar year
    prev_er = merged %>%
      filter(cal_year==merged[i, 'cal_year']-1,
             geo==merged[i, 'geo'],
             age==merged[i, 'age'],
             sex==merged[i, 'sex']) %>% 
      select(emp_rate) %>% as.numeric()
    
    #correction factor for the development of employment rates based on historical data
    corr_factor = emp_fc_helper %>% 
      filter(sex==merged[i, 'sex'], 
             age==merged[i, 'age']) %>% 
      select(merged[i, 'geo']) %>% 
      as.numeric()
    
    merged[i, 'emp_rate'] = min(prev_er*max(corr_factor,1), 0.95)
  }
  
  #constant emp rates from 2030
  else{
    if (merged[i, 'cal_year']>2029) {
      merged[i, 'emp_rate'] = merged %>%
        filter(cal_year==merged[i, 'cal_year']-1,
               geo==merged[i, 'geo'],
               age==merged[i, 'age'],
               sex==merged[i, 'sex']) %>% 
        select(emp_rate) %>% as.numeric()
    }
  }
}





# Function to perform linear interpolation within each group
linear_interpolation <- function(df) {
  df$value <- na.approx(df$employment_rate)
  return(df)
}

# Apply linear interpolation within each group
interpolated_panel_data <- proba %>%
  group_by(cal_year, geo, sex) %>%
  do(linear_interpolation(.))

# View the interpolated panel data
print(interpolated_panel_data)

# Update employed column in master only where it is missing (NA)
merged$employment_rate <- ifelse(is.na(merged$employment_rate), merged$employed.y, merged$employment_rate)
# Remove the temporary columns created by merge
merged <- subset(merged, select = -c(employed.x, employed.y))
# Update the master data frame
#master <- merged


merged %>% 
  filter(age<=65) %>% 
  ggplot() +
  facet_wrap(~birth_year) +
  geom_line(aes(x=cal_year, y=emp_rate, linetype=sex, color=geo)) + 
  geom_vline(xintercept = c(2001, 2011, 2016, 2022), linetype="dotted", color="blue")




