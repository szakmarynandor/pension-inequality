library(eurostat)
library(dplyr)
library(tidyr)
library(openxlsx)
library(tidyverse)
library(stringr)
library(readxl)
library(zoo)

#population structure data from Eurostat (by sex, age, region, 1990-2023)
pop_data <- get_eurostat(id = "demo_r_d2jan", time_format = "date")
pop_data <- pop_data %>% filter(
  sex %in% c("M", "F"),
  geo %in% c("HU", "HU1", "HU10", "HU11", "HU12", "HU21", "HU22", "HU23", "HU31", "HU32", "HU33"),
  age != "UNK" & age !="TOTAL") %>% 
  rename(count=values) %>% 
  mutate(cal_year = year(TIME_PERIOD)) %>% 
  select(cal_year, sex, geo, age, count)
pop_data$age[pop_data$age=="Y_LT1"] <- "Y0"
pop_data$age[pop_data$age=="Y_OPEN"] <- "Y100"
pop_data$geo[pop_data$geo=="HU1"] <- "HU10"
pop_data <- distinct(pop_data, .keep_all = T)
pop_data$birth_year <- pop_data$cal_year - as.numeric(str_extract(pop_data$age, "\\d+"))

# Employment rates from Eurostat by sex, age and NUTS 2 regions (%) (1999-2023)
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

#new code for Közép-Magyarország region
emp_rate_data$geo[emp_rate_data$geo=="HU1"] <- "HU10"
emp_rate_data <- emp_rate_data %>% 
  distinct(.keep_all = T) %>% 
  filter(cal_year!=2001, cal_year!=2011, cal_year!=2016, cal_year!=2022) %>% 
  select(cal_year, geo, birth_year, sex, emp_rate)
  
###

#Income data from KSH with interpolated data
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
rm(inc_2019, inc_2020, inc_2021, inc_2022, inc_2023, inc_df_list)

#Import the interpolated income data for years 2003-2018 and add to income_df
income_2003_18 <- read_xlsx("C:/Nandi/BPM/Szakdoga/adatok/keresetek/income_by_age_region.xlsx", sheet = "regional") %>% 
  pivot_longer(cols = c('M', 'F'), names_to = 'sex', values_to = 'income') %>% 
  select(cal_year, geo, birth_year, sex, income)
if (sum(colnames(income_df)==colnames(income_2003_18))==ncol(income_df)) {
  income_df <- rbind(income_df, income_2003_18)
  print("OK")
  # rm(income_2003_18)
}

# Inflation data for Hungary 2000-2023
inf_data <- read_xlsx("C:/Nandi/BPM/Szakdoga/adatok/inflation_HU.xlsx", sheet = "data")

# Import the masterdata file and then fill it up with the available data
master <- read_xlsx("C:/Nandi/BPM/Szakdoga/adatok/thesis_pension.xlsx", sheet = "masterdata")
master <- master %>% 
  mutate(age = cal_year-birth_year) %>% 
  filter(age>=20 & age<=100, geo !="HU11" & geo != "HU12")

#cenzus data for employment
emp_cenzus <- read_xlsx("C:/Nandi/BPM/Szakdoga/adatok/thesis_pension.xlsx", sheet = "emp_rate")

emp_cenzus <- emp_cenzus %>%
  pivot_longer(cols = -c(1:4), names_to = "age", values_to = "emp_rate") %>% 
  mutate(birth_year = cal_year-as.numeric(age)) %>% 
  filter(geo!="HU11", geo!="HU12") %>%
  select(cal_year, geo, birth_year, sex, emp_rate, age)

#Merge data_frames into masterdata, define key columns:
key_cols <- c("cal_year", "geo", "birth_year", "sex")

# Perform rbind for employment rate data
if (sum(colnames(emp_rate_data)==colnames(emp_cenzus[,-6]))==ncol(emp_rate_data)) {
  emp_rate_df <- rbind(emp_rate_data, emp_cenzus[,-6])
  # rm(emp_cenzus, emp_rate_data)
}

#Merge employment rates into masterdata structure
merged <- merge(master, emp_rate_df, by = key_cols, all.x = TRUE)
#earliest and latest year with employment rate data
c(min(merged$cal_year[!is.na(merged$emp_rate)]), max(merged$cal_year[!is.na(merged$emp_rate)]))

#Merge incomes into masterdata structure
merged <- merge(merged, income_df, by = key_cols, all.x = TRUE)
# merged <- merge(merged, pop_data[, c(key_cols, "count")], by = key_cols, all.x = TRUE)

# Interpolation of employment data with regression for calendar years lower than 2023
proba <- merged %>%
  mutate(age=cal_year-birth_year) %>% 
  filter(cal_year<= 2022, geo !="HU11", geo != "HU12" & age>=20) %>%
  select(cal_year, geo, birth_year, sex, emp_rate, age) %>%
  arrange(cal_year)

# Define new variables
proba$stages <- "1st stage"
proba$stages[proba$age>22] <- "2nd stage"
proba$stages[proba$age>29] <- "3rd stage"
proba$stages[proba$age>39] <- "4th stage"
proba$cohorts <- "A"
proba$cohorts[proba$birth_year>1973] <- "B"
proba$cohorts[proba$birth_year>1982] <- "C"

proba[,c('geo','sex', 'stages', 'cohorts')] <- lapply(proba[,c('geo','sex', 'stages', 'cohorts')], as.factor)

# Model to estimate missing employment rates
model <- lm(emp_rate ~ cal_year + geo + sex + age + I(age^2) + sex*age +
              stages + sex*stages*age + cal_year*geo + cohorts*cal_year, data = proba)
summary(model) #r-squared ~ 0.97
stargazer::stargazer(model, type = "latex")

# Prediction and  imputation of missing emp_rates
for (i in 1:nrow(proba)) {
  if (is.na(proba[i,'emp_rate'])) {
    proba[i,'emp_rate'] = min(max(predict(model, newdata = proba[i, c(1,2,4,6,7,8)]), 0.1),0.95)
    }
}

#plot employment rates for every birth year (cal_year<2023)
proba %>% 
  filter(age<65) %>% 
  ggplot() +
  facet_wrap(~birth_year) +
  geom_line(aes(x=cal_year, y=emp_rate, linetype=sex, color=geo)) + 
  geom_vline(xintercept = c(2001, 2011, 2016, 2022), linetype="dotted", color="blue")

# Avg. yearly relative changes of employment rates by ages, genders and regions
# (based on development between 2016-2022)
emp_fc_helper <- read_xlsx("C:/Nandi/BPM/Szakdoga/adatok/thesis_pension.xlsx", sheet = "emp_changes_regional_16_22")

# Fill up the merged database with estimated and partially forecasted employment data
for (i in 1:nrow(merged)) {
  if (is.na(merged[i, 'emp_rate']) & merged[i, 'cal_year']<=2022) {
    merged[i, 'emp_rate'] = proba %>%
      filter(cal_year==merged[i, 'cal_year'],
             geo==merged[i, 'geo'],
             birth_year==merged[i, 'birth_year'],
             sex==merged[i, 'sex']) %>% 
      select(emp_rate)
  }
  
# Estimated employment rates for ages firstly occuring in the model
  if (merged[i, 'birth_year']==1970 & merged[i, 'cal_year']>=2023 & merged[i, 'cal_year']<2035) {
    # Most recent factual data for emp rates from 2022
    er_2022 = emp_cenzus %>%
      filter(sex==merged[i, 'sex'],
             cal_year==2022,
             geo==merged[i, 'geo'],
             age==merged[i, 'age']) %>% 
      select(emp_rate) %>% as.numeric()
    
    #yearly development based on historical data
    helper = emp_fc_helper %>% 
      filter(sex==merged[i, 'sex'], 
             age==merged[i, 'age']) %>% 
      select(merged[i, 'geo']) %>% 
      as.numeric()
  
  merged[i, 'emp_rate'] = min(0.9, er_2022*min(sqrt(helper)^(min(merged[i, 'cal_year']-2022, 7)),
                                               ifelse(merged[i, 'age']>=63 & merged[i, 'sex']=="M",
                                                      1.25, 1.75)))
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
    if (merged[i, 'cal_year']>2029 & is.na(merged[i, 'emp_rate'])) {
      merged[i, 'emp_rate'] = merged %>%
        filter(cal_year==merged[i, 'cal_year']-1,
               geo==merged[i, 'geo'],
               age==merged[i, 'age'],
               sex==merged[i, 'sex']) %>% 
        select(emp_rate) %>% as.numeric()
  }
}

# Plot of emp_rates by birth_years
merged %>% 
  filter(age<65) %>% 
  ggplot() +
  facet_wrap(~birth_year) +
  annotate("rect", xmin=1999, xmax=2022, ymin=0, ymax=Inf, alpha=0.2, fill="darkgreen") +
  geom_line(aes(x=cal_year, y=emp_rate, linetype=sex, color=geo)) + 
  geom_vline(xintercept = c(2001, 2011, 2016, 2022), linetype="dotted", color="black")


# Extrapolation of income data:
# avg. of regional deviances from national wage indices based on 2017-2023 data * national
# wage index for years 2024-2029; from 2030 (and before 2003) each region's wages develop the same

# regional wage indices
wage_index <- read_xlsx("C:/Nandi/BPM/Szakdoga/adatok/inflation_HU.xlsx", sheet = "regional_wi")

income_df$birth_year <- as.numeric(income_df$birth_year)
income_df$age <- income_df$cal_year-income_df$birth_year

# Backward extrapolation
for (y in 2002:1990) {
  for (j in which(merged$cal_year==y)) {
    #if (is.na(merged[j, 'income'])) {
    #Earliest available income data from 2003
    inc = income_df %>%
      filter(sex==merged[j, 'sex'],
             cal_year==2003,
             geo==merged[j, 'geo'],
             age==merged[j, 'age']) %>% 
      select(income) %>% as.numeric()
    
    # Discount factor
    wi = wage_index %>% 
      filter(cal_year<=2003 & cal_year>merged[j, 'cal_year']) %>% 
      select(merged[j, 'geo']) %>% 
      prod()
    
    merged[j, 'income'] = inc/wi
    #print(wi)
    #}
  }
}

# Forward extrapolation of incomes
for (y in 2024:2049) {
  for (i in which(merged$cal_year==y)) {
    
    if (is.na(merged[i, 'income'])) {
      # Estimated incomes for ages firstly occuring in the model
      if (merged[i, 'birth_year']==1970 & merged[i,'age']<65) {
        # Most recent factual data for incomes from 2023
        inc = income_df %>%
          filter(sex==merged[i, 'sex'],
                 cal_year==2023,
                 geo==merged[i, 'geo'],
                 age==merged[i, 'age']) %>% 
          select(income) %>% as.numeric()
        
        #wage index factor based on historical data
        wi = wage_index %>% 
          filter(cal_year>2023, cal_year<=merged[i, 'cal_year']) %>% 
          select(merged[i, 'geo']) %>% 
          prod()
        
        # Income value to the data frame
        merged[i, 'income'] = inc*wi
      
      } 
      
      if (merged[i, 'birth_year']!=1970) {
        # Income data from previous year for the same sex, age and region
        inc = merged %>%
          filter(sex==merged[i, 'sex'],
                 cal_year==merged[i, 'cal_year']-1,
                 geo==merged[i, 'geo'],
                 age==merged[i, 'age']) %>% 
          select(income) %>% as.numeric()
        
        #wage index factor
        wi = wage_index %>% 
          filter(cal_year==merged[i, 'cal_year']) %>% 
          select(merged[i, 'geo']) %>% 
          as.numeric()
        
        # Income value to the data frame
        merged[i, 'income'] = inc*wi
      }
      
    }
  }
}

sum(is.na(merged$income[merged$age<65]))

# Plot of incomes by birth_years
merged %>% 
  filter(age<65) %>% 
  ggplot() +
  facet_wrap(~birth_year) +
  annotate("rect", xmin=2003, xmax=2018, ymin=0, ymax=Inf, alpha=0.3, fill="red") +
  annotate("rect", xmin=2018, xmax=2023, ymin=0, ymax=Inf, alpha=0.3, fill="green") +
  geom_line(aes(x=cal_year, y=income, linetype=sex, color=geo))

#Pension data from KSH
# List of sheet names
sheets <- c("Közép-Magyarország", "Nyugat-Dunántúl", "Közép-Dunántúl", "Dél-Dunántúl",
                 "Észak-Magyarország", "Észak-Alföld", "Dél-Alföld")
geos <- c("HU10", "HU22", "HU21", "HU23", "HU31", "HU32", "HU33")
# Create data frames for each sheet
first_pensions <- data.frame(age = numeric(), sex = character(), avg_pension = numeric(),
                             geo = character(), cal_year = as.numeric())

for (y in 2012:2023) {
  # Read data from each sheet
  for (sh in 1:length(sheets)) {
    
  df <- read_excel(paste0("C:/Nandi/BPM/Szakdoga/adatok/KSH/", y, "önyd_kész.xlsx"), sheet = sheets[sh], range = "G3:I16")
  df <- pivot_longer(df, cols = c('M', 'F'), names_to = "sex", values_to = "avg_pension")
  df$geo <- geos[sh]
  df$cal_year <- y
  #assign(paste0("pension_", y, "_", sh), df)
  
  first_pensions <- rbind(first_pensions, df)
  
  }
}

#Male pensions by regions at age 65
first_pensions %>% 
  filter(age==65, sex=="M") %>% 
  ggplot() +
  geom_line(aes(x=cal_year, y=avg_pension, color=geo)) +
  labs(title="Average old-age pension of males at age 65")

#Pensions by regions at ages 60-65 for women and at age 65 for men
col1 = "#013C58"
col2 = "#E69C38"
col3 = "#800000"
col4 = "#808000"
col5 = "#104C00"
col6 = "#7195C1"
col7 = "#996633"
col8 = "#E6E122"
szinek <- c(col1, col2, col3, col4, col5, col6, col7, col8)

first_pensions %>% 
  filter(age<65 & age>=60 & sex=="F" | age==65) %>% 
  ggplot() +
  facet_wrap(~age) +
  geom_hline(yintercept = c(seq(100, 300, 50)), col="lightgrey", linetype="dashed")+
  geom_line(aes(x=cal_year, y=avg_pension/1000, color=geo, linetype = sex)) +
  theme(legend.title = element_blank(), text=element_text(size=14, family="serif"),
        panel.background = element_rect(fill = "white"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.line = element_line(),
        legend.key = element_rect(fill = "white")) +
  scale_color_manual(values = szinek[1:7]) +
  scale_linetype_manual(values=c(1,2), labels = c("Nő","Férfi")) +
  labs( y="Átlagos öregségi nyugdíj (ezer Ft)")
  
  

#Real starting pension changes
firstpen_index <- first_pensions %>%
  filter(age==65 | sex=="F" & age<65 & age>=60) %>% 
  pivot_wider(names_from = c(geo, sex, age), values_from = avg_pension)

firstpen_index <- left_join(firstpen_index, inf_data[,c('cal_year', 'inflation')], by = 'cal_year')

realvaltozasok <- as.data.frame(matrix(NA, nrow = nrow(firstpen_index)-1,
                                       ncol = ncol(firstpen_index)-1))
colnames(realvaltozasok) <- colnames(firstpen_index[,-51])
realvaltozasok$cal_year <- 2013:2023

for (i in 1:(ncol(realvaltozasok)-1)) {
 for (j in 1:(nrow(realvaltozasok))) {
   realvaltozasok[j,i+1] <- 1+firstpen_index[j+1, i+1]/firstpen_index[j, i+1]-firstpen_index[j+1, 'inflation']
 } 
}
# avg real first pension increases
avg_real_pi <- matrix(NA,ncol = ncol(realvaltozasok)-1)
colnames(avg_real_pi) <- colnames(realvaltozasok[,-1])
for (i in 1:ncol(avg_real_pi)) {
  avg_real_pi[1,i] <- prod(realvaltozasok[,i+1])^(1/(length(realvaltozasok[,i+1])))
}

c(mean(avg_real_pi), min(avg_real_pi), quantile(avg_real_pi,probs = 0.25), median(avg_real_pi), quantile(avg_real_pi,probs = 0.75), max(avg_real_pi))

# Estimation of future first pensions based on historical real first pension increases

#National data
pension_fc_m <- read_xlsx("C:/Nandi/BPM/Szakdoga/adatok/KSH/orszagos_koreves.xlsx",
                          sheet = "male_65",col_names = T, range = "A1:F13")
pension_fc_m %>%
  ggplot() +
  geom_line(aes(x=cal_year, y=avg_pension))+
  geom_line(aes(x=cal_year, y=income, colour = "red"))

#Model for forecasting pensions at age 65 for males
cor(pension_fc_m[,c(4:6)]) # previous year's income is dominant
male_pension_mod <- lm(log(avg_pension) ~ income, data = pension_fc_m)
summary(male_pension_mod)

pension_fc_m <- rbind(pension_fc_m,
                      data.frame(cal_year=2024:2050,
                                 sex="M",
                                 age=65,
                                 avg_pension=NA,
                                 inflation=inf_data[inf_data$cal_year>=2024 & inf_data$cal_year<=2050, 'inflation'],
                                 income=NA))
# Replacement rates
# rr_m <- mean(0.57,	0.52,	0.55) #EU-SILC survey [ilc_pnp3] 2021-2023
# rr_f <- mean(0.54,	0.54,	0.51)

early_f_helper <- read_xlsx("C:/Nandi/BPM/Szakdoga/adatok/KSH/orszagos_koreves.xlsx",
                            sheet = "early",col_names = T)
# Females
pension_fc_f <- read_xlsx("C:/Nandi/BPM/Szakdoga/adatok/KSH/orszagos_koreves.xlsx",
                          sheet = "female_58_65",col_names = T, range = "A1:F97")

pension_helper <- pop_data %>%
  mutate(age = as.numeric(str_extract(age, "\\d+"))) %>% 
  filter(cal_year>=2012 & cal_year<=2023, sex=="F", age>=60 & age<=65, geo=="HU") %>% 
  select(cal_year, birth_year, sex, count, age)

pension_fc_f <- left_join(pension_fc_f, pension_helper, by = c("cal_year", "sex", "age"))
pension_fc_f$retired <- pension_fc_f$num_pensioners/pension_fc_f$count

# Ratio of female pensioner by ages 58-65 from 2012
pension_fc_f %>% 
  filter(age>=60) %>% 
  ggplot() +
  geom_hline(yintercept = c(seq(0, 1, 0.25)), col="lightgrey", linetype="dashed")+
  facet_wrap(~age) +
  geom_line(aes(x=cal_year, y=retired))+
  theme(legend.title = element_blank(), text=element_text(family="serif"),
        panel.background = element_rect(fill = "white"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.line = element_line()) +
  scale_y_continuous(labels = scales::percent_format(accuracy=1, decimal.mark = ",")) +
  labs(y="Nyugdíjasok aránya (%)")
  
# Latest rates (from 2023) will be used for the following years


### Probabilities of being alive for representative individuals
prob_alive <- read_xlsx("C:/Nandi/BPM/Szakdoga/adatok/Mort_1990-2076_Apr12.xlsx",
                        sheet = "input",col_names = T)
probs <- prob_alive %>%
  pivot_longer(cols = -c(1:3), names_to = "birth_year", values_to = "prob_alive")

probs <- probs %>%
  mutate(birth_year=as.numeric(birth_year), age=cal_year-birth_year, geo=as.factor(geo)) %>% 
  filter(age>=20 & age<=100)

#Plot of probabilities of being alive by birth years
probs %>% 
  ggplot() +
  facet_wrap(~birth_year) +
  geom_line(aes(x=age, y=prob_alive, linetype=sex, color=geo))

#By regions
probs %>% filter(age>=50) %>% 
  ggplot() +
  facet_wrap(sex~geo) +
  geom_line(aes(x=cal_year, y=prob_alive, color=age))

# Merging prob_alive data to merged data frame
merged <- merge(probs[, c(key_cols, "prob_alive")], merged[,-10], by = key_cols, all.x = TRUE)

#########################################################################################
sum(is.na(merged[merged$sex=="M",'retired']))

# fill up missing ratio of retired people and estimate pensions
for (i in 1:nrow(merged)) {
    
  #Ratio of retired females in ages 60-64 based on 2023 data
    if (merged[i, 'sex']=="F" & merged[i, 'age']<65 & merged[i, 'age']>=60) {
      ratio = pension_fc_f %>% 
        filter(sex=="F",
               cal_year==2023,
               age==merged[i, 'age']) %>% 
        select(retired) %>% as.numeric()
      
      #Ratio of retired ppl + emp_rate cannot be over 0.96
    merged[i, 'retired'] = ifelse(ratio + merged[i, 'emp_rate'] >= 1, 1-merged[i, 'emp_rate'], ratio)
    }
  
  #First pensions by indexing last known value with sex, (age) and region-specific
  # real factors till 2030 and with inflation from 2030
  if (merged[i, 'age']==65 | merged[i, 'sex']=="F" & merged[i, 'age']>=60 & merged[i, 'age']<65) {
      pen_2023 = first_pensions %>% 
        filter(sex==merged[i, 'sex'],
               cal_year==2023,
               geo==merged[i, 'geo'],
               age==merged[i, 'age']) %>% 
        select(avg_pension) %>% as.numeric()
      
      #After 2030 there are no regional deviations in real increases of first pensions
      
      if (merged[i, 'cal_year']<=2030) {
        rpi_vec <- c(rep(as.numeric(avg_real_pi[1, paste0(merged[i, 'geo'], "_", merged[i, 'sex'], "_", merged[i, 'age'])]),
                         merged[i, 'cal_year']-2023))
        rpi_vec <- rpi_vec + inf_data[inf_data$cal_year>2023 & inf_data$cal_year<=merged[i, 'cal_year'], 'inflation']-1
      }
      
      if (merged[i, 'cal_year'] > 2030) {
        rpi_vec <- c(rep(as.numeric(avg_real_pi[1, paste0(merged[i, 'geo'], "_", merged[i, 'sex'], "_", merged[i, 'age'])]), 7),
                     rep(ifelse(merged[i, 'sex']=="M", 1.031290, 1.029844), merged[i, 'cal_year']-2030))
        rpi_vec <- rpi_vec + inf_data[inf_data$cal_year>2023 & inf_data$cal_year<=merged[i, 'cal_year'], 'inflation']-1
      }
      
      # rpi = ifelse(merged[i, 'cal_year'] <= 2030,
      #              avg_real_pi[1, paste0(merged[i, 'geo'], "_", merged[i, 'sex'], "_", merged[i, 'age'])]-1,
      #              ifelse(merged[i, 'sex']=="M", 0.031290, 0.029844))
      #last values are the national averages by sex

    merged[i, 'avg_pension'] = pen_2023*(prod(rpi_vec))
  }
}

#check
sum(merged[merged$age<65 & merged$sex=="M" | merged$sex=="F" & merged$age<60,'retired']!=0)
sum(is.na(merged[merged$age==65 | merged$sex=="F" & merged$age<65 & merged$age>=60 ,'avg_pension']))


#Indexing pension data with cpi as in the Hungarian system
merged <- merged %>% arrange(age)

for (i in 1:nrow(merged)) {
  if (merged[i, 'age'] > 65) {
      prev_pension = merged %>% 
        filter(sex==merged[i, 'sex'],
             cal_year==merged[i, 'cal_year']-1,
             birth_year==merged[i, 'birth_year'],
             geo==merged[i, 'geo']) %>%
      select(avg_pension) %>% as.numeric()
    
      cpi = inf_data %>% 
        filter(cal_year==merged[i, 'cal_year']) %>% 
        select(inflation) %>% as.numeric()
    merged[i, 'avg_pension'] = cpi*prev_pension
  }
}

#check
sum(is.na(merged[merged$age>=65, 'avg_pension']))
merged_v2 <- merged %>% 
  select(geo, region, birth_year, sex, cal_year, emp_rate, income, retired, avg_pension, prob_alive, age)
#Missing_data in emp_rate and income columns where age >= 65 and in avg_pension
# column where irrelevant
merged_v2[is.na(merged_v2)] <- 0

# Save final database
save(merged_v2, file="data.RData")
openxlsx::write.xlsx(merged_v2, file = "C:/Nandi/BPM/Szakdoga/adatok/data.xlsx")

# ###########################################################################################
# rm(merged, merged_v2, cf, df, df_long, early_f_helper, emp_cenzus, emp_data)
# rm(emp_data_long, emp_rate_data, emp_rate_df, first_pensions, income_2003_18, income_df, master, pop_data, probs, prob_alive, proba)
# rm(emp_fc_helper, helper, model, male_pension_mod, pension_fc_f, pension_fc_m, pension_helper)





