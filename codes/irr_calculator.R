library(FinCal)
library(dplyr)

data <- readxl::read_xlsx("C:/Nandi/BPM/Szakdoga/adatok/data.xlsx")
inf_data <- readxl::read_xlsx("C:/Nandi/BPM/Szakdoga/adatok/inflation_HU.xlsx", sheet = "data")
reg_decoder <- readxl::read_xlsx("C:/Nandi/BPM/Szakdoga/adatok/thesis_pension.xlsx", sheet = "region")

#empty data frame for relevant data
cf <- as.data.frame(matrix(NA, nrow = 81, ncol = 13,
                           dimnames = list(1:81,c("cal_year", "emp_rate", "income", "contr_rate",
                                              "contribution", "retired", "avg_pension",
                                              "pension", "cashflow", "prob_alive", "exp_cf",
                                              "pp", "ppp"))))
num_rows = length(unique(data$geo))*length(unique(data$birth_year))*2
results <- as.data.frame(matrix(NA, nrow = num_rows, ncol = 6,
                                dimnames = list(1:num_rows, c("geo", "region", "birth_year",
                                                              "sex", "irr_n", "irr_r"))))

#Is there pension for 13th month (yes=1)
pm13 <- 1

# 3 arguments: geo, year of birth, sex                      
irr_calc <- function(region_code, byear, gender) {
  # Filter for relevant data from original df
  helper <- data %>% 
    filter(geo==region_code,
           birth_year==byear,
           sex==gender) %>%
    arrange(cal_year) %>% 
    select(cal_year, emp_rate, income, retired, avg_pension, prob_alive)
  
  cf[, c('cal_year', 'emp_rate', 'income', 'retired', 'avg_pension', 'prob_alive')] <- helper
  
  #Both employee and employer contribution is considered
  for (i in 1:nrow(cf)) {
    #contribution rates
    cf[i, "contr_rate"] = inf_data[inf_data$cal_year==cf[i, 'cal_year'], "cont_rate_by_employee"] + inf_data[inf_data$cal_year==cf[i, 'cal_year'], "cont_rate_by_employer"]
    # price level compared to the price level of 2020
    cf[i, "pp"] <- inf_data[inf_data$cal_year==cf[i, 'cal_year'], "price_lvl_2020"]
    
  }
  
  cf$contribution = cf$emp_rate * cf$contr_rate * cf$income
  cf$pension = cf$retired * cf$avg_pension
  # 13 months of pension payments, 12 months of contribution payments:
  cf$cashflow = cf$pension * (12+pm13) - cf$contribution * 12
  cf$exp_cf = cf$cashflow * cf$prob_alive
  #value of expected cf in 2020 price level
  cf$ppp <- cf$exp_cf/cf$pp
  
  #The result is the irr and the npv of cashflows
  res <- c(irr(cf$exp_cf), irr(cf$ppp))
  return(res)
  }

#trial
irr_calc(region_code = "HU33", byear = 1970, gender = "F")

#Calculate irrs and npvs for all combinations
s=1
for (b in 1970:1985) {
  for (r in unique(data$geo)) {
    for (g in c("M", "F")) {
      
      results[s, 'irr_n'] <- irr_calc(region_code = r, byear = b, gender = g)[1]
      results[s, 'irr_r'] <- irr_calc(region_code = r, byear = b, gender = g)[2]
      results[s, 'geo'] <- r
      results[s, 'birth_year'] <- b
      results[s, 'sex'] <- g
      results[s, 'region'] <- reg_decoder[reg_decoder$geo==r,'region']
      
      s=s+1
      #print(s)
    }
  }
}

