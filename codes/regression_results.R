library(readxl)
results <- read_excel("C:/Nandi/BPM/Szakdoga/adatok/results.xlsx")

results[,c(1,2,4)] <- lapply(results[,c(1,2,4)], as.factor)

results$region <- relevel(results$region, ref = "Közép-Magyarország")

reg_1 <- lm(irr_r ~ I(birth_year-1970) + region  + sex + I(birth_year-1970)*sex, data = results)
summary(reg_1)

stargazer::stargazer(reg_1, type = "text", decimal.mark = ",", digits = 6)
