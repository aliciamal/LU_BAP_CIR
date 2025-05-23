#--------------------------------------------------------
#preparing and joining data
#------------------------------------------------------

library(data.table)

GDELT_data <- fread("final_social_control_with_campaigns_separated2018.csv", header = TRUE)
navco_data <- fread("dta_Movement.csv", header = TRUE)

setnames(navco_data, "CAMPAIGN", "Campaign")

#take out NAs
GDELT_data <- GDELT_data[
  !is.na(Campaign) & 
    !(tolower(trimws(Campaign)) %in% c("**campaign match: none**", "**none**", "none"))
]

## renaming mis-named or cut campaign names
GDELT_data[Campaign == "Anti-Morales protests", Campaign := "Anti-Morales Protests"]

GDELT_data <- GDELT_data[, .(
  V1 = mean(V1, na.rm = TRUE),
  V2 = mean(V2, na.rm = TRUE),
  V3 = mean(V3, na.rm = TRUE)
), by = Campaign]

GDELT_data$V1 <- rowMeans(dta[, c("V2", "V3", "V4")], na.rm = TRUE)

## joining data
joined_data <- merge(GDELT_data, navco_data, by = "Campaign", all = FALSE)

# Recode Success and Violence
joined_data[, Success := fifelse(SUCCESS == 1 & LIMITED == 0 & FAILURE == 0, 1,
                                 fifelse(LIMITED == 1 & SUCCESS == 0 & FAILURE == 0, 0,
                                         fifelse(FAILURE == 1 & SUCCESS == 0 & LIMITED == 0, 0, NA_real_)))]

joined_data[, Violence := fifelse(VIOL == 1 & NONVIOL == 0, 1,
                                  fifelse(NONVIOL == 1 & VIOL == 0, 0, NA_real_))]

#--------------------------------------------
#Empirical Analysis
#--------------------------------------------

library(data.table)
library(modelsummary)  
library(broom)  
dta <- fread("DATA.csv", header = TRUE)

model <- lm(Success ~ V1 + Violence, data = dta)

model1 <- lm(Success ~ V2 + Violence, data = dta)

model2 <- lm(Success ~ V3 + Violence, data = dta)

model3 <- lm(Success ~ V4 + Violence, data = dta)

# Run and print full summaries
summary(model1)
summary(model2)
summary(model3)
summary(model)

tidy(model, conf.int=TRUE)
modelsummary(model,
             stars = TRUE,
             coef_rename = c("(Intercept)" = "Intercept",
                             "V1" = "SC General"),
             gof_map = c("nobs", "r.squared", "adj.r.squared"),
             title = "Effect of Social Control on Movement success",
             notes = ("OLS coefficients with standard errors in parentheses"))

tidy(model1, conf.int=TRUE)
modelsummary(model1,
             stars = TRUE,
             coef_rename = c("(Intercept)" = "Intercept",
                             "V2" = "Tolerance"),
             gof_map = c("nobs", "r.squared", "adj.r.squared"),
             title = "Effect of Tolerance SC on Movement success",
             notes = ("OLS coefficients with standard errors in parentheses"))

tidy(model2, conf.int=TRUE)
modelsummary(model2,
             stars = TRUE,
             coef_rename = c("(Intercept)" = "Intercept",
                             "V3" = "Sanction"),
             gof_map = c("nobs", "r.squared", "adj.r.squared"),
             title = "Effect of Sanction SC on Movement success",
             notes = ("OLS coefficients with standard errors in parentheses"))

tidy(model3, conf.int=TRUE)
modelsummary(model3,
             stars = TRUE,
             coef_rename = c("(Intercept)" = "Intercept",
                             "V4" = "Norms"),
             gof_map = c("nobs", "r.squared", "adj.r.squared"),
             title = "Effect of Normative SC on Movement success",
             notes = ("OLS coefficients with standard errors in parentheses"))

model_list <- list(model1, model2, model3, model4)
names(model_list) <- c("V1", "V2", "V3", "V4")

r2_table <- sapply(model_list, function(m) summary(m)$r.squared)
adj_r2_table <- sapply(model_list, function(m) summary(m)$adj.r.squared)

data.frame(Model = names(r2_table), R2 = r2_table, Adjusted_R2 = adj_r2_table)
library(ggplot2)
library(dplyr)
library(broom)

##--------------------------------------------------------##
#Coefficients Bar Plot
##-------------------------------------------------------##

coef_data <- data.frame(
  Model = c("V1 (General)", "V2 (Tolerance)", "V3 (Sanctions)", "V4 (Norms)"),
  Coefficient = c(-2.15, 1.017, -1.475, 0.97)  # Replace with your actual values
)

# Create the bar plot
ggplot(coef_data, aes(x = Model, y = Coefficient, fill = Model)) +
  geom_bar(stat = "identity", width = 0.6) +
  labs(
    title = "Coefficient Estimates for Social Control Strategies",
    x = "Model",
    y = "Coefficient"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0, linetype = "dashed")  # Adds a reference line at 0

##----------------------------------------------------
# OLS Assumptions
#---------------------------------------------------

library(ggResidpanel)    
library(expss)          
library(tidyverse)       
library(broom)           
library(marginaleffects) 
library(modelsummary)   
library(sjPlot)
library(performance)
library(readr)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(car)
library(countrycode)
library(gtools) 
library(rworldmap)
library(tidyr)
library(haven)
library(nlme)
library(car)

library(ggplot2)


ggplot(data, aes(x = Success, y = V4)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = TRUE, color = "blue", span = 0.75) +
  labs(
    title = "Scatterplot with LOESS Regression Line",
    x = "V1",
    y = "Success"
  ) +
  theme_minimal()

## Assumptions
# Multicolinearity
vif(model1)
vif(model2)
vif(model3)
vif(model4)

#Outliers
Outliers <-augment(model4)
summary(Outliers$.std.resid)

Outliers <- Outliers |>
  mutate(SRE1.96 = case_when(
    .std.resid > 1.96 | .std.resid < -1.96  ~ 1,
    .std.resid > -1.96 & .std.resid < 1.96 ~ 0),
    SRE2.58 = case_when(
      .std.resid > 2.58 | .std.resid < -2.58  ~ 1,
      .std.resid > -2.58 & .std.resid < 2.58 ~ 0),
    SRE3.29 = case_when(
      .std.resid > 3.29 | .std.resid < -3.29  ~ 1,
      .std.resid > -3.29 & .std.resid < 3.29 ~ 0))

fre(Outliers$SRE1.96)
fre(Outliers$SRE2.58)
fre(Outliers$SRE3.29)
mean(Outliers$SRE1.96)
mean(Outliers$SRE2.58)
mean(Outliers$SRE3.29)

#Influential cases
summary(Outliers$.cooksd)
resid_panel(model4, plots = c("cookd"))


##------------------------------------------------------------
#Predicted Probabilities and Logistics regression
##-----------------------------------------------------

# Model 1: V1
model1 <- lm(Success ~ V1 + Violence, data = data)
summary(model1)
pred1 <- predict(model1)
summary(pred1)
sum(pred1 < 0 | pred1 > 1)  # Count of out-of-bounds predictions

# Model 2: V2
model2 <- lm(Success ~ V2 + Violence, data = data)
summary(model2)
pred2 <- predict(model2)
summary(pred2)
sum(pred2 < 0 | pred2 > 1)

# Model 3: V3
model3 <- lm(Success ~ V3 + Violence, data = data)
summary(model3)
pred3 <- predict(model3)
summary(pred3)
sum(pred3 < 0 | pred3 > 1)

# Model 4: V4
model4 <- lm(Success ~ V4 + Violence, data = data)
summary(model4)
pred4 <- predict(model4)
summary(pred4)
sum(pred4 < 0 | pred4 > 1)

# combined Graph
par(mfrow = c(2, 2))
hist(pred1, main = "Predicted: Model 1 (V1)", xlim = c(0, 1), col = "skyblue", breaks = 10)
hist(pred2, main = "Predicted: Model 2 (V2)", xlim = c(0, 1), col = "lightgreen", breaks = 10)
hist(pred3, main = "Predicted: Model 3 (V3)", xlim = c(0, 1), col = "salmon", breaks = 10)
hist(pred4, main = "Predicted: Model 4 (V4)", xlim = c(0, 1), col = "orange", breaks = 10)

model3_logit <- glm(Success ~ V3 + Violence, data = data, family = binomial)
summary(model3_logit)
model2_logit <- glm(Success ~ V2 + Violence, data = data, family = binomial)
summary(model2_logit)
