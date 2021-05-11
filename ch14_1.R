# ==== 14.10 ====
# Assigning Data
population <- c(59, 49, 75, 54, 78,
                56, 60, 82, 69, 83, 
                88, 94, 47, 65, 89, 70)
crime <- c(209, 180, 195, 192, 215,
           197, 208, 189, 213, 201, 
           214, 212, 205, 186, 200, 204)
q10_df <- data.frame(population, crime)

# Scatter plot
plot(q10_df$crime ~ q10_df$population)
# Correlation coefficient
(cor_q10 <- cor(q10_df$crime, q10_df$population))

# Linear model
q10_fit <- lm(q10_df$crime ~ q10_df$population)
summary(q10_fit)

## Visualization
plot(q10_df$crime ~ q10_df$population)
abline(q10_fit)

# Least Squares Method

## b0
mean.x_q10 <- mean(q10_df$population)
mean.y_q10 <- mean(q10_df$crime)
(b0_q10 <- mean.y_q10 - b1_q10 * mean.x_q10)

## b1
sd.x_q10 <- sd(q10_df$population)
sd.y_q10 <- sd(q10_df$crime)
(b1_q10 <- cor_q10 * sd.y_q10 / sd.x_q10)

# Goodness of fit
y.hat_q10 <- b0_q10 + (b1_q10 * q10_df$population)

SSE_q10 <- sum((q10_df$crime - y.hat_q10)^2)
SSR_q10 <- sum((y.hat_q10 - mean.y_q10)^2)
SST_q10 <- SSE_q10 + SSR_q10
(r.squared_q10 <- SSR_q10 / SST_q10)
(cor_q10 ^ 2)

# Residual plot
plot(q10_df$population, q10_fit$residuals)
abline(h = 0)

# Variance
SSE_q10 <- sum((q10_df$crime - y.hat_q10)^2)
(MSE_q10 <- SSE_q10/(16 - 2))


# ==== 14.12 ====
# Assigning data
month <- c("10", "11", "12", "1", "2", "3", "4", "5", "6")
heating_degree <- c(15.6, 26.8, 37.8, 36.4, 35.5, 18.6, 15.3, 7.9, 0.0)
gas_usage <- c(5.2, 6.1, 8.7, 8.5, 8.8, 4.9, 4.5, 2.5, 1.1)
(gas_df <- data.frame(month, heating_degree, gas_usage))

# Scatter plot
plot(gas_df$gas_usage ~ gas_df$heating_degree)
cor_gas <- cor(gas_df$gas_usage, gas_df$heating_degree)

# Linear model
(gas_fit <- lm(gas_df$gas_usage ~ gas_df$heating_degree))
summary(gas_fit)
abline(gas_fit)

# Least Squares Method
## b1
sd.x_gas <- sd(gas_df$heating_degree)
sd.y_gas <- sd(gas_df$gas_usage)
(b1_gas <- cor_gas * sd.y_gas / sd.x_gas)

## b0
mean.x_gas <- mean(gas_df$heating_degree)
mean.y_gas <- mean(gas_df$gas_usage)
(b0_gas <- mean.y_gas - b1_gas * mean.x_gas)

# Goodness of fit
y.hat_gas <- b0_gas + (b1_gas * gas_df$heating_degree)

SSE_gas <- sum((gas_df$gas_usage - y.hat_gas)^2)
SSR_gas <- sum((y.hat_gas - mean.y_gas)^2)
SST_gas <- SSE_gas + SSR_gas
(r.squared_gas <- SSR_gas / SST_gas)

# t-test
MSE_gas <- SSE_gas/(9 - 2)

## t.value
(t.value_gas <- b1_gas/sqrt(MSE_gas/sum((gas_df$heating_degree - mean.x_gas)^2)))

## p.value
(pt(t.value_gas, df = 7, lower.tail = F) * 2)

# F-test
MSR_gas <- sum((y.hat_gas - mean.y_gas)^2) / 1

## f.value
(f.value_gas <- MSR_gas / MSE_gas)

## p.value
(pf(f.value_gas, 1, 7, lower.tail = F))

# b1 Confidence Interval
t_gas <- qt(0.05, 7, lower.tail = F)
s.b1_gas <- sqrt(MSE_gas/sum((gas_df$heating_degree - mean.x_gas)^2))
b1_CI.low_gas <- b1_gas - t_gas * s.b1_gas
b1_CI.up_gas <- b1_gas + t_gas * s.b1_gas



# Residual plot
plot(gas_df$heating_degree, gas_fit$residuals)
abline(h = 0)

# ==== additional 12.14 ====
# Assigning data
month <- c("10", "11", "12", "1", "2", "3", "4", "5", "6")
heating_degree <- c(15.6, 26.8, 37.8, 36.4, 35.5, 18.6, 15.3, 7.9, 0.0)
gas_usage_error <- c(5.2, 6.1, 8.7, 85, 8.8, 4.9, 4.5, 2.5, 1.1)
gas_df_error <- data.frame(month, heating_degree, gas_usage_error)

# Scatter plot
plot(gas_df_error$gas_usage_error ~ gas_df_error$heating_degree)
cor_gas_error <- cor(gas_df_error$gas_usage_error, gas_df_error$heating_degree)

# Linear model
(gas_fit_error <- lm(gas_df_error$gas_usage_error ~ gas_df_error$heating_degree))
summary(gas_fit_error)
abline(gas_fit_error)

# Least Squares Method
## b1
sd.x_gas <- sd(gas_df$heating_degree)
sd.y_gas_error <- sd(gas_df_error$gas_usage_error)
(b1_gas_error <- cor_gas_error * sd.y_gas_error / sd.x_gas)

## b2
mean.x_gas <- mean(gas_df_error$heating_degree)
mean.y_gas_error <- mean(gas_df_error$gas_usage_error)
(b0_gas_error <- mean.y_gas_error - b1_gas_error * mean.x_gas)

# t-test
y.hat_gas_error <- b0_gas_error + (b1_gas_error * gas_df$heating_degree)

SSE_gas_error <- sum((gas_df_error$gas_usage_error - y.hat_gas_error)^2)
MSE_gas_error <- SSE_gas_error/(9 - 2)

## t.value
(t.value_gas_error <- b1_gas_error/sqrt(MSE_gas_error/sum((gas_df$heating_degree - mean.x_gas)^2)))

## p.value
(pt(t.value_gas_error, df = 7, lower.tail = F) * 2)

# F-test
MSR_gas_error <- sum((y.hat_gas_error - mean.y_gas_error)^2) / 1

## f.value
(f.value_gas_error <- MSR_gas_error / MSE_gas_error)

## p.value
(pf(f.value_gas_error, 1, 7, lower.tail = F))
# ==== 14.13 ====
# Assigning data
income <- c(160, 450, 360, 320, 300, 130, 410, 150, 360, 400)
expense <- c(35, 78, 102, 56, 75, 26, 130, 42, 59, 85)
q13_df <- data.frame(income, expense)

# Scatter plot
plot(q13_df$expense ~ q13_df$income)

# Correlation coefficient
(cor_q13 <- cor(q13_df$expense, q13_df$income))

# Linear model
q13_fit <- lm(q13_df$expense ~ q13_df$income)
summary(q13_fit)

## Visualization
plot(q13_df$expense ~ q13_df$income)
abline(q13_fit)

# Least Squares Method
## b1
sd.x_q13 <- sd(q13_df$income)
sd.y_q13 <- sd(q13_df$expense)
(b1_q13 <- cor_q13 * sd.y_q13 / sd.x_q13)

## b2
mean.x_q13 <- mean(q13_df$income)
mean.y_q13 <- mean(q13_df$expense)
(b0_q13 <- mean.y_q13 - b1_q13 * mean.x_q13)

# Correlation coefficient
(ss_xx <- sum((q13_df$income - mean(q13_df$income))^2))
(ss_yy <- sum((q13_df$expense - mean(q13_df$expense))^2))
(ss_xy <- sum((q13_df$income - mean(q13_df$income))*(q13_df$expense - mean(q13_df$expense))))

# Sum of Squares
y.hat_q13 <- b0_q13 + (b1_q13 * q13_df$income)
SSE_q13 <- sum((q13_df$expense - y.hat_q13)^2)
SSR_q13 <- sum((y.hat_q13 - mean.y_q13)^2)
SST_q13 <- SSE_q13 + SSR_q13

# R squared 
(SSR_q13/SST_q13)

MSE_q13 <- SSE_q13/(10 - 2)

(sb1_q13 <- sqrt(MSE_q13/sum((q13_df$expense - mean.x_q13)^2)))

# ==== 14.14 ====
?anscombe
attach(anscombe)

# Correlation coefficient 
(cor_1 <- cor(x1, y1))
(cor_2 <- cor(x2, y2))
(cor_3 <- cor(x3, y3))
(cor_4 <- cor(x4, y4))

# Linear Model
q14.1_fit <- lm(y1 ~ x1)
summary(q14.1_fit)
q14.2_fit <- lm(y2 ~ x2)
summary(q14.2_fit)
q14.3_fit <- lm(y3 ~ x3)
summary(q14.3_fit)
q14.4_fit <- lm(y4 ~ x4)
summary(q14.4_fit)

# R squared
y.hat_gas <- b0_gas + (b1_gas * gas_df$heating_degree)

SSE_gas <- sum((gas_df$gas_usage - y.hat_gas)^2)
SSR_gas <- sum((y.hat_gas - mean.y_gas)^2)
SST_gas <- SSE_gas + SSR_gas
(r.squared_gas <- SSR_gas / SST_gas)
(cor_gas ^ 2)
