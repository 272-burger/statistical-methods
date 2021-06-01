# ====16.8====
attach(trees)

# original
fit <- lm(Volume ~ Height)
summary(fit)

plot(Volume ~ Height)
    title("Scatter plot of Volume ~ Height")
    abline(fit$coefficients[1], fit$coefficients[2])

plot(fit$fitted.values, fit$residuals)
    title("Residual plot of Y-hat")
    abline(h=0)

# log
log_Volume <- log(Volume)
log_Height <- log(Height)

fit_log <- lm(log_Volume ~ log_Height)
summary(fit_log)

plot(log_Volume ~ log_Height)
title("Scatter plot of log_Volume ~ log_Height")
abline(fit_log$coefficients[1], fit_log$coefficients[2])

plot(fit_log$fitted.values, fit_log$residuals)
title("Residual plot of Y-hat")
abline(h=0)

# log_Girth
log_Girth <- log(Girth)
df_q8 <- data.frame(log_Girth, log_Height, log_Volume)

pairs(df_q8)

cor(df_q8)

fit_log2 <- lm(log_Volume ~ log_Girth + log_Height)
summary(fit_log2)

plot(fit_log2$fitted.values, fit_log2$residuals)
    title("residual plot for y-hat")
    abline(h = 0)

plot(log_Girth, fit_log2$residuals)
title("residual plot for log_Girth")
abline(h = 0)

plot(log_Height, fit_log2$residuals)
title("residual plot for log_Height")
abline(h = 0)

# ====16.10====
# original 
speed <- c(4, 5, 5, 5, 5, 7, 7, 8, 8, 8, 8, 9, 9, 9,
           10, 10, 10, 12, 12, 12, 13, 13, 13, 14, 14,
           15, 16, 16, 16, 17, 17, 18, 18, 18, 19, 20,
           21, 21, 21, 22, 24, 25, 25, 25, 25, 26, 26,
           27, 27, 28, 28, 29, 29, 30, 30, 30, 31, 35, 35,
           36, 39, 40, 40)
    
stop <- c(4, 2, 8, 8, 4, 6, 7, 9, 8, 13, 11, 5, 5, 13, 8, 17, 
          14, 11, 21, 19, 18, 27, 15, 14, 16, 16, 19, 14, 34, 29, 22,
          47, 29, 34, 30, 48, 55, 39, 42, 35, 56, 33, 59, 48, 56, 39, 41, 
          78, 57, 64, 84, 68, 54, 60, 101, 67, 77, 85, 107, 79, 138, 
          110, 134)

df_q10 <- data.frame(speed, stop)

plot(stop ~ speed)

fit <- lm(stop ~ speed)
summary(fit)

plot(stop ~ speed)
    abline(fit$coefficients[1], fit$coefficients[2])

plot(fit$fitted.values, fit$residuals)
    title("residual plot of y-hat")
    abline(h=0)


# log transformation
log_speed <- log(speed)
log_stop <- log(stop)

fit_log <- lm(log_stop ~ log_speed)
summary(fit_log)

plot(log_stop ~ log_speed)

plot(fit_log$fitted.values, fit_log$residuals)
    title("residual plot of y-hat")
    abline(h=0)
    
# sqrt transformation
sqrt_speed <- sqrt(speed)
sqrt_stop <- sqrt(stop)

fit_sqrt <- lm(sqrt_stop ~ sqrt_speed)
summary(fit_sqrt)

plot(sqrt_stop ~ sqrt_speed)

plot(fit_sqrt$fitted.values, fit_sqrt$residuals)
title("residual plot of y-hat")
abline(h=0)

# ====16.11====
time <- c(1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 3, 7, 7, 7, 7, 7, 
            28, 28, 28, 28, 28)
strength <- c(13.0, 13.3, 11.8, 21.9, 24.5, 24.7, 29.8, 29.0, 24.1, 24.2, 
              26.2, 32.4, 30.4, 34.5, 33.1, 35.7, 41.8, 42.6, 40.3, 35.7, 37.3)
df_q11 <- data.frame(time, strength)

plot(strength ~ time)
cor(time, strength)
fit_orgin <- lm(strength ~ time)
summary(fit_orgin)

plot(strength ~ time)
    abline(fit_orgin$coefficients[1], fit_orgin$coefficients[2])


inverse_time <- 1 / time
log_strength <- log(strength)
plot(log_strength ~ inverse_time)

fit <- lm(log_strength ~ inverse_time)
summary(fit)

plot(log_strength ~ inverse_time)
    abline(fit$coefficients[1], fit$coefficients[2])
    
plot(fit_orgin$fitted.values, fit_orgin$residuals)
    title("변수변환 전")
    abline(h = 0)

plot(fit$fitted.values, fit$residuals)
    title("변수변환 후")
    abline(h = 0)    
# ====14.18====

pressing <- c(rep(35.0,4), rep(49.5,4), rep(70.0,4), rep(99.0,4), rep(140.0, 4))
tear <- c(112, 119, 117, 113, 108, 99, 112, 118,
          120, 106, 102, 109, 110, 101, 99, 104,
          100, 102, 96, 101)   

plot(tear ~ pressing)

fit <- lm(tear ~ pressing)
summary(fit)

plot(tear ~ pressing)
    abline(fit$coefficients[1], fit$coefficients[2])
    
plot(fit$fitted.values, fit$residuals)
    abline(h=0)

log_pressing <- log(pressing)
plot(tear ~ log_pressing)

sqrt_pressing <- sqrt(pressing)
plot(tear ~ sqrt_pressing)

fit_log <- lm(tear ~ log_pressing)
summary(fit_log)

plot(tear ~ log_pressing)
abline(fit_log$coefficients[1], fit_log$coefficients[2])

fit_sqrt <- lm(tear ~ sqrt_pressing)
summary(fit_sqrt)