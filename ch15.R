# ==== 15.3 ====
# gas <- -122.617 + 0.670 * temp + 4.872 * press

# F-test
SST <- 427.537
R_squared <- 0.948

round(SSR <- R_squared * SST, 3)
round(SSE <- SST - SSR, 3)

round(f_value <- (SSR/2) / (SSE/12), 3)

## p_vlaue
(p_value <- pf(f_value, 2, 12, lower.tail = F))

# MSE
MSE <- SSE / 12
round(sqrt(MSE),3)

# t-test
## 회귀계수
b1 <- 0.670
b2 <- 4.872

## 회귀계수의 표준오차
sb1 <- 0.949
sb2 <- 0.415

## t_value
t_value1 <- b1 / sb1
t_value2 <- b2 / sb2

## p_value
(p_value1 <- pt(t_value1, 12, lower.tail = F))
(p_value2 <- pt(t_value2, 12, lower.tail = F))

# ==== 15.5 ====
# t-test
## beta 2
(t_value2 <- 2.7 / 1.86)
(p_value2 <- pt(t_value2, 26, lower.tail = F))

## beta 3
(t_value3 <- 0.93 / 0.29)
(p_value3 <- pt(t_value3, 26, lower.tail = F))


# ==== 15.11 ====
# Assigning data
y <- c(304.37, 2616.32, 1139.12, 285.43, 
               1413.77, 1555.68, 383.78, 2174.27, 
               845.30, 1125.28, 3462.60, 3682.33)
x1 <- c(89, 513, 231, 68,
       319, 276, 82, 427, 
       193, 224, 729, 951)
x2 <- c(25.5, 294.3, 83.7, 30.7,
        129.8, 180.8, 43.4, 165.2, 
        74.3, 60.8, 319.2, 376.2)
x3 <- c(4, 11, 4, 2,
        6, 6, 4, 10,
        4, 5, 12, 12)

(df_q11 <- data.frame(y, x1, x2, x3))

# Scattor plot
par(mfrow = c(1,3))
plot(y ~ x1)
plot(y ~ x2)
plot(y ~ x3)

# Correlation
cor(df_q11)

# Scattor plot matrix
pairs(df_q11)

# multiple linear model
lm_q11 <- lm(y ~ x1 + x2 + x3)
lm_q11

summary(lm_q11)


## X2 eliminated
lm_q11.2 <- lm(y ~ x1 + x3)
lm_q11.2

summary(lm_q11.2)


# ==== 15.12 ====
# Assigning data
cost <- c(250, 380, 165, 43, 92, 200,
          355, 290, 230, 120, 73, 205, 
          400, 320, 72, 272, 94, 190, 235, 139)
temp <- c(35, 29, 36, 60, 65, 30, 
          10, 7, 21, 55, 54, 48,
          20, 39, 60, 20, 58, 40, 27, 30)
thick <- c(3, 4, 7, 6, 5, 5, 
           6, 10, 9, 2, 12, 5,
           5, 4, 8, 5, 7, 8, 9, 7)
window <- c(10, 1, 9, 8, 8, 9, 14, 9, 11, 
            9, 11, 10, 12, 10, 8, 10, 10, 11, 14, 9)
chimney <- c(6, 10, 3, 9, 6, 5, 7, 10, 
             11, 5, 4, 1, 15, 7, 6, 8, 3, 11, 8, 5)

df_q12 <- data.frame(cost, temp, thick, window, chimney)
colnames(df_q12) <- c("난방비($)", "외부 최저기온", "단열재 두께",
                      "창문수", "굴뚝사용년수")
df_q12

# Scatter plot
par(mfrow=c(2,2))
plot(cost ~ temp)
plot(cost ~ thick)
plot(cost ~ window)
plot(cost ~ chimney)

# correlation
cor(df_q12)

# multiple linear model
lm_q12 <- lm(cost ~ temp + thick + window + chimney)
lm_q12

# F-test
cost_hat <- lm_q12$fitted.values
cost_mean <- mean(cost)

(SSE <- sum((cost - cost_hat) ** 2))
(SSR <- sum((cost_hat - cost_mean) **2))

(f_value <- (SSR/4)/(SSE/15))
(pf(f_value, 4, 15, lower.tail = F))

# t-test
summary(lm_q12)

# eliminated x3, x4
lm_q12.2 <- lm(cost ~ temp + thick)
lm_q12.2
