# ==== 14.15 ====
# Assigning data
toefl <- c(83, 80, 67, 97, 104,
           57, 65, 89, 96, 80,
           104, 77, 68, 73, 75,
           80, 64, 64, 62, 92)
toeic <- c(840, 720, 605, 780, 920,
           585, 645, 670, 770, 635,
           960, 665, 715, 605, 505,
           795, 650, 590, 525, 815)

df_q15 <- data.frame(toefl, toeic)


# Scatter plot
plot(df_q15$toeic ~ df_q15$toefl)

## Outlier
outlier_q15 <- which(df_q15$toefl == 75)
text(df_q15$toefl[outlier_q15], df_q15$toeic[outlier_q15], col = "blue")

# Eliminate outlier
(df_q15 <- df_q15[-c(15),])

# Correaltion coefficient
cor(df_q15$toefl, df_q15$toeic)

## Calculation
toefl.mean <- mean(df_q15$toefl)
toeic.mean <- mean(df_q15$toeic)
toefl.sd <- sd(df_q15$toefl)
toeic.sd <- sd(df_q15$toeic)

(cor_q15 <- (sum((df_q15$toefl - toefl.mean) * (df_q15$toeic - toeic.mean)) / 18) / (toefl.sd * toeic.sd)) # n - 1, n = 19

# LSM: regression equation 1
## linear model
fit_q15 <- lm(df_q15$toeic ~ df_q15$toefl)
summary(fit_q15)

## Calculation
(b1_q15 <- cor_q15 * toeic.sd / toefl.sd)
(b0_q15 <- toeic.mean - (b1_q15 * toefl.mean))


# t-test
toeic.hat <- b0_q15 + (b1_q15 * df_q15$toefl)

SSE_q15 <- sum((df_q15$toeic - toeic.hat) ^ 2)
SSR_q15 <- sum((toeic.hat - toeic.mean) ^ 2)
SST_q15 <- SSE_q15 + SSR_q15

MSE_q15 <- SSE_q15 / 17
sb1_q15 <- sqrt((MSE_q15) / sum((df_q15$toefl - toefl.mean) ** 2))

## t value
(t.value_q15 <- b1_q15 / sb1_q15)

## p value
(pt(t.value_q15, 17, lower.tail = F) * 2)

# F test
## f value
(f.value_q15 <-  SSR_q15 / MSE_q15) # 1, 17

## p value
(pf(f.value_q15, 1, 17, lower.tail = F))

# R squared
(r2_q15 <- SSR_q15 / SST_q15)

# Residual plot
plot(df_q15$toefl, fit_q15$residuals, ylab = "residuals")
abline(h=0)

# LSM: regression equation 2
## linear model
fit_q15.2 <- lm(df_q15$toefl ~ df_q15$toeic)
summary(fit_q15.2)

## Calculation
(b1_q15.2 <- cor_q15 * toefl.sd / toeic.sd)
(b0_q15.2 <- toefl.mean - (b1_q15.2 * toeic.mean))


plot(df_q15$toefl ~ df_q15$toeic)
# equation 1
abline(fit_q15, col = "red")
# reverse equation 2
abline(a = -fit_q15.2$coefficients[1]/fit_q15.2$coefficients[2],
       b = 1/fit_q15.2$coefficients[2], col = "blue")

# ==== 14.17 ====
# Assigning data
library(MASS)
attach(mammals)


# Scatter plot
plot(brain ~ body)


# LSM
## linear model
fit_q17 <- lm(brain ~ body)
summary(fit_q17)

## Calculation
brain.mean <- mean(mammals$brain)
body.mean <- mean(mammals$body)

brain.sd <-  sd(mammals$brain)
body.sd <- sd(mammals$body)
cor_q17 <- cor(body, brain)

(b1_q17 <- cor_q17 * brain.sd /body.sd)
(b0_q17 <- brain.mean - (b1_q17 * body.mean))

# Histrogram
hist(mammals$body)
hist(mammals$brain)

# Changing variables
## Scatter plot
mammals_log <- log(mammals)
plot(mammals_log$brain ~ mammals_log$body)

## LSM
## linear model
fit_q17_log <- lm(mammals_log$brain ~ mammals_log$body)
summary(fit_q17_log )

## Calculation
brain.mean_log  <- mean(mammals_log$brain)
body.mean_log  <- mean(mammals_log$body)

brain.sd_log  <-  sd(mammals_log$brain)
body.sd_log  <- sd(mammals_log$body)
cor_q17_log  <- cor(mammals_log$body, mammals_log$brain)

(b1_q17_log  <- cor_q17_log * brain.sd_log  / body.sd_log)
(b0_q17_log  <- brain.mean_log  - (b1_q17_log  * body.mean_log))

## R squared
(r2_q16 <- cor_q17_log ^ 2)

## Residual plot
plot(mammals_log$body, fit_q17_log$residuals, ylab = "residuals")
abline(h = 0)

# ==== 14.19 ====
# Assigning data
year <- c(1940:1967)
spawners <- c(963, 572, 305, 272, 824, 940, 486, 307,
              1066, 480, 393, 176, 237, 700,511, 87, 
              370, 448, 819, 799, 273, 936, 558, 597,
              848, 619, 397, 616)
recruits <- c(2215, 1334, 800, 438, 3071, 957, 934, 971,
              2257, 1451, 686, 127, 700, 1381, 1393, 363, 
              368, 2067, 644, 1747, 744, 1087, 1335, 1981,
              627, 1099, 1532, 2086)
df_q19 <- data.frame(year, spawners, recruits)

# Scatter plot
plot(df_q19$recruits ~ df_q19$spawners)

# Outlier
outlier_q19.1 <- which(df_q19$year == 1951)
outlier_q19.2 <- which(df_q19$year == 1955)
text(df_q19$spawners[outlier_q19.1], df_q19$recruits[outlier_q19.1], col = "blue")
text(df_q19$spawners[outlier_q19.2], df_q19$recruits[outlier_q19.2], col = "red")


# Eliminate outlier
df_q19 <- df_q19[-c(12,16),]
plot(df_q19$recruits ~ df_q19$spawners)

# LSM
fit_q19 <- lm(df_q19$recruits ~ df_q19$spawners)
summary(fit_q19)

## Calculation
recruits.mean <- mean(df_q19$recruits)
spawners.mean <- mean(df_q19$spawners)

recruits.sd <-  sd(df_q19$recruits)
spawners.sd <- sd(df_q19$spawners)
cor_q19 <- cor(df_q19$spawners,df_q19$recruits)

(b1_q19 <- cor_q19 * recruits.sd / spawners.sd)
(b0_q19 <- recruits.mean - (b1_q19 * spawners.mean))

plot(df_q19$recruits ~ df_q19$spawners)
abline(fit_q19)

# R squared
recruits.hat <- b0_q19 + (b1_q19 * df_q19$spawners)

SSE_q19 <- sum((df_q19$recruits - recruits.hat) ^ 2)
SSR_q19 <- sum((recruits.hat - recruits.mean) ^ 2)
SST_q19 <- SSE_q19 + SSR_q19

(r2_q19 <- SSR_q19 / SST_q19)

# Residual plot
plot(df_q19$spawners, fit_q19$residuals, ylab = "residuals")
abline(h=0)

# Changing variables
# Scatter plot
log_y <- log(df_q19$recruits/df_q19$spawners)
plot(log_y ~ df_q19$spawners)

# LSM
fit_q19_log <- lm(log_y ~ df_q19$spawners)
summary(fit_q19_log)
## Calculation
log_y.mean <- mean(log_y)
spawners.mean <- mean(df_q19$spawners)

log_y.sd <-  sd(log_y)
spawners.sd <- sd(df_q19$spawners)
cor_q19_log <- cor(df_q19$spawners,log_y)

(b1_q19_log <- cor_q19_log * log_y.sd / spawners.sd)
(b0_q19_log <- log_y.mean - (b1_q19_log * spawners.mean))

# R squared
log_y.hat <- b0_q19_log + (b1_q19_log * df_q19$spawners)

SSE_q19_log <- sum((log_y - log_y.hat) ^ 2)
SSR_q19_log <- sum((log_y.hat - log_y.mean) ^ 2)
SST_q19_log <- SSE_q19_log + SSR_q19_log

(r2_q19_log <- SSR_q19_log / SST_q19_log)

# Residual plot
plot(df_q19$spawners, fit_q19_log$residuals, ylab = "residuals")
abline(h=0)

# F vlaue
## original
MSE_q19 <- SSE_q19 / 24 # n - 2
MSR_q19 <- SSR_q19
(f.value_q19 <-  MSR_q19 / MSE_q19)

## log transformation
MSE_q19_log <- SSE_q19_log / 24
MSR_q19_log <- SSR_q19_log
(f.value_q19_log <-  MSR_q19_log / MSE_q19_log)

# ==== 14.21 ====
# Assigning data
temp <- seq(16.5, 25.5, 1)
SIDS <- c(171, 153, 102, 79, 56, 52, 30, 17, 5, 3)
three_months <- c(1.070, 1.033, 1.020, 1.114, 0.857, 0.673, 0.733, 1.118, 1.200, 3.667)
twelve_months <- c(1.105, 0.967, 1.206, 0.962, 0.804, 0.846, 0.833, 0.471, 1.200, 0.333)

temp.k <- rep(temp, SIDS)
three_months.k <- rep(three_months, SIDS)
twelve_months.k <- rep(twelve_months, SIDS)

df <- data.frame(temp.k, three_months.k, twelve_months.k)

# Scatter plot
plot(df$temp.k, df$three_months.k, ylim = c(0, 4))
plot(df$temp.k, df$twelve_months.k, ylim = c(0, 4))

# 19
# Assigning data
temp_19 <- c(19.7, 20.9, 19.1, 19.8, 20.4, 22.9, 21.5, 20.0, 19.4, 21.5,
             21.1, 19.9, 21.9, 19.7, 20.1, 20.5, 20.5, 19.5, 20.6, 21.1,
             19.1, 19.4, 19.0, 19.0, 19.6, 20.7, 19.6, 21.4, 22.4, 19.1)
SIDS_19 <- c(0, 0, 0, 0, 1, 1, 0, 2, 0, 1,
             1, 1, 1, 0, 0, 0, 0, 2, 1, 1, 
             0, 1, 2, 4, 0, 0, 1, 0, 0, 1)
df_q21_19 <- data.frame(temp_19, SIDS_19)

plot(jitter(SIDS_19) ~ jitter(temp_19))
