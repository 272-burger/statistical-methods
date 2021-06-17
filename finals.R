# ====2====
# lambda
lambda  <-  (0*156+1*63+2*29+3*8+4*4+5*1+6*1)/262

# expected frequency
round(exp <- 262 * dpois(c(0:6), lambda),2)
exp

# collapsing
obs <- c(156, 63, 29, 14)
exp[4] <- sum(exp[4:7])
exp <- exp[1:4]
exp


# test statistic
(chisq <- sum((obs - exp)^2 / exp))

# p value
df <- length(obs) - 1 - 1
(p_value <- pchisq(chisq, df, lower.tail = F))
## visualization
curve(dchisq(x, df), to = 20, main = "lambda = 0.66, df = 2")
    abline(v = chisq, col = "red")
    abline(v = qchisq(0.95, df), col = "blue")
# ====3====
# assigning data
response <- c(0.88, 0.76, 0.67, 0.78, 0.77, 0.56, 0.67, 0.65, 0.86, 
              0.85, 0.85, 0.77, 0.75, 0.65, 0.67, 0.67, 0.98, 0.87,
              0.89, 0.86, 0.87, 0.80, 0.94, 0.75, 0.77, 0.75, 0.86,
              0.88, 0.87, 0.88, 0.87, 0.76, 0.79, 0.77, 0.85, 0.87)
drink <- gl(2, 18, 36, labels = list("음주전", "음주후"))
driver <- gl(18, 1, 36, labels = list("1", "2", "3", "4", "5",
                                      "6", "7", "8", "9", "10",
                                      "11", "12", "13", "14", "15",
                                      "16", "17", "18"))
df <- data.frame(response, drink, driver)

# visualization
library(gplots)
plotmeans(response ~ interaction(drink, driver), data = df, bars = F,
          connect = list(1:2, 3:4, 5:6, 7:8, 9:10, 11:12, 13:14, 15:16, 17:18,
                         9:20, 21:22, 23:24, 25:26, 27:28, 29:30, 31:32, 33:34, 35:36))

# randomized block design
block <- driver
summary(aov(response ~ drink + block, data = df))

# Calculataion
## Means 
response.mean <- mean(response)
drink.means <- as.vector((aggregate(response ~ drink, data = df, mean))$response)
block.means <- as.vector((aggregate(response ~ block, data = df, mean))$response)  

## Sum of Squares
sstr <- 0
for (i in 1:2){
    sstr <- 18 * (drink.means[i] - response.mean) ** 2 + sstr
}


ssb <- 0
for (i in 1:18){
    ssb <- 2 * (block.means[i] - response.mean) ** 2 + ssb
}

sst.response <- sum((response - response.mean) ** 2)
sse <- sst.response - sum(sstr, ssb)

## Mean sum of squares
### k = 2, b = 18
(mstr <- sstr / 1)
(msb <- ssb / 17)
(mse <- sse / 17)

# p value
f_value <- mstr / mse
(p_value <- pf(f_value, 1, 17, lower.tail = F))

# block
f_value.block <- msb / mse
(p_value <- pf(f_value.block, 1, 17, lower.tail = F))



# ====5====
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

df <- data.frame(cost, temp, thick, window, chimney)
colnames(df) <- c("난방비($)", "외부 최저기온", "단열재 두께",
                      "창문수", "굴뚝사용년수")


# Scatter plot
par(mfrow=c(2,2))
plot(cost ~ temp)
plot(cost ~ thick)
plot(cost ~ window)
plot(cost ~ chimney)

cor(df)

# variable selection

# forward selection ; F value
lm <- lm(cost~1, data = df) 
addterm(lm, ~.+temp+thick+window+chimney, test = "F")

fwd.lm1 <- lm(cost~temp, data = df) # temp(외부최저기온) 선택
addterm(fwd.lm1, ~.+thick+window+chimney, test = "F")

fwd.lm2 <- lm(cost~temp+thick, data = df) # thick(단열재두께) 선택
addterm(fwd.lm2, ~.+window+chimney, test = "F")

# backward elimination ; F value
lmfull <- lm(cost~temp+thick+window+chimney, data=df)
dropterm(lmfull, test="F")

bwd.lm1 <- lm(cost~temp+thick+chimney, data=df) # window(창문수) 제거
dropterm(bwd.lm1, test="F")

bwd.lm2 <- lm(cost~temp+thick, data=df) # chimney(굴뚝사용년수)제거
dropterm(bwd.lm2, test="F")


# stepwise selection ; AIC
mystep <- step(lm, scope=list(upper=lmfull), direction = "both")
mystep

# 회귀모형
mystep
mystep$anova
summary(mystep)

# Model comparison
## stepwise vs forward, backward
model.stepwise <- lm(cost~temp+thick+chimney, data = df)
summary(model.stepwise)
AIC(model.stepwise)

model.forward <- lm(cost~temp+thick, data=df)
summary(model.forward)
AIC(model.forward)
