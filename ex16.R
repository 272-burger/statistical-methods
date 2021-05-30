# ====1.변수변환====

attach(trees)

# Girth ~ Volume
# X변수변환
## scatter plot
plot(Girth, Volume)

## linear model
fit <- lm(Volume ~ Girth)
summary(fit)

## residual plot
## 잔차도가 곡선형태를 보이고 있어 변수변환이 필요함

plot(fit$fitted.values, fit$residuals)
    title("Residual plot of Y-hat")
    abline (h=0)

plot(Girth, mylm$residuals)
    title("Residual plot of Y-hat")
    abline(h=0)
    
## X변수를 자승변환: Volume ~ Girth^2
## R-squared 값 커짐 (변수변환 후 적합도 커진 거 비교해주기)
Girth2 = Girth * Girth

fit_X2 <- lm(Volume ~ Girth2)
summary(fit_X2)

plot(fit_X2$fitted.values, fit_X2$residuals)
title("Residual plot of Y-hat")
abline (h=0)

plot(Girth2, fit_X2$residuals)
title("Residual plot of Y-hat")
abline(h=0)

## X변수를 삼승변환: Volume ~ Girth^3
## 오히려 안 좋아. X변수의 자승변환 모형에서의 적합이 더 우수하므로 
## 더 높은 차수의 변환은 더 이상 시도하지 않음. 
Girth3 = Girth2 * Girth
fit_X3 <- lm(Girth3 ~ Volume)
summary(fit_X3)

# Y변수변환
# 로그변환
log_Volume <- log(Volume)
## scatter plot
plot(log_Volume ~ Girth)

## linear model
fit_logY <- lm(log_Volume ~ Girth)
summary(fit_logY)

## residual plot
## 나팔형태: 분산이 감소하는 경향. X자승한 거보다는 잔차도 상으로
## 더 좋다고 보기 어려움
plot(fit_logY$fitted.values, fit_logY$residuals)
title("Residual plot of Y-hat")
abline (h=0)

plot(Girth, fit_logY$residuals)
title("Residual plot of Y-hat")
abline(h=0)

# 제곱근변환
## cf) 자승변환한 경우와 수학적으로 같다고 볼 수 있음. (y > 0, x > 0)
## 통계량이 상당히 비슷함
sqrt_Volume <- sqrt(Volume)
fit_sqrtY <- lm(sqrt_Volume ~ Girth)
summary(fit_sqrtY)

# ====2.모형비교====
# 잔차표준오차, R-square, adjusted R-square, F통계량, Cp통계량 등으로
# 적합성 비교 가능

## 산점도와 적합성 비교: (1) Y ~ X (2) Y ~ X^2 (3) sqrt(Y) ~ X

plot(Girth, Volume)
    abline(fit$coefficients[1], fit$coefficients[2])

plot(Girth2, Volume)
    abline(fit_X2$coefficients[1], fit_X2$coefficients[2])

plot(Girth, sqrt_Volume)
    abline(fit_sqrtY$coefficients[1], fit_sqrtY$coefficients[2])






