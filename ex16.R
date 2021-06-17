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







# ====3.로지스틱회귀====
# 예제 16.2
?mtcars
attach(mtcars)
    
windows()
par(mfcol=c(1,2))
plot(am~wt)
plot(am~hp)
# cf. hp보다 wt가 로지스틱 회귀를 적합하기 더 좋음

## 겹쳐있는 자료들은 jittering
par(mfcol=c(1,2))
plot(jitter(am)~wt)
plot(jitter(am)~hp)

# 로지스틱 회귀; glm (generalized linear model, 일반선형화모형)
## X변수 하나; wt
## family = binomial
logit1 <- glm(am~wt, family = binomial)
summary(logit1)

windows()
plot(am~wt, ylab="am", xlab="wt")
curve(exp(logit1$coefficients[1] + logit1$coefficients[2]*x/
              (1+exp(logit1$coefficients[1]+logit1$coefficients[2]*x)), add = T))

# ====4.변수선택====
library(MASS)
attach(cement)

pairs(cement)
# x1이나 x3의 선형성은 약함
# x2와 x4의 선형성이 강함 cor = -0.97
cor(cement)

# 전진선택
## step; AIC 기준으로 변수선택
### AIC 작을수록 우수한 모형
lm <- lm(y~1, data = cement) # 절편항만 있는 모형
fwdAIC <- step(lm, direction = "forward", scope=(~x1+x2+x3+x4), data = cement)

## addterm; F통계량 기준으로 변수선택
### F통계량 클수록 우수한 모형
lm <- lm(y~1, data = cement) 
addterm(lm, ~.+x1+x2+x3+x4, test = "F")

fwd.lm1 <- lm(y~x4, data = cement) # x4 선택
addterm(fwd.lm1, ~.+x1+x2+x3, test = "F")

fwd.lm2 <- lm(y~x1+x4, data = cement)
addterm(fwd.lm2, ~.+x2+x3, test = "F")

# 후진제거
## step; AIC
lmfull <- lm(y~x1+x2+x3+x4, data=cement)
bwdAIC <- step(lmfull, direction = "backward", scope = (~x1+x2+x3+x4))

# 요 예제의 경우 전진선택과 후진제거의 결과가 동일함
## dropterm; F통계량
lmfull <- lm(y~x1+x2+x3+x4, data=cement)
dropterm(lmfull, test="F")

bwd.lm1 <- lm(y~x1+x2+x4, data=cement) #x3 제거
dropterm(bwd.lm1, test="F")

bwd.lm2 <- lm(y~x1+x2, data=cement) #x4 제거
dropterm(bwd.lm2, test="F")

# 단계적 선택
## AIC
lm <- lm(y~1, data=cement) # 절편만 있는 모형
lmfull <- lm(y~., data=cement) # X변수 전부 포함한 모형
mystep <- step(lm, scope=list(upper=lmfull), direction = "both")

# mystep 회귀분석
mystep$anova
summary(mystep)

# Model comparison
## stepwise vs forward
model.stepwise <- lm(y~x1+x2, data = cement)
summary(model.stepwise)
AIC(model.stepwise)

model.forward <- lm(y~x1+x2+x4, data=cement)
summary(model.forward)
AIC(model.forward)
