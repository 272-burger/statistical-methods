# assigning data
mydata <- matrix(c(16, 19, 18, 20, 24, 26, 30, 32, 31, 34, 5, 6, 
                   7, 8, 9, 11, 12, 13, 14, 15, 2, 1.9, 4, 5.6, 
                   6.1, 6.2, 7, 7.2, 8, 9), nrow = 10, ncol = 3)

colnames(mydata) <- c("매출액", "광고비", "설비투자")
(mydata <- data.frame(mydata))

attach(mydata)

# scatter plot
## aspect ratio 45도 가이드라인; 선형성 판단
plot(매출액~광고비)
plot(매출액~설비투자)

# scatter plot matrix
pairs(mydata)

## lattice 패키지 사용
windows()
library(lattice)
splom(mydata)

# 상관계수; 선형 방향과 강도 파악
cor(mydata)
## 산점도에서는 매출액과 설비투자가 곡선경향이 있으나 
## 상관계수는 0.917로 두 변수의 선형성이 높다고 할 수 있다.
## 다중공선성 판단 정도.. 는 ..

# 단순선형회귀
par(mfrow = c(1,3))

plot(매출액~광고비)
lm1 <- lm(매출액~광고비)
summary(lm1)
curve(lm1$coefficient[1]+lm1$coefficient[2]*x, add = T)
title(main = expression(paste(hat("y"), " =6.63+1.83x  p값 = 0.000 R-sq = 0.97")))

plot(매출액~설비투자)
lm2 <- lm(매출액~설비투자)
summary(lm2)
curve(lm2$coefficient[1]+lm2$coefficient[2]*x, add=T)
title(main = expression(paste(hat("y"), " =10.72+2.50x  p값 = 0.00018 R-sq = 0.84")))

plot(설비투자~광고비)
cor(설비투자, 광고비)
title(main="r = 0.95")

# 다중회귀: 매출액 = f(광고비, 설비투자)

lm12 <- lm(매출액 ~ 광고비 + 설비투자)
lm12
summary(lm12)
confint(lm12)
## 설비투자 변수를 제거하고 회귀분석 다시
## confint 0 포함여부

# residual plot
par(mfrow = c(1,3))
plot(lm12$fitted, lm12$residuals, xlab = "매출액 - hat", ylab = "잔차", main = "잔차그림")
abline(0,0)

plot(광고비, lm12$residuals, xlab="광고비", ylab="잔차", main="잔차그림")
abline(0,0)

plot(설비투자, lm12$residuals, xlab="설비투자", ylab="잔차", main="잔차그림")
abline(0,0)
## 이 경우 자료의 수가 적어 어떤 패턴을 단정적으로 말하기는 어렵긴 하나

# dummy variable
설비투자가변수 <- c(rep(0,4), rep(1,6))
dummy.data <- data.frame(매출액, 광고비, 설비투자가변수)
pairs(dummy.data)
## 설비투자가변수가 0일때 매출액이 적고, 1일때 매출액이 큼

mylm <- lm(매출액 ~ 광고비 + 설비투자가변수)
mylm
summary(mylm)
## 설비투자가변수가 비유의적이어서 회귀식에서
## 제거해야 하지만 여기서는 예시의 목적으로 아래 그림들을 추가로 그려본다

par(mfcol=c(1,3))

plot(매출액[설비투자가변수==0] ~ 광고비[설비투자가변수==0], 
    xlim=c(5,15), ylim=c(16,34), pch=19, xlab="광고비", ylab="매출액")
title("설비투자가변수=0")
lines(x <- c(4,9), y=8.04+1.57*x)
text(9, 18, "y=8.04+1.57x")

plot(매출액[설비투자가변수==1] ~ 광고비[설비투자가변수==1],
        xlim=c(5,15), ylim=c(16,34), xlab="광고비", ylab="매출액")
title("설비투자가변수=1")
lines(x <- c(8,16), y=10.13+1.57*x, lty=3)
text(9.5, 30, "y=10.13+1.57x")

plot(매출액 ~ 광고비, xlab="광고비", ylab="매출액")
title("전체자료")
points(광고비[설비투자가변수==0], 매출액[설비투자가변수==0], pch=19)
lines(x <- c(4,9), y=8.04+1.57*x)
lines(x <- c(8, 16), y=10.13+1.57*x, lty=3)
text(9.5, 30, "y=10.13+1.57x")

## 두 직선의 절편 차 = 더미 계수의 절편값 = 2.088

# 다항회귀
# assigning data
x <-  c(250, 260, 270, 280, 290, 300, 310, 320, 330, 340)
y <-  c(45, 51, 56, 70, 72, 86, 81, 67, 53, 40)

plot(x,y, xlab = "온도", ylab ="강도")

# 이 예제의 산점도는 곡선형태를 분명히 보이므로 1차 회귀 없이 바로 이차 회귀를
# 시도할 수 있음. 여기서는 잔차도를 비교하기 위하여 1차 회귀를 적합함
# 산점도에서는 쉽게 보이지 않던 곡선형태가 잔차도에서는 뚜렷이 보일 수 있음

# 우선 1차 회귀해보면
mylm1 <- lm(y~x)

anova(mylm1)
summary(mylm1)

curve(mylm1$coeff[1] + mylm1$coeff[2]*x, add=T)
title(main="1차 회귀")

windows()
plot(mylm1$fitted, mylm1$residuals, xlab = "y-hat", ylab = "r", main = "1차 회귀 잔차그림")
abline(0, 0)

# 이차항 추가
x2 <- x*x
mylm2 <- lm(y~x+x2)

anova(mylm2)
summary(mylm2)
# 이차항의 계수가 유의적임
# 특별한 경우를 젤외하고 제일 고차항의 계수가 유의적이라면
# 낮은 차수의 항은 관례적으로 추가로 검정하지 않는다 (절편항 포함)
# 낮은 차수의 항이 비유의적이라 하더라도 회귀식에서 제외하지 않음

windows()
plot(x, y, xlab="온도", ylab="강도")
curve(mylm2$coeff[1] + mylm2$coeff[2]*x + mylm2$coeff[3]*x2,
      add=T)
title(main = "2차 다항회귀")

windows()
plot(mylm2$fitted, mylm2$residuals, xlab = "y-hat",
     ylab = "r", main = "2차 다항회귀 잔차그림")
abline(0, 0)

## 일차 다항회귀 잔차그림 vs 이차 다항회귀 잔차그림
## 상당히 fitting이 개선되었음을 비교해볼 수 있음

# 삼차항 추가
x3 <- x^3
mylm3 <- lm(y~x+x2+x3)

windows()
plot(x, y, xlab="온도", ylab="강도", main="3차 다항회귀")
curve(mylm3$coeff[1] + mylm3$coeff[2]*x + mylm3$coeff[3]*x^2
      + mylm3$coeff[4]*x^3, add=T)

summary(mylm3)

plot(mylm3$fitted, mylm3$residuals)
abline(0,0)

## 3차 회귀식에서의 잔차들은 (-7, 6) 범위로 2차 회귀식보다 0쪽으로 줄었으나 
## 줄어든 정도는 아주 많이 둔화됨
## 3차 잔차그림; 2차 회귀식의 경우보다 오히려 더 뚜렷한 곡선형태가 나타남
## 따라서 이 경우 2차 회귀분석이 가장 적합하다는 결론을 내릴 수 있음

## 그러나 2차 다항곡선과 3차 다항곡선을 적합하여 보면 3차가 더 적합해 보임
## if 예측력 관점이라면 3차 곡선이 나을것
## 통계적 관례로는 2차 곡선이 나음
## 둘 다 제시하는 것이 바람직!
