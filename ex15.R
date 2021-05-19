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
lines(x <- c(8, 16), y=10.13+1.57*x, lty=3)
text(9.5, 30, "y=10.13+1.57x")

