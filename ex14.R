# 14_example
## trees 자료에 대한 회귀분석

?trees
attach(trees)

## 회귀분석은 항상 산점도로 시작한다
plot(Volume~Girth) # 점들이 45도 선에 보이게끔 그래프 좌우축 조정
# Y변수 ~ X변수

## 상관계수의 절댓값이 1에 가까울수록 적합이 잘 되는 회귀직선식을 얻을 수 있다.
cor(Girth, Volume)

(fit <- lm(Volume~Girth)) # lm == linear model (선형모형)
abline(fit$coefficient[1], fit$conefficient[2]) # 절편, 기울기
# 잉 이거 실행 안 되는디.. 

## 회귀계수의 t검정, 잔차분산의 추정값, 결정계수 R^2
summary(fit)
# Girth에 대한 t-test = 기울기에 대한 t-test

## 회귀식의 유의성 검정 - 분산분석표
anova(fit)

