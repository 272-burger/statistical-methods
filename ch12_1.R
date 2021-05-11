# ======12.4======= 
# Assigning sample data
obs <- c(4, 13, 35, 32, 15, 1)

# Discretizing
class <- -2:2
c.prob <- pnorm(class, 0.02, sqrt(1.03)) 
for (i in 5:2){
  c.prob[i] <- c.prob[i] - c.prob[i-1]
}

# Calculating exp
exp.prob <- c(c.prob, 1-sum(c.prob)) 
exp <- exp.prob * 100 

# Collapsing exp
exp.prob[2] <- sum(exp.prob[1:2])
exp.prob[5] <- sum(exp.prob[5:6])
exp.prob <- exp.prob [2:5]
exp <- exp.prob * 100

# Collapsing obs
obs <- c(4, 13, 35, 32, 15, 1)
obs[2] <- sum(obs[1:2])
obs[5] <- sum(obs[5:6])
obs <- obs[2:5]

# Calculating chisq
chisq <- (obs - exp)^2/exp
round(chisq,4)
sum(chisq)

# Calculating p.value
df = length(obs)-2-1
p.value <- 1 - pchisq(sum(chisq), df)
p.value

# Testing
chisq.test(obs, p = exp.prob)

# Visualization
curve(dchisq(x, df = 1), to = 10, main = "df = 1")
abline(v = sum(chisq), col = "blue")
abline(v = qchisq(0.95, df = 1), col = "red")

curve(dchisq(x, df = 3), to = 10, main = "df = 3")
abline(v = sum(chisq), col = "blue")
abline(v = qchisq(0.95, df = 3), col = "red")


#=========12.16==========
# lambda = 4
# Assigning sample data
obs <- c(1, 2, 4, 6, 10, 8, 7, 4, 3, 5)

# Caculating exp
x <- 0:9
exp.prob <- dpois(x, lambda=4) 
exp.prob[10] <- 1 - sum(exp.prob[1:9])

exp <- exp.prob * sum(obs) 


# Collapsing
obs[3] <- sum(obs[1:3])
obs[8] <- sum(obs[8:10])
exp.prob[3] <- sum(exp.prob[1:3])
exp.prob[8] <- sum(exp.prob[8:10])

obs <- obs[3:8]
exp.prob <- exp.prob[3:8]

exp <- exp.prob * sum(obs)

exp.prob
exp

# Calculating chisq
chisq <- (obs-exp)^2/exp
chisq
sum(chisq)

# Calculating p.value
df = length(obs) - 1
p.value <- 1 - pchisq(sum(chisq), df)
p.value

# Testing
chisq.test(obs, p=exp.prob)

# lambda = 4.94
# Calculating exp
x <- 0:9

obs <- c(1, 2, 4, 6, 10, 8, 7, 4, 3, 5)
exp.prob <- dpois(x, lambda=4.94)
exp <- exp.prob*sum(obs)


# Collapsing
obs[3] <- sum(obs[1:3])
obs[8] <- sum(obs[8:10])
exp.prob[3] <- sum(exp.prob[1:3])
exp.prob[8] <- sum(exp.prob[8:10])

obs <- obs[3:8]
exp.prob <- exp.prob[3:8]

exp <- exp.prob * sum(obs)

round(exp.prob,4)
round(exp, 4)

# Calculating chisq
chisq <- (obs-exp)^2/exp
chisq
sum(chisq)

# Calculating p.value
df = length(obs) -1 -1
p.value <- 1 - pchisq(sum(chisq), df)
p.value

#========12.19========
# A
# Assigning sample data
obs <- c (1.13, 0.78, 1.78, 4.71, 3.06, 0.55, 
          0.94, 1.92, 1.59, 2.56, 1.09, 1.79,
          0.77, 1.36, 1.28, 1.01, 0.23, 0.38, 
          1.66, 3.42, 1.29, 2.96, 3.12, 2.17,  
          1.34, 0.46, 1.29, 1.99, 2.14, 1.10)

# Calculating mean, sd
obs.mean <- mean(obs)
obs.sd <- sd(obs) 
round(obs.mean, 4)
round((obs.sd)^2, 4)

# Discretizing obs
prob <- rep(1/6, 6) 
p <- cumsum(prob)
z <- qnorm(p, obs.mean, obs.sd)

count <- as.vector(6)
for (i in 1:6) {
  count[i] <- length(obs [obs < z[i]])
}
count
for (i in 6:2) {
  count[i] <- count[i] - count[i-1]
}
count

obs <- count

# Calculating chisq
exp <- rep(5,6)
chisq <- (obs-exp)^2/exp
chisq
sum(chisq)

# Calculating p.value
df = length(obs) - 2 - 1
p.value <- 1 - pchisq(sum(chisq),df)
p.value

# B
# sqrt conversion
obs <- sqrt(obs)







