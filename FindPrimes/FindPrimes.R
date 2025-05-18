# 素数求解函数1####
# 平方根试除法
# 时间复杂度O(sqrt(n))
is.prime <- function(n) {
  if (n <= 1) {
    return(FALSE)
  } else if (n == 2) {
    return(TRUE)
  }
  for (i in 2:(as.integer(sqrt(n)) + 1)) {
    if (n %% i == 0) {
      return(FALSE)
    }
  }
  return(TRUE)
}


# 素数求解函数2####
# 平方根试除法（简化语句）
# 时间复杂度O(sqrt(n))
is.prime <- function(n){
  if (n == 2) {
    return(TRUE)
  }
  ifelse(0 %in% c(n %% c(2:ceiling(sqrt(n)))), FALSE, TRUE)
}


# 求取指定该范围内的素数####
# 统计运算时间、素数个数
min.number <- 1
max.number <- 100000
primes <- c()

start.time <- Sys.time()

for (i in min.number:max.number) {
  if (is.prime(i)) {
    primes <- c(primes, i)
  }
}

end.time <- Sys.time()
execution.time <- end.time - start.time

print(paste("Took", as.numeric(execution.time), "seconds"))
print(paste("Got", length(primes), "primes"))


# 素数求解函数3####
# 埃拉托斯特尼筛法The sieve of Eratosthenes
# 时间复杂度O(n*log2(log2(n)))
PrimeNumber <- function(n){
  if (n >= 2) {
    sieve <- seq(2, n)
    primes <- c()
    
    for (i in seq(2, n)) {
      if (any(sieve == i)) {
        primes <- c(primes, i)
        sieve <- c(sieve[(sieve %% i) != 0], i)
      }
    }
    return(primes)
  } else {
    stop("Input value of n should be at least 2.")
  }
}


# 时间复杂度可视化比较####
library(ggplot2)

# 生成数据
n <- 2:100
f1 <- n
f2 <- sqrt(n)
f3 <- n*log2(log2(n))

# 组合数据框
df <- data.frame(n, f1, f2, f3)

# 绘制点线图
ggplot(df, aes(x=n)) + 
  geom_line(aes(y=f1), color="red", linewidth=1) +
  geom_line(aes(y=f2), color="blue", linewidth=1) +
  geom_line(aes(y=f3), color="green", linewidth=1) +
  labs(title="Functions", x="n", y="O(n)") +
  scale_color_manual(values=c("red", "blue", "green")) +
  theme_bw()

