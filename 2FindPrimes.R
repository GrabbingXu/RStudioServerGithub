# 判断素数####
library(dplyr)

is.prime1 <- function(n) {
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

is.prime2 <- function(n) {
  if (n == 2) {
    return(TRUE)
  }
  ifelse(0 %in% c(n %% c(2:ceiling(sqrt(n)))), FALSE, TRUE)
}



# 找出素数####
find.prime1 <- function(n) {
  primes <- c()
  start.time <- Sys.time()
  for (i in 1:n) {
    if (is.prime1(i)) {
      primes <- c(primes, i)
    }
  }
  end.time <- Sys.time()
  execution.time <- end.time - start.time
  # print(paste("Took", as.numeric(execution.time), "seconds"))
  # print(paste("Got", length(primes), "primes"))
  # return(primes)
  return(list(Primes = length(primes), Time = as.numeric(execution.time)))
}

find.prime2 <- function(n) {
  primes <- c()
  start.time <- Sys.time()
  for (i in 1:n) {
    if (is.prime2(i)) {
      primes <- c(primes, i)
    }
  }
  end.time <- Sys.time()
  execution.time <- end.time - start.time
  # print(paste("Took", as.numeric(execution.time), "seconds"))
  # print(paste("Got", length(primes), "primes"))
  # return(primes)
  return(list(Primes = length(primes), Time = as.numeric(execution.time)))
}

find.prime3 <- function(n) {
  if (n >= 2) {
    sieve <- seq(2, n)
    primes <- c()
    start.time <- Sys.time()
    for (i in seq(2, n)) {
      
      if (any(sieve == i)) {
        primes <- c(primes, i)
        sieve <- c(sieve[(sieve %% i) != 0], i)
      }
    }
    end.time <- Sys.time()
    execution.time <- end.time - start.time
    # print(paste("Took", as.numeric(execution.time), "seconds"))
    # print(paste("Got", length(primes), "primes"))
    # return(primes)
    return(list(Primes = length(primes), Time = as.numeric(execution.time)))
  } else {
    stop("Input value of n should be at least 2.")
  }
}



# 集成函数####
find.prime <- function(x, n) {
  if (x == 1) {
    return(find.prime1(n))
  } else if (x == 2) {
    return(find.prime2(n))
  } else if (x == 3) {
    return(find.prime3(n))
  } else {
    stop("Invalid value of x. Must be 1, 2, or 3.")
  }
}

# find.prime(1,1000)



# 用时情况####
count <- function(x, n, m) {
  times <- numeric(m)
  
  for (i in 1:m) {
    result <- find.prime(x, n)
    times[i] <- result$Time
  }
  avg_time <- mean(times)
  min_time <- min(times)
  max_time <- max(times)
  cat("Average:", avg_time, "\n")
  cat("Minimum:", min_time, "\n")
  cat("Maximum:", max_time, "\n")
  return(times)
}

# times <- count(1,1000,100)



# 平均用时####
mcount <- function(x, i, m) {
  mtimes <- matrix(NA, nrow = m, ncol = i)
  
  total.time = 0
  for (n in 1:i) {
    
    start.time <- Sys.time()
    
    cat(paste("Finding primes in", n, "takes:\n"))
    mtimes[,n] <- count(x, n, m)
    
    end.time <- Sys.time()
    time.diff <- as.numeric(difftime(end.time, start.time, units = "secs"))
    total.time =total.time + time.diff
    hours <- floor(total.time / 3600)
    minutes <- floor((total.time %% 3600) / 60)
    seconds <- round(total.time %% 60, 2)
    
    cat(paste0("NO.", n, " ", m, "-loop took"), round(time.diff,2), "sec\n", 
        paste0("It cost ", hours, "h ", minutes, "m ", seconds, "s\n"))
  }
  
  return(mtimes)
}

# mtimes <- mcount(1,1000,100)



# 集群运算####
library(parallel)

pmcount <- function(x, i, m) {
  pmtimes <- matrix(NA, nrow = m, ncol = i)
  
  total.time <- 0
  
  cl <- makeCluster(detectCores())
  # clusterExport(cl = cl,
  #               varlist = list("count","x","i","m"),
  #               envir=environment())
  
  for (n in 1:i) {
    start.time <- Sys.time()
    
    pmtimes[, n] <- unlist(mclapply(1:m, function(j) count(x, n, 1), mc.cores = length(cl)))
    
    end.time <- Sys.time()
    time.diff <- as.numeric(difftime(end.time, start.time, units = "secs"))
    total.time <- total.time + time.diff
    hours <- floor(total.time / 3600)
    minutes <- floor((total.time %% 3600) / 60)
    seconds <- round(total.time %% 60, 2)
    
    cat(paste0("NO.", n, " ", m, "-loop took"), round(time.diff,2), "sec\n", 
        paste0("It cost ", hours, "h ", minutes, "m ", seconds, "s\n"))
  }
  
  stopCluster(cl)
  
  return(pmtimes)
}

# pmtimes <- pmcount(1,1000,100)



# 大数运算####
# 迭代优化！
# 目标逻辑--in i {1:m save 1.RDS; (1+m):(2m) save 2.RDS etc.}
# 目前逻辑--in i {1:m save 1.RDS; 1:2m save 2.RDS etc.}
# 应从mcount源码开始修改，从n in 1:i开始定义i=j*h+k
lmcount <- function(x, i, m, h) {
  j = (i /  h) %>% floor()
  k = (i %% h)
  pb <- txtProgressBar(min = 0, max = i, style = 3)

  if (j>0) {
    for (f in 1:j) {
      mcount(x, f*h, m) %>% saveRDS(., paste0("lmtimes",f,".RDS"))
      message("Saving lmtimes", f, ".RDS now")
      setTxtProgressBar(pb, f*h)
      Sys.sleep(1)
    }
  }

  if (k>0) {
    mcount(x, k, m) %>% saveRDS(., paste0("lmtimes",j+1,".RDS"))
    message("Saving lmtimes", j+1, ".RDS now")
    setTxtProgressBar(pb, j*h+k)
    Sys.sleep(1)
    k = 1
  }

  close(pb)
  message("Compution completed and Saved ", j+k, " lmtimes.RDS in total")
  gc()
}

# lmcount(1,1000*10,100,1000)



# 集群大数运算####
# 基于大数运算增加集群分布
# 大数运算代码结构有变化，尚未同步至此：
# //1 if(k)嵌套至if(j)中
# //2 新定义参数h决定从i中每计算h个值时保存RDS
# 优化不够，还要再改
# 离散监控，CPU爆炸
# cat()print()message()warning()stop()
# 中间输出未正确写入集群（当前写法适用于i<=1000，i>1000时触发BUG）
library(parallel)

plmcount <- function(x, i, m) {
  j = (i /  1000) %>% floor()
  k = (i %% 1000)

  total.time <- 0

  cl <- makeCluster(detectCores())

  if (j>0) {
    start.time <- Sys.time()

    for (f in 1:j) {
      mclapply(1:m, function(j) mcount(x, f*1000, 1), mc.cores = length(cl)) %>% saveRDS(., paste0("plmtimes",f,".RDS"))

      end.time <- Sys.time()
      time.diff <- as.numeric(difftime(end.time, start.time, units = "secs"))
      total.time <- total.time + time.diff
      hours <- floor(total.time / 3600)
      minutes <- floor((total.time %% 3600) / 60)
      seconds <- round(total.time %% 60, 2)

      cat(paste0("NO.", f, " ", m, "-loop took"), round(time.diff,2), "sec\n",
          paste0("It cost ", hours, "h ", minutes, "m ", seconds, "s\n"))

      gc()
    }
  }

  if (k>0) {
    start.time <- Sys.time()

    mclapply(1:m, function(j) mcount(x, k, 1), mc.cores = length(cl)) %>% saveRDS(., paste0("plmtimes",j+1,".RDS"))

    end.time <- Sys.time()
    time.diff <- as.numeric(difftime(end.time, start.time, units = "secs"))
    total.time <- total.time + time.diff
    hours <- floor(total.time / 3600)
    minutes <- floor((total.time %% 3600) / 60)
    seconds <- round(total.time %% 60, 2)

    cat(paste0("NO.", j+1, " ", m, "-loop took"), round(time.diff,2), "sec\n",
        paste0("It cost ", hours, "h ", minutes, "m ", seconds, "s\n"))

    k = 1
    gc()
  }

  stopCluster(cl)

  message("Saved ", j+k, "lmtimes.RDS in total")
}

# plmcount(1,1000,100)



# 查看内存####
# print(sapply(ls(), function(x) {object.size(get(x))}))
# 释放内存
# gc()
