# 判断素数####
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
  mtimes <- matrix(NA, nrow = m, ncol = i-1)
  
  total.time = 0
  for (n in 2:i) {
    
    start.time <- Sys.time()
    
    cat(paste("Finding primes in", n, "takes:\n"))
    mtimes[,n-1] <- count(x, n, m)
    
    end.time <- Sys.time()
    time.diff <- as.numeric(difftime(end.time, start.time, units = "secs"))
    total.time =total.time + time.diff
    hours <- floor(total.time / 3600)
    minutes <- floor((total.time %% 3600) / 60)
    seconds <- round(total.time %% 60, 2)
    
    cat(paste0("NO.", n, " ", m, "-times-loop takes"), round(time.diff,2), "sec & costs", hours, "h", minutes, "m", seconds, "s\n")
  }
  
  return(mtimes)
}

mtimes <- mcount(1,1000,100)




