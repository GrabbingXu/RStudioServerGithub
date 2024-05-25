lmcount <- function(x, i, m, h) {
  j = (i /  h) %>% floor()
  k = (i %% h)
  times <- numeric(m)
  
  total.time = 0
  pb <- txtProgressBar(min = 0, max = i, style = 3)
  
  if (j > 0) {
    for (f in 1:j) {
      for (n in ((f-1)*h+1):f*h) {
        mtimes <- matrix(NA, nrow = m, ncol = h)
        for (t in 1:m) {
          start.time <- Sys.time()
          
          result <- find.prime(x, n)
          num <- result$Primes[1]
          times[t] <- result$Time
          
          end.time <- Sys.time()
          time.diff <- as.numeric(difftime(end.time, start.time, units = "secs"))
          total.time =total.time + time.diff
          hr <- floor(total.time / 3600)
          min <- floor((total.time %% 3600) / 60)
          sec <- round(total.time %% 60, 2)
          cat(n,"Found",num,"primes","in",paste0(mean(times),"s within ",hr,"h ",min,"m ",sec,"s"),"\n")
          setTxtProgressBar(pb, n)
        }
        mtimes[, n-(f-1)*h] <- times
      }
      message("Saving lmtimes", f, ".RDS now")
      saveRDS(mtimes, paste0("lmtimes",f,".RDS"))
      Sys.sleep(1)
    }
  }
  
  if (k > 0) {
    for (n in (j*h+1):(j*h+k)) {
      mtimes <- matrix(NA, nrow = m, ncol = h)
      
      start.time <- Sys.time()
      
      mtimes[, n-j*h] <- count(x, n, m)
      num <- result$Primes[1]
      times[t] <- result$Time
      
      end.time <- Sys.time()
      time.diff <- as.numeric(difftime(end.time, start.time, units = "secs"))
      total.time =total.time + time.diff
      hr <- floor(total.time / 3600)
      min <- floor((total.time %% 3600) / 60)
      sec <- round(total.time %% 60, 2)
      cat(n,"Found",num,"primes","in",paste0(mean(times),"s within ",hr,"h ",min,"m ",sec,"s"),"\n")
      setTxtProgressBar(pb, n)
      
      k = 1
      mtimes[, n-(f-1)*h] <- times
    }
    message("Saving lmtimes", j+1, ".RDS now")
    saveRDS(mtimes, paste0("lmtimes",j+1,".RDS"))
  }
  close(pb)
  message("Compution completed and Saved ", j+k, " lmtimes.RDS in total")
  gc()
}


lmcount(1,100,100,10)
# cat应该尝试外移一层
# mtimes[, n-(f-1)*h] <- times报错超出边界
