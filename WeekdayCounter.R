# 初始化####
rm(list = ls())
gc()
library(dplyr)
library(progress)
library(lubridate)

# Tomohiko Sakamoto星期计算法####
fun_ts <- function(date_obj) {
  # 解析日期
  year <- date_obj %>% year()
  month <- date_obj %>% month()
  day <- date_obj %>% day()
  
  # 计算逻辑
  if (month < 3) {
    year <- year - 1
  }
  t <- c(0, 3, 2, 5, 0, 3, 5, 1, 4, 6, 2, 4)
  weekday <- (year + year %/% 4 - year %/% 100 + year %/% 400 + t[month] + day) %% 7
  ifelse(weekday == 0, 7, weekday)
}

# 参考函数####
fun_ref <- function(date_obj) {
  # 定义星期数字映射
  weekdays_map <- c(
    "Monday" = 1,
    "Tuesday" = 2,
    "Wednesday" = 3,
    "Thursday" = 4,
    "Friday" = 5,
    "Saturday" = 6,
    "Sunday" = 7
  )
  
  # 星期计算
  weekday_name <- date_obj %>% weekdays()
  
  # 数字映射
  unname(weekdays_map[weekday_name])
}

# 星期计算示例：yyyymmdd>=15821015####
today() %>% fun_ts()
today() %>% fun_ref()


# 生成日期####
fun_date <- function(date) {
  dates <- ymd("1582-10-15") + days(0:as.integer(ymd(date) - ymd("1582-10-15")))
  return(dates)
}

dates <- today() %>% fun_date()

# 运算计时####
# 通用计时执行函数
fun_time <- function(dates, fun, fun_name) {
  # 初始化报告数据框
  report <- data.frame(
    result = integer(length(dates)),
    time = numeric(length(dates)),
    times = numeric(length(dates)),
    stringsAsFactors = FALSE
  )
  
  # 初始化累计时间
  total_time <- 0
  
  # 创建进度条
  pb <- progress_bar$new(
    format = paste0(fun_name, " :current/:total(:percent)[:bar]:tick_rate/s(:elapsed:spin:eta)"),
    total = length(dates),
    clear = FALSE
  )
  
  # 逐个日期执行计算
  for (i in seq_along(dates)) {
    start_time <- Sys.time()
    
    # 执行计算逻辑
    report$result[i] <- fun(dates[i])
    
    # 记录单次耗时
    end_time <- Sys.time()
    elapsed <- as.numeric(end_time - start_time, units = "secs")
    
    # 更新累计时间
    total_time <- total_time + elapsed
    
    # 记录到报告
    report$time[i] <- round(elapsed, 6)
    report$times[i] <- round(total_time, 6)
    
    # 更新进度条
    pb$tick(tokens = list(
      current_time = format(Sys.time(), "%H:%M:%S"),
      avg_time = round(mean(report$time[1:i]), 6)
    ))
  }
  
  return(report)
}

# 封装测试函数
count_ts <- function(dates) {
  fun_time(dates, fun_ts, "Tomohiko Sakamoto算法")
}

count_ref <- function(dates) {
  fun_time(dates, fun_ref, "参考函数")
}

# 运算计时示例####
report_ts <- count_ts(dates)
report_ref <- count_ref(dates)


