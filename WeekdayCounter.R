# 初始化####
rm(list = ls())
gc()
library(dplyr)
library(progress)
library(lubridate)

# Kim Larsen星期计算法####
fun_kl <- function(date_input) {
  # 日期类型转换
  if (inherits(date_input, "Date")) {
    date_str <- format(date_input, "%Y%m%d")
  } else {
    date_str <- date_input
  }
  
  # 解析日期
  year <- as.integer(substr(date_str, 1, 4))
  month <- as.integer(substr(date_str, 5, 6))
  day <- as.integer(substr(date_str, 7, 8))
  
  # Kim Larsen计算逻辑
  if (month < 3) {
    month <- month + 12
    year <- year - 1
  }
  J <- year %/% 100
  K <- year %% 100
  h <- (day + floor(13 * (month + 1) / 5) + K + floor(K / 4) + floor(J / 4) + 5 * J) %% 7
  (h + 5) %% 7 + 1
}

# Tomohiko Sakamoto星期计算法####
fun_ts <- function(date_input) {
  # 日期类型转换
  if (inherits(date_input, "Date")) {
    date_str <- format(date_input, "%Y%m%d")
  } else {
    date_str <- date_input
  }
  
  # 解析日期
  year <- as.integer(substr(date_str, 1, 4))
  month <- as.integer(substr(date_str, 5, 6))
  day <- as.integer(substr(date_str, 7, 8))
  
  # Tomohiko Sakamoto计算逻辑
  if (month < 3) {
    year <- year - 1
    month <- month + 12
  }
  t <- c(0, 3, 2, 5, 0, 3, 5, 1, 4, 6, 2, 4)
  weekday <- (year + year %/% 4 - year %/% 100 + year %/% 400 + t[month] + day) %% 7
  ifelse(weekday == 0, 7, weekday)
}

# 参考函数####
# 星期数字映射
fun_map <- function(weekday_name) {
  # 定义映射关系（严格匹配大小写）
  weekdays_map <- c(
    "Monday" = 1,
    "Tuesday" = 2,
    "Wednesday" = 3,
    "Thursday" = 4,
    "Friday" = 5,
    "Saturday" = 6,
    "Sunday" = 7
  )
  
  # 输入验证
  if (!is.character(weekday_name) || 
      !all(weekday_name %in% names(weekdays_map))) {
    message("输入必须为英文星期全称（如 'Monday'）")
    return(NA)
  }
  
  # 返回对应数字
  unname(weekdays_map[weekday_name])
}

fun_ref <- function(date_input) {
  date_input %>% weekdays() %>% fun_map()
}

# 星期计算示例：yyyymmdd(yyyy-mm-dd)>=15821015####
today() %>% fun_kl()
today() %>% fun_ts()
today() %>% fun_ref()

# 日期合法性检查####
fun_check <- function(date_input) {
  # 日期类型转换
  if (inherits(date_input, "Date")) {
    date_str <- format(date_input, "%Y%m%d")
  } else {
    date_str <- date_input
  }
  
  # 解析日期
  year <- as.integer(substr(date_str, 1, 4))
  month <- as.integer(substr(date_str, 5, 6))
  day <- as.integer(substr(date_str, 7, 8))
  
  # 检查1: 年份应为正整数
  if (!is.numeric(year) || year < 1 || year != as.integer(year)) {
    return("年份应为正整数")
  }
  
  # 检查2: 月份应在1-12之间且为正整数
  if (!is.numeric(month) || month < 1 || month > 12 || month != as.integer(month)) {
    return("月份应在1-12之间且为整数")
  }
  
  # 检查3: 日数不得超过当月最大天数且为正整数
  max_days <- c(31, 28 + as.integer((year %% 4 == 0 & year %% 100 != 0) | year %% 400 == 0),
                31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  if (!is.numeric(day) || day < 1 || day > max_days[month] || day != as.integer(day)) {
    return("日数不得超过当月最大天数且为正整数")
  }
  
  # 检查4: 日期不得早于格里高利历起始日 (1582-10-15)
  if (year < 1582 || 
      (year == 1582 & month < 10) || 
      (year == 1582 & month == 10 & day < 15)) {
    return("日期不得早于格里高利历起始日（1582年10月15日）")
  }
  
  return(TRUE)
}

# 批量检查并生成报告
fun_report <- function(dates) {
  # 初始化记录容器
  invalid_indices <- integer(0)   # 无效日期索引
  invalid_dates_str <- character(0)  # 无效日期字符串
  error_msgs <- character(0)      # 错误信息
  
  # 添加进度条
  pb <- progress_bar$new(
    format = "日期检查进度::current/:total(:percent)[:bar]:tick_rate/s(:elapsed:spin:eta)",
    total = length(dates),
    clear = FALSE
    )
  # 遍历检查每个日期
  for (i in seq_along(dates)) {
    pb$tick() #更新进度条
    
    date <- dates[i]
    check_result <- fun_check(date)
    
    # 记录无效日期信息
    if (check_result != TRUE) {
      invalid_indices <- c(invalid_indices, i)
      invalid_dates_str <- c(invalid_dates_str, format(date, "%Y-%m-%d"))
      error_msgs <- c(error_msgs, check_result)
      
      # 实时输出错误信息
      message("\n! 发现无效日期: ", 
              sprintf("[%d] %s → %s", 
                      i, invalid_dates_str[length(invalid_dates_str)], 
                      error_msgs[length(error_msgs)]))
    }
  }
  
  # 生成汇总报告
  total <- length(dates)
  valid_count <- total - length(invalid_indices)
  invalid_count <- length(invalid_indices)
  
  # 格式化输出
  cat("\n===== 日期检查报告 =====\n")
  cat(sprintf("检查对象：dates（共 %d 个日期）\n", total))
  cat(sprintf("有效日期：%d 个\n", valid_count))
  cat(sprintf("无效日期：%d 个\n", invalid_count))
  
  if (invalid_count > 0) {
    cat("无效日期索引：\n")
    cat(paste(
      sprintf("[%d] %s（原因：%s）", 
              invalid_indices, 
              invalid_dates_str, 
              error_msgs),
      collapse = "\n"
    ))
  }
}

# 日期检查示例####
# 生成日期
fun_date <- function(date) {
  dates <- ymd("1582-10-15") + days(0:as.integer(ymd(date) - ymd("1582-10-15")))
  return(dates)
}

dates <- today() %>% fun_date()

fun_report(dates)

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
count_kl <- function(dates) {
  fun_time(dates, fun_kl, "Kim Larsen算法")
}

count_ts <- function(dates) {
  fun_time(dates, fun_ts, "Tomohiko Sakamoto算法")
}

count_ref <- function(dates) {
  fun_time(dates, fun_ref, "参考函数")
}

# 运算计时示例####
report_kl <- count_kl(dates)
report_ts <- count_ts(dates)
report_ref <- count_ref(dates)
# 查看示例报告
head(report_kl)
head(report_ts)
head(report_ref)

