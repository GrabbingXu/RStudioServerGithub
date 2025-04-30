# 初始化####
rm(list = ls())
gc()
library(dplyr)
library(ggplot2)
library(reshape2)
library(progress)
library(lubridate)

# Tomohiko Sakamoto星期计算法####
fun_ts <- function(dates_obj) {
  # 解析日期
  years <- as.POSIXlt(dates_obj)$year + 1900
  months <- as.POSIXlt(dates_obj)$mon + 1
  days <- as.POSIXlt(dates_obj)$mday
  
  # 计算逻辑
  adjust <- months < 3
  years[adjust] <- years[adjust] - 1
  t <- c(0, 3, 2, 5, 0, 3, 5, 1, 4, 6, 2, 4)
  weekday <- (years + years %/% 4 - years %/% 100 + years %/% 400 + t[months] + days) %% 7
  ifelse(weekday == 0, 7, weekday)
}

# 参考函数####
fun_ref <- function(dates_obj) {
  # 星期计算
  weekday <- as.POSIXlt(dates_obj)$wday
  ifelse(weekday == 0, 7, weekday)
}

# 生成日期(yyyymmdd>=15821015)####
fun_date <- function(date) {
  dates <- ymd("1582-10-15") + days(0:as.integer(ymd(date) - ymd("1582-10-15")))
  return(dates)
}

dates <- today() %>% fun_date()

# 运算用时测试####
fun_time <- function(data, models, times, progress = TRUE) {
  # 提取模型名称
  model_expr <- substitute(models)
  if (is.call(model_expr) && identical(model_expr[[1]], quote(c))) {
    # 处理多函数列表
    model_names <- sapply(model_expr[-1], deparse)
    models <- as.list(models)
  } else {
    # 处理单个函数
    model_names <- deparse(model_expr)
    models <- list(models)
  }
  
  # 初始化结果存储矩阵
  result <- matrix(nrow = 1, ncol = length(models))
  colnames(result) <- model_names
  
  # 遍历每个模型
  for (i in seq_along(models)) {
    model <- models[[i]]
    model_name <- model_names[i]
    
    # 初始化进度条
    if (progress) {
      pb <- progress_bar$new(
        format = paste0(model_name, " :current/:total(:percent)[:bar]:tick_rate/s(:elapsed:spin:eta)"),
        total = times,
        clear = FALSE
      )
    } else {
      pb <- list(tick = function() {})
    }
    
    # 执行多次测试
    durations <- numeric(times)
    for (j in 1:times) {
      start_time <- Sys.time()
      model(data)
      durations[j] <- as.numeric(Sys.time() - start_time)
      pb$tick()
    }
    
    # 记录平均耗时
    result[1, i] <- mean(durations) %>% format(scientific = TRUE, digits = 7)
  }
  
  return(as.data.frame(result))
}

fun_time(data=dates, models=c(fun_ts,fun_ref), times=100)

# 不同数据规模下运算用时测试####
fun_time_scale <- function(data, models, times) {
  # 提取模型名称
  model_expr <- substitute(models)
  if (is.call(model_expr) && identical(model_expr[[1]], quote(c))) {
    # 处理多函数列表
    model_names <- sapply(model_expr[-1], deparse)
  } else {
    # 处理单个函数
    model_names <- deparse(model_expr)
  }
  
  # 初始化结果矩阵
  n <- length(data)
  result <- matrix(nrow = n, ncol = length(models))
  colnames(result) <- model_names
  rownames(result) <- as.character(data)
  
  # 初始化进度条
  pb <- progress_bar$new(
    format = "Data scale :current/:total(:percent)[:bar]:tick_rate/s(:elapsed:spin:eta)",
    total = n,
    clear = FALSE
  )
  
  # 遍历数据规模
  for (k in 1:n) {
    current_data <- data[1:k]

    # 执行时间测试
    time_result <- eval(substitute(
      fun_time(
        data = current_data,
        models = MODELS_EXPR,
        times = times,
        progress = FALSE
      ),
      list(MODELS_EXPR = model_expr)
    ))
    
    # 记录结果
    result[k, ] <- as.numeric(time_result)
    pb$tick()
  }
  
  return(result)
}

result <- fun_time_scale(data=dates,models=c(fun_ts,fun_ref),times=100)

# 数据可视化####
# 将结果矩阵转换为长格式数据框
data_long <- melt(result, varnames = c("DataScale", "Model"), value.name = "TimeCost")
# 行名为日期类型，用行号作为DataScale：
data_long$DataScale <- rep(1:nrow(result), times = ncol(result))

# 计算每个模型的线性拟合公式
formula_df <- by(data_long, data_long$Model, function(sub) {
  fit <- lm(TimeCost ~ DataScale, data = sub)
  eq <- paste0("y = ", sprintf("%.2e", coef(fit)[2]), "x + ", sprintf("%.2e", coef(fit)[1]))
  r2 <- paste0("R^2 = ", sprintf("%.4f", summary(fit)$r.squared))
  data.frame(
    Model = unique(sub$Model),
    eq = eq,
    r2 = r2,
    x = max(sub$DataScale) * 0.2, # 公式显示位置
    y = max(sub$TimeCost) * 0.9
  )
})
formula_df <- do.call(rbind, formula_df)

# 绘制图形
ggplot(data_long, aes(x = DataScale, y = TimeCost, color = Model)) +
  geom_point(alpha = 0.7) + # 散点图
  geom_smooth(method = "lm", se = FALSE, formula = y ~ x) + # 拟合线
  geom_text(
    data = formula_df,
    aes(x = x, y = y, label = eq, color = Model),
    hjust = 0, show.legend = FALSE
  ) +
  geom_text(
    data = formula_df,
    aes(x = x, y = y * 0.85, label = r2, color = Model),
    hjust = 0, show.legend = FALSE
  ) +
  labs(
    title = "Time Cost vs Data Scale by Model",
    x = "Data Scale (Number of Samples)",
    y = "Time Cost (Seconds)"
  ) +
  scale_color_brewer(palette = "Set1") + # 颜色方案
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  )
