# 加载包
library(zoo)       # 滚动相关分析
library(lubridate) # 日期处理
library(tidyverse) # 数据处理及可视化

# 加载数据集
data("EuStockMarkets")

# 检查缺失值
EuStockMarkets %>% 
  as_tibble() %>% 
  summarise_all(~sum(is.na(.))) 

# 提取时间日期
time_date <- EuStockMarkets %>% 
  time() %>%                              # 提取时间序列
  as.numeric() %>%                        # 转换为数值
  as_tibble() %>%                         # 转换为 tibble 对象
  mutate(date = date_decimal(value) %>%   # 计算日期列
           round_date("day") %>%          # 取整到天
           as_date()) %>%                 # POSIXct转Date格式
  rename(time = value)                    # 重命名列名

print(time_date)

# 检查每年记录次数
time_date %>% 
  mutate(year = year(date)) %>%           # 从date列解析年份
  count(year, name = "days")              # 统计每年的行数

# 检查重复日期
time_date %>% 
  mutate(date_only = date) %>%  # 提取日期部分(忽略时间)
  count(date_only) %>%          # 统计每个日期出现的次数
  filter(n > 1)                 # 筛选重复日期

# 将股票数据转换为数据框并合并日期
eu_stocks <- as_tibble(EuStockMarkets) %>% 
  mutate(date = time_date$date)

# 生成所有股票指数对组合
index_pairs <- combn(
  x = names(eu_stocks)[names(eu_stocks) != "date"], 
  m = 2, 
  simplify = FALSE
)

# 定义滑动窗口时间宽度
window_size <- frequency(EuStockMarkets) / 2

# 计算滚动相关系数
rolling_cor <- map_dfr(index_pairs, ~ {
  pair <- .
  df_pair <- eu_stocks %>% select(all_of(pair), date)
  
  corr_values <- rollapply(
        data = df_pair[, 1:2],
        width = window_size,
        FUN = function(x) cor(x[,1], x[,2], use = "pairwise.complete.obs"),
        by.column = FALSE,
        fill = NA
  )
  
  tibble(
    date = df_pair$date,
    correlation = corr_values,
    pair = paste(pair, collapse = "-")
  )
})

# 检查滚动计算结果中的NA比例
rolling_cor %>% 
  group_by(pair) %>% 
  summarise(na_rate = mean(is.na(correlation)))

# 绘制滚动窗口相关分析图
ggplot(rolling_cor, aes(x = date, y = correlation, fill = pair)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  geom_area(position = "identity", alpha = 0.35, na.rm = TRUE) +
  scale_fill_manual(
    values = c("#E41A1C", "#377EB8", "#4DAF4A", 
               "#984EA3", "#FF7F00", "#A65628")
  ) +
  scale_x_date(
    date_breaks = "1 years",
    date_labels = "%Y",
    expand = c(0.01, 0)
  ) +
  scale_y_continuous(limits = c(-1, 1)) +
  labs(
    x = "Year",
    y = paste0("Rolling ",window_size,"-Day Correlation"),
    title = paste0("Dynamic Correlations Between European Stock Indices (",min(year(eu_stocks$date)),"-",max(year(eu_stocks$date)),")"),
    subtitle = paste0(length(index_pairs)," Possible Index Pairs with ",window_size,"-Day Rolling Window"),
    caption = "Data Source: EuStockMarkets dataset"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10)
  )
