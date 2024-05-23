# Manually set working directory####
# Load needed packages####
library(dplyr)
library(lubridate)
library(readxl)



# Sample package ID####
# ID forms be like: yyyymmdd+number
# example: 2024022900001

# Sample dates####
# Dates' range from 2024-01-01 to 2026-12-31
dates <- seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "day")
# extract years\months\days from dates
years <- year(dates)
months <- month(dates)
days <- day(dates)
# formate dates as required string
for_dates <- sprintf("%d%02d%02d", years, months, days)

# Sample numbers####
# numbers are up to 99999 for the upper sales volume(8000*125%) == 10000
# numbers originate from everyday and follow a normal distribution
# about 8000 pacs pass by PIVAS daily
# 5000 of them are packaged for distribution
# only 3000 pacs need admixture
n = 1234

set.seed(n)
num_total <- round(
  abs(
    rnorm(
      n = length(for_dates),
      mean = 8000,
      sd = 700)# 8000±3*700=[5900,10100]
  )
) %>% lapply(seq_len)

# sum_total <- sapply(num_total, length)

sum_admix <- round(
  abs(
    rnorm(
      n = length(for_dates),
      mean = 3000,
      sd= 350# 3000±3*350=[1950,4050]
    )
  )
)

num_admix = list()
type = list()

for (i in 1:length(num_total)){
  num_admix[[i]] = sample(
    x = num_total[[i]],
    size = sum_admix[i],
    replace = FALSE
  )
  
  type[[i]] = factor(
    ifelse(
      num_total[[i]] %in% num_admix[[i]],
      "admixture",
      "distribution"
    ),
    levels = c("admixture", "distribution")
  )
}
# table(type[[1]])

# num_total %>% unlist() %>% table() %>% plot(main = "dens_to", xlab="Value", ylab="Density")
# num_admix %>% unlist() %>% table() %>% plot(main = "dens_ad", xlab="Value", ylab="Density")

# transform numbers into character formatted as 00001~99999 
for_num <- lapply(num_total, function(x) sprintf("%05d", x))

# Merge formatted numbers with formatted dates to form IDs####
ID <- lapply(1:length(for_dates), function(i) {
  paste0(for_dates[i], for_num[[i]])
}) %>% unlist()




# Merge types and ID####
type_raw <- unlist(type)
df <- data.frame(
  ID = ID,
  type = type_raw
) %>% filter(type == "admixture")



# Create variable####
# df <- df %>% mutate(
#   yearmonth = substring(ID, 1, 6) %>% as.numeric(),
#   risk1 = NA,
#   risk2 = NA,
#   risk3 = NA,
#   drug1 = NA,
#   price1 = NA,
#   drug2 = NA,
#   price2 = NA,
#   drug3 = NA,
#   price3 = NA,
#   time_work = NA,
#   salary = NA,
#   hat = NA,
#   price_hat = NA,
#   gloves = NA,
#   price_gloves = NA,
#   mask = NA,
#   price_mask = NA,
#   label = NA,
#   price_label = NA,
#   ethanol = NA,
#   price_ethanol = NA,
#   water = NA,
#   price_water = NA,
#   electricity = NA,
#   price_electricity = NA,
#   time_airCleaner = NA,
#   price_airCleaner = NA,
#   time_cleanBench = NA,
#   price_cleanBench = NA,
#   time_maintenance = NA,
#   price_maintenance = NA,
#   time_acquisition = NA,
#   price_acquisition = NA,
#   time_cart = NA,
#   price_cart = NA,
#   time_area = NA,
#   price_area = NA,
#   management = NA
# )



# 原始循环####
# total_time = 0
# for (n in 1:nrow(df)) {
#   start_time <- Sys.time()
#   if (n==1) {
#     df[n,]$risk1 = .7
#     df[n,]$risk2 = .8
#     df[n,]$risk3 = .9
#     df[n,]$drug1 = 1
#     df[n,]$price1 = 1.75
#     df[n,]$drug2 = 2
#     df[n,]$price2 = 42
#     df[n,]$drug3 = 0
#     df[n,]$price3 = 3
#     df[n,]$time_work = 0.55
#     df[n,]$salary = 155.56
#     df[n,]$hat = 2
#     df[n,]$price_hat = .3
#     df[n,]$gloves = 2
#     df[n,]$price_gloves = 1.9
#     df[n,]$mask = 2
#     df[n,]$price_mask = .28
#     df[n,]$label = 2
#     df[n,]$price_label = .46
#     df[n,]$ethanol = .001
#     df[n,]$price_ethanol = 8
#     df[n,]$water = .01
#     df[n,]$price_water = 3
#     df[n,]$electricity = 4.5
#     df[n,]$price_electricity = 1
#     df[n,]$time_airCleaner = .08
#     df[n,]$price_airCleaner = 19.73
#     df[n,]$time_cleanBench = .11
#     df[n,]$price_cleanBench = 1.07
#     df[n,]$time_maintenance = .084
#     df[n,]$price_maintenance = 14.2
#     df[n,]$time_acquisition = .1
#     df[n,]$price_acquisition = 6.31
#     df[n,]$time_cart = .48
#     df[n,]$price_cart = .06
#     df[n,]$time_area = 5
#     df[n,]$price_area = .004
#     df[n,]$management = 6.7
#   } else {
#     if (df[n,]$yearmonth - df[n-1,]$yearmonth >= 1) {
#       df[n,]$risk1 = df[n-1,]$risk1^.99
#       df[n,]$risk2 = df[n-1,]$risk2^.98
#       df[n,]$risk3 = df[n-1,]$risk3^.97
# 
#       df[n,]$time_work = abs(rnorm(1, df[n-1,]$time_work^0.999, df[n-1,]$time_work * 0.001))
#       df[n,]$ethanol = abs(rnorm(1, df[n-1,]$ethanol^0.999, df[n-1,]$ethanol * 0.001))
#       df[n,]$water = abs(rnorm(1, df[n-1,]$water^0.999, df[n-1,]$water * 0.001))
#       df[n,]$electricity = abs(rnorm(1, df[n-1,]$electricity^0.999, df[n-1,]$electricity * 0.001))
#       df[n,]$time_airCleaner = abs(rnorm(1, df[n-1,]$time_airCleaner^0.999, df[n-1,]$time_airCleaner * 0.001))
#       df[n,]$time_cleanBench = abs(rnorm(1, df[n-1,]$time_cleanBench^0.999, df[n-1,]$time_cleanBench * 0.001))
#       df[n,]$time_maintenance = abs(rnorm(1, df[n-1,]$maintenance^0.999, df[n-1,]$maintenance * 0.001))
#       df[n,]$time_acquisition = abs(rnorm(1, df[n-1,]$acquisition^0.999, df[n-1,]$acquisition * 0.001))
#       df[n,]$time_cart = abs(rnorm(1, df[n-1,]$time_cart^0.999, df[n-1,]$time_cart * 0.001))
#       df[n,]$time_area = abs(rnorm(1, df[n-1,]$time_area^0.999, df[n-1,]$time_area * 0.001))
#     } else {
#       df[n,]$risk1 = df[n-1,]$risk1
#       df[n,]$risk2 = df[n-1,]$risk2
#       df[n,]$risk3 = df[n-1,]$risk3
# 
#       df[n,]$time_work = abs(rnorm(1, df[n-1,]$time_work, df[n-1,]$time_work * 0.001))
#       df[n,]$ethanol = abs(rnorm(1, df[n-1,]$ethanol^0.999, df[n-1,]$ethanol * 0.001))
#       df[n,]$water = abs(rnorm(1, df[n-1,]$water^0.999, df[n-1,]$water * 0.001))
#       df[n,]$electricity = abs(rnorm(1, df[n-1,]$electricity^0.999, df[n-1,]$electricity * 0.001))
#       df[n,]$time_airCleaner = abs(rnorm(1, df[n-1,]$time_airCleaner^0.999, df[n-1,]$time_airCleaner * 0.001))
#       df[n,]$time_cleanBench = abs(rnorm(1, df[n-1,]$time_cleanBench^0.999, df[n-1,]$time_cleanBench * 0.001))
#       df[n,]$time_maintenance = abs(rnorm(1, df[n-1,]$maintenance^0.999, df[n-1,]$maintenance * 0.001))
#       df[n,]$time_acquisition = abs(rnorm(1, df[n-1,]$acquisition^0.999, df[n-1,]$acquisition * 0.001))
#       df[n,]$time_cart = abs(rnorm(1, df[n-1,]$time_cart^0.999, df[n-1,]$time_cart * 0.001))
#       df[n,]$time_area = abs(rnorm(1, df[n-1,]$time_area^0.999, df[n-1,]$time_area * 0.001))
#     }
#     df[n,]$drug1 = sample(0:1,1)
#     df[n,]$drug2 = sample(0:2,1)
#     df[n,]$drug3 = sample(c(0,2),1)
#     df[n,]$hat = sample(2:4,1)
#     df[n,]$gloves = sample(2:4,1)
#     df[n,]$mask = sample(2:4,1)
#     df[n,]$label = sample(2:4,1)
# 
#     df[n,]$price1 = df[n-1,]$price1
#     df[n,]$price2 = df[n-1,]$price2
#     df[n,]$price3 = df[n-1,]$price3
#     df[n,]$salary = df[n-1,]$salary
#     df[n,]$price_hat = df[n-1,]$price_hat
#     df[n,]$price_gloves = df[n-1,]$price_gloves
#     df[n,]$price_mask = df[n-1,]$price_mask
#     df[n,]$price_label = df[n-1,]$price_label
#     df[n,]$price_ethanol = df[n-1,]$price_ethanol
#     df[n,]$price_water = df[n-1,]$price_water
#     df[n,]$price_electricity = df[n-1,]$price_electricity
#     df[n,]$price_airCleaner = df[n-1,]$price_airCleaner
#     df[n,]$price_cleanBench = df[n-1,]$price_cleanBench
#     df[n,]$price_maintenance = df[n-1,]$price_maintenance
#     df[n,]$price_acquisition = df[n-1,]$price_acquisition
#     df[n,]$price_cart = df[n-1,]$price_cart
#     df[n,]$price_area = df[n-1,]$price_area
#     df[n,]$management = df[n-1,]$management
#   }
#   end_time <- Sys.time()
#   time_diff <- as.numeric(difftime(end_time, start_time, units = "secs"))
#   total_time =total_time + time_diff
#   hours <- floor(total_time / 3600)
#   minutes <- floor((total_time %% 3600) / 60)
#   seconds <- round(total_time %% 60, 2)
# 
#   cat("第", n, "次循环耗时", round(time_diff,2), "秒,", "共耗时", hours, "时", minutes, "分", seconds, "秒\n")
# }



# 第一种写法1行1秒~50小时
# 这一种写法1行0.01秒~0.5小时####
# rand_vals <- data.frame(
#   time_work = abs(rnorm(nrow(df), 0.55, 0.001)),
#   ethanol = abs(rnorm(nrow(df), 0.001, 0.001)),
#   water = abs(rnorm(nrow(df), 0.01, 0.001)),
#   electricity = abs(rnorm(nrow(df), 4.5, 0.001)),
#   time_airCleaner = abs(rnorm(nrow(df), 0.08, 0.001)),
#   time_cleanBench = abs(rnorm(nrow(df), 0.11, 0.001)),
#   maintenance = abs(rnorm(nrow(df), 0.084, 0.001)),
#   acquisition = abs(rnorm(nrow(df), 0.1, 0.001)),
#   time_cart = abs(rnorm(nrow(df), 0.48, 0.001)),
#   time_area = abs(rnorm(nrow(df), 5, 0.001))
# )
# 
# df[1, c("risk1", "risk2", "risk3", "drug1", "price1", "drug2", "price2", "drug3", "price3",
#         "time_work", "salary", "hat", "price_hat", "gloves", "price_gloves", "mask", "price_mask",
#         "label", "price_label", "ethanol", "price_ethanol", "water", "price_water", "electricity",
#         "price_electricity", "time_airCleaner", "price_airCleaner", "time_cleanBench", "price_cleanBench",
#         "maintenance", "price_maintenance", "acquisition", "price_acquisition", "time_cart", "price_cart",
#         "time_area", "price_area", "management")] <- c(0.7, 0.8, 0.9, 1, 1.75, 2, 42, 0, 3,
#                                                        0.55, 155.56, 2, 0.3, 2, 1.9, 2, 0.28,
#                                                        2, 0.46, 0.001, 8, 0.01, 3, 4.5, 1,
#                                                        0.08, 19.73, 0.11, 1.07, 0.084, 14.2,
#                                                        0.1, 6.31, 0.48, 0.06, 5, 0.004, 6.7)
# 
# 
# total_time = 0
# for (n in 2:nrow(df)) {
#   start_time <- Sys.time()
#   if (df[n, "yearmonth"] - df[n-1, "yearmonth"] >= 1) {
#     df[n, c("risk1", "risk2", "risk3")] <- df[n-1, c("risk1", "risk2", "risk3")]^c(0.99, 0.98, 0.97)
#   }
# 
#   df[n, "time_work"] <- rand_vals[n, "time_work"]
#   df[n, "ethanol"] <- rand_vals[n, "ethanol"]
#   df[n, "water"] <- rand_vals[n, "water"]
#   df[n, "electricity"] <- rand_vals[n, "electricity"]
#   df[n, "time_airCleaner"] <- rand_vals[n, "time_airCleaner"]
#   df[n, "time_cleanBench"] <- rand_vals[n, "time_cleanBench"]
#   df[n, "maintenance"] <- rand_vals[n, "maintenance"]
#   df[n, "acquisition"] <- rand_vals[n, "acquisition"]
#   df[n, "time_cart"] <- rand_vals[n, "time_cart"]
#   df[n, "time_area"] <- rand_vals[n, "time_area"]
# 
#   df[n, c("drug1", "drug2", "drug3", "hat", "gloves", "mask", "label")] <- sample(c(0, 1), 7, replace=TRUE)
# 
#   df[n, c("price1", "price2", "price3", "salary", "price_hat", "price_gloves", "price_mask",
#           "price_label", "price_ethanol", "price_water", "price_electricity", "price_airCleaner",
#           "price_cleanBench", "price_maintenance", "price_acquisition", "price_cart", "price_area",
#           "management")] <- df[n-1, c("price1", "price2", "price3", "salary", "price_hat", "price_gloves",
#                                       "price_mask", "price_label", "price_ethanol", "price_water",
#                                       "price_electricity", "price_airCleaner", "price_cleanBench",
#                                       "price_maintenance", "price_acquisition", "price_cart", "price_area",
#                                       "management")]
#   end_time <- Sys.time()
#   time_diff <- as.numeric(difftime(end_time, start_time, units = "secs"))
#   total_time =total_time + time_diff
#   hours <- floor(total_time / 3600)
#   minutes <- floor((total_time %% 3600) / 60)
#   seconds <- round(total_time %% 60, 2)
# 
#   # 输出消耗时间
#   cat("第", n, "次循环耗时", round(time_diff,2), "秒,", "共耗时", hours, "时", minutes, "分", seconds, "秒\n")
# }



# 第三种写法####
# 将变量分类
# 按月变化的预先生成数据
# 直接在创建数据框时写入，减少插入数据框索引耗时
# 统计每月业务量
yearmonth = substring(df$ID, 1, 6) %>% as.numeric()
sum_yearmonth <- table(yearmonth) %>% as.numeric()

# 对逐月变化变量统一生成####
risk1 = c()
risk2 = c()
risk3 = c()
time_work = c()
time_airCleaner = c()
time_cleanBench = c()
time_maintenance = c()
time_acquisition = c()
time_cart = c()

for (m in 1:length(sum_yearmonth)){
  risk1. = rep(.7*.99^(m-1), sum_yearmonth[m])
  risk2. = rep(.8*.99^(m-1), sum_yearmonth[m])
  risk3. = rep(.9*.99^(m-1), sum_yearmonth[m])
  risk1 = c(risk1,risk1.)
  risk2 = c(risk2,risk2.)
  risk3 = c(risk3,risk3.)
  time_work. = abs(rnorm(n=sum_yearmonth[m], mean=.55*.999^(m-1), sd=.55*.999^(m-1)*.001))
  time_airCleaner. = abs(rnorm(n=sum_yearmonth[m], mean=.08*.999^(m-1), sd=.08*.999^(m-1)*.001))
  time_cleanBench. = abs(rnorm(n=sum_yearmonth[m], mean=.11*.999^(m-1), sd=.11*.999^(m-1)*.001))
  time_maintenance. = abs(rnorm(n=sum_yearmonth[m], mean=.084*.999^(m-1), sd=.084*.999^(m-1)*.001))
  time_acquisition. = abs(rnorm(n=sum_yearmonth[m], mean=.1*.999^(m-1), sd=.1*.999^(m-1)*.001))
  time_cart. = abs(rnorm(n=sum_yearmonth[m], mean=.48*.999^(m-1), sd=.48*.999^(m-1)*.001))
  time_work = c(time_work, time_work.)
  time_airCleaner = c(time_airCleaner, time_airCleaner.)
  time_cleanBench = c(time_cleanBench, time_cleanBench.)
  time_maintenance = c(time_maintenance, time_maintenance.)
  time_acquisition = c(time_acquisition, time_acquisition.)
  time_cart = c(time_cart, time_cart.)
} 

# 对整数抽样变量统一生成####

drug1 = sample(0:1, sum(sum_yearmonth), replace = TRUE)
drug2 = sample(0:2, sum(sum_yearmonth), replace = TRUE)
drug3 = sample(c(0,2), sum(sum_yearmonth), replace = TRUE)
hat = sample(2:4, sum(sum_yearmonth), replace = TRUE)
gloves = sample(2:4, sum(sum_yearmonth), replace = TRUE)
mask = sample(2:4, sum(sum_yearmonth), replace = TRUE)
label = sample(2:4, sum(sum_yearmonth), replace = TRUE)


# 对全年不变变量统一生成####

price1 = rep(1.75, sum(sum_yearmonth))
price2 = rep(42, sum(sum_yearmonth))
price3 = rep(3, sum(sum_yearmonth))
salary = rep(155.56, sum(sum_yearmonth))
price_hat = rep(.3, sum(sum_yearmonth))
price_gloves = rep(1.9, sum(sum_yearmonth))
price_mask = rep(.28, sum(sum_yearmonth))
price_label = rep(.46, sum(sum_yearmonth))
ethanol = rep(.001, sum(sum_yearmonth))
price_ethanol = rep(9, sum(sum_yearmonth))
water = rep(.01, sum(sum_yearmonth))
price_water = rep(3, sum(sum_yearmonth))
electricity = rep(4.5, sum(sum_yearmonth))
price_electricity = rep(1, sum(sum_yearmonth))
price_airCleaner = rep(19.73, sum(sum_yearmonth))
price_cleanBench = rep(1.07, sum(sum_yearmonth))
price_maintenance = rep(14.2, sum(sum_yearmonth))
price_acquisition = rep(6.31, sum(sum_yearmonth))
price_cart = rep(.06, sum(sum_yearmonth))
time_area = rep(5, sum(sum_yearmonth))
price_area = rep(.004, sum(sum_yearmonth))
management = rep(6.7, sum(sum_yearmonth))

# 所有变量写入数据框####
variables <- c("risk1", "risk2", "risk3", "drug1", "price1", "drug2", "price2", "drug3", "price3", "time_work", "salary", "hat", "price_hat", "gloves", "price_gloves", "mask", "price_mask", "label", "price_label", "ethanol", "price_ethanol", "water", "price_water", "electricity", "price_electricity", "time_airCleaner", "price_airCleaner", "time_cleanBench", "price_cleanBench", "time_maintenance", "price_maintenance", "time_acquisition", "price_acquisition", "time_cart", "price_cart", "time_area", "price_area", "management")

for (variable in variables) {
  df[[variable]] <- get(variable)
}



# Counting cost####
df <- df %>% 
  mutate(
    cost = unlist(
      rowSums(
        .[,seq(6, ncol(.) - 2, by = 2)] * .[,seq(7, ncol(.) - 1, by = 2)]
      ) + .[,ncol(.)]
    )
  )



# Save and Load data####
# row.names=FALSE failed
# write.csv(df[1:8990,],file = "df.csv",row.names=FALSE,fileEncoding = "utf-8")
# ID omitted
# df=read.csv("df.csv")
# saveRDS(df,"df.Rds")
