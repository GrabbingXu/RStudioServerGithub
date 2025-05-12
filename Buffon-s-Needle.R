# https://blog.csdn.net/2301_79376014/article/details/142071980

# 设置实验参数
a <- 1       # 平行线间距
l <- 0.5     # 针的长度
n <- 100000  # 投掷次数

# 初始化存储变量
m <- 0
x_history <- numeric(n)
phi_history <- numeric(n)
joint_list <- logical(n)

# 执行模拟实验
set.seed(123) # 确保可重复性
for (i in 1:n) {
  x <- runif(1, 0, a/2)
  phi <- runif(1, 0, pi)
  
  x_history[i] <- x
  phi_history[i] <- phi
  
  if (l * sin(phi)/2 >= x) {
    m <- m + 1
    joint_list[i] <- TRUE
  } else {
    joint_list[i] <- FALSE
  }
}

# 计算圆周率
PI <- (2 * l * n) / (a * m)
cat("模拟所得圆周率:", PI, "\n")

# 可视化结果
plot_colors <- ifelse(joint_list, "red", "blue")
plot(phi_history, x_history, 
     col = scales::alpha(plot_colors, 0.5), 
     pch = 19, cex = 0.3,
     xlab = "Needle Angle (φ)", 
     ylab = "Midpoint Distance (y)",
     main = paste("Buffon's Needle Experiment (n=", format(n, big.mark = ","), ")", sep = ""),
     xaxt = "n", yaxt = "n")

# 设置坐标轴
axis(1, at = c(0, pi/2, pi), labels = c("0", "π/2", "π"))
axis(2, at = c(0, a/4, a/2), labels = c("0", "a/4", "a/2"))

# 添加图例
legend("topright", 
       legend = c("Intersected", "Not Intersected"), 
       col = c("red", "blue"), 
       pch = 19,
       title = "Intersection Status")

# 添加理论边界曲线
phi_seq <- seq(0, pi, length.out = 200)
y_line <- (l/2) * sin(phi_seq)
lines(phi_seq, y_line, col = "darkgreen", lwd = 4, lty = 2)
