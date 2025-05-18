# 蒲丰投针法估算π值
set.seed(123)  # 设置随机种子以确保结果可重复

# 参数设置
N <- 1e6       # 投针次数
l <- 1         # 针的长度
d <- 1         # 平行线间距

# 生成随机投针数据
y0 <- runif(N, 0, d)          # 针的中点距最近平行线的距离（均匀分布在0到d之间）
theta <- runif(N, 0, pi)      # 针的倾斜角度（均匀分布在0到π之间）

# 计算针两端的垂直位置
lower_end <- y0 - (l/2) * sin(theta)  # 下端位置
upper_end <- y0 + (l/2) * sin(theta)  # 上端位置

# 判断是否与平行线相交（接触或跨越）
crosses <- (lower_end <= 0) | (upper_end >= d)

# 统计相交次数并估算π值
x <- sum(crosses)             # 相交次数
pi_est <- (2 * N) / x         # 根据公式 π ≈ 2N/x

# 输出结果
cat("投针次数:", N, "\n")
cat("相交次数:", x, "\n")
cat("估算的π值:", pi_est, "\n")
cat("实际π值:", pi, "\n")
cat("绝对误差:", abs(pi - pi_est), "\n")