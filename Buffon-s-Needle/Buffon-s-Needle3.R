# 蒲丰投针法估算π值####
fun_buffon <- function(seed=123, N=1e6, l=1, d=1) {
  set.seed(seed)  # 设置随机种子以确保结果可重复
  
  # 参数设置
  N <- N         # 投针次数
  l <- l         # 针的长度
  d <- d         # 平行线间距
  
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
  abs_err <- abs(pi - pi_est)   # 绝对误差
  
  # 输出结果
  data.frame(
    seed = seed,          # 种子数
    trials = N,           # 投针次数
    crosses = x,          # 相交次数
    pi_estimate = pi_est, # 估算π值
    abs_error = abs_err   # 绝对误差
  )
}

fun_buffon(123,1e7) + gc()

# 增加随机种子数####
fun_seed <- function(seeds=seq(100), N=1e6, l=1, d=1, progress = TRUE) {
  # 检查依赖包
  if (progress && !requireNamespace("progress", quietly = TRUE)) {
    stop("需要安装 progress 包: install.packages('progress')")
  }
  
  # 进度条
  if (progress) {
    require(progress)
    pb <- progress_bar$new(
      format = paste0("Buffon's Needle Test :current/:total(:percent)[:bar]:tick_rate/s(:elapsed:spin:eta)"),
      total = length(seeds),
      clear = FALSE
    )
  }
  
  # 初始化结果存储
  results <- data.frame()
  
  # 循环计算
  for (seed in seeds) {
    # 执行单次实验并合并结果
    results <- rbind(results, fun_buffon(seed, N, l, d))
    
    # 更新进度条
    if (progress) pb$tick()
    gc()
  }
  
  # 返回结果
  cat(sprintf(
    "
    ============== 综合统计结果 ==============
    实验次数:        %15d 次
    平均投针次数:    %15s 次
    平均相交次数:    %15s 次
    估算π均值:      %18.6f [%.6f, %.6f]
    平均绝对误差:    %18.6f [%.6f, %.6f]
    ==========================================
    ", 
    length(results$seed),
    format(mean(results$trials), big.mark = ","),
    format(mean(results$crosses), big.mark = ","),
    mean(results$pi_estimate),
    t.test(results$pi_estimate)$conf.int[1],
    t.test(results$pi_estimate)$conf.int[2],
    mean(results$abs_error),
    t.test(results$abs_error)$conf.int[1],
    t.test(results$abs_error)$conf.int[2]
  )
  )
  
  return(results)
  gc()
}

results <- fun_seed(seq(100),1e7)

