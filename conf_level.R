# 指定图形的中文字体


par(family  = 'STKaiti')
install.packages("showtext")
library(showtext)
showtext_auto()


# --------------------------
# 置信水平模拟完整代码
# --------------------------

# 1. 设定参数与生成数据
# --------------------------
true_mean <- 165     # 真实平均身高
std_dev <- 10        # 标准差
sample_size <- 30    # 每次抽样人数
n_simulations <- 100 # 模拟100次抽样
set.seed(123)        # 固定随机种子使结果可重复

# 生成100次抽样数据（矩阵格式：100行x30列）
samples <- matrix(
  rnorm(n_simulations * sample_size, mean = true_mean, sd = std_dev),
  nrow = n_simulations,
  ncol = sample_size
)

# 2. 计算置信区间
# --------------------------
# 计算每次抽样的均值、标准差和标准误
sample_means <- apply(samples, 1, mean)
sample_sds <- apply(samples, 1, sd)
standard_errors <- sample_sds / sqrt(sample_size)

# 计算t分布临界值（95%置信水平，双尾）
t_critical <- qt(0.975, df = sample_size - 1)

# 计算置信区间上下限
lower_bounds <- sample_means - t_critical * standard_errors
upper_bounds <- sample_means + t_critical * standard_errors

# 3. 统计覆盖率
# --------------------------
contains_true_mean <- (lower_bounds <= true_mean) & (true_mean <= upper_bounds)
coverage_rate <- mean(contains_true_mean) * 100
cat(sprintf("95%%置信区间覆盖真实均值的比例：%.1f%%\n", coverage_rate))


# 4. 可视化前20次抽样结果（基础绘图）
# --------------------------
# 创建绘图参数
plot(
  1, type = "n",
  xlim = c(min(lower_bounds[1:20]) - 2, max(upper_bounds[1:20]) + 2),
  ylim = c(0, 21),
  xlab = "身高 (cm)", ylab = "抽样次数",
  main = "95%置信区间覆盖真实均值示例（前20次抽样）"
)

# 绘制置信区间和样本均值
for (i in 1:20) {
  # 绘制置信区间线段
  lines(c(lower_bounds[i], upper_bounds[i]), c(i, i), col = "blue", lwd = 2)
  # 标记样本均值点
  points(sample_means[i], i, pch = 19, col = "red", cex = 0.8)
}

# 添加真实均值参考线
abline(v = true_mean, lty = 2, col = "darkgreen", lwd = 2)
legend(
  "topright",
  legend = c("置信区间", "样本均值", "真实均值"),
  col = c("blue", "red", "darkgreen"),
  lty = c(1, NA, 2),
  pch = c(NA, 19, NA),
  bg = "white"
)
