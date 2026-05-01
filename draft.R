methods("plot")

library(tidyverse)

rnorm(100) %>% 
  hist(breaks = seq(-4, 4, 1), 
       plot = FALSE) %>% 
  plot(col = "#004785", 
       border = "white", 
       xlim = c(-4, 4),
       ylim = c(0, 40),
       xaxt = "n")

axis(1, seq(-4, 4, 1))


# 1. 模拟数据与统计计算
h_info <- rnorm(100) %>% hist(plot = FALSE)

# 2. 绘制主体
plot(h_info, 
     col = "#044875",      # 海军蓝填充
     border = "white",     # 白色边框
     xlim = c(-4, 4),      # X轴范围
     ylim = c(0, 30),      # Y轴范围
     main = "Professional Histogram",
     xlab = "Standard Deviations",
     ylab = "Frequency",
     xaxt = "n",           # 禁用默认X轴
     las = 1)              # 关键：使所有坐标轴标签始终水平显示

# 3. 添加自定义 X 轴
axis(side = 1, at = seq(-4, 4, by = 1))

# 4. (可选) 添加一条虚线表示均值，增加学术感
abline(v = 0, col = "red", lty = 2, lwd = 1.5)

options()$repos



# anova -------------------------------------------------------------------

# 安装并加载必要的包
# install.packages("ggpubr")
library(gtsummary)
library(ggpubr)

# 提取并清理数据（去除缺失值以防报错）
df <- na.omit(trial[, c("grade", "marker")])

# 定义需要进行两两比较的组别
my_comparisons <- list( c("I", "II"), c("II", "III"), c("I", "III") )

# 绘制方差分析图
ggboxplot(trial, x = "grade", y = "marker",
          color = "grade",             
          palette = "jco",             # 使用《临床肿瘤学杂志》(JCO)的经典配色
          add = "jitter",              
          ylab = "Tumor Marker Level", xlab = "Tumor Grade",       
          legend = "none") +
  stat_compare_means(comparisons = list(c("I", "II"), c("II", "III"), c("I", "III")),
                     method = "t.test", 
                     label = "p.format",
                     hide.ns = TRUE) +
  stat_anova_test(label.y = 4.5)

# 绘制分组箱线图并标注t检验结果
ggboxplot(iris, x = "Species", y = "Sepal.Length",
          color = "Species", palette = "jco",
          ylab = "Sepal Length", xlab = "Species",
          add = "jitter") +
  stat_compare_means(comparisons = list(c("setosa", "versicolor"),
                                        c("setosa", "virginica"),
                                        c("versicolor", "virginica")),
                     method = "t.test",
                     label = "p.format",
                     hide.ns = TRUE)


library(gtsummary)
library(ggpubr)
library(dplyr)
library(tidyr)

# 第一步：必须先剔除相关变量的缺失值 (NA)
# 否则 stat_anova_test 在底层调用时会默默失败，不显示任何结果
clean_trial <- trial %>%
  drop_na(grade, marker)

# 第二步：获取最高点的值，用于稍微扩大Y轴上限，防止文字被边缘切掉
max_val <- max(clean_trial$marker)

# 第三步：绘图
ggboxplot(clean_trial, x = "grade", y = "marker",
          color = "grade",             
          palette = "jco",             
          add = "jitter",              
          ylab = "Tumor Marker Level", xlab = "Tumor Grade",       
          legend = "none") +
  
  # 适当扩大Y轴范围（最高点加上大概20%的空间），给两两比较连线和ANOVA文字留出位置
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.2))) +
  
  # 两两比较 (t 检验)
  stat_compare_means(comparisons = list(c("I", "II"), c("II", "III"), c("I", "III")),
                     method = "t.test", 
                     label = "p.format",
                     hide.ns = TRUE) +
  
  # 全局 ANOVA 结果：使用 npc="top" 相对位置，无论数据多大都会贴在图表上方
  stat_anova_test(label.y.npc = "top", vjust = 1)


# 安装并加载包
# install.packages("tidyplots")
library(gtsummary)
library(tidyplots)
library(ggpubr) # 依然需要它来提供 stat_anova_test
library(dplyr)
library(tidyr)

# 第一步：清洗数据（去除缺失值）
clean_trial <- trial %>% drop_na(grade, marker)

# 第二步：使用 tidyplots 流水线绘图
clean_trial %>%
  tidyplot(x = grade, y = marker, color = grade) %>% 
  
  # 1. 添加箱线图
  add_boxplot() %>% 
  
  # 2. 添加抖动散点
  add_data_points_jitter() %>% 
  
  # 3. 添加两两比较的 t检验 P值（它会自动为您计算并标注，不需要手动写 comparisons 列表！）
  add_test_pvalue(method = "t.test", hide.ns = TRUE) %>% 
  
  # 4. 移除多余的图例
  remove_legend() %>% 
  
  # 5. 跨界融合：使用 add() 将 ggpubr 的 ANOVA 统计量图层叠加上去
  add(stat_anova_test(label.y.npc = "top", vjust = 1))
