# ---- L3: RStudio窗口与基础绘图 ---------------------------------------------

# 矩阵散点图
data(mtcars)
plot(mtcars)

# 散点图
plot(mtcars$wt, mtcars$mpg,
     col  = "skyblue",
     pch  = 19,
     cex  = 1.5,
     las  = 1,
     main = "Weight & MPG",
     xlab = "Weight (1000 lbs)",
     ylab = "Miles/(US) gallon")

# 分组箱线图
plot(as.factor(mtcars$cyl), mtcars$mpg,
     col      = c("#8DA0CB", "#FC8D62", "#66C2A5"),
     border   = "gray40",
     las      = 1,
     main     = "MPG Distribution by Engine Cylinders",
     xlab     = "Number of Cylinders",
     ylab     = "Miles/(US) gallon",
     font.main = 2)

# 条形图
plot(as.factor(mtcars$cyl),
     col      = c("#8DA0CB", "#FC8D62", "#66C2A5"),
     border   = "gray50",
     las      = 1,
     main     = "Distribution of Cylinders",
     xlab     = "Number of Cylinders",
     font.main = 2,
     ylim     = c(0, 15))

# 回归模型诊断图
model1 <- lm(mpg ~ wt + cyl + disp, data = mtcars)
par(mfrow = c(2, 2))
plot(model1)
par(mfrow = c(1, 1))

# 正弦函数图像
plot(sin, -2*pi, 2*pi,
     col  = "skyblue",
     lwd  = 2,
     main = "Sine Wave Form")

# 标准正态分布概率密度函数
plot(dnorm, from = -4, to = 4,
     main = "N(0,1) PDF",
     col  = "skyblue",
     lwd  = 3)


# ---- L4: 只用5分钟!Base R跑通完整统计分析 -------------------------------------------

# 1 描述统计
# five-number summary
summary(mtcars)

# descriptive statistics
sapply(mtcars,
       function(x) c(Mean   = mean(x),
                     Sd     = sd(x),
                     Median = median(x),
                     Min    = min(x),
                     Max    = max(x)))

# 2 可视化
# 直方图
hist(mtcars$mpg,
     col    = "lightblue",
     border = "white",
     main   = "Distribution of Miles Per Gallon",
     xlab   = "Miles/(US) gallon",
     ylab   = "Frequency",
     ylim   = c(0, 14),
     las    = 1)

# 条形图（分组）
plot(as.factor(mtcars$cyl), as.factor(mtcars$am),
     col  = c("lightblue", "salmon"),
     main = "Cylinders vs Transmission",
     xlab = "Cylinders",
     ylab = "Transmission (0=Auto, 1=Manual)")

# 分组箱线图（两组）
plot(as.factor(mtcars$am), mtcars$mpg,
     col  = c("lightblue", "salmon"),
     cex  = 1.5,
     main = "Transmission Type & MPG",
     xlab = "Transmission (0 = automatic, 1 = manual)",
     ylab = "Miles/(US) gallon",
     las  = 1)

# 分组箱线图（三组，水平方向）
plot(as.factor(mtcars$cyl), mtcars$mpg,
     col        = c("#8DA0CB", "#FC8D62", "#66C2A5"),
     cex        = 1.5,
     main       = "MPG by Number of Cylinders",
     xlab       = "Number of Cylinders",
     ylab       = "Miles/(US) gallon",
     las        = 1,
     horizontal = TRUE)

# 散点图
plot(mtcars$wt, mtcars$mpg,
     col  = "lightblue",
     pch  = 19,
     cex  = 1.5,
     main = "Weight & MPG",
     xlab = "Weight (1000 lbs)",
     ylab = "Miles/(US) gallon")

# 3 统计检验
# 独立性检验
fisher.test(table(mtcars$cyl, mtcars$am))

# t 检验
t.test(mtcars$mpg ~ mtcars$am)

# 方差分析（ANOVA）
aov_result <- aov(mpg ~ as.factor(cyl), data = mtcars)
summary(aov_result)

# 4 线性回归
model1 <- lm(mpg ~ wt + cyl + am, data = mtcars)
summary(model1)


# ---- L5: R包的安装和加载 ---------------------------------------------------

library(tidyverse)

# 从 CRAN 安装（eval: false）
install.packages("tidyverse")

# 从 GitHub 安装（eval: false）
install.packages("remotes")   # 只需安装一次
remotes::install_github("dill/emoGG")
library(emoGG)

# 从 Bioconductor 安装（eval: false）
install.packages("BiocManager")
BiocManager::install("DESeq2")

# 加载包并显式调用
library(psych)
psych::describe(mtcars)

# 函数名冲突示例（eval: false）
library(dplyr)
library(MASS)        # 后加载的 MASS 遮盖了 dplyr 的 select
select(mtcars, mpg, wt)        # 可能报错
dplyr::select(mtcars, mpg, wt) # 明确使用 dplyr::select

# 查看当前冲突的函数
conflicts()

# 使用 pacman 智能加载（eval: false）
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, psych, corrplot)


# ---- L6: R语言自学力：高效查阅帮助文档 -------------------------------------

# 查看帮助文档的几种方式
?hist       # 精确查找
??hist      # 模糊查找
help(hist)  # 函数式调用

# 官方示例演示（eval: false）
example(boxplot)

# 理解函数输入与输出
mpg_hist <- hist(mtcars$mpg,
                 breaks = "Sturges",
                 col    = "lightgray",
                 main   = "MPG Distribution",
                 xlab   = "Miles Per Gallon")

mpg_hist            # 查看返回的对象
mpg_hist$counts     # 提取频数
mpg_hist$breaks     # 提取分隔点

# 数据集帮助文档
?mtcars

# 图形参数帮助
?par

# 加载包后再查帮助
library(corrplot)
?corrplot


# ---- L7: 项目的创建和管理 --------------------------------------------------

library(tidyverse)

# 使用相对路径读取数据（eval: false）
library(tidyverse)
df <- read_csv("data/raw_data.csv")

# 高保真 SVG 矢量图导出（eval: false）
# install.packages("svglite")
library(tidyverse)
library(svglite)

pic1 <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width,
                         color = Species)) +
  geom_point(size = 3, alpha = 0.8) +
  theme_minimal()

ggsave(filename = "output/fig1_sepal_scatter.svg",
       plot = pic1, width = 8, height = 6)

