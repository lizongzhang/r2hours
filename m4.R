# ---- L13: 数据文件的导入 ----

# 安装与加载（eval: false）
# install.packages("tidyverse")
# library(tidyverse)

# 导入 Excel 文件（eval: false）
# library(readxl)
# df <- read_xlsx("stroke.xlsx")
# df <- read_xlsx("stroke.xlsx",
#                  sheet = 1,        # 导入第几个工作表
#                  skip = 0,         # 跳过前几行
#                  col_names = TRUE) # 第一行作为变量名

# 导入 CSV 文件（eval: false）
# library(readr)
# df <- read_csv("stroke.csv")   # tidyverse 推荐方式
# df <- read.csv("stroke.csv")   # 基础 R 函数

# 导入 TXT 文件（eval: false）
# df <- read_delim("stroke.txt", delim = ";")   # 分号分隔
# df <- read_delim("stroke.txt", delim = " ")   # 空格分隔


# ---- L14: 数据可视化 ----

library(tidyverse)

# 加载数据，查看结构
data(mpg)
str(mpg)

# 第1层：建立画布，指定数据和映射
ggplot(mpg, aes(x = displ, y = hwy))

# 第2层：加上散点图层
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point()

# 第3层：再加一条趋势线
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# aes() 内：颜色随 drv 变量变化，自动生成图例
ggplot(mpg, aes(x = displ, y = hwy, color = drv)) +
  geom_point()

# aes() 外：所有点统一设为蓝色，没有图例
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(color = "steelblue")

# 散点图：颜色区分驱动方式，大小反映气缸数
ggplot(mpg, aes(x = displ, y = hwy,
                color = drv, size = cyl)) +
  geom_point(alpha = 0.7) +
  labs(title  = "发动机排量与高速油耗的关系",
       x      = "发动机排量（升）",
       y      = "高速油耗（mpg）",
       color  = "驱动方式",
       size   = "气缸数") +
  theme_minimal()

# 散点图：只映射 class 颜色
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = class))

# 箱线图：不同车型的高速油耗分布
ggplot(mpg, aes(x = class, y = hwy, fill = class)) +
  geom_boxplot() +
  labs(title = "不同车型的高速油耗分布",
       x     = "车型",
       y     = "高速油耗（mpg）") +
  theme_minimal() +
  theme(legend.position = "none")

# 箱线图：配合因子控制 x 轴顺序（按油耗中位数排列）
library(forcats)
ggplot(mpg, aes(x = fct_reorder(class, hwy, median),
                y = hwy,
                fill = class)) +
  geom_boxplot() +
  labs(title = "车型按油耗中位数排列",
       x = "车型", y = "高速油耗（mpg）") +
  theme_minimal() +
  theme(legend.position = "none")

# 柱状图：geom_bar() 自动统计频次
ggplot(mpg, aes(x = class, fill = class)) +
  geom_bar() +
  labs(title = "各车型数量",
       x = "车型", y = "数量") +
  theme_minimal() +
  theme(legend.position = "none")

# 柱状图：按频数降序排列
ggplot(mpg, aes(x = fct_infreq(class), fill = class)) +
  geom_bar() +
  labs(title = "各车型数量（按频数降序）",
       x = "车型", y = "数量") +
  theme_minimal() +
  theme(legend.position = "none")

# 分组柱状图：展示车型和驱动方式
ggplot(mpg, aes(x = class, fill = drv)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("#8DA0CB", "#FC8D62", "#66C2A5")) +
  labs(title = "各车型中不同驱动方式的数量",
       x = "车型", y = "数量", fill = "驱动方式") +
  theme_minimal()

# 直方图：查看油耗的分布
ggplot(mpg, aes(x = hwy)) +
  geom_histogram(binwidth = 2,
                 fill = "#8DA0CB",
                 color = "white") +
  labs(title = "高速油耗的分布",
       x = "高速油耗（mpg）", y = "频次") +
  theme_minimal()

# 密度图：按驱动方式分组，比较分布形状
ggplot(mpg, aes(x = hwy, fill = drv)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("#8DA0CB", "#FC8D62", "#66C2A5")) +
  labs(title = "不同驱动方式的油耗分布",
       x = "高速油耗（mpg）", y = "密度",
       fill = "驱动方式") +
  theme_minimal()

# 分面：按驱动方式分面，各画一张散点图
ggplot(mpg, aes(x = displ, y = hwy, color = drv)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ drv) +
  scale_color_manual(values = c("#8DA0CB", "#FC8D62", "#66C2A5")) +
  labs(title = "不同驱动方式下排量与油耗的关系",
       x = "发动机排量（升）", y = "高速油耗（mpg）") +
  theme_minimal() +
  theme(legend.position = "none")

# 标题与主题：labs() + scale_color_manual + theme_minimal
ggplot(mpg, aes(x = displ, y = hwy, color = drv)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(values = c("#8DA0CB","#FC8D62","#66C2A5"),
                     labels  = c("四驱", "前驱", "后驱")) +
  labs(title    = "发动机排量与高速油耗",
       subtitle = "按驱动方式分组",
       caption  = "数据来源：ggplot2::mpg",
       x        = "发动机排量（升）",
       y        = "高速油耗（mpg）",
       color    = "驱动方式") +
  theme_minimal()

# 常用主题演示
p <- ggplot(mpg, aes(x = displ, y = hwy)) + geom_point()

p + theme_gray()      # 默认主题（灰色背景）
p + theme_minimal()   # 简洁风格（最常用）
p + theme_classic()   # 经典风格（期刊常用）
p + theme_bw()        # 黑白风格

# 报错示例1：图层间用 |> 会报错（eval: false）
# ggplot(mpg, aes(x = displ, y = hwy)) |>
#   geom_point()

# 正确：图层之间用 +
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point()

# 报错示例2：固定颜色写进 aes()（eval: false）
# ggplot(mpg, aes(x = displ, y = hwy, color = "steelblue")) +
#   geom_point()

# 正确：固定颜色写在 aes() 外面
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(color = "steelblue")

# 报错示例3：+ 放在行首会报错（eval: false）
# ggplot(mpg, aes(x = displ, y = hwy))
#   + geom_point()

# 正确：+ 放在当前行的行尾
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point()

# 报错示例4：geom_bar() 用于已汇总数据（eval: false）
# df <- data.frame(class = c("suv","compact","pickup"),
#                  n     = c(62, 47, 33))
# ggplot(df, aes(x = class, y = n)) +
#   geom_bar()   # 错误：应使用 geom_col()

# 正确：已有汇总数据用 geom_col()
df <- data.frame(class = c("suv","compact","pickup"),
                 n     = c(62, 47, 33))
ggplot(df, aes(x = class, y = n, fill = class)) +
  geom_col() +
  theme_minimal() +
  theme(legend.position = "none")


# ---- L15: ggstatsplot 统计检验的可视化 ----

# 安装与加载（eval: false）
# install.packages(c("tidyverse", "ggstatsplot"))
# library(tidyverse)
# library(ggstatsplot)

library(ggstatsplot)

# 一行代码：箱线图 + 统计检验 + 效应量
ggbetweenstats(data = iris,
               x = Species, y = Sepal.Length)

# 三种鸢尾花的花萼长度比较
ggbetweenstats(
  data  = iris,
  x     = Species,
  y     = Sepal.Length,
  title = "三种鸢尾花的花萼长度比较"
)

# 两组比较：手动挡 vs 自动挡的油耗
mtcars$am_label <- factor(mtcars$am,
                          levels = c(0, 1),
                          labels = c("自动挡", "手动挡"))
ggbetweenstats(
  data  = mtcars,
  x     = am_label,
  y     = mpg,
  title = "自动挡与手动挡的油耗比较"
)

# 常用参数示例
ggbetweenstats(
  data        = iris,
  x           = Species,
  y           = Petal.Length,
  title       = "三种鸢尾花的花瓣长度",
  xlab        = "品种",
  ylab        = "花瓣长度（cm）",
  point.args  = list(alpha = 0.3, size = 2),
  centrality.plotting = TRUE
)

# 相关分析：花瓣长度与花瓣宽度
ggscatterstats(
  data  = iris,
  x     = Petal.Length,
  y     = Petal.Width,
  title = "花瓣长度与花瓣宽度的相关关系"
)

# 相关分析：车重与油耗
ggscatterstats(
  data  = mtcars,
  x     = wt,
  y     = mpg,
  xlab  = "车重（千磅）",
  ylab  = "油耗（mpg）",
  title = "车重与油耗的相关关系"
)

# 单变量分布：花萼长度分布 + 单样本 t 检验
gghistostats(
  data       = iris,
  x          = Sepal.Length,
  color      = "steelblue",
  title      = "花萼长度的分布",
  xlab       = "花萼长度（cm）",
  test.value = 5.8
)

# 分类变量：气缸数与变速箱类型的关系（卡方检验）
library(tidyverse)
mtcars <- mtcars |>
  mutate(am = factor(am,
                     levels = c(0, 1),
                     labels = c("自动", "手动")))

ggbarstats(
  data  = mtcars,
  x     = cyl,
  y     = am,
  title = "气缸数和变速箱类型分布",
  xlab  = "气缸数",
  legend.title = "变速箱"
)

# 相关系数矩阵
ggcorrmat(
  data     = mtcars,
  cor.vars = c(mpg, hp, wt, disp, qsec),
  title    = "汽车主要性能指标的相关矩阵"
)

# 分面组合：按变速箱类型分组，分别画散点图
grouped_ggscatterstats(
  data         = mtcars,
  x            = wt,
  y            = mpg,
  grouping.var = am,
  title.prefix = "变速箱："
)

# 查看统计符号（问题1演示）
ggbetweenstats(data = iris, x = Species, y = Sepal.Width)

# 修改��形外观：ggstatsplot 返回 ggplot 对象，可继续用 + 修改
p <- ggbetweenstats(
  data  = iris,
  x     = Species,
  y     = Sepal.Length,
  title = "三种鸢尾花的花萼长度"
)
p +
  scale_color_manual(values = c("#8DA0CB","#FC8D62","#66C2A5")) +
  theme(plot.title = element_text(size = 16, face = "bold"))

# 关闭统计信息只看图形
ggbetweenstats(
  data             = iris,
  x                = Species,
  y                = Sepal.Length,
  results.subtitle = FALSE
)


# ---- L16: 保存输出结果 ----

# 安装与加载（eval: false）
# install.packages(c("tidyverse", "modelsummary", "flextable", "officer", "openxlsx"))
# library(tidyverse)
# library(modelsummary)
# library(flextable)
# library(officer)
# library(openxlsx)

library(tidyverse)
library(modelsummary)
library(flextable)
library(officer)

# 查看当前工作目录（eval: false）
# getwd()

# 整理汇总数据
summary_df <- mtcars |>
  group_by(cyl) |>
  summarise(
    n        = n(),
    mpg_mean = mean(mpg) |> round(1),
    hp_mean  = mean(hp)  |> round(1)
  )

summary_df

# 保存为 CSV（row.names = FALSE 避免多出行号列）
write.csv(summary_df,
          file      = "output/summary_df.csv",
          row.names = FALSE)

# 保存为 xlsx
library(openxlsx)
write.xlsx(summary_df,
           file      = "output/summary_df.xlsx",
           sheetName = "汇总表",
           overwrite = TRUE)

# 画一张散点图用于演示保存
p <- ggplot(mpg, aes(x = displ, y = hwy, color = drv)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values = c("#8DA0CB", "#FC8D62", "#66C2A5")) +
  labs(title = "发动机排量与高速油耗",
       x     = "排量（升）",
       y     = "高速油耗（mpg）",
       color = "驱动方式") +
  theme_minimal()

p

# 保存为 PNG（适合插入 Word / PPT，dpi=300 适合印刷）
ggsave(filename = "output/scat_disp_hwy.png",
       plot     = p,
       width    = 8,
       height   = 5,
       dpi      = 300)

# 保存为 SVG（矢量图，放大不失真，适合期刊投稿）
ggsave(filename = "output/scat_disp_hwy.svg",
       plot     = p,
       width    = 8,
       height   = 5)

# 保存为 PDF（也是矢量图，适合 LaTeX 和期刊）
ggsave(filename = "output/scat_disp_hwy.pdf",
       plot     = p,
       width    = 8,
       height   = 5)

# 表1：描述性统计量 → 导出到 Word
datasummary(mpg + cyl + disp + hp + drat + wt ~
              N + Mean + SD + Median + Min + Max,
            data   = mtcars,
            fmt    = 3,
            output = "flextable") |>
  theme_apa() |>
  autofit() |>
  font(fontname = "Times New Roman", part = "all") |>
  save_as_docx(path = "output/Table1_APA_Style.docx")

# 表2：回归模型估计结果 → 导出到 Word
m1 <- lm(mpg ~ wt, data = mtcars)
m2 <- lm(mpg ~ wt + hp, data = mtcars)
m3 <- lm(mpg ~ wt + hp + cyl, data = mtcars)

models <- list(
  "Model 1" = m1,
  "Model 2" = m2,
  "Model 3" = m3
)

modelsummary(models,
             fmt    = 3,
             stars  = TRUE,
             output = "flextable") |>
  theme_apa() |>
  autofit() |>
  save_as_docx(path = "output/Table2_Regression_Results.docx")

# 报错示例1：Windows 反斜杠路径问题（eval: false）
# write.csv(df, file = "C:\Users\name\Desktop\output.csv")  # 错误
# write.csv(df, file = "C:/Users/name/Desktop/output.csv")  # 正确：正斜杠
# write.csv(df, file = "C:\\Users\\name\\Desktop\\output.csv")  # 正确：双���斜杠
# write.csv(df, file = "output/output.csv")                 # 最佳：相对路径

# 报错示例2：文件被 Excel 占用时换文件名
write.xlsx(summary_df, "output/summary_df_v2.xlsx", overwrite = TRUE)


# ---- L17: 初学者最容易犯的5个低级错误 ----

# 赋值示例（快捷键：Win: Alt+-，Mac: Option+-）
apple  <- 5
banana <- 10

# 计算不保存（结果只显示在 Console）
apple + banana

# 计算并保存结果
total <- apple + banana
total

# 坑2：先赋值再使用
price <- 100
price * 0.8

# 坑3：大小写区分
price * 0.8            # ✅ 正确
# Price * 0.8          # ❌ 报错：object 'Price' not found

# 坑4：英文标点
round(pi, 5)           # ✅ 英文逗号
# round(pi，5)         # ❌ 中文逗号，报错

# 本节完整汇总代码
# ── 赋值符号快捷键 ──────────────────────────────
# Win: Alt + -
# Mac: Option + -

# ── 执行代码快捷键 ──────────────────────────────
# Win: Ctrl + Enter
# Mac: Command + Enter

# ── 赋值示例 ────────────────────────────────────
apple  <- 5
banana <- 10

# ── 计算：结果不保存 ─────────────────────────────
apple + banana           # 输出 15，但未保存

# ── 计算：结果保存 ───────────────────────────────
total <- apple + banana
total                    # 输出 15

# ── 坑2：先赋值，再使用 ─────────────────────────
price <- 100             # 第一步：赋值
price * 0.8              # 第二步：使用，输出 80

# ── 坑3：大小写区分 ──────────────────────────────
price * 0.8              # ✅ 正确
# Price * 0.8            # ❌ 报错：object 'Price' not found

# ── 坑4：英文标点 ────────────────────────────────
round(pi, 5)             # ✅ 英文逗号，输出 3.14159
# round(pi，5)           # ❌ 中文逗号，报错

# ── 坑5：未完成代码导致 Console 出现"+" ──────────
# 8 + 10 +               # ← 写了这行就 Ctrl+Enter，Console 会出现 +
#                           按 Esc 键退出，再重新写完整的代码
8 + 10 + 5               # ✅ 写完整再执行



