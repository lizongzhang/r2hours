# ================================================
# ---- L8: R中的向量思维：彻底搞懂 c() 函数 ----------------------------------

library(tidyverse)

# 创建向量
weights <- c(60, 72, 55, 88)
vars    <- c("mpg", "wt", "hp")

# 向量类型优先级转换
c(TRUE, FALSE, 3)           # 逻辑遇数值 → 1/0
c(1, 2, "three")            # 数值遇字符 → 字符串
c(TRUE, 1L, 3.14, "hello")  # 混合 → 全部字符串

# c() 合并向量，结果仍是一维向量
a <- c(1, 2, 3)
b <- c(4, 5, 6)
c(a, b)
c(1, 2, 3, 4, 5, 6)

# 创建向量（正确写法）
x <- c(1, 2, 3)

# 索引
data(mtcars)
mtcars[c(1, 2), ]   # 提取第 1 行和第 2 行


vec <- c("A", "B", "C", "D")
vec[c(1, 2, 3)]     # 提取第 1、2、3 个元素

# 参数传递多个值
plot(as.factor(mtcars$cyl), mtcars$mpg,
     col      = c("#8DA0CB", "#FC8D62", "#66C2A5"),
     border   = "gray40",
     las      = 1,
     ylim     = c(0, 40),
     main     = "MPG Distribution by Engine Cylinders",
     xlab     = "Number of Cylinders",
     ylab     = "Miles/(US) gallon",
     font.main = 2)

sapply(mtcars, function(x) c(Mean = mean(x), Sd = sd(x)))

# 逻辑判断 %in%
x <- 1
x %in% c(1, 2, 3)

# 提取 4 缸或 6 缸的汽车
mtcars[mtcars$cyl %in% c(4, 6), ]

# 避免将变量命名为 c（不推荐但可运行）
c <- 10
x <- c(c, 20, 30)
rm(c)   # 用完记得删除，避免遮盖内置函数


# ---- L9: 数据框常用操作 ----------------------------------------------------

# 手动创建数据框
df <- data.frame(
  name   = c("张三", "李四", "王五", "赵六"),
  age    = c(22, 25, 21, 23),
  score  = c(88.5, 92.0, 75.5, 85.0),
  passed = c(TRUE, TRUE, FALSE, TRUE)
)
df

# 查看前几行 / 后几行
head(mtcars)
head(mtcars, 3)
tail(mtcars)

# 查看数据结构
str(mtcars)

# 行数、列数
nrow(mtcars)
ncol(mtcars)
dim(mtcars)

# 列名、行名
names(mtcars)
rownames(mtcars) |> head(5)

# 描述统计
summary(mtcars)
summary(mtcars[, c("mpg", "hp", "wt")])

# 用 $ 访问列
mtcars$mpg
mean(mtcars$mpg)
unique(mtcars$cyl)
table(mtcars$cyl)

# 新增列
df <- head(mtcars, 5)
df$kpl <- df$mpg * 0.425
df[, c("mpg", "kpl")]

# 删除列
df$kpl <- NULL
names(df)

# $ 访问不存在的列（返回 NULL，不报错）
mtcars$xyz

# 修改列名
df <- head(mtcars, 3)
names(df)[names(df) == "mpg"] <- "miles_per_gallon"
names(df)

# 用 [ ] 按位置或名称索引
mtcars[1, 2]              # 第 1 行，第 2 列
mtcars[1:3, 1:3]          # 前 3 行，前 3 列
mtcars[c(1, 3), ]         # 第 1 行和第 3 行
mtcars[, c("mpg", "hp")] |> head(3)   # 指定列名
mtcars[1:5, "mpg"]        # 指定行范围 + 列名
mtcars$mpg[1:5]           # 等价写法

# 逻辑条件筛选行
mtcars[mtcars$mpg > 25, ]
mtcars[mtcars$mpg > 20 & mtcars$cyl == 4, ]
mtcars[mtcars$cyl %in% c(4, 6), ] |> head(4)

# 常见报错场景演示
mtcars[100, ]             # 行超界 → 返回 NA 行
# mtcars[1, 100]          # 列超界 → 报错（取消注释可测试）

x <- c(10, 20, 30, 40, 50)
# x[1, 3]                 # 向量不能用二维索引 → 报错
x[c(1, 3)]               # 正确写法

mtcars$MPG                # 大小写错误 → NULL
mtcars$mileage            # 不存在的列 → NULL

mtcars[mtcars$mpg > 25, ] |> head(3)   # 筛选时须写数据框名

mtcars[, 1:3] |> head(3)  # 正确：取前 3 列


# ---- L10: 缺失值 NA 的处理 ----

# NA 的类型
NA            # 逻辑型 NA（默认）
NA_real_      # 数值型 NA
NA_integer_   # 整数型 NA
NA_character_ # 字符型 NA

is.na(NA)

# 构造含 NA 的演示数据框
df <- data.frame(
  name   = c("张三", "李四", "王五", "赵六", "钱七", "周九", "吴十"),
  age    = c(22, NA, 25, 21, NA, 28, 24),
  score  = c(88.5, 92.0, NA, 55.5, 85.0, 91.0, 78.5),
  passed = c(TRUE, TRUE, NA, FALSE, TRUE, TRUE, TRUE)
)
df

# is.na() — 逐元素判断是否为 NA
is.na(df)

# 统计每列有多少个 NA
colSums(is.na(df))

# 统计每行有多少个 NA
rowSums(is.na(df))

# 缺失值总数与缺失率
sum(is.na(df))
colMeans(is.na(df))

# NA 导致聚合函数返回 NA
mean(df$age)
sum(df$age)
max(df$age)
min(df$age)

# 加上 na.rm = TRUE
mean(df$age,  na.rm = TRUE)
sum(df$age,   na.rm = TRUE)
max(df$age,   na.rm = TRUE)
min(df$age,   na.rm = TRUE)

# 不能用 == NA，要用 is.na()
x <- NA
x == NA    # 错误：返回 NA，不是 TRUE
is.na(x)   # 正确：返回 TRUE

# cor() 相关系数矩阵
cor(df$age, df$score)                          # 返回 NA
cor(df$age, df$score, use = "complete.obs")    # 只用完整行

# lm() 默认删除含 NA 的行
model <- lm(score ~ age, data = df)
summary(model)
nobs(model)   # 实际使用的行数
nrow(df)      # 原始数据行数

# table() 默认不统计 NA
table(df$passed)
table(df$passed, useNA = "ifany")
table(df$passed, useNA = "always")

# which() 与 NA 的陷阱
which(df$age > 21)
df[df$age > 21, ]          # NA 行也会混入结果
df[which(df$age > 21), ]   # 用 which() 避免 NA 行混入

# 方法1：删除含 NA 的行 — na.omit()
df_clean <- na.omit(df)
df_clean

# 方法2：只删除特定列含 NA 的行
df_age_clean <- df[!is.na(df$age), ]
df_age_clean

df_both_clean <- df[!is.na(df$age) & !is.na(df$score), ]
df_both_clean

# 方法3：用均值填补 NA — replace()
age_mean <- mean(df$age, na.rm = TRUE)
df$age_filled <- replace(df$age, is.na(df$age), age_mean)
df[, c("age", "age_filled")]

# 方法4：用 ifelse() 填补
df$score_filled <- ifelse(is.na(df$score), 0, df$score)
df[, c("score", "score_filled")]

# 方法5：complete.cases() — 筛选完整行
complete.cases(df)
df[complete.cases(df), ]
df[complete.cases(df[, c("age", "score")]), ]

# 实战：airquality 数据集
data(airquality)
str(airquality)
colSums(is.na(airquality))
summary(airquality)
mean(airquality$Ozone,   na.rm = TRUE)
mean(airquality$Solar.R, na.rm = TRUE)

air_clean <- na.omit(airquality)
nrow(airquality)   # 原始行数
nrow(air_clean)    # 清洗后行数
model <- lm(Ozone ~ Solar.R + Wind + Temp, data = air_clean)
summary(model)


# ---- L11: 因子 Factor 的底层逻辑 ----

library(tidyverse)
library(showtext)
showtext_auto()

# install.packages("tidyverse")  # （eval: false）
# library(tidyverse)

# 本节使用的数据：ggplot2::mpg
data(mpg)
str(mpg)

# class 和 drv 都是字符串
class(mpg$class)
class(mpg$drv)
unique(mpg$class)
unique(mpg$drv)

# 创建因子：factor()
mpg$class_f <- factor(mpg$class)
levels(mpg$class_f)   # 默认按字母顺序

factor(mpg$drv)

# 手动指定 levels 和 labels
mpg$drv_f <- factor(mpg$drv,
                    levels = c("f", "r", "4"),
                    labels = c("front", "rear", "4-wheel"))

levels(mpg$drv_f)
table(mpg$drv_f)
table(mpg$drv)

# 因子用于绘图：控制类别顺序
boxplot(hwy ~ drv, data = mpg,
        col = c("#8DA0CB", "#FC8D62", "#66C2A5"))   # 字符串：字母顺序

boxplot(hwy ~ drv_f, data = mpg,
        col = c("#8DA0CB", "#FC8D62", "#66C2A5"))   # 因子：levels 定义的顺序

# 因子用于回归：控制参照组
model1 <- lm(hwy ~ drv, data = mpg)    # 字符串：参照组不可控
summary(model1)$coefficients

model2 <- lm(hwy ~ drv_f, data = mpg)  # 因子：前驱为参照组
summary(model2)$coefficients

# forcats: fct_reorder() — 按另一个变量的值排序
mpg$class_f <- fct_reorder(mpg$class_f, mpg$hwy, median)
levels(mpg$class_f)
table(mpg$class_f)

# 字符串 class：x 轴按字母顺序
boxplot(hwy ~ class, data = mpg,
        main = "字符串 class：字母顺序",
        xlab = "", ylab = "高速油耗 (mpg)",
        col  = rainbow(7),
        las  = 2)

# 因子 class_f：x 轴按油耗中位数排列
mpg$class_f <- fct_reorder(mpg$class_f, mpg$hwy, median)
boxplot(hwy ~ class_f, data = mpg,
        main = "因子 class_f：按油耗中位数排列",
        xlab = "", ylab = "高速油耗 (mpg)",
        col  = rainbow(7),
        las  = 2)

# ggplot2 排序绘图
ggplot(mpg, aes(x = class_f, y = hwy)) +
  geom_boxplot(aes(fill = class_f)) +
  theme_minimal()

# fct_rev() — 反转类别顺序
mpg$class_f_rev <- fct_rev(mpg$class_f)
levels(mpg$class_f_rev)

par(mar = c(4, 7, 3, 1))
boxplot(hwy ~ class_f_rev, data = mpg,
        main       = "fct_rev()：反转后高油耗排最右",
        xlab       = "高速油耗 (mpg)",
        ylab       = "",
        col        = rainbow(7),
        horizontal = TRUE,
        las        = 1)

# fct_lump_n() — 合并低频类别
table(mpg$manufacturer)

mpg$mfr_lumped <- fct_lump_n(mpg$manufacturer, n = 5)
table(mpg$mfr_lumped)

mpg$mfr_lumped <- fct_reorder(mpg$mfr_lumped, mpg$hwy, median)

par(mar = c(4, 7, 3, 1))
boxplot(hwy ~ mfr_lumped, data = mpg,
        main       = "fct_lump_n()：保留前 5 个品牌",
        xlab       = "高速油耗 (mpg)",
        ylab       = "",
        col        = c("#8DA0CB", "#FC8D62", "#66C2A5",
                       "#E78AC3", "#A6D854", "#FFD92F"),
        horizontal = TRUE,
        las        = 1)


# ---- L12: 掌握管道符实现高效数据管理 ----

library(tidyverse)
data(mtcars)
head(mtcars)

# 管道符 vs 嵌套写法
round(mean(sqrt(mtcars$hp)), 2)          # 嵌套写法
mtcars$hp |> sqrt() |> mean() |> round(2)  # 管道符写法

mtcars$hp |> 
  sqrt() |> 
  mean() |> 
  round(2)

# 嵌套 vs 中间变量 vs 管道符
result1 <- round(mean(mtcars$hp), 1)
hp_mean  <- mean(mtcars$hp)
result2  <- round(hp_mean, 1)
result1
result2

result <- mtcars$hp |> mean() |> round(1)
result

# |> 与 %>% 的对比
mtcars$hp |> mean() |> round(1)    # 原生管道符（R 4.1+）
mtcars$hp %>% mean() %>% round(1)  # magrittr 管道符

# filter() — 筛选行
mtcars |>
  filter(cyl == 4)

mtcars |>
  filter(cyl == 4, am == 1)

mtcars |>
  filter(mpg > 25 | hp > 200)

# select() — 选择列
mtcars |>
  select(mpg, cyl, hp, wt, am) |>
  head(5)

mtcars |>
  select(-qsec, -vs, -gear, -carb) |>
  head(5)

# mutate() — 追加新列
mtcars |>
  mutate(kpl = mpg * 0.425) |>
  select(mpg, kpl) |>
  head(5)

mtcars |>
  mutate(
    kpl   = mpg * 0.425,
    wt_kg = wt * 453.6
  ) |>
  select(mpg, kpl, wt, wt_kg) |>
  head(5)

# mutate() + case_when() — 多条件重新编码
mtcars |>
  mutate(
    mpg_group = case_when(
      mpg < 18             ~ "高油耗",
      mpg >= 18 & mpg < 25 ~ "中油耗",
      mpg >= 25            ~ "低油耗"
    )
  ) |>
  select(mpg, mpg_group) |>
  head(8)

mtcars |>
  mutate(
    am_label = case_when(
      am == 0 ~ "自动挡",
      am == 1 ~ "手动挡"
    )
  ) |>
  select(am, am_label) |>
  head(8)

# arrange() — 排序
mtcars |>
  select(mpg, cyl, hp) |>
  arrange(mpg) |>
  head(5)

mtcars |>
  select(mpg, cyl, hp) |>
  arrange(desc(mpg)) |>
  head(5)

# group_by() + summarise() — 分组汇总
mtcars |>
  group_by(cyl) |>
  summarise(
    n        = n(),
    mpg_mean = mean(mpg) |> round(1),
    hp_mean  = mean(hp)  |> round(1)
  )

mtcars |>
  mutate(am_label = ifelse(am == 0, "自动挡", "手动挡")) |>
  group_by(cyl, am_label) |>
  summarise(
    n        = n(),
    mpg_mean = mean(mpg) |> round(1),
    .groups  = "drop"
  )

# 串联所有步骤
mtcars |>
  filter(am == 1) |>                         # 第1步：只保留手动挡
  mutate(cyl = factor(cyl)) |>              # 第2步：气缸数转为因子
  group_by(cyl) |>                          # 第3步：按气缸数分组
  summarise(
    n        = n(),
    mpg_mean = mean(mpg) |> round(1)        # 第4步：计算平均油耗
  ) |>
  arrange(desc(mpg_mean))                   # 第5步：从高到低排列

# 常见报错示例
# mtcars |> cyl          # 报错：管道符后忘记写函数（eval: false）
# mtcars > head()        # 报错：|> 写成了 >（eval: false）
mtcars |> head()         # 正确写法

# group_by() 后忘记 summarise()
mtcars |>
  group_by(cyl) |>
  head(3)         # 仍带分组标记，结果可能混淆

# ungroup() 清除分组标记
mtcars |>
  group_by(cyl) |>
  mutate(mpg_centered = mpg - mean(mpg)) |>
  ungroup() |>
  select(cyl, mpg, mpg_centered) |>
  head(5)
