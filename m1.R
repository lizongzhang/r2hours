# ---- L1: 重构AI时代的R学习观 ------------------------------------------------

library(tidyverse)

# 4.0 环境准备（eval: false）
install.packages(c("tidyverse", "gtsummary", "ggpubr",
                   "broom", "broom.helpers"))

# 4.1 人口统计学与临床特征表
library(tidyverse)
library(gtsummary)
trial %>%
  tbl_summary(
    by = trt,
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)")
  ) %>%
  add_p()

# 4.2 结局指标对比
library(tidyverse)
library(gtsummary)
trial %>%
  select(trt, age, marker, response) %>%
  tbl_summary(
    by = trt,
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     response ~ "{p}"),
    digits = all_continuous() ~ 2,
    missing = "no"
  ) %>%
  add_difference()

# 4.3 组间可视化
library(ggpubr)
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

# 4.4 回归结果的表格化输出
library(broom)
library(gtsummary)
library(tidyverse)

trial %>%
  glm(response ~ age + stage, ., family = binomial()) %>%
  tbl_regression()


# ---- L2: R与RStudio的安装与配置 --------------------------------------------
# 本节无可运行的 R 代码（仅含命令行示例与配置说明）