library(survival)
library(broom)
library(kableExtra)

data(cancer)
data(lung)
head(lung)


# KableExtra --------------------------------------------------------------



# 加载必要的包
library(survival)
library(broom)  # 用于整理模型结果
library(knitr)  # 用于格式化表格
library(kableExtra)  # 美化表格输出

# 加载 lung 数据集
data(lung)

# 构建 Cox 比例风险模型
cox_model <- coxph(Surv(time, status) ~ age + sex + ph.ecog, data = lung)

# 使用 broom 包中的 tidy 函数整理模型结果
cox_results <- tidy(cox_model, exponentiate = TRUE, conf.int = TRUE)

# 增加列名解释
colnames(cox_results) <- c("变量", "估计值(HR)", "标准误", "z值", "p值", "置信区间下限", "置信区间上限")

# 输出结果为表格
cox_results %>%
  kable("html", caption = "Cox比例风险模型估计结果") %>%
  kable_styling(full_width = FALSE, position = "center")

# gtsummary ---------------------------------------------------------------



# 加载必要的包
library(survival)
library(gtsummary)

# 加载 lung 数据集
data(lung)

# 构建 Cox 比例风险模型
cox_model <- coxph(Surv(time, status) ~ age + sex + ph.ecog, data = lung)

# 使用 gtsummary 包格式化输出
cox_model %>%
  tbl_regression(exponentiate = TRUE) %>%  # 输出 HR (风险比)
  as_gt() %>%  # 转换为 gt 表格
  gt::gtsave("cox_model_results.html")  # 保存为 HTML 文件

# 
x <- rnorm(100)
