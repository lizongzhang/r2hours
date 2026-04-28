library(tidyverse)
library(readxl)

stroke <- read_excel("stroke.xlsx")

glimpse(stroke)

#将数值型的sex, status, stroke_type转换为factor
stroke <- stroke %>% 
  mutate(sex = factor(sex, 
                      levels = c(1,2),
                      labels = c("male", "female")),
         status = factor(status, 
                         levels = c(1,2),
                         labels = c("alive", "dead")),
         stroke_type = factor(stroke_type, 
                              levels = c(0,1),
                              labels = c("Ischaemic",
                                         "Haemorrhagic")))

glimpse(stroke)

library(gtsummary)

stroke %>% 
  tbl_summary()

stroke %>% 
  tbl_summary(by = status) 


model1 <- stroke %>% 
  glm(status ~ gcs + stroke_type + sex + dm + sbp + age, 
      data = ., 
      family = binomial(link = 'logit'))


# 报告bi
model1 %>% 
  tbl_regression(intercept = TRUE,
                 estimate_fun = function(x) style_number(x, digits = 3),
                 pvalue_fun = function(x) style_pvalue(x, digits = 3)) %>% 
  as_gt()

library(ggstatsplot)

stroke %>% 
  gghistostats(age,
              test.value = 58)


stroke %>% 
  ggbetweenstats(stroke_type,
                 age)


# geom_errorbar -----------------------------------------------------------

# 加载必要的包
library(tidyverse)
library(dplyr)

# 准备数据：按 Species 分组，计算均值与标准误
summary_data <- iris %>%
  group_by(Species) %>%
  summarise(
    mean_sepal = mean(Sepal.Length),
    se_sepal = sd(Sepal.Length) / sqrt(n())
  )

# 绘图
ggplot(summary_data, aes(x = Species, y = mean_sepal)) +
  geom_col(fill = "skyblue", width = 0.6) +  # 柱状图
  geom_errorbar(aes(ymin = mean_sepal - se_sepal,
                    ymax = mean_sepal + se_sepal),
                width = 0.2, linewidth = 1) +     # 工字型误差线
  labs(
    title = "Sepal Length by Species with Standard Error",
    x = "Species",
    y = "Mean Sepal Length ± SE"
  ) +
  theme_minimal(base_size = 14)


# stroke 示例 ------------------------------------------------------------

# 准备数据：按 Species 分组，计算均值与标准误
summary_data <- stroke %>%
  group_by(stroke_type) %>%
  summarise(
    mean_age = mean(age),
    se_age = sd(age) / sqrt(n())
  )
summary_data


# 绘图
ggplot(summary_data, aes(x = stroke_type, y = mean_age)) +
  geom_col(fill = "skyblue", width = 0.6) +  # 柱状图
  geom_errorbar(aes(ymin = mean_age - se_age,
                    ymax = mean_age + se_age),
                width = 0.2, linewidth = 1) +     # 工字型误差线
  labs(
    title = "Age by Stroke Type with Standard Error",
    x = "Stroke Type",
    y = "Age"
  ) +
  theme_minimal(base_size = 14)



# iris 示例三类误差图（Mean ± SD / SE / CI）-----------------------------------------------------

# 加载必要包
library(ggplot2)
library(dplyr)
library(gridExtra)

# 1. 准备数据：计算均值、SD、SE、CI
summary_data <- iris %>%
  group_by(Species) %>%
  summarise(
    mean = mean(Petal.Length),
    sd = sd(Petal.Length),
    n = n(),
    se = sd / sqrt(n),
    ci = qt(0.975, df = n - 1) * se  # 95% CI = t * SE
  )

# 2. 绘图函数封装
make_plot <- function(error_type = c("sd", "se", "ci"), fill_color = "#999999", title = "Mean ± Error") {
  ymin <- switch(error_type,
                 sd = summary_data$mean - summary_data$sd,
                 se = summary_data$mean - summary_data$se,
                 ci = summary_data$mean - summary_data$ci)
  
  ymax <- switch(error_type,
                 sd = summary_data$mean + summary_data$sd,
                 se = summary_data$mean + summary_data$se,
                 ci = summary_data$mean + summary_data$ci)
  
  ggplot(summary_data, aes(x = Species, y = mean)) +
    geom_col(fill = fill_color, width = 0.6) +
    geom_errorbar(aes(ymin = ymin, ymax = ymax),
                  width = 0.2, color = "#333333", size = 1) +
    labs(title = title, x = "Species", y = "Petal Length (cm)") +
    theme_minimal(base_size = 14) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
}

# 3. 分别绘制三种图
p_sd <- make_plot("sd", fill_color = "#B0C4DE", title = "Mean ± SD")
p_se <- make_plot("se", fill_color = "#87CEFA", title = "Mean ± SE")
p_ci <- make_plot("ci", fill_color = "#4682B4", title = "Mean ± 95% CI")

# 4. 拼图显示
grid.arrange(p_sd, p_se, p_ci, nrow = 1)


library(ggpubr)

ggbarplot(iris, x = "Species", y = "Petal.Length",
          add = "mean_ci",               # 添加均值 ± 95% CI
          fill = "#4682B4",              # 医学风格配色
          color = "black",               # 边框
          width = 0.6,
          ylab = "Petal Length (cm)",
          xlab = "Species",
          title = "Petal Length by Species (Mean ± 95% CI)") +
  stat_compare_means(method = "anova", label.y = 8.5) +         # 整体 ANOVA 检验
  stat_compare_means(comparisons = list(c("setosa", "versicolor"),
                                        c("setosa", "virginica"),
                                        c("versicolor", "virginica")),
                     method = "t.test", label = "p.signif", label.y = c(8, 8.2, 8.4)) +
  theme_pubr(base_size = 14)



    