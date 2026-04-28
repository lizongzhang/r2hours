# Ctrl + Shift + C 代码转化为注释

# Ctrl + Shift + R 添加小节

# ## 二级标题


#今晚讲义：https://lizongzhang.github.io/deepseekcamp/L10.html

# 需要安装和调用的包
# install.packages("gt") 
library(gt)
# install.packages("gtsummary")
library(gtsummary)
# install.packages("tidyverse")
library(tidyverse)
# install.package("labelled")
library(labelled)
# install.packages("report")
library(report)
# install.packages("psych")
library(psych)
# install.packages("ggpubr")
library(ggpubr)
# install.packages("ggstatsplot")
library(ggstatsplot)
# install.packages("dlookr")
library(dlookr)
# install.packages("car")
library(car)
# install.packages("broom")
library(broom)
# install.packages("rstantools")
library(rstantools)
# install.packages("effectsize")
library(effectsize)
# install.packages("datarium")   # for selfesteem data
library(datarium)
# install.packages(afex)
library(afex)
# install.package(BSDA)
library(BSDA)
# install.packages("report")
library(report)
# install.packages(rstatix)
library(rstatix)






# 单个样本 ------------------------------------------------------------------


# 描述统计 ------------------------------------------------------------------

psych::describe(trial)



## 正态性检验 Shapiro-Wilk test ------------------------------------------------------------------


# 正态性检验
# shapiro test
shapiro.test(trial$age)

# qqplot
ggpubr::ggqqplot(trial$age)


## 参数检验t test ------------------------------------------------------------------


#均值的t检验，原假设：mu = 45, 备择假设：mu != 45，双侧检验
t.test(trial$age, 
       mu = 45, 
       alternative = "two.sided", 
       conf.level = 0.95)

# report::report()解读输出结果 ------------------------------------------------------------------

t.test(trial$age,
       mu = 45,
       alternative = "two.sided",
       conf.level = 0.95) %>%
  report()



ggstatsplot::gghistostats(
  data = trial,
  x = age,
  test.value = 45,
  type = "parametric",
  normal.curve = TRUE
)



## 非参数检验Wilcoxon Signed Rank test 

shapiro.test(trial$marker) 

wilcox.test(trial$marker, 
            mu = 0.7, 
            alternative = "two.sided", 
            conf.level = 0.95) %>% 
  report()

gghistostats(
  data = trial,
  x = marker,
  test.value = 0.7,
  type = "nonparametric",
  normal.curve = TRUE
)




# 两个样本 ------------------------------------------------------------------


## 两个独立样本 ------------------------------------------------------------------


### 参数检验 t test ------------------------------------------------------------------



# dlookr::normality()函数检验数据是否服从正态分布 ------------------------------------------------------------------

trial %>% 
  group_by(death) %>% 
  dlookr::normality(age)


#### 方差齐性检验 Levene's Test ------------------------------------------------------------------


# trail中的death是整数型，需要转换为factor型，否则会报错
# leveneTest(age ~ death, data = trial)
# Error in leveneTest.formula(age ~ death, data = trial) : 
# Levene's test is not appropriate with quantitative explanatory variables.

car::leveneTest(age ~ as.factor(death), trial)


#### 方差相等 Student's t test ------------------------------------------------------------------


t.test(age ~ death, data = trial, var.equal = TRUE) %>% 
  report()

# variance equal: Student's t test ------------------------------------------------------------------

ggbetweenstats(
  data = trial,
  x = death,
  y = age,
  type = "parametric",
  plot.type = "box",
  title = "Age by Death",
  xlab = "Death",
  ylab = "Age",
  var.equal = TRUE
)


#### 方差不等 Welch's t test ------------------------------------------------------------------



#只作演示，trial中death划分的两个组别，方差相等
t.test(age ~ death, data = trial, var.equal = FALSE) %>% 
  report()

# 检验结果的可视化：方差不等的Welch's t test ------------------------------------------------------------------

ggbetweenstats(
  data = trial,
  x = death,
  y = age,
  type = "parametric",
  plot.type = "box",
  title = "Age by Death",
  xlab = "Death",
  ylab = "Age",
  var.equal = FALSE
)



### 非参数检验 Mann-Whitney U-test (Wilcoxon Rank Sum Test)  ------------------------------------------------------------------



# dlookr::normality()函数检验数据是否服从正态分布
trial %>% 
  group_by(death) %>% 
  dlookr::normality(marker)

#wilcox.test()下面的写法无法导出到report()
wilcox.test(data = trial, marker ~ death)

#wilcox.test()下面的写法可以导出到report()
wilcox.test(trial$marker[trial$death == 1],  
            trial$marker[trial$death == 0]) %>% 
  report()

ggbetweenstats(
  data = trial,
  x    = death, 
  y    = marker, 
  type = "nonparametric")

library(effectsize)

interpret_rank_biserial(0.04)

?interpret_rank_biserial

# 保存图片
ggsave(filename = "mwu.jpg", plot = last_plot(), width = 6, height = 4)



## 两个配对样本 ------------------------------------------------------------------


### 参数检验 t test ------------------------------------------------------------------


# 适用场景：配对样本，差值序列服从正态分布
library(BSDA)
data(Fitness)
Fitness

# make wide format
d <- Fitness %>% 
  pivot_wider(
    id_cols     = subject, 
    names_from  = test, 
    values_from = number) %>% 
  mutate(difference =  After - Before)
d

shapiro.test(d$difference)

set.seed(1)   # for Bayesian reproducibility
ggwithinstats(
  data = Fitness,
  x    = test, 
  y    = number, 
  type = "parametric"
)

library(effectsize)

interpret_hedges_g(0.83)

?interpret_hedges_g()

t.test(Fitness$number ~ Fitness$test, paired = TRUE)

t.test(Fitness$number ~ Fitness$test, paired = TRUE) %>% 
  report()



#Paired Samples test is actually One-Sample test on the difference
# old way to do the tests
# install.packages("broom")
library(broom)
bind_rows(
  t.test(d$difference) %>% tidy(),
  t.test(d$After, d$Before, paired = T) %>% tidy()
) 




# 错误使用非参数检验
wilcox.test(d$difference)

#错误使用非参数配对样本的检验
ggwithinstats(
  data = Fitness,
  x    = test, 
  y    = number, 
  type = "nonparametric"
)


# 错误使用两个独立样本的t检验, p值等于0.51
ggbetweenstats(
  data = Fitness,
  x    = test, 
  y    = number, 
  type = "parametric", 
  var.equal = T
)



### 非参数检验 Wilcoxon Signed Rank Test ------------------------------------------------------------------



# 适用场景：配对样本，n<30, 差值序列不服从正态分布
# 独立样本，两个序列不服从正态分布Mann-Whitney-Wilcoxon-test

library(BSDA)
data(Speed)
Speed

# HO: median difference = 0
# H1: median difference != 0

# 配对样本本质上研究的是差值序列，所以要考察差值是否服从正态分布

# Speed中已有差值序列
# p值小于0.05，拒绝原假设，差值不服从正态分布。需要使用配对样本非参数Wilcoxon signed rank test.
#若差值序列服从正态分布，则需使用配对样本的t检验。
shapiro.test(Speed$differ)

#也可自行计算差值序列
shapiro.test(Speed$after - Speed$before)

# 宽型数据转换为长型数据

# 方法一：tidyr包中的gather()函数
d <- Speed %>% 
  gather(key = "speed", value = "score", before, after)
d

# 方法二：tidyr包中的pivot_longer()函数
Speed_long <- Speed %>% 
  pivot_longer(cols = c("before", "after"), 
               names_to = "speed", 
               values_to = "score") 
Speed_long

ggwithinstats(
  data = d, 
  x = speed, # grouping variable
  y = score, # response variable
  type = "nonparametric"  #Paired Samples non-paramertic Wilcoxon Signed Rank Test
)

# 保存图片
ggsave(filename = "wilcoxon.jpg", plot = last_plot(), width = 6, height = 4)

ggwithinstats(
  data = Speed_long, 
  x = speed, 
  y = score,
  type = "nonparametric"
)

# 配对样本的Wilcoxon signed rank test
# 计算Wilcoxon signed rank statistic
Speed %>% 
  filter(signranks >0) %>% 
  select(signranks) %>% 
  sum()

# p值只能表明是否存在差异，不能表达这个差异是否很大。 
# 为了表达差异的大小，需要计算效应量effect size，即rank biserial correlation coefficient
# rank biserial correlation coefficient = 0.68
# very large ,positive & significant effect.

# how to explain rank biserial?
library(effectsize)

interpret_rank_biserial(0.68)

#查看interpret_rank_biserial()帮助
?interpret_rank_biserial()

# 如果误用配对样本的t检验，p = 0.34, 会得出不存在差异的错误结论。
ggwithinstats(
  data = Speed_long, 
  x = speed, 
  y = score,
  type = "parametric"
)

wilcox.test(Speed$differ) 

wilcox.test(Speed$differ) %>% 
  report()
wilcox.test(Speed$after, Speed$before, paired = TRUE) 

wilcox.test(Speed$after, Speed$before, paired = TRUE) %>% 
  report()  

# 若要做右侧检验
wilcox.test(Speed$after, Speed$before, paired = TRUE, alternative = "greater")



## 定性数据的两个配对样本 McNemar Test ------------------------------------------------------------------



set.seed(9) # for reproducibility 
data <- data.frame(
  before = sample(c("+", "-", "+", "+"), 20, replace = TRUE),
  after  = sample(c("-", "+", "-", "-"), 20, replace = TRUE))

data

# 适用场景：配对样本，n<30, 两个定性变量，且两个定性变量的水平数相同
# install.packages("ggstatsplot")
library(ggstatsplot)

ggbarstats(
  data = data,
  x    = before, 
  y    = after,
  paired = TRUE, 
  label = "both"
)

# 保存图片
ggsave(filename = "mcnemar2.jpg", plot = last_plot(), width = 5, height = 3)



# install.packages("effectsize")
library(effectsize)
interpret_cohens_g(0.39)




#若误用皮尔逊卡方检验，p = 0.34, 会得出不存在差异的错误结论。
ggbarstats(
  data = data,
  x    = before, 
  y    = after,
  label = "both"
)


# 多个样本 ------------------------------------------------------------------


## 多个独立样本 ------------------------------------------------------------------


### 参数检验 ANOVA ------------------------------------------------------------------


#### 正态性检验 Shapiro-Wilk Test ------------------------------------------------------------------



# 适用场景：多个样本，每个样本服从正态分布，且方差相等
trial %>% 
  group_by(stage) %>%
  dlookr::normality(age)


#### 方差齐性检验 Levene's Test ------------------------------------------------------------------



car::leveneTest(age ~ stage, data = trial)


#### Fisher's ANOVA ------------------------------------------------------------------



ggbetweenstats(
  data = trial,
  x    = stage, 
  y    = age, 
  type = "parametric", 
  var.equal = TRUE)

# effect size
# install.packages("effectsize")
library(effectsize)
interpret_omega_squared(0)
?interpret_omega_squared()

# Bayes Factor, which is conceptually similar to the p-value indicates an extreme evidence for the alternative hypothesis
interpret_bf(exp(3.39))




# 适用场景：多个样本，每个样本服从正态分布，且方差相等
# install.packages("tidyverse")  # for everything ;)
library(tidyverse)

#install.packages("ISLR")
library(ISLR)

set.seed(4)  # for reproducibility
d <- Wage %>% 
  group_by(education) %>% 
  sample_n(30)

# install.packages("dlookr")
library(dlookr)
d %>% 
  group_by(education) %>% 
  normality(wage)

# install.packages("ggstatsplot")
# It’s also important, because (1) even very different means with huge variance (samples a and b) may not be significantly different (p = 0.1) while (2) even very similar means with small variance (samples c and d) can be significantly different (p = 0.04).

# A small p-value of Levene’s Test tells us that our variances differ and that we need to use Welch’s ANOVA.
#install.packages("car")
library(car)
leveneTest(wage ~ education, d)

# install.packages("ggstatsplot")
library(ggstatsplot)

set.seed(4)   # for Bayesian reproducibility of 95% CIs
# 若提示要安装rstantools，安装即可
#install.packages("rstantools")
library(rstantools)
ggbetweenstats(
  data = d,
  x    = education, 
  y    = wage, 
  type = "parametric", 
  var.equal = FALSE)

# effect size
# install.packages("effectsize")
library(effectsize)
interpret_omega_squared(0.34)

?interpret_omega_squared()

# Bayes Factor, which is conceptually similar to the p-value indicates an extreme evidence for the alternative hypothesis
interpret_bf(exp(-17.13))



####  Welch's ANOVA ------------------------------------------------------------------



#仅作举例用，实际上该用Fisher's ANOVA
ggbetweenstats(
  data = trial,
  x    = stage, 
  y    = age, 
  type = "parametric", 
  var.equal = FALSE)


### 非参数检验 Kruskal-Wallis Test ------------------------------------------------------------------


trial %>% 
  group_by(stage) %>%
  dlookr::normality(marker)

ggbetweenstats(
  data = trial,
  x    = stage, 
  y    = marker, 
  type = "nonparametric")



#若误用了参数检验，将导致错误的P值 ------------------------------------------------------------------

ggbetweenstats(
  data = trial,
  x    = stage, 
  y    = marker, 
  type = "parametric")


## 多个配对样本 ------------------------------------------------------------------


### Repeated Measures ANOVA ------------------------------------------------------------------



# install.packages("datarium")   # for selfesteem data
library(datarium)
datarium::selfesteem

# make long format
d <- selfesteem %>%
  gather(key = "time", value = "score", t1, t2, t3) 
d



library(ggstatsplot)

set.seed(1)   # for Bayesian reproducibility
ggwithinstats(
  data = d,
  x    = time, 
  y    = score, 
  type = "parametric"
)



diffs <- selfesteem %>%
  mutate(
    diff_t3_t1 = t3 - t1,
    diff_t3_t2 = t3 - t2,
    diff_t2_t1 = t2 - t1 )

diffs



ggwithinstats(
  data = d,
  x    = time, 
  y    = score, 
  outlier.tagging = T,
  type = "robust", 
  p.adjust.method = "bonferroni", 
  pairwise.display = "all",
  # pairwise.comparisons = FALSE,   
  results.subtitle = F,
  bf.message = F
) + 
  ylab("selfesteem score")+
  theme_classic()+
  theme(legend.position = "top")



#Check Sphericity & Normality assumptions
# old hard way
# install.packages(afex)
library(afex)
hard <- aov_ez(
  data   = d,
  id     = "id", 
  dv     = "score",  
  within = "time")

summary(hard)



### Friedman Test



# install.packages("tidyverse")  # for everything ;)
library(tidyverse)

# install.packages("datarium")   # for marketing data
library(datarium)
head(marketing)

d <- marketing %>%
  select(youtube, facebook, newspaper) %>% 
  rowid_to_column() %>% 
  gather(key = "channel", value = "money", youtube:newspaper) %>% 
  group_by(channel) %>% 
  slice(20:35) %>% # looks better 
  ungroup()

d


# customise the result
ggwithinstats(
  data = d,
  x    = channel, 
  y    = money, 
  type = "nonparametric",
  p.adjust.method = "bonferroni", 
  # pairwise.display = "all",
  # pairwise.comparisons = FALSE,   
  # results.subtitle = F
) + 
  ylab("money spend [thousands of US dollars]")+
  theme_classic()+
  theme(legend.position = "top")




# install.packages("ggstatsplot")
library(ggstatsplot)
ggwithinstats(
  data = d,
  x    = channel, 
  y    = money, 
  type = "nonparametric"
)


## 定性数据的多个配对样本Cochran’s Q Test ------------------------------------------------------------------



# install.packages("tidyverse")
library(tidyverse)

# get the data
set.seed(9) # for reproducibility 
data_wide <- data.frame(
  before = sample(c("+","-","+"), 30, replace = TRUE),
  month  = sample(c("-","+","-"), 30, replace = TRUE),
  year   = sample(c("-","-","+"), 30, replace = TRUE)) %>% 
  mutate(id = 1:nrow(.))

data_long <- data_wide %>% 
  gather(key = "vaccine_time", value = "outcome", before:year) %>% 
  mutate_all(factor)

library(ggstatsplot)
ggbarstats(
  data = data_long, 
  x    = outcome, 
  y    = vaccine_time, 
  paired = T, 
  label = "both"
)
# install.packages(rstatix)
library(rstatix)

# Cochran’s Q Test
cochran_qtest(data_long, outcome ~ vaccine_time|id)


# 自定义图像美学属性


# variance equal: Student's t test
ggbetweenstats(
  data = trial,
  x = death,
  y = age,
  type = "parametric",
  title = "Age by Death",
  xlab = "Death",
  ylab = "Age",
  var.equal = TRUE,
  ggtheme = theme_bw(),
  palette = "Set2"
) +
  scale_y_continuous(sec.axis = dup_axis())

# 其他工具 {ggpubr}


# install.packages("ggpubr")
library(ggplot2)
library(ggpubr)

# 加载数据集
data(iris)

# 绘制分组箱线图
ggboxplot(iris, x = "Species", y = "Sepal.Length", 
          color = "Species", palette = "jco", #jco代表Journal of Clinical Oncology主题
          add = "jitter", 
          ylab = "Sepal Length", xlab = "Species",
          title = "Boxplot of Sepal Length by Species")



