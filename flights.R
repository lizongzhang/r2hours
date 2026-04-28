

library(tidyverse)
library(nycflights13)
data(flights)

str(flights)


# na.omit() 剔除有缺失值的行-------------------------------------------------------------------

mean(flights$air_time) #有NA的序列求均值，返回NA
mean(flights$air_time, na.rm = TRUE) 

#剔除任意一个变量有缺失值的个案，保留所有变量都有值的个案
#方法一： na.omit()
flights %>% 
  na.omit()

# 第17-18行的输出并未保存，若要保存需要将其存为对象中
mydata <- flights %>% 
  na.omit()

#方法二：filter(complete.cases(.)) 
flights %>% 
  filter(complete.cases(.))

# 查看有缺失值的个案 filter(!complete.cases(.)) 
flights %>% 
  filter(!complete.cases(.))

#剔除某个变量有缺失的个案

# 方法一： drop_na(var_name)
flights %>% 
  drop_na(tailnum) 

# 方法二： filter(!is.na(var_name))
flights %>% 
  filter(!is.na(air_time))


# distinct() 剔除重复的行-------------------------------------------------------------------
#所有列都相同，视为重复值
mydata <-flights %>% 
  na.omit() %>% 
  distinct() 

#指定列相同，才视为重复值
flights %>% 
  na.omit() %>% 
  distinct(flight, tailnum) 

# filter() 提取满足条件的行----------------------------------------------------------------
# 用逗号分隔多个需要同时满足的条件
flights %>% 
  filter(month == 1, day ==1)

# 且 &
flights %>% 
  filter ((month ==1 | month ==2) & carrier == "UA")

# 或 ｜
flights %>% 
  filter(month == 1 | day ==1)

# %in% 属于
flights %>% 
  filter(carrier %in% c("UA","DL", "AA")) 

# != 不等于
flights %>% 
  filter(month ==1, carrier != "UA")

flights %>% 
  filter ((month ==1 | month ==2) & carrier == "UA") 

# arrange() 行排序--------------------------------------------------------------------
# 升序
flights %>% 
  select(month, day, distance) %>% 
  arrange(distance)

# 降序desc()
flights %>% 
  select(month, day, distance) %>% 
  arrange(desc(distance))

# 按距离降序，月份升序
flights %>% 
  select(distance, month, day, flight) %>% 
  arrange(desc(distance), month)

# select() 选择列------------------------------------------------------------------
#选择需要的变量，创建新的对象mydata,否则select的结果并未保存
mydata <- flights %>%
  select(month, day, arr_time, distance)
mydata

#选择一个范围，变量1：变量2
mydata <- flights %>% 
  select(year:carrier)

#选择一个范围，第#列：第#列
mydata <- flights %>% 
  select(1:10)

#不选month和day
mydata <- flights %>% 
  select(-month, -day)

#设置条件 contains(), starts_with, ends_with
flights %>% select(-contains("time"))
flights %>%  select(starts_with("d"))
flights %>%  select(year, month, ends_with("time"))

# mutate () 追加列------------------------------------------------------------------

#追加新的变量, 注意要将存为一个新的对象，否则追加的变量并未保存
mydata <- flights %>% 
  mutate(speed = distance/air_time*60)

# rename () 重命名------------------------------------------------------------------
# 新名称 = 旧名称
mydata <- flights  %>% 
  rename(destination = dest)

# recode () 重新编码-----------------------------------------------------------------
# 旧值 = 新值
mydata <- flights %>% 
  mutate(company = recode(carrier,
                          "UA" = "United Airlines",
                          "B6"="JetBlue Airways",
                          "EV"="ExpressJet Airlines",
                          "DL"="Delta Air Lines",
                          "AA"="American Airlines",
                          "MQ"="Envoy Air",
                          "US"="US Airways",
                          "9E"="Endeavor Air",
                          "WN"="Southwest Airlines",
                          "VX"="Virgin America",
                          "FL"="AirTran Airways",
                          "AS"="Alaska Airlines",
                          "YV"="Mesa Airlines",
                          "HA"="Hawaiian Airlines",
                          "OO"="SkyWest Airlines")
         )

# if_else()  二值转换-----------------------------------------------------------------
mydata <- flights %>% 
  mutate(type = if_else(air_time <= 120, 
                        "short", "long")
         )

# case_when() 多值转换---------------------------------------------------------------
mydata <- flights %>% 
  mutate(status = case_when(
    arr_delay > 0 ~ "delayed",  # 条件 ～ 值
    arr_delay == 0 ~ "on time",
    arr_delay < 0 ~ "ahead")
  )
table(mydata$status) %>% 
  sort(decreasing = TRUE) %>% 
  prop.table()

mydata <- flights %>% 
  mutate(big4 = case_when(
    carrier == "UA" ~ "US Airways",
    carrier == "B6" ~ "JetBlue Airways",
    carrier == "EV" ~ "ExpressJet Airlines",
    carrier == "DL" ~ "Delta Air Lines",
    .default = "other") #不属于前述情况的，都转换为other
  )
table(mydata$big4) %>% 
  sort(decreasing = TRUE) %>% 
  prop.table()


# cut() 将定量变量转换为分组区间 ------------------------------------------------------

summary(flights$air_time)
mydata <- flights %>% 
  na.omit() %>%
  mutate(air_time_cat = cut(air_time, 
                        breaks = c(0, 60, 90, 120, 150, 180, Inf),
                        labels = c("(0,60]", "(60-90]", 
                                   "(90,120]", "(120-150]",
                                   "(150,180]", "180+")) 
         )




  
# as.factor 转换为因子---------------------------------------------------------------
mydata <- flights %>% 
  mutate(status = case_when(
    arr_delay > 0 ~ "delayed",
    arr_delay == 0 ~ "on time",
    arr_delay < 0 ~ "ahead")
  )
table(mydata$status) #字符串变量按字母顺序排列

mydata <- mydata %>% 
  mutate(status =as.factor(status)) 
levels(mydata$status) #查看因子水平顺序
table(mydata$status)  #按默认因子水平顺序排列

mydata <- mydata %>% 
  mutate(status = fct_relevel(status, #自定义因子水平顺序
                              "delayed", "on time", "ahead"))
table(mydata$status) #按自定义因子水平顺序排列
levels(mydata$status)




# forcats其他函数 -------------------------------------------------------------



# 按频数降序排列因子

mydata %>%
  na.omit() %>% 
  ggplot(aes(fct_infreq(status),
             fill = status))+ 
  geom_bar()
mydata %>%
  na.omit() %>% 
  ggplot(aes(fct_infreq(as.factor(month)),
             fill = as.factor(month)))+ 
  geom_bar()



# 按中位数降序排列因子
mydata %>% 
  na.omit() %>% 
  ggplot(aes(fct_reorder(as.factor(origin), 
                         air_time,
                         "median"),
             air_time,
             fill = as.factor(origin)))+
  geom_boxplot()+
  coord_flip()

#Reduce by frequency
mydata %>% 
  na.omit() %>% 
  mutate(big3 = fct_lump(carrier, 
                        n = 3, 
                        other_level = "other")) %>% 
  janitor::tabyl(big3)


# count() -----------------------------------------------------------------
# count()统计行数
flights %>% 
  na.omit() %>% 
  count()

# count()绘制频数分布表

#单个变量
flights %>% 
  na.omit() %>% 
  count(month, sort = TRUE) %>% 
  mutate(percent = scales::percent(n/sum(n))) #计算百分比

#两个变量
flights %>% 
  na.omit() %>% 
  count(month, origin) %>% #按两个定性变量分组统计
  pivot_wider(names_from = origin, values_from = n) 

#count()分组求和，按month分组，各组air_time之和
flights %>% 
  na.omit() %>% 
  count(month, wt = air_time, sort = TRUE) # wt = 需要求和的变量

#count()分组求和，按carrier分组，各组air_time之和
flights %>% 
  na.omit() %>% 
  count(carrier, wt = air_time, sort = TRUE)


## count{dplyr}与table{stats}比较 --------------------------------------------

# count{dplyr}的输出是一个数据框
# count{dplyr}可以对变量按定性变量分组后，求出各个组别的定量变量的和

# table{stats}的输出是一个table对象, 不是数据框。
# table{stats}的输出pipe到addmargins()可以添加行的合计，列的合计，总计。
# table{stats}的输出pipe到prop.table()可以计算总计百分比、行百分比、列百分比
# table{stats}的输出pipe到barplot(), pie()绘制条形图、饼图


## table() -----------------------------------------------------------------
table(flights$month) #单个变量
table(flights$month) %>% 
  prop.table() %>%  #计算总计百分比
  round(3) #保留三位小数

table(flights$month, flights$origin) #两个变量

table(flights$month, flights$origin) %>%
  addmargins() #添加行的合计，列的合计，总计

table(flights$month, flights$origin) %>% 
  prop.table() %>%  #计算总计百分比
  round(3)

table(flights$month, flights$origin) %>% 
  prop.table(1) %>% #计算行百分比
  round(3)
  
table(flights$month, flights$origin) %>% 
  prop.table(2) %>% #计算列百分比
  round(3)







# summarise() -------------------------------------------------------------

# 计算单个变量的均值、中位数、标准差、样本量、缺失值个数
flights %>% 
  summarise(ave = mean(dep_delay, na.rm = TRUE),
            med = median(dep_delay, na.rm = TRUE),
            stdev = sd(dep_delay, na.rm = TRUE),
            cases = n(),
            cases_miss = sum(is.na(dep_delay)))

# 计算指定的多个变量的均值、中位数、标准差
# across()对多个变量应用同一个函数 
# .cols = c()指定需要计算的变量 
# .fns = list()指定需要计算的函数
flights %>% 
  na.omit() %>%
  summarise(
    across(.cols = c(arr_delay, air_time, distance),
           .fns = list(ave = mean, 
                       med = median,
                       stdev = sd))
  ) %>% 
  print(width = Inf)

flights %>% 
  na.omit() %>% 
  group_by(origin) %>%
  summarise(across(
    .cols = where(is.numeric),  #计算数值型变量
    .fns = mean)) %>%
  print(width = Inf) #显示所有的列


# summarise_all()对所有变量进行同样的操作
# 但要注意字符串变量无法计算均值、中位数、标准差
flights %>% 
  na.omit() %>% 
  group_by(origin) %>%
  select(is.numeric) %>% #选择所有数值型变量
  summarise_all(list(ave = mean, med = median, stdev = sd)) %>%  #对所有变量进行同样的操作
  print(width = Inf)

# group_by() --------------------------------------------------------------

# 按month分组，计算各组的平均延误时间
flights %>% 
  na.omit() %>% 
  group_by(month) %>% 
  summarise(ave_delay = mean(arr_delay))

# 按origin分组，计算各组的数值型变量的均值
flights %>% 
  na.omit() %>% 
  group_by(origin) %>%
  summarise(across(
    .cols = where(is.numeric),  # 数值型变量
    .fns = mean)) %>%
  print(width = Inf) #显示所有的列

# 按origin分组，再计算百分位数
flights %>% 
  na.omit() %>% 
  group_by(origin) %>% 
  summarise(
    p2 = quantile(dep_delay, probs = 0.02),
    p25 = quantile(dep_delay, probs = 0.05),
    p50 = quantile(dep_delay, probs = 0.5),
    p75 = quantile(dep_delay, probs = 0.75),
    p98 = quantile(dep_delay, probs = 0.98)
  )

# psych::describeBy
psych::describeBy(flights$air_time, group = flights$origin, 
                  mat = TRUE, digits = 3) %>% 
  as.data.frame() %>% 
  select(group1, mean, median, sd, min, max) 

# pivot_wider -------------------------------------------------------------
score <- tibble(
  subject = c("chinese", "math", "english"),
  `2020` = c(80, 96, 92),
  `2021` = c(85, 95, 90),
  `2022` = c(83, 98, 96)
)
score

score_longer <- pivot_longer(score, 
             cols = c(`2020`, `2021`, `2022`), 
             names_to = "year", 
             values_to = "grade")
score_longer


score_wider <- pivot_wider(score_longer, 
             names_from = "year", 
             values_from = "grade")
score_wider



# pivot_wider()将长数据转换为宽数据

flights_by_origin <- flights %>% 
  na.omit() %>%
  group_by(origin) %>%
  count(month) %>% 
  mutate(percent = scales::percent(n / sum(n))  #计算百分比
  ) 
flights_by_origin

flights_by_origin %>%
  select(-n) %>% #删除列n
  pivot_wider(names_from = month, values_from = percent) #将origin列转换为列名，percent列转换为值


flights_by_month <- flights %>% 
  na.omit() %>%
  group_by(month) %>%
  count(origin) %>% 
  mutate(percent = scales::percent(n / sum(n))  #计算百分比
  )
flights_by_month

flights_by_month %>%
  select(-n) %>% #删除n列
  pivot_wider(names_from = origin, values_from = percent)


# pivot_longer ------------------------------------------------------------

flights_by_month %>%
  select(-n) %>% #删除n列
  pivot_wider(names_from = origin, values_from = percent) %>% 
  pivot_longer(
    cols = c("EWR", "JFK", "LGA"), #列名
    names_to = "origin", #新列名
    values_to = "percent" #新值
  )


# complex example ---------------------------------------------------------



flights %>% 
  na.omit() %>% 
  summarise(
    cases = n(),
    air_time_mean = mean(air_time),
    air_time_median = median(air_time),
    air_time_sd = sd(air_time),
    EWR_flights = sum(origin == "EWR"), #对满足条件的行计数
    JFK_flights = sum(origin == "JFK"),
    LGA_flights = sum(origin == "LGA"),
    EWR_flights_percent = scales::percent(EWR_flights/cases),
    JFK_flights_percent = scales::percent(JFK_flights/cases),
    LGA_flights_percent = scales::percent(LGA_flights/cases)
  ) %>% 
  print( width = Inf)  #显示tibble所有的列width = Inf

flights %>% 
  na.omit() %>% 
  group_by(month) %>% #分组后再计算统计量
  summarise(
    cases = n(),
    air_time_mean = mean(air_time),
    air_time_median = median(air_time),
    air_time_sd = sd(air_time),
    EWR_flights = sum(origin == "EWR"), #对满足条件的行计数
    JFK_flights = sum(origin == "JFK"),
    LGA_flights = sum(origin == "LGA"),
    EWR_flights_percent = scales::percent(EWR_flights/cases),
    JFK_flights_percent = scales::percent(JFK_flights/cases),
    LGA_flights_percent = scales::percent(LGA_flights/cases)
  ) %>% 
  print( width = Inf) 

# 计算满足条件的统计量
flights %>% 
  na.omit() %>% 
  group_by(month) %>% #分组后再计算统计量
  summarise(
    max_dep_delay_JFK = max(dep_delay[origin == "JFK"]),
    max_dep_delay_LGA = max(dep_delay[origin == "LGA"]),
    max_dep_delay_EWR = max(dep_delay[origin == "EWR"]),
  ) 





# 12个月份，3家机场出发地的航班数量
flights %>% 
  na.omit() %>% 
  group_by(month) %>% 
  summarise(
    total_flights  = n(),
    JFK_cases   = sum(origin == "JFK"),
    LGA_cases = sum(origin == "LGA"),
    EWR_cases = sum(origin == "EWR")
  )


#3家机场航班提前/准点/延误的数量
flights %>% 
  na.omit() %>%
  mutate(status = case_when(
    arr_delay > 0 ~ "delayed",  # 条件 ～ 值
    arr_delay == 0 ~ "on time",
    arr_delay < 0 ~ "ahead")
  ) %>% 
  group_by(origin) %>%
  summarise(
    cases = n(),
    ahead_cases = sum(status == "ahead"),
    on_time_cases = sum(status == "on time"),
    delayed_cases = sum(status == "delayed")
  )






# draft -------------------------------------------------------------------

data(mtcars)

library(tidyverse)

select(mtcars, mpg, cyl, disp)

flights %>% 
  count(tailnum, sort = TRUE) %>% 
  left_join(planes, by = "tailnum") %>% 
  select(manufacturer, model, n) %>% 
  group_by(manufacturer) %>% 
  summarise(n = sum(n), sort = TRUE) %>% 
  top_n(10, n) %>% 
  ggplot(aes(x = reorder(manufacturer, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Manufacturer", 
       y = "Number of flights", 
       title = "Top 10 manufacturers by number of flights")

filter(mtcars, cyl == 4)

`Main pixel`
