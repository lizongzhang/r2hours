library(tidyverse)
data(mpg)

count(mpg, drv)

mpg %>% 
  ggplot(aes(drv, fill = drv)) +
  geom_bar()


# fct_recode() --------------------------------------------------------------

mpg %>% 
  ggplot(aes(drv, fill = drv)) +
  geom_bar() +
  scale_fill_discrete(labels = c("four wheels", 
                                  "front wheels",
                                  "rear wheels"))

mpg %>% 
  ggplot(aes(drv, fill = drv)) +
  geom_bar() +
  scale_x_discrete(labels = c("four wheels", 
                              "front wheels",
                              "rear wheels")) +
  theme(legend.position = "none")

mpg %>% 
  ggplot(aes(displ, cty, col = drv)) +
  geom_point() +
  scale_color_discrete(labels = c("four wheels", 
                                  "front wheels",
                                  "rear wheels"))
mpg <- mpg %>%
  mutate(drv = fct_recode(drv,
                          "front wheels" = "f",
                          "rear wheels" = "r",
                          "four wheels" = "4"))

mpg %>% 
  ggplot(aes(drv, fill = drv)) +
  geom_bar()


# fct_infreq() --------------------------------------------------------------
mpg %>% 
  ggplot(aes(fct_infreq(drv), fill = drv)) +
  geom_bar()


# fct_rev() ---------------------------------------------------------------

mpg %>% 
  ggplot(aes(fct_rev(fct_infreq(drv)), fill = drv)) +
  geom_bar()

# fct_reorder() -----------------------------------------------------------
mpg %>% 
  group_by(drv) %>%
  summarise(mean_cty = mean(cty)) %>% 
  mutate(drv = fct_reorder(drv, mean_cty)) %>%
  ggplot(aes(drv, mean_cty, fill = drv)) +
  geom_col()

mpg %>% 
  group_by(drv) %>%
  summarise(mean_cty = mean(cty)) %>% 
  mutate(drv = fct_reorder(drv, mean_cty)) %>%
  ggplot(aes(drv, mean_cty, fill = drv)) +
  geom_col()

mpg %>% 
  group_by(drv) %>%
  summarise(mean_cty = mean(cty)) %>% 
  mutate(drv = fct_reorder(drv, -mean_cty)) %>%
  ggplot(aes(drv, mean_cty, fill = drv)) +
  geom_col()

mpg %>% 
  group_by(drv) %>%
  summarise(mean_cty = mean(cty)) %>% 
  ggplot(aes(fct_reorder(drv, mean_cty), 
             mean_cty, 
             fill = drv)) +
  geom_col()

mpg %>%
  ggplot(aes(cty, fct_reorder(class, cty),
             fill = class)) +
  geom_boxplot()

mpg %>%
  ggplot(aes(cty, fct_reorder(class, -cty),
             fill = class)) +
  geom_boxplot()




# 合并类别 --------------------------------------------------------------------
library(tidyverse)
data(mpg)

count(mpg, class) %>% arrange(desc(n))

fct_lump_n(mpg$class, 4) %>% fct_count()

fct_lump_min(mpg$class, 30) %>% fct_count()

fct_lump_lowfreq(mpg$class) %>% fct_count()

fct_lump_prop(mpg$class, 0.14) %>% fct_count()

mpg %>%
  group_by(class) %>%
  summarise(proportion = n() / nrow(mpg)) %>%
  arrange(desc(proportion))


fct_collapse(mpg$class,
             "compact" = c("compact", "subcompact"),
             "midsize" = c("midsize", "minivan", "pickup")) %>% 
  fct_count()

# c: CNG (Compressed Natural Gas)
# d: Diesel
# e: Ethanol blend, typically referring to gasoline blended with a certain proportion of ethanol
# p: Premium unleaded gasoline 辛烷值较高，通常在91到93之间（在美国），有的甚至更高
# r: Regular unleaded gasoline 辛烷值较低，通常是87（在美国）

mpg %>% 
  count(fl) %>% 
  arrange(desc(n))

fct_other(mpg$fl, keep = c("p", "r")) %>% fct_count()

fct_other(mpg$fl, drop = c("c", "d", "e")) %>% fct_count()

# 保存合并后的类别

mpg <- mpg %>% 
  mutate(fl = fct_other(fl, keep = c("p", "r")),
         class = fct_collapse(class,
                               "compact" = c("compact", "subcompact"),
                               "midsize" = c("midsize", "minivan", "pickup"))
         )

mpg %>% count(fl)

mpg %>% count(class)


# box plot sorted by median ------------------------------------------------
library(tidyverse)
data(mpg)

mpg %>% 
  ggplot(aes(class, cty, fill = class)) +
  geom_boxplot()+
  labs(fill = "Class", x = "Class")

# 按中位数升序
mpg %>% 
  ggplot(aes(fct_reorder(class, cty), 
             cty, 
             fill = class)) +
  geom_boxplot()+
  labs(fill = "Class", x = "Class")

mpg %>% 
  ggplot(aes(fct_reorder(class, cty), 
             cty, 
             fill = fct_reorder(class, cty))) +
  geom_boxplot()+
  labs(fill = "Class", x = "Class")

mpg %>% 
  ggplot(aes(fct_reorder(as.factor(cyl), cty), 
             cty, 
             fill = as.factor(cyl))) +
  geom_boxplot()+
  labs(fill = "Number of Cylinders", 
       x = "Number of Cylinders")

# 按中位数降序
mpg %>% 
  ggplot(aes(fct_reorder(class, -cty), 
             cty, 
             fill = fct_reorder(class, -cty))) +
  geom_boxplot()+
  labs(fill = "Class", x = "Class")

mpg %>% 
  ggplot(aes(fct_reorder(class, cty), 
             cty, 
             fill = fct_reorder(class, cty))) +
  geom_boxplot()+
  labs(fill = "Class", x = "Class")



# boxplot sorted by mean ------------------------------------------------
mpg %>% 
  ggplot(aes(fct_reorder(class, cty, .fun = mean), 
             cty, fill = class)) +
  geom_boxplot() +
  labs(fill = "Class", x = "Class")


# 控制图例标签的顺序
library(tidyverse)
library(nycflights13)
data(flights)

flights %>% 
  count(carrier, month) %>%
  group_by(carrier) %>%
  ggplot(aes(month, n, 
             color = carrier)) +
  geom_line() +
  labs(color = "Carrier") +
  scale_x_continuous(breaks = 1:12)



flights %>% 
  count(carrier, month) %>%
  group_by(carrier) %>%
  ggplot(aes(month, n, 
             color = fct_reorder2(carrier, month, n))) +
  geom_line() +
  labs(color = "Carrier") +
  scale_x_continuous(breaks = 1:12)







