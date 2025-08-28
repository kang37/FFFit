# Preparation ----
library(googlesheets4)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(showtext)
showtext_auto()

# Data ----
# 训练数据。
train_record <- read_sheet(
  ss = paste0(
    "https://docs.google.com/spreadsheets/", 
    "d/13QYDHTo04Ee61e3BOLu05HHbnilhIsaOS3sT5EWjt6U/", 
    "edit?gid=135844309#gid=135844309"
  ), 
  sheet = "训练内容", range = "A:F"
) %>% 
  select(
    "date" = "日期", "exercise" = "项目", 
    "weight" = "重量", "rep" = "次数", "set" = "组数"
  ) %>% 
  # 生成每个动作的次数，并以初始重量为1进行标准化。
  group_by(exercise) %>% 
  mutate(
    date_id = row_number(), 
    weight_init = first(weight), weight_scale = weight / weight_init
  ) %>% 
  ungroup() %>% 
  # 提取年份和月份信息。
  mutate(year = substr(date, 1, 4), month = substr(date, 5, 6), .before = 1)

# 跑步数据。
run <- read_sheet(
  ss = paste0(
    "https://docs.google.com/spreadsheets/", 
    "d/13QYDHTo04Ee61e3BOLu05HHbnilhIsaOS3sT5EWjt6U/", 
    "edit?gid=135844309#gid=135844309"
  ), 
  sheet = "跑步", range = "A:D"
) %>% 
  select(
    "date" = "日期", "speed" = "时速公里", "duration" = "时间"
  ) %>% 
  arrange(date) %>% 
  mutate(date_id = row_number())

# 次数较多项目的初始重量和当前重量。
train_first_last <- 
  train_record %>% 
  group_by(exercise) %>% 
  mutate(date_id_max = max(date_id)) %>% 
  ungroup() %>% 
  filter(date_id_max > 20) %>% 
  # 只保留初始重量和当前重量。
  filter(date_id == 1 | date_id == date_id_max) %>% 
  # 重命名初始重量和当前重量对应的日期ID。
  mutate(date_id = case_when(
    date_id == 1 ~ "初始重量", date_id == date_id_max ~ "当前重量"
  ))
# 对应的宽数据。
train_first_last_wide <- train_first_last %>% 
  pivot_wider(id_cols = exercise, names_from = date_id, values_from = weight)

# Analysis ----
# 全部项目的重量变化。
# ggplot(train_record) + 
#   geom_line(aes(date_id, weight_scale)) + 
#   facet_wrap(.~ exercise)

fig_exercise <- 
  train_record %>% 
  # 计算每个月的平均重量水平和总次数。
  mutate(year_month = paste0(year, "-", month)) %>% 
  group_by(year_month, exercise) %>% 
  summarise(weight = mean(weight), rep = mean(rep), .groups = "drop") %>% 
  # 各项目总工做过几次。
  group_by(exercise) %>% 
  mutate(train_days = n()) %>% 
  ungroup() %>% 
  filter(train_days > 5) %>% 
  ggplot() + 
  geom_col(aes(year_month, weight, fill = rep), linewidth = 0.7) + 
  scale_fill_gradient2(
    low = "darkgreen", high = "darkred", mid = "orange", midpoint = 12
  ) + 
  facet_wrap(.~ exercise) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(x = "年份-月份", y = "平均重量", fill = "平均重复数")

fig_run <- 
  run %>% 
  ggplot() + 
  geom_line(aes(date_id, speed, col = duration), linewidth = 0.7) + 
  scale_color_gradient2(
    low = "darkgreen", high = "darkred", mid = "orange", midpoint = 12
  ) + 
  theme_bw() + 
  labs(x = "训练日期ID", y = "时速（公里）", col = "时长")

print(fig_exercise | fig_run)
