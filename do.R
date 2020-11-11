# SETUP, create indicators for dashboard
library(tidyverse)
library(tidylog)
# 1. fix duplicated names
cleaned %>%
  map(colnames)

library(magrittr)
cleaned$`表2.7 各区户籍老年人口年龄构成（2017）` %<>%
  rename(`老年合计（单位：万人）` = `合计（单位：万人）`)

cleaned$`表11.6 各区各类房屋分布情况（2017）` %<>%
  select(-非居住房屋)

cleaned$`表14.3 各区建筑业主要指标（2017）` %<>%
  rename(`建筑业年末从业人员（万人）` = `年末从业人员（万人）`)

cleaned$`表13.2 各区工业企业主要指标（2017）` %>%
  rename(`工业年末从业人员（万人）` = `年末从业人员（万人）`)

cleaned$`表20.15 各区普通小学基本情况（2017）` %<>%
  rename_if(is.numeric, ~ paste0("小学", .x))

cleaned$`表20.13 各区普通中学基本情况（2017）` %<>%
  rename_if(is.numeric, ~ paste0("中学", .x))

master <- plyr::join_all(cleaned, by = "地区")

population <- master %>%
  transmute(
    地区 = 地区,
    `外来人口占常住人口比例(%)` = `其中外来人口` / `年末常住人口（万人）` * 100,
    `净迁入人口占常住人口比例(%)` = (`2017市外迁入` - `2017迁往市外`) / `年末常住人口（万人）` / 100,
    `老龄人口(60岁及以上）占常住人口比例(%)` = `60岁及以上` / `年末常住人口（万人）` * 100
  ) %>%
  pivot_longer(-地区) %>%
  mutate(类别 = "城市人口")

housing <- master %>%
  transmute(
    地区 = 地区,
    `人均居住房屋面积（平方米/人）` = `其中居住房屋` / `年末常住人口（万人）`,
    `旧式里弄占住房面积比例(%)` = `旧式里弄` / `其中居住房屋` * 100,
    `人均绿地面积（公顷/人）` = `绿地面积（公顷）` / `年末常住人口（万人）`
  ) %>%
  pivot_longer(-地区) %>%
  mutate(类别 = "居住环境")

economics <- master %>%
  transmute(
    地区 = 地区,
    `工业生产力（元/人）` = `工业总产值（亿元）` / `年末从业人员（万人）` * 10000,
    `建筑业生产力（元/人）` = `总产值（亿元）` / `建筑业年末从业人员（万人）` * 10000,
    `商用建筑物占全部房屋比例（%）` = (`非居住房屋` - `学校`) / `全部房屋合计（单位：万平方米）` * 100
  ) %>%
  pivot_longer(-地区) %>%
  mutate(类别 = "经济发展")

education <- master %>%
  transmute(
    地区 = 地区,
    `专任教师: 小学在校学生 =（1：？）` = `小学在校学生（人）` / `小学其中#专任教师`,
    `医护人员: 常住人口 =（1：？）` = `年末常住人口（万人）` * 10000 / `卫生技术人员（人）`,
    `床位数: 老年人口 =（1：？）` = `60岁及以上` * 10000 / `床位数（张）`
  ) %>%
  pivot_longer(-地区) %>%
  mutate(类别 = "教育卫生")


indicators <- bind_rows(population, housing, economics, education) %>% view()
save(indicators, file = "MapShanghai/indicators.rda")

library(tidytext)

show_plot <- function(type) {
  indicators %>%
    mutate(全市 = (地区 != "全市")) %>%
    filter(类别 == type) %>%
    mutate(地区 = reorder_within(地区, value, name)) %>%
    ggplot(
      aes(
        x = 地区,
        y = value,
        fill = 类别,
        color = 全市
      )
    ) +
    geom_col() +
    scale_x_reordered() +
    scale_color_manual(values = c("#f1a340", "#f7f7f7")) +
    scale_fill_manual(values = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c")) +
    facet_wrap(~name, scales = "free") +
    coord_flip() +
    labs(x = "", y = "") +
    theme_classic() +
    theme(
      text = element_text(family = "Hei"),
      legend.position = "none"
    )
}


plotly::ggplotly(show_plot("教育卫生"))
