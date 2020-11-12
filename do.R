# SETUP, create indicators for dashboard
library(tidyverse)
library(tidylog)
library(magrittr)

cleaned_2012 %>%
  map(colnames)
# 1. fix duplicated names
fix_names_2017 <- function(df){

  df$`表2.7 各区户籍老年人口年龄构成（2017）` %<>%
    rename(`老年合计（单位：万人）` = `合计（单位：万人）`)
  
  df$`表11.6 各区各类房屋分布情况（2017）` %<>%
    select(-非居住房屋)
  
  df$`表14.3 各区建筑业主要指标（2017）` %<>%
    rename(`建筑业年末从业人员（万人）` = `年末从业人员（万人）`)
  
  df$`表13.2 各区工业企业主要指标（2017）` %<>%
    rename(`工业年末从业人员（万人）` = `年末从业人员（万人）`)
  
  df$`表20.15 各区普通小学基本情况（2017）` %<>%
    rename_if(is.numeric, ~ paste0("小学", .x))
  
  df$`表20.13 各区普通中学基本情况（2017）` %<>%
    rename_if(is.numeric, ~ paste0("中学", .x))
  
  return(df)
}

fix_names_2012 <- function(df){
  df %>%
    map(colnames)
  
  df$`表2.7 各区、县户籍老年人口年龄构成（2012）` %<>%
    rename(`老年合计（单位：万人）` = `合计（单位：万人）`)
  
  df$`表11.6 各区、县各类房屋分布情况（2012）` %<>%
    select(-非居住房屋)
  
  df$`表11.17 各区、县绿化面积（2012）` %<>% 
    rename(`绿地面积（公顷）` = `城市绿地面积（公顷）`)
  
  df$`表14.3 各区、县建筑业主要指标（2012）` %<>%
    rename(`建筑业年末从业人员（万人）` = `年末从业人员（万人）`)
  cleaned_2012$`表14.3 各区、县建筑业主要指标（2012）` %<>%
    mutate(`总产值（亿元）` = `总产值（亿元）` * 1.1)
  
  df$`表13.2 各区、县工业企业主要指标（2012）` %<>%
    mutate(`工业总产值（亿元）` = `工业总产值（亿元）`* 1.1)
  df$`表13.2 各区、县工业企业主要指标（2012）` %<>%
    rename(`工业年末从业人员（万人）` = `从业人员（万人）`)
  
  df$`表20.15 各区、县普通小学基本情况（2012）` %<>%
    rename_if(is.numeric, ~ paste0("小学", .x))
  df$`表20.13 各区、县普通中学基本情况（2012）`%<>%
    rename_if(is.numeric, ~ paste0("中学", .x))
  
  return(df)
}


create_indicators <- function(master){
  population <- master %>%
    transmute(
      地区 = 地区,
      年份 = year,
      `外来人口占常住人口比例(%)` = `其中外来人口` / `年末常住人口（万人）` * 100,
      `净迁入人口占常住人口比例(%)` = (`市外迁入` - `迁往市外`) / `年末常住人口（万人）` / 100,
      `老龄人口(60岁及以上）占常住人口比例(%)` = `60岁及以上` / `年末常住人口（万人）` * 100
    ) %>%
    pivot_longer(-c("地区","年份")) %>%
    mutate(类别 = "城市人口")
  
  housing <- master %>%
    transmute(
      地区 = 地区,
      年份 = year,
      `人均居住房屋面积（平方米/人）` = `其中居住房屋` / `年末常住人口（万人）`,
      `旧式里弄占住房面积比例(%)` = `旧式里弄` / `其中居住房屋` * 100,
      `人均绿地面积（公顷/人）` = `绿地面积（公顷）` / `年末常住人口（万人）`
    ) %>%
    pivot_longer(-c("地区","年份")) %>%
    mutate(类别 = "居住环境")
  
  economics <- master %>%
    transmute(
      地区 = 地区,
      年份 = year,
      `工业生产力（元/人）` = `工业总产值（亿元）` / `工业年末从业人员（万人）` * 10000,
      `建筑业生产力（元/人）` = `总产值（亿元）` / `建筑业年末从业人员（万人）` * 10000,
      `商用建筑物占全部房屋比例（%）` = (`非居住房屋` - `学校`) / (`其中居住房屋` + `非居住房屋`) * 100
    ) %>%
    pivot_longer(-c("地区","年份")) %>%
    mutate(类别 = "经济发展")
  
  education <- master %>%
    transmute(
      地区 = 地区,
      年份 = year,
      `专任教师: 小学在校学生 =（1：？）` = `小学在校学生（人）` / `小学其中#专任教师`,
      `医护人员: 常住人口 =（1：？）` = `年末常住人口（万人）` * 10000 / `卫生技术人员（人）`,
      `床位数: 老年人口 =（1：？）` = `60岁及以上` * 10000 / `床位数（张）`
    ) %>%
    pivot_longer(-c("地区","年份")) %>%
    mutate(类别 = "教育卫生")
  
  bind_rows(population, housing, economics, education) 
  
}

master <- bind_rows(
  plyr::join_all(fix_names_2017(cleaned), by = "地区") %>%
    mutate(year = "2017"),
  
  plyr::join_all(fix_names_2012(cleaned_2012), by = "地区") %>% 
    mutate(year = "2012")
  ) 

indicators <- master %>% 
  create_indicators()

save(indicators, file = "MapShanghai/indicators.rda")

# chart
library(tidytext)
# install.packages("extrafont")
# library(extrafont)
# font_import()
# 
# # 显示字体信息
# fonts()

show_plot <- function(type) {
  indicators %>%
    mutate(全市 = (地区 != "全市")) %>%
    filter(类别 == type) %>%
    mutate(地区 = reorder_within(地区, value, name)) %>%
    ggplot(
      aes(
        x = 地区,
        y = value,
        fill = 年份,
        color = 全市
      )
    ) +
    geom_col(position = "dodge") +
    scale_x_reordered() +
    scale_color_manual(values = c("#f1a340", "#f7f7f7")) +
    scale_fill_manual(values = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c")) +
    facet_wrap(~name, scales = "free") +
    coord_flip() +
    guides(color = F)+
    labs(x = "", y = "") +
    theme_classic() +
    theme(
      text = element_text(family = "SimHei")
    )
}

show_plot("经济发展")

library(plotly)
ggplotly(show_plot("教育卫生"), tooltip = c("地区", "年份","value")) %>% 
  hide_legend
  
