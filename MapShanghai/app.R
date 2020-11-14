#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(sf)
library(plotly)
library(DT)
library(shinythemes)
library(tidytext)

load("shanghai.rda")
# load("emerg.rda")
load("data.rda")
load("indicators.rda")

# df <- cleaned$`表2.2 各区土地面积、常住人口及人口密度（2017）` %>%
#     rename(name = 地区, area = `行政区划面积（平方公里）`) %>%
#     mutate(across(-name, as.numeric))

# map_data <- shanghai %>%
#     left_join(df, by = "name")

create_map <- function(df, col) {
  #
  col <- ifelse(col %in% paste0("`", colnames(df)[-1], "`"),
    col, colnames(df)[[2]]
  )

  map <- ggplot(data = df, aes_string(fill = col, text = "name")) +
    geom_sf() +
    ggthemes::theme_map() +
    scale_fill_distiller(direction = 1) +
    theme(text = element_text(family = "Hei"))

  ggplotly(map)
}

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
    guides(color = F) +
    labs(x = "", y = "") +
    theme_classic() +
    theme(
      text = element_text(family = "SimHei")
    )
}

# Define UI for application that draws a histogram
ui <- navbarPage(
  theme = shinytheme("cerulean"),

  # app windows title
  tags$head(
    tags$title("上海开放数据创新应用大赛-Data Whale复赛作品")
  ),



  tabPanel(
    "上海各区县2017统计年鉴数据",

    # app first row
    titlePanel(
      fluidRow(
        column(5, tags$a(img(src = "soda-datawhale.png", height = 100),
          href = "https://data-whale.netlify.app/"
        )),
        column(7, HTML(
          paste(
            h4("欢迎使用上海各区县城市生活指标可视化追踪平台。"),
            h6("1. 请在页面左侧选择统计年鉴数据表，点击确定显示数据表。单击数据表的指标列即可按数值大小排序"),
            h6("2. 选定数据表后， 可在页面右侧选择绘制区域热力图的数据。鼠标移到地图上方即可获取具体数值、对地图进行局部放大、图片下载等操作")
          )
        ))
      ),
    ),


    # app second row
    h2("上海各区县2017统计年鉴数据热力图"),

    # Sidebar
    sidebarLayout(
      sidebarPanel(
        selectizeInput("dataset", "选择数据表",
          choices = names(cleaned)
        ),
        splitLayout(
          actionButton("confirmdata", "确定"),
          downloadButton("download", "下载数据")
        ),
        div(style = "overflow-x: scroll", DT::dataTableOutput("data")),
        width = 6
      ),

      # Main panel viz
      mainPanel(
        selectizeInput("col", "选择地图参数",
          choices = "请选择参数"
        ),
        actionButton("confirmcol", "确定"),
        plotlyOutput("map"),
        width = 6
      )
    )
  ),

  tabPanel(
    "上海各区县 2012-2017 城市生活指标追踪",

    sidebarLayout(
      sidebarPanel(
        h4("2012 与 2017 年指标比较"),

        radioButtons("var", "请选择内容",
          choices = c("城市人口", "居住环境", "经济发展", "教育卫生")
        ),

        h5("数据来源："),
        p("上海市统计局 上海统计年鉴"),
        h5("指标注释："),
        p("1. 医护人员包括执业医师和注册护士"),
        p("2. 2012年生产力数据根据国家统计局公布的CPI指数调整为2017年价格"),
        p("3. 商业用房包括工厂、仓库、办公建筑、商场店铺、医院、旅馆、影剧院等")
      ),

      mainPanel(
        plotlyOutput("dashboard")
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  setup <- eventReactive(input$confirmdata, {
    dt <- cleaned[[input$dataset]]

    updateSelectizeInput(session, "col", "选择地图参数",
      choices = paste0("`", colnames(dt)[-1], "`"),
      selected = colnames(dt)[2]
    )

    dt
  })

  col <- reactive({
    col <- input$col
  })

  output$data <- renderDataTable({
    datatable <- setup()
    DT::datatable(datatable) %>%
      formatRound(columns = colnames(datatable)[-1], digits = 1)
  })

  output$download <- downloadHandler(
    filename = function() {
      paste("SODA_", Sys.Date(), ".csv")
    },
    content = function(filename) {
      write.csv(setup(), filename)
    }
  )

  output$map <- renderPlotly({
    map <- setup() %>%
      rename(name = 地区)

    map_data <- shanghai %>%
      left_join(map, by = "name")

    validate(
      need(input$col, "请选择地图参数"),
      need(input$confirmcol, "请点击确定")
    )

    create_map(map_data, col())
  })

  output$dashboard <- renderPlotly({
    ggplotly(show_plot(input$var), tooltip = c("地区", "年份", "value")) %>%
      hide_legend()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
