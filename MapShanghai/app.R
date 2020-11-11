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
# Define UI for application that draws a histogram
ui <- navbarPage(
  theme = shinytheme("cerulean"),

  # app windows title
  tags$head(
    tags$title("上海区县地图")
  ),

  # app first row
  titlePanel(
    fluidRow(
      column(5, img(height = 100, src = "soda-datawhale.png")),
      column(7, h4("使用说明：\n1. 选择数据表，点击确定\n2. 选择绘制区域热力图的参数，点击确定")),
    ),
  ),

  tabPanel(
    "数据表",


    # app second row
    h2("上海区县地图"),

    # Sidebar
    sidebarLayout(
      sidebarPanel(
        selectizeInput("dataset", "选择数据表",
          choices = names(cleaned)
        ),
        actionButton("confirmdata", "确定"),
        DT::dataTableOutput("data")
      ),

      # Main panel viz
      mainPanel(
        selectizeInput("col", "选择地图参数",
          choices = "请选择参数"
        ),
        actionButton("confirmcol", "确定"),
        plotlyOutput("map")
      )
    )
  ),

  tabPanel(
    "dashboard",
    selectizeInput("var","请选择内容",
                   choices = c("城市人口","居住环境","经济发展","教育卫生")),
    mainPanel(
      plotlyOutput("dashboard")
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
      plotly::ggplotly(show_plot(input$var))
  })
}

# Run the application
shinyApp(ui = ui, server = server)

