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

load("shanghai.rda")
load("emerg.rda")

map_data <- merge(shanghai, emerg)

create_map <- function(col){
    # col <- enquo(col)
    map <- ggplot(map_data, aes_string(fill = col, text = "name"))+
        geom_sf()+
        ggthemes::theme_map()+
        theme(text = element_text(family = "Hei"))
    
    ggplotly(map)
}


# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("cerulean"),
    
    # app windows title
    tags$head(
        tags$title("上海区县地图")
    ),
    
    # app first row
    titlePanel(
        fluidRow(
            column(5, img(height = 100, src = "soda-datawhale.png")),
            column(7, h4("placeholder text to introduce the app, its relationship with the r package, etc.")),
        ),
        
    ),
    
    # app second row
    h2("上海区县地图"),

    # Sidebar
    sidebarLayout(
        sidebarPanel(
            selectizeInput("dataset", "选择数据表", choices = "TBD"),
            selectizeInput("col","选择参数",
                           choices = colnames(emerg)[-1]),
            DT::dataTableOutput("data")
        ),

        # Main panel viz
        mainPanel(
           plotlyOutput("map")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$data <- renderDataTable(emerg %>% select(name, input$col))

    output$map <- renderPlotly(create_map(input$col))
}

# Run the application 
shinyApp(ui = ui, server = server)

