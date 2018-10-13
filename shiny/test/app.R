library(ggplot2)
library(shiny)

# Define UI for slider demo app ----
ui <- fluidPage(

  # App title ----
  titlePanel("过滤指标"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar to demonstrate various slider options ----
    sidebarPanel(

      # Input: Simple integer interval ----
      sliderInput("de1year", "高位下降百分比（1年期）:",
                  min = 0, max = 1,
                  value = c(0,1), step = 0.1),

      # Input: Decimal interval with step value ----
      sliderInput("pos1year", "价位（1年期）:",
                  min = 0, max = 1,
                  value = c(0,1), step = 0.1),

      # Input: Specification of range within an interval ----
      sliderInput("profit", "净利润:",
                  min = 0, max = 30327,
                  value = c(0,30327)),

      # Input: Custom currency format for with basic animation ----
      sliderInput("subvalue", "流通股:",
                  min = 3, max = 14478,
                  value = c(0,14478), step = 100,
                  pre = "", sep = "",
                  animate = F),

      # Input: Animation with custom interval (in ms) ----
      # to control speed, plus looping
      sliderInput("ZYratio", "质押比:",
                  min = 0, max = 1,
                  value = c(0,1), step = 0.1,
                  # animate = animationOptions(interval = 300, loop = TRUE)
                  ),
      conditionalPanel(
        'input.dataset === "filterCode3"',
        checkboxGroupInput("show_vars", "Columns in filterCode3 to show:",
                           names(filterCode3), selected = names(filterCode3))
      )

    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Table summarizing the values entered ----
      # tableOutput("values")
      tabsetPanel(
        id = 'dataset',
        tabPanel("filterCode3", DT::dataTableOutput("mytable1"))
        )

    )
  )
)

# Define server logic for slider examples ----
server <- function(input, output) {
  filterCode3 <- readRDS("/Users/surgery/Project/HOME/github/bullStock/tushare/filterCode3.rds")

  output$mytable1 <- DT::renderDataTable({
    # DT::datatable(diamonds2[, input$show_vars, drop = FALSE])
    filterCode4 <- filterCode3[filterCode3$de1year >= input$de1year[1] & filterCode3$de1year <= input$de1year[2] 
                              & filterCode3$pos1year >= input$pos1year[1] & filterCode3$pos1year <= input$pos1year[2]
                              & filterCode3$profit >= input$profit[1] & filterCode3$profit <= input$profit[2]
                              & filterCode3$subvalue >= input$subvalue[1] & filterCode3$subvalue <= input$subvalue[2]
                              & filterCode3$ZYratio >= input$ZYratio[1] & filterCode3$ZYratio <= input$ZYratio[2]
                              ,]
    DT::datatable(filterCode4[, input$show_vars, drop = FALSE])
  })

  # Reactive expression to create data frame of all input values ----
  # sliderValues <- reactive({

  #   # data.frame(
  #   #   Name = c("Integer",
  #   #            "Decimal",
  #   #            "Range",
  #   #            "Custom Format",
  #   #            "Animation"),
  #   #   Value = as.character(c(input$integer,
  #   #                          input$decimal,
  #   #                          paste(input$range, collapse = " "),
  #   #                          input$format,
  #   #                          input$animation)),
  #   #   stringsAsFactors = FALSE)
  #   filterCode3[filterCode3$de1year >= input$de1year[1] & filterCode3$de1year <= input$de1year[2],]
  # })

  # Show the values in an HTML table ----
  # output$values <- renderTable({
  #   sliderValues()
  # })

}

# Create Shiny app ----
shinyApp(ui, server)