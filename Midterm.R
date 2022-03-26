library(fpp3)
library(shiny)
library(seasonal)
# Path where data is
file_path <- 'multiTimeline.csv'

# Data starts in 3rd row, skip first 2 rows
g_trends <- read.csv(file_path, skip = 2)
# Rename columns
names(g_trends) <- c("Month", "Interest")
# Convert Month to date
g_trends$Month <- yearmonth(g_trends$Month)
# Convert to tsibble
g_trends <- tsibble(g_trends)
#autoplot(g_trends)

 
ui <- fluidPage(
  img(src='https://assets.privy.com/picture_photos/1711448/medium/7a215aa9d7734813a5cbcacaf35c9699?1618002674', 
      align = "right",
      width = '40%'),
    checkboxInput("checkbox", label = "Forecast?", value = TRUE),
  selectInput(inputId = "select", label = h3("Select a Plot"), 
              choices = c("Seasonality", "Decomposition", "Autocorrelation"), 
              selected = 1),
  dateRangeInput(
    inputId = 'selected_daterange',
    label = 'Select Date Range',
    start = min(g_trends$Month),
    end = max(g_trends$Month)),
  
  
  fluidRow(  # fluid rows have total width of 12
    column(11, plotOutput('ts_plot'))),
    fluidRow(column(11, plotOutput('three'))
  )
)

server <- function(input, output, session) {
  
 
  output$ts_plot <- renderPlot({
    min_date <- input$selected_daterange[1]
    max_date <- input$selected_daterange[2]
    df_trends <- g_trends %>%
      filter(Month >= min_date, Month <= max_date)
    if (input$checkbox) {
      df_trends %>%
        model(TSLM(Interest~ trend()+season())) %>% 
        forecast() %>%
        autoplot(df_trends)
    } else {
      autoplot(df_trends)
    }
    
    })
  output$three<- renderPlot({
    min_date <- input$selected_daterange[1]
    max_date <- input$selected_daterange[2]
    df_trends <- g_trends %>%
      filter(Month >= min_date, Month <= max_date)
  if (input$select == 'Decomposition') {
    df_trends %>%
      model(
        classical_decomposition(Interest, type = "additive")
      ) %>%
      components() %>%
      autoplot() +
      labs(title = "Classical additive decomposition of total
                  Boba bubble tea interest")}
    else if (input$select == 'Seasonality') { gg_season(df_trends)}
  else if (input$select == 'Autocorrelation') {autoplot(ACF(df_trends))}
})
}
shinyApp(ui, server)
