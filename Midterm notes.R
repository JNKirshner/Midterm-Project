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
 autoplot(g_trends)

 plot(tsibble(g_trends))
 #Autocorrelation
 plot(ACF(g_trends))
 
#Seasonality
 gg_season(g_trends)
 
 #CHOICES FOR DECOMP FEATURE
 #lambda
 lambda <- g_trends %>%
   features(Interest, features = guerrero) %>%
   pull(lambda_guerrero)
 g_trends%>%
   autoplot(box_cox(Interest, lambda)) 
 
 lambda <- g_trends %>%
   features(Interest, features = guerrero) %>%
   pull(lambda_guerrero)
 g_trends%>%
   autoplot(box_cox(Interest, lambda)) 
#classical
 g_trends %>%
   model(
     classical_decomposition(Interest, type = "additive")
   ) %>%
   components() %>%
   autoplot() +
   labs(title = "Classical additive decomposition of total
                  Boba bubble tea interest")
 
 #x11
 x11_dcmp <- g_trends %>%
   model(x11 = X_13ARIMA_SEATS(Interest ~ x11())) %>%
   components()
 autoplot(x11_dcmp) +
   labs(title =
          "Decomposition of total boba tea interest using X-11.")
#SEATS
 seats_dcmp <- g_trends %>%
   model(seats = X_13ARIMA_SEATS(Interest ~ seats())) %>%
   components()
 autoplot(seats_dcmp) +
   labs(title =
          "Decomposition of total Boba bubble tea interest using SEATS")
 
 
 img(src='app.jpg', align = "right"),
 
 output$app.jpg <- renderImage({ input$app.jpg})
 
 
df_trends %>%
   model(STL (Interest)) %>%
   components()
 autoplot(STL_decomp) +
   labs(title =
          "Decomposition: Boba bubble tea interest")