library(shiny)
library(forecast)

# Define UI ----
ui <- fluidPage(
  titlePanel("Peramalan Harga Saham PT WIKA dan PT Waskita"),
  h4("Created by : Ayu"),
  h5("Statistika Bisnis ITS"),
  
  sidebarLayout(
    sidebarPanel(
      imageOutput(outputId = "logo", width=300, height=300),
      selectInput(inputId = "saham", label = "Choose Company Stock", choices = c("WIKA","Waskita"), selected = "WIKA"),
      radioButtons(inputId ="metode", label = "Model terpilih", choices = c("ARIMA(0,3,1)", "ARIMA(1,3,1)"),choiceValues = "ARIMA(0,3,1)"),
    ),
    mainPanel(
      h4("Plot time series Saham PT WIKA dan PT Waskita", align = "center"),
      plotOutput(outputId="tsplot"),
      
      h4("Plot Hasil Peramalan", align = "center"),
      plotOutput(outputId="forplot"),
    )
  ),
  h4("Model yang digunakan", align="center"),
  fluidRow(verbatimTextOutput(outputId = "text")),
  h4("Hasil Ramalan", align="center"),
  fluidRow(dataTableOutput(outputId = "table")),
)

# Define server logic ----
server <- function(input, output) {
  output$logo <- renderImage ({
    list(src = "C:/Users/HP/Downloads/WIKA.png", 
         width = "80%", height = 200)
  })
  
  output$tsplot <- renderPlot ({
    library(fpp2)
    require(gridExtra)
    data <- read.csv("D:/Punya Ayuk/campus life must go on/hell 5/EVD/Harga Saham WIKA Waskita.csv", header=TRUE)
    p1 <- autoplot(ts(data$WIKA)) + ylab("Nilai Saham PT WIKA") + ggtitle("WIKA")
    p2 <- autoplot(ts(data$Waskita)) + ylab ("Nilai Saham PT Waskita") + ggtitle("Waskita")
    grid.arrange(p1,p2,ncol=2)
  })
  
  output$forplot <- renderPlot({
    data <- read.csv("D:/Punya Ayuk/campus life must go on/hell 5/EVD/Harga Saham WIKA Waskita.csv", header=TRUE)
    if(input$saham == "WIKA" && input$metode == "ARIMA(0,3,1") {
      mod <- arima(data$WIKA, order=c(0,3,1), include.mean = F) #include mean (F=ada differencing) (T=tidak ada differencing)
    } else if (input$saham == "WIKA" && input$metode == "ARIMA(1,3,1)") {
      mod <- arima(data$WIKA, order=c(1,3,1), include.mean = F)
    } else if (input$saham == "Waskita" && input$metode == "ARIMA(0,2,1)") {
      mod <- arima(data$Waskita, order=c(0,2,1), include.mean = F)
    } else if (input$saham == "Waskita" && input$metode == "ARIMA(1,2,1)") {
      mod <- arima(data$Waskita, order=c(1,2,1), include.mean = F)
    } else {
      mod <- arima(data$Waskita, order=c(2,2,1), include.mean = F)
    }
    ramal <- forecast(mod, h=10)
    p3 <- autoplot(ramal) + ylab("Nilai Saham")+ggtitle("Peramalan 10 hari ke depan")
    p3
  })
  
  output$text <- renderPrint({
    data <- read.csv("D:/Punya Ayuk/campus life must go on/hell 5/EVD/Harga Saham WIKA Waskita.csv", header=TRUE)
    if (input$saham == "WIKA" && input$metode =="ARIMA(0,3,1)"){
      mod <- arima(data$WIKA, order=c(0,3,1), include.mean = F)
    } else if (input$saham =="WIKA" && input$metode =="ARIMA(1,3,1)"){
      mod <- print("Bukan Model Terpilih")
    } else if (input$saham =="Waskita" && input$metode =="ARIMA(1,2,1)"){
      mod <- arima(data$Waskita, order=c(1,2,1), include.mean = F)
    } else if (input$saham =="Waskita" && input$metode =="ARIMA(0,2,1)"){
      mod <- print("Bukan Model Terpilih")
    } else {
      mod <- print("Bukan model terpilih")
    }
    model <- mod
    model
  })
  
  output$table <- renderDataTable({
    data <- read.csv("D:/Punya Ayuk/campus life must go on/hell 5/EVD/Harga Saham WIKA Waskita.csv", header=TRUE)
    if(input$saham == "WIKA" && input$metode == "ARIMA(0,3,1") {
      mod <- arima(data$WIKA, order=c(0,3,1), include.mean = F) #include mean (F=ada differencing) (T=tidak ada differencing)
    } else if (input$saham == "WIKA" && input$metode == "ARIMA(1,3,1)") {
      mod <- arima(data$WIKA, order=c(1,3,1), include.mean = F)
    } else if (input$saham == "Waskita" && input$metode == "ARIMA(0,2,1)") {
      mod <- arima(data$Waskita, order=c(0,2,1), include.mean = F)
    } else if (input$saham == "Microsoft" && input$metode == "ARIMA(1,2,1)") {
      mod <- arima(data$Waskita, order=c(0,2,1), include.mean = F)
    } else {
      mod <- arima(data$WIKA, order=c(2,3,1), include.mean = F)
    }
    fore1<-forecast(mod, h =10)
    fore1<-as.data.frame(fore1)
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)