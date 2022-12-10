
library(shiny)
require(devtools)
library(lubridate)
library(leaflet)
library(highcharter)
library(rCharts)
library(quanteda)
library(quanteda.textplots)
library(RColorBrewer)
# UI

ui <- fluidPage(
  
  # Application title
  titlePanel("Is the World Bracing for a Recession?"),
  p(
    class = "text-primary",
    paste("Rhowena Vespa"
          
    ) 
  ),
  p(
    class = "text-black",
    paste("Using twitter data, this app helps to analyze the topic **recession** using Sentiment Analysis, Wordcloud and Geomapping.
          The corpus included 41,751 tweets from December 2, 2022 until December 8, 2022. Sentiment analysis was conducted using NRC Emotion Lexicon dictionary.
          In generating a wordcloud, the corpus was preprocessed and cleaned by removing stopwords, punctuations, numbers, symbols and the keyword:recession.
          Geographic Information Mapping system using Esri was utilized in generating an interactive geomap of tweets worldwide."
          
    )
  ),
    
  p(
    class = "text-black",
    paste("The sentiment analysis revealed the emotion FEAR having the highest score among all other emotions.
          The sentiment results on December 7th are very interesting, whereby SURPRISE AND FEAR scored the highest
          and all other emotions scored equally. Upon research, Reuters.com reported that December 7th marked the 5th 
          day Wallstreet markets were down and recession sentiments support this news story."
          
    )
    
  ),
  
  
  sidebarLayout(
    sidebarPanel(
      helpText(h3("Sentiment Analysis", style = "font-family: 'arial'; font-si14pt")),
                 p(
                   class = "text-black",
                   paste("The sentiment analysis revealed the emotion FEAR having the highest score among all other emotions. Check out emotions on December 7th."
                   )   
                 ),
      
      
      checkboxGroupInput("type", 
                         label = (helpText(h5("Click on All Emotion Types",))),
                         
                         choices = list("anger" = "anger", 
                                        "anticipation" = "anticipation", 
                                        "disgust" = "disgust", 
                                        "fear" = "fear",
                                        "joy" = "joy",
                                        "sadness" = "sadness",
                                        "surprise"="surprise",
                                        "trust"="trust"),
                         selected = "fear"),
      helpText(h3("Wordcloud", style = "font-family: 'arial'; font-si14pt")),                   
      p(
        class = "text-black",
        paste("Adjust the widget to reveal prominent words based on retweets. Observe that 
              words **bank** and **economy** came up at all retweet counts. As the 
              retweet counts increase, more negative words like **deny**, **bad**, **disastrous**,
              become prominent in the wordcloud"
        )
      ),                   
      
      
      sliderInput("slider1", h5("Wordcloud at Different Retweet Counts"),
                  min = 0, max = 100, value = 1),
      
      helpText(h3("GeoMap", style = "font-family: 'arial'; font-si14pt")),  
      p(
        class = "text-black",
        paste("Adjust the widget to Reveal Tweet Location based on Retweet Counts."
        )
      ),
      
      sliderInput("slider2", h5("Geomap at Different Retweet Counts"),
                  min = 0, max = 60, value = 1),
      
      p(
        class = "text-black",
        paste("Adjust the widget and zoom to filter other continents such as Africa, North America, Asia, Europe, Australia."
        )
      )
      
     , width = 4),
    
    
    mainPanel(
      highchartOutput("sent_chart", width = "100%", height = "400px"),
      
      plotOutput("wordcloud",width = "100%", height = "400px"),
      
      leafletOutput("mymap",width = "100%", height = "400px"),
      p(class = "text-muted",paste("GIS map of Twitter Data on Recession")
      )
    )
  )
)

# SERVER 
server <- function(input, output) {
  
  senti_data <- read.csv("rec_sent_aggreg.csv")
  
  geocodes <- read.csv("RecGeo.csv", header = TRUE)
  
  senti_data$date <- as.Date(senti_data$date_label)
  
  output$sent_chart <- renderHighchart({
    
    highchart() %>%
      hc_add_series(data = senti_data[senti_data$variable %in% input$type,],"line", hcaes(x = date, y = value, group=variable)) %>%
      hc_xAxis(type = "datetime")
  }) 
  
  recession <- read.csv("recession.csv")
  recession$text <- as.character(recession$text)
  
  output$wordcloud <- renderPlot({
    dfm <- dfm(recession[recession$retweet_count >= input$slider1,]$text, remove = c(stopwords("english"), remove = c("recession", "rt","&","amp"), remove_numbers = TRUE, remove_symbols = TRUE, remove_punct = TRUE))
    dfm <- dfm_select(dfm)
    set.seed(100)
    textplot_wordcloud(dfm, scale=c(8,2),random.order=FALSE, rot.per=0.5, use.r.layout=FALSE,min_size = 0.1, min_count = 10, max_words = 100,color = c("blue", "yellow","red", "green","orange","purple"))
  })
  output$mymap <- renderLeaflet({
    
    usericon <- makeIcon(
      iconUrl = geocodes$source,
      iconWidth = 5, iconHeight = 5
    )
    
    
    leaflet(data = geocodes[geocodes$retweet_count >= input$slider2,]) %>% 
      addTiles() %>%
      setView(lng = 8.6753, lat = 9.0820, zoom = 1) %>% 
      addMarkers(lng = ~lng, lat = ~lat,popup = ~ as.character(text)) %>% 
      addProviderTiles(providers$Esri.WorldStreetMap) %>%  #more layers:http://leaflet-extras.github.io/leaflet-providers/preview/
      addCircleMarkers(
        stroke = FALSE, fillOpacity = 0.5)
  })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

