library(tidyverse)
library(plotly)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(cowplot)
library(tidytext)
library(textdata)
library(wordcloud)
library(lubridate)
library(shiny)
library(RColorBrewer)
library(fmsb)
library(radarchart)
library(thematic)
library(bslib)
library(ggridges)
library(sentimentr)
library(magrittr)
library(fontawesome)

load("new.RData")
load("nrc.RData")
new <- new[new$date != "2014-10-09",]

token <- new %>% 
  unnest_tokens(word, tweet)

data("stop_words")
tidy_token <- token %>% 
  anti_join(stop_words) %>%                            
  mutate(word = iconv(word, from = "UTF-8", to = "ASCII")) %>% 
  na.omit() %>%                                        
  filter(nchar(word) > 3,                              
         !(word %in% c("https", "t.co")))

emotions <- token %>% 
  inner_join(nrc) 

tweet_bigrams <- new %>%
  unnest_tokens(bigram, tweet, token = "ngrams", n = 2)

bigrams_separated <- tweet_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word1 %in% c("https", "t.co")) %>%
  filter(!word2 %in% c("https", "t.co"))

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigram_tf_idf <- bigrams_united %>%
  mutate(year = year(date), month = month.name[month(date)]) %>%
  count(year, bigram) %>%
  bind_tf_idf(bigram, year, n) %>%
  select(year, bigram, tf_idf, bigram_count = n) %>%
  arrange(desc(bigram_count))


df <- emotions[!grepl("positive", emotions$sentiment),]
df <- df[!grepl("negative", df$sentiment),]

new2 <- new %>% 
  get_sentences(new$tweet)

sent <- new2 %>%
  inner_join(sentiment(new2)) %>%
  mutate(year = year(date), month = month.name[month(date)]) %>%
  group_by(year, month) %>%
  filter(!is.na(month))


# Shiny App
light <- bs_theme(primary = "#CC6600", 
                  base_font = font_google("Tajawal"),
                  heading_font = font_google("Tajawal"))

dark <- bs_theme(bg = "#1A1A1C", 
                 fg = "#DFE3EB", 
                 primary = "#CC6600", 
                 secondary = "#00DAC6",
                 success = "#4F9B29",
                 info = "#28B3ED",
                 warning = "#FD7424",
                 danger = "#F7367E",
                 base_font = font_google("Tajawal"),
                 heading_font = font_google("Tajawal"))

ui <- fluidPage(
  
  titlePanel("Exploring Donald Trump's Twitter attacks"),
  
  theme = light, 
  div(
    class = "custom-control custom-switch", 
    tags$input(
      id = "dark_mode", type = "checkbox", class = "custom-control-input",
      onclick = HTML("Shiny.setInputValue('dark_mode', document.getElementById('dark_mode').value);")),
    tags$label(
      "Dark mode", `for` = "dark_mode", class = "custom-control-label")),
  
  fluidRow(
    column(2,
           div(HTML("<em>The plots and table all react to the year input below. Select one or more years.</em>")),
           checkboxGroupInput("yr", h5("Select year(s)"),
                              choices = list("2015" = "2015", 
                                             "2016" = "2016",
                                             "2017" = "2017",
                                             "2018" = "2018",
                                             "2019" = "2019",
                                             "2020" = "2020",
                                             "2021"= "2021"),
                              selected = c("2015")), class = "p-3 border rounded"),
    column(10,
           tabsetPanel(
             type = "pills",
             
             tabPanel("Introduction", icon = icon("chalkboard-teacher"),
                      tags$img(src = "https://i.guim.co.uk/img/media/3dec7b94c0c963b5210f7396594c372877c2c8d5/0_0_5760_3456/master/5760.jpg?width=620&quality=45&auto=format&fit=max&dpr=2&s=5a88451d66e42b4ebb79b171bd6ee36e", 
                               width="600px", height="300px"),
                      div(HTML("<em>Photograph: Andre M Chang/ZUMA Wire/Rex/Shutterstock</em>")),
                      p(""),
                      p("On January 8, 2021, Twitter suspended the account of former U.S President, Donald J. Trump for allegedly inciting violence and breaking their rules. 
                        This ban received a lot of mixed reviews from both Twitter and non-Twitter users all over the world."),
                      p("The purpose of this shiny application is to analyze all purported negative tweets by Donald Trump from when he announced his candidacy in 2015 to 2021 when he was banned from Twitter. 
                        This project was inspired by this", a("New York Times article", href = "https://www.nytimes.com/interactive/2021/01/19/upshot/trump-complete-insult-list.html"),
                        "and the dataset used was gotten from ", a("here.", href = "https://www.kaggle.com/ayushggarg/all-trumps-twitter-insults-20152021")), 
                      p("I created this app for my R/Shiny course and this is my first shiny project. 
                        Feel free to connect with me via any of the channels below and I hope you enjoy using this aplication. :)"),
                      tags$li(class="dropdown",tags$a(href="https://www.linkedin.com/in/ijeoma-uhuegbulem/" ,icon("linkedin"), "LinkedIn", target="_blank")),
                      tags$li(class="dropdown",tags$a(href="https://github.com/iuhuegbulem" ,icon("github"), "Github", target="_blank")),
                      class = "p-3 border rounded"),
             
             tabPanel("Exploratory analysis", icon = icon("search"),
                      tags$b("Top 25 frequent targets and insults"),
                      fluidRow(
                        column(6, plotOutput('pt')), 
                        column(6, plotOutput('pt2'))),
                      tags$br(),
                      tags$b("Total tweet count per month"),
                      div(HTML("<em>(Hover around the graph for more info)</em>")),
                      plotlyOutput('pt3'),
                      class = "p-3 border rounded"),
             
             tabPanel("Text analysis", icon = icon("text-height"),
                      tags$b("Wordcloud of frequent words"),
                      fluidRow(
                        column(8, plotOutput('wc', width = "500px", height = "500px")),
                        column(4, sliderInput("freq", h5("Frequency"),         
                                              min = 10, max = 400, 
                                              value = c(10, 400), step = 10, sep = ""),
                               sliderInput("max",
                                           h5("Maximum Number of Words"),
                                           min = 20,  max = 400,  
                                           value = c(20, 400), step = 10, sep = ""))),
                      tags$b("Top bigrams, count and Term Frequency — Inverse Document Frequency scores"),
                      div(HTML("<em>(Hover around the bubbles for more info)</em>")),
                      plotlyOutput('bigram'),
                      class = "p-3 border rounded"),
             
             tabPanel("Sentiment analysis", icon = icon("smile"),
                      div(HTML("<em>The goal of this analysis is to determine if
                               Artificial Intelligence corroborates real human sentiments.</em>")),
                      p(""),
                      tags$b("Average sentiment score using SentimentR"),
                      plotOutput('sa'),
                      tags$br(),
                      tags$b("Yearly sentiments using NRC lexicon"),
                      div(HTML("<em>(Hover around the points on the chart for more info)</em>")),
                      tags$br(),
                      chartJSRadarOutput('sa2'),
                      class = "p-3 border rounded"),
             
             tabPanel("Table", icon = icon("table"),
                      h2("The data"),
                      p("This table shows the full text contained in the dataset. You can search for keywords, filter and sort through the tweets. Have fun!"),
                      dataTableOutput("tweet_table"),
                      class = "p-3 border rounded")
           ))
  ))

server <- function(input, output, session) {
  
  observe({
    session$setCurrentTheme(
      if (isTRUE(input$dark_mode)) dark else light
    )
  })
  
  thematic::thematic_shiny()
  
  new2 <- reactive({
    new %>%
      filter(year(date) %in% as.numeric(input$yr))
  })
  
  output$tweet_table <- renderDataTable(new2(), options = list(pageLength = 10))
  
  wordcloud_rep <- repeatable(wordcloud)
  output$wc <- renderPlot({
    cloud_data <- tidy_token %>%
      filter(year(date) %in% as.numeric(input$yr)) %>%
      count(word)
    wordcloud(words = cloud_data$word, 
              freq = cloud_data$n, min.freq = input$freq,
              max.words = input$max, random.order = FALSE,
              rot.per=0.35, 
              colors=brewer.pal(8, "Dark2"))
  }) 
  
  output$bigram <- renderPlotly({
    bigram_tf_idf %>%
      filter(year %in% as.numeric(input$yr)) %>%
      transform(year = as.character(year)) %>%
      head(25) %>%
      ggplot(aes(x=tf_idf, y=bigram, size = bigram_count, fill = year)) +
      geom_point(alpha=0.7, show.legend = FALSE) + 
      scale_fill_brewer(name="Year", palette="Paired") +
      theme(text = element_text(size=10), axis.line = element_line(colour = "black")) + 
      labs(x = "tf-idf score",
           y = "bigram", size = "bigram count")
  }) 
  
  output$pt <- renderPlot({
    new %>%
      filter(year(date) %in% as.numeric(input$yr)) %>%
      mutate(length = nchar(target)) %>%
      count(target, sort = TRUE) %>%
      head(25) %>%                       
      ggplot(aes(x = reorder(target,n), y = n)) + 
      geom_col(fill = "cornflowerblue", color = "black") + 
      theme(text = element_text(size=10), axis.line = element_line(colour = "black")) + 
      coord_flip() + 
      xlab("Most frequent targets") + ylab("No of tweets")
  })
  
  output$pt2 <- renderPlot ({
    tidy_token %>%
      filter(year(date) %in% as.numeric(input$yr)) %>%
      count(insult, sort = TRUE) %>%         
      head(25) %>%                        
      ggplot(aes(x = reorder(insult,n), y = n)) + 
      geom_col(fill = "cyan3", color = "black") + 
      theme(text = element_text(size=10), axis.line = element_line(colour = "black")) + 
      coord_flip() + xlab("Most frequent insults") + ylab("No.of tweets")
  })
  
  output$pt3 <- renderPlotly ({
    new %>%
      filter(year(date) %in% as.numeric(input$yr)) %>%
      mutate(year = year(date), month = month.name[month(date)]) %>%
      group_by(year, month) %>%
      count(year, month) %>%
      select(year, month, tweet_count = n) %>%
      transform(month = factor(month, levels=month.name, ordered = T)) %>%
      transform(year = as.character(year)) %>%
      filter(!is.na(month)) %>%
      ggplot(aes(x=month, y=tweet_count, fill = year)) +
      geom_col(show.legend = FALSE) + 
      scale_fill_brewer(name="Year", palette="Paired") +
      theme(text = element_text(size=10), axis.text.x=element_text(angle=45,hjust=1,vjust=0.5), axis.line = element_line(colour = "black")) + 
      labs(x = "Month", y = "Tweet count per month")
    
  })
  
  output$sa2 <- renderChartJSRadar({
    yrc <- emotions %>%
      mutate(year = year(date)) %>%
      group_by(year, sentiment) %>%
      count(year, sentiment) %>%
      select(year, sentiment, sentiment_year_count = n)%>%
      spread(year, sentiment_year_count) 
    
    chartJSRadar(yrc, 
                 main = "(check boxes below to view specific years)",
                 scaleLineWidth=5,
                 showToolTipLabel = TRUE, responsive = logical)
  })
  
  
  output$sa <- renderPlot ({
    sent %>%
      filter(year %in% as.numeric(input$yr)) %>%
      ggplot() +
      geom_density_ridges(aes(
        x = mean(sentiment),
        y = as.character(year), 
        fill=as.factor(year)),
        rel_min_height = 0.01,
        alpha = 0.7,
        scale = 3) +
      theme(text = element_text(size=10), axis.text.x=element_text(angle=45,hjust=1,vjust=0.5)) + 
      facet_wrap(~factor(month, levels=month.name, ordered = T), scales='free') +
      labs(x = "Average SentimentR score",
           y = "Year") + 
      scale_fill_discrete(guide=FALSE)
  })
}

shinyApp(ui = ui, server = server)