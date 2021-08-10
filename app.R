library(shiny)
library(shinythemes)
library(clock)
library(DT)
library(plotly)
library(wordcloud)
library(ggwordcloud)
library(sf)
library(tmap)
library(tidytext)
library(tidyverse)

# MC1 Prep








# MC1 Q1








# MC1 Q2








# MC1 Q3








# MC3 Prep

df_1 = read_csv("data/csv-1700-1830.csv")
df_2 = read_csv("data/csv-1831-2000.csv")
df_3 = read_csv("data/csv-2001-2131.csv")
df = bind_rows(df_1,df_2,df_3)
df$`date(yyyyMMddHHmmss)` <- as.character(df$`date(yyyyMMddHHmmss)`)
df$`date(yyyyMMddHHmmss)` <- date_time_parse(df$`date(yyyyMMddHHmmss)`, zone = "",
                                             format = "%Y%m%d%H%M%S")
rm(df_1,df_2,df_3)

df <- df %>%
  rename(dt = `date(yyyyMMddHHmmss)`) %>%
  mutate(hour = get_hour(dt), min = get_minute(dt)) %>%
  rowid_to_column("ID")

# MC3 Q1

author <- df %>%
  count(author) %>%
  select(author)

set.seed(2021)

js_author = c("KronosQuoth", "Clevvah4Evah")
df_rmjs <- df %>%
  filter(!author %in% js_author)

stopwords = c("rt")
df_tidyclean <- df_rmjs %>%
  mutate(message = iconv(message,"UTF-8","UTF-8","")) %>%
  unnest_tokens(word, message, token = "tweets") %>%
  filter(str_detect(word, "[a-zA-Z']$"), str_detect(word, "@", negate = T),
         !word %in% stop_words$word, !word %in% stopwords)

df_tc_wordcloud <- df_tidyclean %>%
  count(word, sort = T)

df_tidyclean_tfidf <- df_tidyclean %>%
    group_by(word) %>%
    mutate(total_count = n()) %>%
    ungroup() %>%
    filter(total_count > 2) %>%
    select(-total_count) %>%
    count(hour, word) %>%
    bind_tf_idf(word, hour, n) %>%
    arrange(desc(tf_idf))

df_tidyclean_bigram <- df_rmjs %>%
  filter(type == "mbdata") %>%
  mutate(message = gsub("@\\w+[-\\w+]*", "", message)) %>% # remove author
  unnest_tokens(bigram, message, token = "ngrams", n = 2) %>%
  filter(bigram != 'NA') %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word, !word1 %in% stopwords) %>%
  filter(!word2 %in% stop_words$word, !word2 %in% stopwords) %>%
  unite(bigram, word1:word2, sep = " ")

df_tidyclean_bigram_tfidf <- df_tidyclean_bigram %>%
  group_by(bigram) %>%
  mutate(total_count = n()) %>%
  ungroup() %>%
  filter(total_count > 2) %>%
  select(-total_count) %>%
  count(hour, bigram) %>%
  bind_tf_idf(bigram, hour, n) %>%
  arrange(desc(tf_idf))

# MC3 Q2

df_t <- df %>%
  filter(type == "ccdata") %>%
  filter(str_length(message) > 30) %>%
  mutate(hour_15min = date_time_build(2014,01,23,hour,min%/%15*15, zone = "")) %>%
  group_by(hour_15min) %>%
  summarise(count = n(), combined_text = paste0(message, collapse = " | ")) %>%
  ungroup

d <- highlight_key(df_t)
p <- d %>% ggplot(aes(x = hour_15min, y = count)) + geom_col()

gg <- highlight(ggplotly(p), "plotly_click")

df_tidyclean_afinn <- df_tidyclean %>%
  filter(type == "mbdata") %>%
  left_join(get_sentiments("afinn")) %>%
  replace_na(list(value = 0)) %>%
  group_by(hour,min) %>%
  summarise(net_sentiment = sum(value)) %>%
  ungroup

df_tidyclean_nrc <- df_tidyclean %>% 
  left_join(get_sentiments("nrc")) %>%
  drop_na(sentiment) %>%
  filter(sentiment == "fear" | sentiment == "surprise") %>%
  group_by(type,hour,min,sentiment) %>%
  summarise(count = n()) %>%
  ungroup 

# MC3 Q3

Abila_st <- st_read(dsn = "data/Geospatial",
                    layer = "Abila")

Abila_st_lines <- Abila_st %>%
  filter(sapply(geometry, length) > 2)

df_ccdata_1str <- df %>%
  filter(type == "ccdata") %>%
  separate(location, c("number", "street"), sep = "(?<=[0-9]) ", convert = T) %>%
  drop_na(street) %>%
  mutate(number = as.integer(number)) %>%
  mutate(number = floor(number / 100) * 100)

Abila_1str <- Abila_st %>%
  mutate(From = pmin(FRADDL,FRADDR)) %>%
  mutate(From = floor(From / 100) * 100) %>%
  unite(Part1, FEDIRP:FENAME, sep = ". ", remove = F, na.rm = T) %>%
  unite(FullName, c(Part1, FETYPE), sep = " ") %>%
  select(-FEDIRP:-TOADDR) %>%
  mutate(geometry = st_centroid(geometry))

df_ccdata_1str_sf <- Abila_1str %>%
  inner_join(df_ccdata_1str, by = c("From" = "number", "FullName" = "street")) %>%
  group_by(ID) %>%
  top_n(1,TLID) # to clear some duplicate messages mapped to multiple TLIDs

df_unmap_labeled <- read_csv("data/df_unmap_labeled.csv")
df_ccdata_Xstr <- df %>%
  filter(type == "ccdata") %>%
  left_join(df_unmap_labeled, by = "location") %>%
  drop_na(longitude.y) %>%
  select(-longitude.x,-latitude.x)

df_ccdata_Xstr_sf <- st_as_sf(df_ccdata_Xstr, 
                              coords = c("longitude.y", "latitude.y"),
                              crs= 4326)

df_ccdata_Xstr_gps_point <- df_ccdata_Xstr_sf %>%
  mutate(message = iconv(message,"UTF-8","UTF-8","")) %>%
  group_by(ID, message, dt, hour, min) %>%
  summarize(do_union=FALSE) %>%
  ungroup %>%
  st_cast("POINT")

df_mbdata_gps <- df %>%
  filter(type == "mbdata") %>%
  drop_na(latitude,longitude)

df_mbdata_gps_sf <- st_as_sf(df_mbdata_gps, 
                             coords = c("longitude", "latitude"),
                             crs= 4326)

df_mbdata_gps_point <- df_mbdata_gps_sf %>%
  mutate(message = iconv(message,"UTF-8","UTF-8","")) %>%
  group_by(ID, author, message, dt, hour, min) %>%
  summarize(do_union=FALSE) %>%
  ungroup %>%
  st_cast("POINT")

df_mbdata_sentiment <- df_rmjs %>%
  filter(type == "mbdata") %>%
  mutate(message = iconv(message,"UTF-8","UTF-8","")) %>%
  unnest_tokens(word, message, token = "tweets") %>%
  filter(str_detect(word, "[a-zA-Z']$"), str_detect(word, "@", negate = T)) %>%
  left_join(get_sentiments("afinn")) %>% left_join(get_sentiments("nrc")) %>%
  replace_na(list(value = 0, sentiment = 0)) %>%
  mutate(fear_surp = ifelse(sentiment == "fear" | sentiment == "surprise",1,0)) %>%
  group_by(ID) %>%
  summarise(net_sentiment = sum(value), sum_fear_surp = sum(fear_surp)) %>%
  ungroup

df_mbdata_gps_point_sentiment <- df_mbdata_gps_point %>%
  inner_join(df_mbdata_sentiment) %>%
  filter(net_sentiment < 0 | sum_fear_surp > 0)

# UI

ui <- navbarPage("Team 15 Project", theme = shinytheme("sandstone"),
                 tabPanel("Introduction",
                          titlePanel("Introduction to Team 15's Shiny App"),
                          fluidRow(
                            column(8,"This Shiny App is built by Team 15 of SMU AY2020-2021 Sem 3
                                       ISSS608. The project tackles the VAST2021 Challenge, focusing
                                       on Mini-Challenges 1 and 3.")
                          )
                 ),
                 navbarMenu("Mini-Challenge 1 Q1",
                            tabPanel("Part 1",
                            ),
                            tabPanel("Part 2"),
                            tabPanel("Part 3")
                 ),
                 navbarMenu("Mini-Challenge 1 Q2",
                            tabPanel("Part 1",
                            ),
                            tabPanel("Part 2"),
                            tabPanel("Part 3")
                 ),
                 navbarMenu("Mini-Challenge 1 Q3",
                            tabPanel("Part 1",
                            ),
                            tabPanel("Part 2"),
                            tabPanel("Part 3")
                 ),
                 navbarMenu("Mini-Challenge 3 Q1",
                            tabPanel("Part 1",
                                     titlePanel("Exploring the Authors"),
                                     sidebarLayout(
                                       sidebarPanel(
                                         plotOutput("AuthorCount"),
                                         br(),
                                         selectInput(inputId = "author_chosen",
                                                     label = "Choose author to display messages: ",
                                                     choices = author,
                                                     selected = "KronosQuoth")
                                       ),
                                       mainPanel(
                                         DT::dataTableOutput(outputId = "AuthorMessages")
                                       )
                                     )
                            ),
                            tabPanel("Part 2",
                                     titlePanel("Exploring the Topics - Wordcloud"),
                                     sidebarLayout(
                                       sidebarPanel(
                                           sliderInput(inputId = "min_wordcount",
                                                       label = "Minimum Wordcount for Inclusion:",
                                                       min = 4, max = 40, value = 10),
                                           sliderInput(inputId = "wc_size",
                                                       label = "Wordcloud Size:",
                                                       min = 6, max = 24, value = 12),
                                           radioButtons(inputId = "wordcloud_type",
                                                        label = "Wordcloud Type",
                                                        choices = c("Single","Multiple (by hour)"),
                                                        selected = "Single")
                                       ),
                                       mainPanel(
                                           plotOutput("Wordcloud")
                                       )
                                     )
                            ),
                            tabPanel("Part 3",
                                     titlePanel("Exploring the Topics - TF-IDF"),
                                     sidebarLayout(
                                       sidebarPanel(
                                           radioButtons(inputId = "tfidf_type",
                                                        label = "Type of TF-IDF Plot",
                                                        choices = c("Unigram (single word)",
                                                                    "Bigram (word pair)"),
                                                        selected = "Unigram (single word)"),
                                           sliderInput(inputId = "top_n",
                                                       label = "Filter for top N words by TF-IDF:",
                                                       min = 5, max = 20, value = 10),
                                           "Note: more than N words may appear because
                                           of equal tf-idf ranking",
                                           checkboxInput(inputId = "show_data1",
                                                         label = "Show data table",
                                                         value = F)
                                       ),
                                       mainPanel(
                                           plotOutput("Tfidf"), 
                                           dataTableOutput(outputId = "Tfidf_DT")
                                       )
                                     )
                            )
                 ),
                 navbarMenu("Mini-Challenge 3 Q2",
                            tabPanel("Part 1",
                                     titlePanel("Breakdown of Call Centre Data"),
                                     sidebarLayout(
                                       sidebarPanel(
                                         
                                       ),
                                       mainPanel(
                                         # may need to redesign due to crosstalk limitation
                                         # suggest tabsetPanel
                                         plotlyOutput("CCDataGraphTable")
                                       )
                                     )
                            ),
                            tabPanel("Part 2",
                                     titlePanel("Count of Messages over time"),
                                     sidebarLayout(
                                       sidebarPanel(
                                         # change intervals
                                       ),
                                       mainPanel(
                                         plotOutput("MessageCountByTime")
                                       )
                                     )
                            ),
                            tabPanel("Part 3",
                                     titlePanel("Sentiment (AFINN)"),
                                     sidebarLayout(
                                       sidebarPanel(
                                         
                                       ),
                                       mainPanel(
                                         tabsetPanel(
                                           tabPanel("Plot", plotOutput("SentimentAfinn"))
                                         )
                                       )
                                     )
                            ),
                            tabPanel("Part 4",
                                     titlePanel("Sentiment (NRC)"),
                                     sidebarLayout(
                                       sidebarPanel(
                                         
                                       ),
                                       mainPanel(
                                         tabsetPanel(
                                           tabPanel("Plot", plotOutput("SentimentNRC"))
                                         )
                                       )
                                     )
                            )
                 ),
                 tabPanel("Mini-Challenge 3 Q3",
                          titlePanel("Geomap of messages"),
                          sidebarLayout(
                            sidebarPanel(
                              # checklist for sentiment filter
                              # checklist for mbdata and/or ccdata
                              # filter by time
                            ),
                            mainPanel(
                              tmapOutput("data_GIS")
                            )
                          )
                 )
                 
)

server <- function(input, output) {
  
  output$AuthorCount <- renderPlot({
    df %>%
      filter(type == "mbdata") %>%
      group_by(hour, author) %>%
      summarise(n = n()) %>%
      top_n(10,n) %>%
      mutate(author = reorder_within(author,n,hour)) %>%
      ggplot(aes(x = n, y = author, fill = hour)) + geom_col(show.legend = F) +
      facet_wrap(~hour, scales = "free") + scale_y_reordered() +
      ggtitle("Count of messages from authors over time") +
      theme(plot.title = element_text(hjust = 0, face = "bold"))
  })
  
  output$AuthorMessages <- DT::renderDataTable({
    DT::datatable(
      df %>%
        filter(author %in% input$author_chosen) %>%
        select(dt, author, message)
    ) %>% formatDate(1, "toLocaleString")
  })
  
  # output$Wordcloud_single <- renderPlot({
  #   wordcloud(df_tc_wordcloud$word, df_tc_wordcloud$n, max.words = 100)
  # })

  
  output$Wordcloud <- renderPlot({
      
      if(input$wordcloud_type == "Single"){
          df_tidyclean %>%
              count(word, sort = T) %>%
              filter(n > input$min_wordcount) %>%
              ggplot(aes(label = word, size = n)) + geom_text_wordcloud(rm_outside = T) +
              scale_size_area(max_size = input$wc_size)
      }else{
          df_tidyclean %>%
              count(hour, word, sort = T) %>%
              filter(n > input$min_wordcount) %>%
              ggplot(aes(label = word, size = n)) + geom_text_wordcloud(rm_outside = T) +
              scale_size_area(max_size = input$wc_size) +
              facet_wrap(~hour)
      }
      
  })
  
  
  output$Tfidf <- renderPlot({
      
      if(input$tfidf_type == "Unigram (single word)"){
          df_tidyclean_tfidf %>%
              group_by(hour) %>%
              slice_max(tf_idf, 
                        n = input$top_n) %>%
              ungroup() %>%
              mutate(word = reorder_within(word, tf_idf, hour)) %>%
              ggplot(aes(tf_idf, word, fill = hour)) +
              geom_col(show.legend = FALSE) +
              facet_wrap(~ hour, scales = "free") + scale_y_reordered() + xlab("tf-idf")
      }else{
          df_tidyclean_bigram_tfidf %>%
              group_by(hour) %>%
              slice_max(tf_idf, 
                        n = input$top_n) %>% 
              ungroup() %>%
              mutate(bigram = reorder_within(bigram, tf_idf, hour)) %>%
              ggplot(aes(tf_idf, bigram, fill = hour)) +
              geom_col(show.legend = FALSE) +
              facet_wrap(~ hour, scales = "free") + scale_y_reordered() + xlab("tf-idf")
      }
  })
  
  output$Tfidf_DT <- DT::renderDataTable({
      if(input$show_data1){
          df_rmjs %>%
              mutate(message = iconv(message,"UTF-8","UTF-8","")) %>%
              select(dt, message, author) %>%
              datatable(options = list(search = list(search = "alexandrias ithakis"))) %>%
              formatDate(1, "toLocaleString")
      }
  })
  
  # output$CCDataGraphTable <- renderPlotly({
  #     crosstalk::bscols(gg, DT::datatable(d) %>% formatDate(1, "toLocaleString"), widths = 5)
  # })
  
  output$MessageCountByTime <- renderPlot({
    df_rmjs %>%
      group_by(type,hour,min, .drop = F) %>%
      summarise(count = n()) %>%
      ungroup %>%
      mutate(hour_min = date_time_build(2014,01,23,hour,min, zone = "")) %>%
      ggplot(aes(x = hour_min, y = count, fill = type)) + geom_col() +
      facet_wrap(~type, nrow = 2, scales = "free_y") +
      xlab("time") + ylab("count of messages")
  })
  
  output$SentimentAfinn <- renderPlot({
    df_tidyclean_afinn %>%
      mutate(hour_min = date_time_build(2014,01,23,hour,min, zone = "")) %>%
      ggplot(aes(x = hour_min, y = net_sentiment)) + geom_col()
  })
  
  output$SentimentNRC <- renderPlot({
    df_tidyclean_nrc %>%
      mutate(hour_min = date_time_build(2014,01,23,hour,min, zone = "")) %>%
      ggplot(aes(x = hour_min, y = count, fill = sentiment)) + geom_col() +
      facet_wrap(~sentiment, nrow = 2)  
  })
  
  output$data_GIS <- renderTmap({
    tmap_mode("plot")
    tm_shape(Abila_st_lines) + tm_lines() +
      tm_shape(df_ccdata_1str_sf %>% filter(str_length(message)>30)) +
      tm_bubbles("hour", size = 0.2, border.col = "darkgrey", jitter = 0.7, alpha = 0.7,
                 palette="BuGn", popup.vars = c("dt","message")) +
      tm_shape(df_ccdata_Xstr_gps_point) +
      tm_bubbles("hour", size = 0.2, border.col = "darkgrey", jitter = 0.7, alpha = 0.7,
                 palette="BuGn", legend.col.show = F, popup.vars = c("dt","message")) +
      tm_shape(df_mbdata_gps_point_sentiment) +
      tm_bubbles("hour", size = 0.2, border.col = "darkgrey", jitter = 1.2, alpha = 0.7,
                 popup.vars = c("dt","author","message"))
  })
}

shinyApp(ui = ui, server = server)
