library(shiny)
library(shinythemes)
library(clock)
library(DT)
library(plotly)
library(ggwordcloud)
library(sf)
library(tmap)
library(wordcloud2)
library(shinydashboard)

<<<<<<< HEAD
packages = c('tidyverse','tm','wordcloud','tidytext','ggwordcloud','udpipe','textplot','widyr','RColorBrewer','tidyr',
             'lubridate','tidygraph','ggraph')
for(p in packages){
  if(!require(p, character.only = T)){
    install.packages(p)
  }
  library(p, character.only = T)
}

=======
library(wordcloud)
library(tm)
library(udpipe)
library(textplot)
library(widyr)
library(RColorBrewer)
library(textnets)
library(textdata)
library(Matrix)
library(SnowballC)
library(reshape2)
library(igraph)
library(ggraph)
library(networkD3)
>>>>>>> 73ecf7d55d5bece3446e6a0777a514f58ea838f7
library(devtools)
library(tidytext)
library(tidyverse)

# packages = c('tidyverse','tm','wordcloud','tidytext','ggwordcloud','udpipe','textplot','widyr','RColorBrewer')
# for(p in packages){
#   if(!require(p, character.only = T)){
#     install.packages(p)
#   }
#   library(p, character.only = T)
# }

# library(devtools)
# install_github("cbail/textnets")
# packages = c('textnets','dplyr','Matrix','stringr','SnowballC','reshape2','igraph','ggraph','networkD3')
# for(p in packages){
#   if(!require(p, character.only = T)){
#     install.packages(p)
#   }
#   library(p, character.only = T)
# }


# MC1 Data Processing

#Read data from Excel
articles=read_csv("data/cleanArticles.csv")

#Data for dropdowns
newsgroup <- unique(articles$newsgroup)
newsgroup <- sort(newsgroup)
newsgroup1 <- unique(articles$newsgroup)
newsgroup1[length(newsgroup1)+1] <- "select a value below"
newsgroup1 <- sort(newsgroup1)
clusters=c(1,2,3,4,5,6)

#Preparing data for textnet
articles$Published=as.Date(articles$Published,format="%Y-%m-%d")
textNet_data=articles %>%
  group_by(newsgroup) %>%
  summarise_all(funs(toString(na.omit(.))))
textNet_data=textNet_data[,!(names(textNet_data) %in% c("id","Location","Published","Title"))]
news_text_data <- PrepText(textNet_data, textvar="Content", groupvar="newsgroup", node_type = "groups",
                           remove_stop_words=TRUE, remove_numbers=TRUE)
news_text_network <- CreateTextnet(news_text_data)

text_communities <- TextCommunities(news_text_network)
articles$cluster=text_communities$modularity_class[match(articles$newsgroup,text_communities$
                                                           group)]

#Preparing data for wordcloude and correlation graphs
usenet_words <- articles %>%
  unnest_tokens(word, Content) %>%
  filter(str_detect(word, "[a-z']$"),
         !word %in% stop_words$word)
words_by_newsgroup <- usenet_words %>%
  count(newsgroup, word, sort = TRUE) %>%
  ungroup()

newsgroup_cors <- words_by_newsgroup %>%
  pairwise_cor(newsgroup, 
               word, 
               n, 
               sort = TRUE)

#Newtwork Graph

edges <- read.csv("data/cleanEmail.csv")
nodes<- read.csv("data/cleanEmployee.csv")

#official emails
edges_official=edges %>% 
  filter(MainSubject == "Work related") %>%
  group_by(Source,Target,SentDate) %>%
  summarise(Weight=n()) %>%
  filter(Weight >1) %>%
  ungroup()
network_graphOfficial = tbl_graph(nodes=nodes, edges=edges_official,
                                  directed=TRUE  )

#unofficial emails
edges_unofficial=edges %>%
  filter(MainSubject == "Non-work related") %>%
  group_by(Source,Target,SentDate) %>%
  summarise(Weight=n()) %>%
  filter(Weight >1) %>%
  ungroup()
network_graphUnofficial = tbl_graph(nodes=nodes, edges=edges_unofficial,
                                    directed=TRUE  )
##dropdowns
graphNode=c('CitizenshipCountry','CurrentEmploymentType','Gender','CitizenshipCountry','CurrentEmploymentTitle')

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

# import downloaded CSV sentiment lexicon due to Shiny limitation
afinn_df = read_csv("data/sentiment_csv/afinn_df.csv")
nrc_df = read_csv("data/sentiment_csv/nrc_df.csv")

df_tidyclean_afinn <- df_tidyclean %>%
  filter(type == "mbdata") %>%
  left_join(afinn_df) %>%
  replace_na(list(value = 0)) %>%
  group_by(hour,min) %>%
  summarise(net_sentiment = sum(value)) %>%
  ungroup

df_tidyclean_nrc <- df_tidyclean %>%
  filter(type == "mbdata") %>%
  left_join(nrc_df) %>%
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
  left_join(afinn_df) %>% left_join(nrc_df) %>%
  replace_na(list(value = 0, sentiment = 0)) %>%
  mutate(fear_surp = ifelse(sentiment == "fear" | sentiment == "surprise",1,0)) %>%
  group_by(ID) %>%
  summarise(net_sentiment = sum(value), sum_fear_surp = sum(fear_surp)) %>%
  ungroup

df_mbdata_gps_point_sentiment <- df_mbdata_gps_point %>%
  inner_join(df_mbdata_sentiment) %>%
  filter(net_sentiment < 0 | sum_fear_surp > 0)

# UI

ui <- navbarPage("Group 15 Project", theme = shinytheme("sandstone"),
                 tabPanel("Introduction",
                          titlePanel("Introduction to Group 15's Shiny App"),
                          fluidRow(
                            column(8,"This Shiny App is built by Team 15 of SMU AY2020-2021 Sem 3
                                       ISSS608. The project tackles the VAST2021 Challenge, focusing
                                       on Mini-Challenges 1 and 3.")
                          )
                 ),
                 navbarMenu("History of GASTech",
                            tabPanel("Text Analysis",
                                     titlePanel(HTML("<center>Text Analysis of News Articles</center>")),
                                     sidebarLayout(
                                       sidebarPanel(
                                         conditionalPanel(condition="input.tabselected==1",h4("test"),
                                         radioButtons(
                                           inputId = "source",
                                           label = "Word source",
                                           choices = c(
                                             "Content" = "content",
                                             "Titles" = "title"
                                           )
                                         ),
                                         hr(),
                                         selectInput(inputId="newsgroup","Newsgroup",newsgroup, multiple = TRUE,selected=c("News Online Today", "The World","Centrum Sentinel")),
                                       ),
                                       conditionalPanel(condition="input.tabselected==2",
                                                        checkboxGroupInput("variable", "Variables to show:",
                                                                           c("Show 3D Visualization" = 1
                                                                             )),h5("Note: If the checkbox is selected, please scroll down to view the 3D visualization.") ,
                                       ),
                                       conditionalPanel(condition="input.tabselected==3",
<<<<<<< HEAD
                                                        selectInput(inputId="cluster","Cluster",choices=clusters,selected=1),
                                                        hr(),
                                                        numericInput("nval1", "Top most occur word pairs",
                                                                     value = 15, min = 1
                                                        ),
                                                        hr(),
                                                        checkboxInput("viewbigram", "View Bigram Plot for Selected Cluster", FALSE),
                                                        conditionalPanel(
                                                          condition = "input.viewbigram == 1",
                                                          hr(),
                                                          numericInput("nval2", "Count of Phrases",
                                                                       value = 10, min = 1
                                                          )
                                                        )
                                                        
                                                        
=======
                                                        selectInput(inputId="cluster","Cluster",choices=clusters,selected=1)
>>>>>>> 73ecf7d55d5bece3446e6a0777a514f58ea838f7
                                       ),
                                       conditionalPanel(condition="input.tabselected==4",
                                                        sliderInput(inputId="value","Correlation Range",min=round(min(newsgroup_cors$correlation),2),max=round(max(newsgroup_cors$correlation),2),value=c(0.84,0.92),sep="",animate=FALSE),
                                                        checkboxInput("option", "Compare Newsgroups with WordClound", FALSE),
                                                        conditionalPanel(
                                                          condition = "input.option == 1",
                                                          hr(),
                                                          selectInput(inputId="option1","Select the first newsgroup",choices=newsgroup,selected="The World")
                                                        ),
                                                        conditionalPanel(
                                                          condition = "input.option == 1",
                                                          numericInput("num", "Maximum number of words",
                                                                       value = 80, min = 5
                                                          )
                                                        ),
                                                        conditionalPanel(
                                                          condition = "input.option == 1",
                                                          hr(),
                                                          selectInput(inputId="option2","Select the second newsgroup",choices=newsgroup1,selected = "select a value below")
                                                        ),
                                                        conditionalPanel(
                                                          condition = "input.option == 1",
                                                          numericInput("num1", "Maximum number of words",
                                                                       value = 80, min = 5
                                                          )
                                                        )
                                       )
                                       ),

                                       mainPanel(
                                         tabsetPanel(type="tabs",id="tabselected",selected=1,
                                           tabPanel("Comparision Cloud",icon = icon("fas fa-cloud"), plotOutput("cloud",  width = "100%"),value=1),
                                           tabPanel("3D-Viz of Clustering",icon = icon("fas fa-cubes"), fluidRow(box(plotOutput("tn")), box(plotOutput("cluster"))),fluidRow((forceNetworkOutput("textnet"))),value=2),
<<<<<<< HEAD
                                           tabPanel("Text Plot",icon = icon("fas fa-align-left"), fluidRow(plotOutput("textplot",  width = "100%")),fluidRow(textOutput("text1"),plotOutput("bigram",  width = "100%")),value=3),
                                           tabPanel("Correlation Graph",icon = icon("fas fa-stream"), fluidRow(plotOutput("correlation",  width = "100%")), fluidRow(box(wordcloud2Output("wordcloudd1")), box(wordcloud2Output("wordcloudd2"))),value=4)
                                         ) 
=======
                                           tabPanel("Text Plot",icon = icon("fas fa-cloud"), plotOutput("textplot",  width = "100%"),value=3),
                                           tabPanel("Correlation Graph",icon = icon("fas fa-cloud"), fluidRow(plotOutput("correlation",  width = "100%")), fluidRow(box(wordcloud2Output("wordcloudd1")), box(wordcloud2Output("wordcloudd2"))),value=4)
                                         )
>>>>>>> 73ecf7d55d5bece3446e6a0777a514f58ea838f7
                                       )
                                     )
                            ),
<<<<<<< HEAD
                            tabPanel("Network Graph",
                                     titlePanel(HTML("<center>Network Graph of Employees of GasTech</center>")),
                                     sidebarLayout(
                                       sidebarPanel(
                                         conditionalPanel(condition="input.tabselected2==5",h4("Note: Please give it some time to load."),
                                                          radioButtons(
                                                            inputId = "workType",
                                                            label = "Select the type of Email Relationship",
                                                            choices = c(
                                                              "Work Related" = "work",
                                                              "Non-Work Related" = "nonwork"
                                                            )
                                                          ),
                                                          hr(),
                                                          selectInput(inputId="color","Nodes Type",choices=graphNode,selected='CurrentEmploymentType')
                                                          )
                                      
                                       ),
                                       
                                       mainPanel(
                                         tabsetPanel(type="tabs",id="tabselected2",selected=5,
                                                     tabPanel("Relationships",icon = icon("fas fa-handshake"), plotOutput("relation",  width = "100%"),value=5)
                                                   ) 
                                       )
                                     )  
=======
                            tabPanel("Network Graph"

>>>>>>> 73ecf7d55d5bece3446e6a0777a514f58ea838f7
                            )
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
                                                        selected = "Bigram (word pair)"),
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
                                     titlePanel("Investigating Call Centre reports over time"),
                                     sidebarLayout(
                                       sidebarPanel(
                                         selectInput(inputId = "time_int1",
                                                     label = "Choose time intervals",
                                                     choices = c("1 Min" = 1,
                                                                 "5 Min" = 5,
                                                                 "10 Min" = 10,
                                                                 "15 Min" = 15,
                                                                 "30 Min" = 30),
                                                     selected = 15),
                                         radioButtons(inputId = "msg_length",
                                                      label = "Select only long messages?",
                                                      choices = c("Yes" = 30,
                                                                  "No" = 0),
                                                      selected = 30),
                                         "Note: long messages tend to be more important incidents
                                         due to the need to report on more incident details.
                                         The length is set at >30 characters",
                                         checkboxInput(inputId = "show_data2",
                                                       label = "Show data table",
                                                       value = F)
                                       ),
                                       mainPanel(
                                           plotOutput("CCDataGraph"),
                                           dataTableOutput(outputId = "CCDataTable")
                                       )
                                     )
                            ),
                            tabPanel("Part 2",
                                     titlePanel("Investigating Microblog messages over time"),
                                     sidebarLayout(
                                       sidebarPanel(
                                         selectInput(inputId = "time_int2",
                                                label = "Choose time intervals",
                                                choices = c("1 Min" = 1,
                                                            "5 Min" = 5,
                                                            "10 Min" = 10,
                                                            "15 Min" = 15,
                                                            "30 Min" = 30),
                                                selected = 1),
                                         checkboxInput(inputId = "show_bigram",
                                                       label = "Show TF-IDF Bigram (from Q1)",
                                                       value = T),
                                         plotOutput("Tfidf_bigram_optional")
                                       ),
                                       mainPanel(
                                         tabsetPanel(
                                           tabPanel("Message Count", plotOutput("MessageCountByTime")),
                                           tabPanel("Unique Authors", plotOutput("UniqueAuthors")),
                                           tabPanel("Sentiment (AFINN)", plotOutput("SentimentAfinn")),
                                           tabPanel("Sentiment (NRC)", plotOutput("SentimentNRC"))
                                         )
                                       )
                                     )
                            )
                 ),
                 tabPanel("Mini-Challenge 3 Q3",
                          titlePanel("Geomap of messages with geo-located data"),
                          sidebarLayout(
                            sidebarPanel(
                              selectInput(inputId = "data_type",
                                          label = "Choose messages to include",
                                          choices = c("MB Data only" = 1,
                                                      "CC Data only" = 2,
                                                      "Both MB and CC Data" = 3),
                                          selected = 3),
                              checkboxInput(inputId = "filter_sentiment",
                                            label = "Filter only messages with negative sentiment",
                                            value = T),
                              sliderInput(inputId = "timeframe",
                                          label = "Timeframe:",
                                          min = as.POSIXct("2014-01-24 17:00"),
                                          max = as.POSIXct("2014-01-24 22:00"),
                                          value = c(as.POSIXct("2014-01-24 17:00"),
                                                    as.POSIXct("2014-01-24 22:00")),
                                          timeFormat = "%H:%M"
                                          ),
                              sliderInput(inputId = "jitter_level",
                                          label = "Amount of jitter:",
                                          min = 0, max = 2, value = 1, step = 0.05),
                              sliderInput(inputId = "alpha_level",
                                          label = "Alpha level (lower: more transparent)",
                                          min = 0, max = 1, value = 0.7),
                              sliderInput(inputId = "bubble_size",
                                          label = "Size of bubble",
                                          min = 0, max = 1, value = 0.2)
                            ),
                            mainPanel(
                              tmapOutput("data_GIS")
                            )
                          )
                 )
                 
)

server <- function(input, output) {
  ##history of Gastech server
  
  ####Text Analysis
  ###1)comaprision cloud
  data_source <- reactive({
    subdata <- subset(articles,articles$newsgroup %in% input$newsgroup)
    
    if (input$source == "title") {
      data=subdata %>%
        group_by(newsgroup) %>%
        summarise_all(funs(toString(na.omit(.))))
      data=data$Title
    }
    else if (input$source == "content") {
      data=subdata %>%
        group_by(newsgroup) %>%
        summarise_all(funs(toString(na.omit(.))))
      data=data$Content
    }
    return(data)
  })
  
  create_wordcloud <- function(data, newsgroup ) {
    #punctuation, digits, stopwords, single letters and white space  removal
    data=gsub(pattern="\\W",replace=" ",data)
    data=gsub(pattern="\\d",replace=" ",data)
    data=tolower(data)
    data=removeWords(data,stopwords("english"))
    data=gsub(pattern="\\b[A-z]\\b{1}",replace=" ",data)
    data=stripWhitespace(data)
    corpus=Corpus(VectorSource(data))
    tdm=TermDocumentMatrix(corpus)
    m=as.matrix(tdm)
    colnames(m)=newsgroup
    comparison.cloud(m,max.words = 100,random.order=FALSE,colors=brewer.pal(max(5,ncol(m)),"Dark2") ,title.size=1,
                     title.colors=NULL, match.colors=FALSE,
                     title.bg.colors="grey90")
  }
  
  output$cloud <- renderPlot({
    create_wordcloud(data_source(),
                     newsgroup = input$newsgroup
                     
    )
  }, height = 700, width = 800 
  )
  
  
  ###2) 3D Visualization
  output$textnet <- renderForceNetwork({
    req(input$variable)
    if(input$variable==1){
      VisTextNetD3(news_text_network, .30)}
  }
  )
  
  output$tn <- renderPlot({
    VisTextNet(news_text_network, .30, label_degree_cut=1)
  }
  )

  output$cluster <- renderPlot({
    ggplot(text_communities %>% filter(modularity_class %in% c(1,2,3,4,5,6)), 
           aes(label=group, 
               color=modularity_class)) +
      geom_text_wordcloud(eccentricity = 1) +
      scale_size_area(max_size = 15) +
      theme_minimal() +  
      ggtitle("Segmentation of Newsgroups into Clusters")+
      theme(plot.title = element_text(hjust = 0.5))+
      facet_wrap(~modularity_class)
  }
  )
  
  
  ###3) textplot
  output$textplot <- renderPlot({
    x <- subset(usenet_words, cluster == input$cluster)
    x <- cooccurrence(x, group = "id", term = "word")
    textplot_cooccurrence(x, top_n = input$nval1, subtitle = paste0("showing Cluster ",input$cluster))
  }
  )
  ###birgam
  output$bigram <- renderPlot({
    if(input$viewbigram==1){
    output$text1 <- renderText("BIGRAM PLOT:")
    sub_articles <- subset(articles,articles$cluster %in% input$cluster)
    bigrams <- sub_articles %>%
      unnest_tokens(bigram, 
                    Content, 
                    token = "ngrams", 
                    n = 2)
    bigrams_separated <- bigrams %>%
      filter(bigram != 'NA') %>%
      separate(bigram, c("word1", "word2"), 
               sep = " ")
    bigrams_filtered <- bigrams_separated %>%
      filter(!word1 %in% stop_words$word) %>%
      filter(!word2 %in% stop_words$word)
    bigram_counts <- bigrams_filtered %>% 
      count(word1, word2, sort = TRUE)
    bigram_graph <- bigram_counts %>%
      filter(n > input$nval2) %>%
      graph_from_data_frame()
    
    set.seed(123)
    a <- grid::arrow(type = "closed", 
                     length = unit(.15,
                                   "inches"))
    ggraph(bigram_graph, 
           layout = "fr") +
      geom_edge_link(aes(edge_alpha = n), 
                     show.legend = FALSE,
                     arrow = a, 
                     end_cap = circle(.07,
                                      'inches')) +
      geom_node_point(color = "lightblue", 
                      size = 5) +
      geom_node_text(aes(label = name), 
                     vjust = 1, 
                     hjust = 1) +
      theme_void()
    }else{
      output$text1 <- renderText("")
    }
  }
  )
  
  ###4) correlation
  output$correlation <- renderPlot({

    set.seed(123)
    newsgroup_cors %>%
      filter(correlation >= input$value[1] & correlation <= input$value[2]) %>%
      graph_from_data_frame() %>%
      ggraph(layout = "fr") +
      geom_edge_link(aes(alpha = correlation, 
                         width = correlation)) +
      geom_node_point(size = 3, 
                      color = "lightblue") +
      geom_node_text(aes(label = name),
                     color = "red",
                     repel = TRUE) +
      theme_void()
  }
  )
  ###5) wordclouds
  output$wordcloudd1 <- renderWordcloud2({
    if(input$option==1){
    subdata1=articles %>% filter(newsgroup %in% input$option1)
    data1 <- paste(subdata1$Content)
    if (is.character(data1)) {
      corpus <- Corpus(VectorSource(data1))
      corpus <- tm_map(corpus, tolower)
      corpus <- tm_map(corpus, removePunctuation)
      corpus <- tm_map(corpus, removeNumbers)
      corpus <- tm_map(corpus, removeWords, stopwords(tolower("English")))
      tdm <- as.matrix(TermDocumentMatrix(corpus))
      data1 <- sort(rowSums(tdm), decreasing =TRUE)
      data1 <- data.frame(word = names(data1), freq = as.numeric(data1))
    }
    # Make sure a proper num_words is provided
    if (!is.numeric(input$num) || input$num < 3) {
      input$num <- 3
    }
    data1 <- head(data1, n = input$num)
    wordcloud2(data1, backgroundColor = 'white')
    }
  }
  )
  
  output$wordcloudd2 <- renderWordcloud2({
    if(input$option==1){
    subdata2=articles %>% filter(newsgroup %in% input$option2)
    data2 <- paste(subdata2$Content)
    if (is.character(data2)) {
      corpus <- Corpus(VectorSource(data2))
      corpus <- tm_map(corpus, tolower)
      corpus <- tm_map(corpus, removePunctuation)
      corpus <- tm_map(corpus, removeNumbers)
      corpus <- tm_map(corpus, removeWords, stopwords(tolower("English")))
      tdm <- as.matrix(TermDocumentMatrix(corpus))
      data2 <- sort(rowSums(tdm), decreasing = TRUE)
      data2 <- data.frame(word = names(data2), freq = as.numeric(data2))
    }
    if (!is.numeric(input$num1) || input$num1 < 3) {
      input$num1 <- 3
    }
    data2 <- head(data2, n = input$num1)
    wordcloud2(data2, backgroundColor = 'white')
    }
  }
  )
  
  
  ####Network Graph
  

  ###Email Work type Related
  output$relation <- renderPlot({
    if (input$workType == "work") {
      print(input$color)
      set.seed(123)
      g <- ggraph(network_graphOfficial, 
                  layout = "nicely") + 
        geom_edge_link(aes(width=Weight), 
                       alpha=0.2) +
        scale_edge_width(range = c(0.1, 5)) +
        geom_node_point(aes(colour = input$color, 
                            size = 3))
      g + theme_graph()
    }else{
      print(input$color)
      set.seed(123)
      g <- ggraph(network_graphUnofficial, 
                  layout = "nicely") +
        geom_edge_link(aes(width=Weight), 
                       alpha=0.2) +
        scale_edge_width(range = c(0.1, 5)) +
        geom_node_point(aes(colour = input$color), 
                        size = 3)
      g + theme_graph()
    }
  }
  )
  
  
  
  
  
  
  
  
  
  
  
  
  ##mc3 server
  
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
  
  output$CCDataGraph <- renderPlot({
    df %>%
      filter(type == "ccdata") %>%
      filter(str_length(message) > input$msg_length) %>%
      mutate(hour_15min = date_time_build(2014,01,23,hour,
                                          min%/%as.integer(input$time_int1)*as.integer(input$time_int1),
                                          zone = "")) %>%
      group_by(hour_15min) %>%
      summarise(count = n(), combined_text = paste0(message, collapse = " | ")) %>%
      ungroup %>%
      ggplot(aes(x = hour_15min, y = count)) + geom_col()
  })
  
  output$CCDataTable <- renderDataTable({
    if(input$show_data2){
      df %>%
      filter(type == "ccdata") %>%
      filter(str_length(message) > input$msg_length) %>%
      select(dt, message, location) %>%
      datatable() %>% formatDate(1, "toLocaleString")
    }
  })
  
  output$Tfidf_bigram_optional <- renderPlot({
    if(input$show_bigram){
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
  
  output$MessageCountByTime <- renderPlot({
    df_rmjs %>%
      filter(type == "mbdata") %>%
      group_by(hour,min) %>%
      summarise(count = n()) %>%
      ungroup %>%
      mutate(hour_min = date_time_build(2014,01,23,hour,
                                        min%/%as.integer(input$time_int2)*as.integer(input$time_int2),
                                        zone = "")) %>%
      group_by(hour_min) %>%
      summarise(count2 = sum(count)) %>%
      ungroup %>%
      ggplot(aes(x = hour_min, y = count2)) + geom_col()
  })
  
  output$UniqueAuthors <- renderPlot({
    df_rmjs %>%
      filter(type == "mbdata") %>%
      group_by(hour,min) %>%
      summarise(author_count = n_distinct(author)) %>%
      ungroup %>%
      mutate(hour_min = date_time_build(2014,01,23,hour,
                                        min%/%as.integer(input$time_int2)*as.integer(input$time_int2),
                                        zone = "")) %>%
      ggplot(aes(x = hour_min, y = author_count)) + geom_col()
  })
  
  output$SentimentAfinn <- renderPlot({
    df_tidyclean_afinn %>%
      mutate(hour_min = date_time_build(2014,01,23,hour,
                                        min%/%as.integer(input$time_int2)*as.integer(input$time_int2),
                                        zone = "")) %>%
      ggplot(aes(x = hour_min, y = net_sentiment)) + geom_col()
  })
  
  output$SentimentNRC <- renderPlot({
    df_tidyclean_nrc %>%
      mutate(hour_min = date_time_build(2014,01,23,hour,
                                        min%/%as.integer(input$time_int2)*as.integer(input$time_int2),
                                        zone = "")) %>%
      ggplot(aes(x = hour_min, y = count, fill = sentiment)) + geom_col(show.legend = F) +
      facet_wrap(~sentiment, nrow = 2)
  })
  
  
  output$data_GIS <- renderTmap({
    tm_shape(Abila_st_lines) + tm_lines() +
      tm_shape(df_ccdata_1str_sf %>% filter(str_length(message)>30)) +
      tm_bubbles("hour", size = input$bubble_size, border.col = "darkgrey",
                 jitter = input$jitter_level, alpha = input$alpha_level,
                 palette="BuGn", popup.vars = c("dt","message")) +
      tm_shape(df_ccdata_Xstr_gps_point) +
      tm_bubbles("hour", size = input$bubble_size, border.col = "darkgrey",
                 jitter = input$jitter_level, alpha = input$alpha_level,
                 palette="BuGn", legend.col.show = F, popup.vars = c("dt","message")) +
      tm_shape(df_mbdata_gps_point_sentiment) +
      tm_bubbles("hour", size = input$bubble_size, border.col = "darkgrey",
                 jitter = input$jitter_level, alpha = input$alpha_level,
                 popup.vars = c("dt","author","message"))
  })
}

shinyApp(ui = ui, server = server)
