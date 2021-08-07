library(shiny)
library(clock)
library(DT)
library(ggwordcloud)
library(tidytext)
library(tidyverse)


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

author <- df %>%
    count(author) %>%
    select(author)


js_author = c("KronosQuoth", "ClevvahEvah")
df_rmjs <- df %>%
    filter(!author %in% js_author)
stopwords = c("rt")
df_tidyclean <- df_rmjs %>%
    mutate(message = iconv(message,"UTF-8","UTF-8","")) %>%
    unnest_tokens(word, message, token = "tweets") %>%
    filter(str_detect(word, "[a-zA-Z']$"), str_detect(word, "@", negate = T),
           !word %in% stop_words$word, !word %in% stopwords)


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


ui <- navbarPage("Team 15 Project",
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
                                             # consider adding authors and stopwords to the inputs
                                         ),
                                         mainPanel(
                                             plotOutput("Wordclouds")
                                         )
                                     )
                            ),
                            tabPanel("Part 3",
                                     titlePanel("Exploring the Topics - TF-IDF"),
                                     sidebarLayout(
                                         sidebarPanel(
                                             # consider adding authors and stopwords to the inputs
                                         ),
                                         mainPanel(
                                             # can select between unigram and bigram
                                             plotOutput("TfidfBigram")
                                         )
                                     )
                            )
                 ),
                 navbarMenu("Mini-Challenge 3 Q2",
                            tabPanel("Part 1"),
                            tabPanel("Part 2"),
                            tabPanel("Part 3")
                 ),
                 navbarMenu("Mini-Challenge 3 Q3",
                            tabPanel("Part 1"),
                            tabPanel("Part 2"),
                            tabPanel("Part 3")
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
    
    output$Wordclouds <- renderPlot({
        df_tidyclean %>%
            count(hour, word, sort = T) %>%
            filter(n > 10) %>%
            ggplot(aes(label = word, size = n)) + geom_text_wordcloud(rm_outside = T) +
            facet_wrap(~hour)
    })
    
    output$TfidfBigram <- renderPlot({
        df_tidyclean_bigram_tfidf %>%
            group_by(hour) %>%
            slice_max(tf_idf, 
                      n = 12) %>% # more may appear because of equal tf-idf rank
            ungroup() %>%
            mutate(bigram = reorder_within(bigram, tf_idf, hour)) %>%
            ggplot(aes(tf_idf, bigram, fill = hour)) +
            geom_col(show.legend = FALSE) +
            facet_wrap(~ hour, scales = "free") + scale_y_reordered() + xlab("tf-idf")
    })
}

shinyApp(ui = ui, server = server)
