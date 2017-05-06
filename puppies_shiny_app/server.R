library(readr);library(plyr);library(dplyr);library(stringr);library(pbapply);library(tidytext);library(ggplot2);library(tidyr);library(igraph);library(ggraph);library(stringdist);library(tweenr);library(shiny);library(shinythemes);library(lazyeval);library(wordcloud2)
data("stop_words")
dog_breed_wiki_df <- read_csv("dog_breeds_wiki.csv")
puppy_listings <- read_csv("puppy_info.csv")
server <- function(input, output){
     
     count_bigrams <- function(dataset, minwords = 3) {
          dataset %>%
               unnest_tokens(bigram, content, token = "ngrams", n = 2) %>%
               separate(bigram, c("word1", "word2"), sep = " ") %>%
               filter(!word1 %in% stop_words$word,
                      !word2 %in% stop_words$word,
                      !str_detect(word1, "[0-9]"),
                      !str_detect(word2, "[0-9]"),
                      nchar(word1) > minwords,
                      nchar(word2) > minwords) %>%
               count(word1, word2, sort = TRUE)
     }
     
     visualize_bigrams <- function(bigrams) {
          set.seed(2016)
          a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
          
          bigrams %>%
               filter(n > 1) %>% 
               graph_from_data_frame() %>%
               ggraph(layout = "fr") +
               geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
               geom_node_point(color = "lightgreen", size = 4) +
               geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
               theme_void()
     }
     
     plot_dog_graph <- function(dog_breed, minwords_select=3){
          tmp <- filter(dog_breed_wiki_df, str_replace_all(breed, "_", " ") == dog_breed)
          count_bigrams(tmp, minwords = minwords_select) %>% 
               visualize_bigrams()
     }
     #Dog Breed Wikipedia Wordcloud function
     get_wiki_word_cloud <- function(dog_breed, cloud_shape = "circle"){
          #preprocess the words being inputted into the wordcloud
          wiki_words <- unlist(str_split(dog_breed_wiki_df[str_replace_all(dog_breed_wiki_df$breed, "_", " ") == dog_breed, 2], " ")) %>% 
               str_replace_all("[0-9]+", "") %>% 
               str_replace_all("(dog)s?", "") %>% 
               str_replace_all("(kennel)|(club)|(breed)|(type)s?", "")
          #remove english stopwords
          wiki_words <- wiki_words[!(wiki_words %in% stop_words$word)]
          #get word frequencies
          wiki_word_freqs <- table(wiki_words)
          #plot wordcloud
          wordcloud2(data = wiki_word_freqs, color = "random-dark", shuffle = T, shape = cloud_shape, size = 3)
     }
     #dog tf-idf
     dog_words <- dog_breed_wiki_df %>% 
          unnest_tokens(word, content) %>% 
          anti_join(stop_words) %>% 
          filter(nchar(word) > 2) %>% 
          count(breed, word, sort=T) %>% 
          ungroup()
     dog_words_tfidf <- dog_words %>%
          bind_tf_idf(word, breed, n) %>% 
          arrange(desc(tf_idf))
     ###
     #Begin Shiny Output
     ###
     
     #TAB 1
     output$wiki_word_graph <- renderPlot({
          plot_dog_graph(input$"dog_breed_select", minwords_select = input$"minwords_select")
     })
     output$wiki_table <- renderTable({
          count_bigrams(filter(dog_breed_wiki_df, str_replace_all(breed, "_", " ") == input$"dog_breed_select"))[1:10,]  
     })
     output$wiki_word_cloud <- renderWordcloud2({
          get_wiki_word_cloud(input$dog_breed_select, input$word_cloud_shape_select)
     })
     
     #TAB 2
     output$dog_tf_idf <- renderPlot({
          dog_tfidf_plot_df <- dog_words_tfidf %>%
               filter(str_replace_all(breed, "_", " ") %in% input$dog_breed_list) %>%
               arrange(desc(tf_idf)) %>% 
               mutate(word = factor(word, levels =rev(unique(word)))) %>% 
               group_by(breed) %>% 
               top_n(15, tf_idf)
          
          
          ggplot(dog_tfidf_plot_df[1:20,], aes(word, tf_idf, fill = breed)) + geom_col(show.legend = F) + labs(x = NULL, y = "tf-idf") + facet_wrap(~breed, ncol=2, scales="free") + coord_flip()
     })
     output$dog_tf_idf2 <- renderPlot({
          dog_tfidf_plot_df <- dog_words_tfidf %>% 
               filter(str_replace_all(breed, "_", " ") %in% input$dog_breed_list) %>%
               arrange(desc(tf_idf)) %>% 
               mutate(word = factor(word, levels =rev(unique(word)))) %>% 
               group_by(breed) %>% 
               top_n(15, tf_idf)
          
          ggplot(dog_tfidf_plot_df[1:30,], aes(word, tf_idf, fill = breed)) + geom_col() + labs(x = NULL, y = "tf-idf") + coord_flip()
     })
     
     #TAB 3
     output$puppy_name_bar_chart <- renderPlot({
          puppy_breed <- puppy_listings %>% 
               filter(str_replace_all(breed, "_", " ") == input$puppy_list_breed_select) %>% 
               filter(gender == input$puppy_gender_select) %>% 
               filter(dad_weight >= input$dad_weight_select[1] & dad_weight <= input$dad_weight_select[2]) %>% 
               filter(mom_weight >= input$mom_weight_select[1] & mom_weight <= input$mom_weight_select[2]) %>% 
               filter(price >= input$price_select[1] & price <= input$price_select)
          ggplot(data = puppy_breed, aes(x=breed, fill = as.factor(color))) + geom_bar()
     })
}