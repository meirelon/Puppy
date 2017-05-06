library(readr);library(plyr);library(dplyr);library(stringr);library(pbapply);library(tidytext);library(ggplot2);library(tidyr);library(igraph);library(ggraph);library(stringdist);library(tweenr);library(shiny);library(shinythemes);library(lazyeval);library(wordcloud2)
load('dog_breed_names')
load('puppy_listing_breeds')
ui <- fluidPage(
     theme = shinytheme("flatly"),
     #create the header
     tags$h3(style = "font-family: Lucida Sans", "So you say you want a puppy..."),
     tags$p(style = "font-family: Arial", "A Visualization Dashboard by, Michael Nestel"),
     tags$hr(),
     #End of Header
     
     #Create the main panel which will consist of all the visualizations separated by individual tabs
     mainPanel(
          #create the tabset panels
          tabsetPanel(
               tabPanel("Wiki Dog Breeds", tags$hr(), p("Summary information about the each dog breed courtesy of Wikipedia"),
                        selectInput("dog_breed_select", "Choose Dog Breed", dog_breed_names),
                        numericInput("minwords_select", label = "Minimum Word Pairs", value = 3, min = 0, max = 5),
                        selectInput("word_cloud_shape_select", label = "Choose Shape of Wordcloud", choices = c("circle", "cardioid", "diamond", "triangle-forward", "triangle", "pentagon", "star")),
                        wordcloud2Output("wiki_word_cloud"),
                        plotOutput("wiki_word_graph"),
                        tableOutput("wiki_table")
          ),
               tabPanel("Wiki Dog Breed Comparison", tags$hr(), p("Picking out the keywords most important for comparing Dog Breeds"),
                    selectizeInput("dog_breed_list", label = "Select ANY Combination of Dog Breeds", choices = dog_breed_names, multiple = T, options = list(create = F, placeholder = 'American Bulldog')),
                    plotOutput("dog_tf_idf"),
                    plotOutput("dog_tf_idf2")
               ),
               tabPanel("Picking Your Puppy", tags$hr(), p("Puppy Listings Visualized"),
                        selectizeInput("puppy_list_breed_select", label = "Choose Puppy Breed", choices = puppy_listing_breeds, multiple = T, options = list(create = F, placeholder = 'Any Puppy Breeds')),
                        sliderInput("price_select", label = "Puppy Price Range", value = c(0, 7395), min = 0, max = 7395, step = 5),
                        sliderInput("mom_weight_select", "Select Mothers Weight Range", value = c(3, 175), min = 3, max = 175, step = 1),
                        sliderInput("dad_weight_select", "select Fathers Weight Range", value = c(2, 190), min = 2, max = 190, step = 1),
                        selectInput("puppy_gender_select", "Choose Gender", c("Male", "Female")),
                        plotOutput("puppy_name_bar_chart")
               )
          )
     )
)