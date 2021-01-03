# ======================================== DEFINE USER INTERFACE ==========================================

              ## Remember to run the setup-up script before running the Shiny application ##

# Define header
header <- dashboardHeader(title = "Text-Analytical Toolbox")

# Define sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Introduction", tabName = "introduction", icon = icon("book-reader")), # Introductory Page
    menuItem("Semantics", tabName = "semantics", icon = icon("project-diagram")), # Exploring semantic relationships page
    menuItem("Word Cloud Generator", tabName = "wc", icon = icon("cloud")), # Word cloud generator page
    menuItem("Sentiment Analysis", tabName = "sa", icon = icon("theater-masks")) # Sentiment analysis
  )
)

# Define loading spinner to indicate to the user that the website is working
options(spinner.color = "#0275D8", spinner.color.background = "#ffffff", spinner.size = 1)

# Define the body of application
body <- dashboardBody(
  tabItems(
    
    # === Introduction Page === #
    tabItem(tabName = "introduction",
            br(),
            br(),
            h1(strong("Welcome to a Digital Text-Analytical Toolbox for Danish"), align = "center"),
            br(),
            fluidRow(
              box(width = 12, background = "yellow",
                  h4("This Shiny Application Platform is designed to aid Danish speakers without preexisting programming experience in their work with Danish text analysis. This platform enables quantitative text analysis as a supplement to the traditional, qualitative methods we are used to. The user is able to explore semantic relations among Danish words, generate word clouds based on self-chosen texts, and perform sentiment analysis. Hopefully such tools open up new avenues for Danish text analysis. Enjoy!")
              )
            ),
            h2(strong("Overview"), align = "center"),
            br(),
            
            # Tool 1: Exploring Semantic Relations as predicted by the semantic model
            fluidRow(
              infoBoxOutput("semantic_models"), # infobox
              box(title = "Explore Semantic Relationships Between Words", width = 8, background = "yellow", collapsible = T, collapsed = F,
                  h5("This panel allows you to explore semantic relationships between words of interest. Hence, you can input any words of choice and see which words are closest in terms of semantic similarity. Furthermore, you can find the semantic similarity between two words. Perhaps you want to examine whether the word prime minister is closest to man or woman. The semantic similarities are based on a semantic model trained on a Danish corpus. "))),
            br(),
            
            # Tool 2: Word Cloud Generator
            fluidRow(
              infoBoxOutput("WordCloud"), # infobox
              box(title = "Read About The Word Cloud Generator", width = 8, background = "yellow", collapsible = T, collapsed = F,
                  h5("A word cloud is a visual representation of the contents of a text based on word frequencies. Word that are used frequently appear larger in font size as well as more centralised in the word cloud. A word cloud can be a good place to start a text analysis, because it provides an overview of the text in question. A word cloud can have many applications. For instance, one could imagine generating a word cloud based on various historical accounts from World War II to get an overview of the contents."))),
            br(),
            
            # Tool 3: Sentiment Analysis
            fluidRow(
              infoBoxOutput("SentimentAnalysis"), #infobox
              box(title = "Read About Sentiment Analysis", width = 8, background = "yellow", collapsible = T, collapsed = F,
                  h5("Sentiment analysis is a way of quantifying the sentiment of a text based on assessing the positive and negative words that appear within it. With sentiment analysis one can get an overview of the overall sentiment within a text, but one can also use sentiment analysis to quantify the sentiment of a single statement or word.")))
    ),
    
    # === Semantic Models Panel === # 
    tabItem(tabName = "semantics",
            
            # Infobox
            fluidRow(
              box(title = strong("Danish Semantic Model"), width = 12, background = "yellow",
                  h4("Within this panel you can explore semantic relations among words based on a model of your choice. The models differ in the corpus they have been trained on. Hence if you choose to explore the semantic relations that accompany the word ’girl’ the relations will differ depending on the model you choose. You can read about the different models below."))
            ),
            
            # Infobox
            fluidRow(
              box(title = strong("Semantic Relations"), width = 12, background = "yellow", collapsible = T, collapsed = F,
                  h4("Below you can explore a self-chosen word's semantic relations with other words. Hence, you can input any word of your choice, and find out which words are closest to that particular word in terms of semantic similarity as predicted by the coNLL17-model, which is a Danish semantic model trained on a large Danish corpus. To the right, visualizations of the semantic relations will appear."))
            ),
            fluidRow(
              box(width = 6, height = 600,
                  
                  # Define where the user should input the word
                  textInput(inputId = "sm_text1",
                            label = h4(span(strong("Input a word"), style = "color:orange"),
                                       bsButton("model_info1", label = "", icon = icon("info-circle", lib = "font-awesome"), size = "extra-small"),
                                       bsPopover(id = "model_info1", title = "",
                                                 content = paste0("Input any word of your choice and find out which words are closest to that particular word in terms of semantic similarity as predicted by the semantic model. Semantic similarity is a measure of how similar two words are. The semantic similarity is provided as a number between 0 and 1 in which 0 means that the words are highly dissimilar and 1 means that the words are highly similar."),
                                                 placement = "right", 
                                                 trigger = "hover", 
                                                 options = list(container = "body")
                                       )),
                            placeholder = "Insert a Danish word here, e.g. ost"),
                  
                  # Define output of semantic similarity 
                  withSpinner(tableOutput("closest_words"), type = 1)),
              
              # Semantic Network Plots
              box(width = 6, height = 600,
                  tabBox(
                    side = "right", height = "550px", width = 12,
                    title = tagList(shiny::icon("hubspot"), "Plots"),
                    selected = "Plot 1",
                    tabPanel("Plot 3", withSpinner(plotOutput("semantics_plot3"), type = 1)),
                    tabPanel("Plot 2", withSpinner(plotOutput("semantics_plot2"), type = 1)),
                    tabPanel("Plot 1", withSpinner(plotOutput("semantics_plot1"), type = 1))
                  ))
            ),
            
            # Semantic Similarity between two words
            # Infobox
            fluidRow(
              box(title = strong("Semantic Similarity"), width = 12, background = "yellow", collapsible = T, collapsed = F,
                  h4("Below you can examine the semantic similarity between two self-chosen words. Hence, if you are interested in finding out the semantic similarity between words such as 'statsminister' and 'kvinde' and compare it to the semantic similarity between 'statsminister' and 'mand' you can simply input these words and see what happens. The semantic similarity is a measure of how similar the words are, and ranges between 0 and 1, in which 0 means that the words are highly dissimilar and 1 means that the words are highly similar."))
            ),
            
            # Defining where the user should input the chosen words
            fluidRow(
              box(title = span(strong("Find the semantic similarity between two words"), style = "color:orange"), width = 6, height = 250,
                  textInput(inputId = "semanticdistance_word1",
                            label = "Insert the first word here",
                            placeholder = "Insert a word here, e.g. 'statsminister'"),
                  textInput(inputId = "semanticdistance_word2",
                            label = "Insert the second word here",
                            placeholder = "Insert a word here, e.g. 'kvinde'")
              ),
              
              # Infobox
              box(width = 6, height = 250,
                  bsButton("model_info2", label = "", icon = icon("info-circle", lib = "font-awesome"), size = "extra-small"),
                  bsPopover(id = "model_info2", title = "",
                            content = paste0("The semantic similarity lies somewhere between 0 and 1 depending on the similarity between the words in question. A semantic similarity of 0 means that the words are highly dissimilar, while a semantic similarity score of 1 means that the words are highly similar. If the output is NA, it means that one of the words you have inserted does not exist in the corpus, and the semantic similarity can therefore not be computed."),
                            placement = "left",
                            trigger = "hover", 
                            options = list(container = "body")),
                  
                  # Output: the semantic similarity between the two words
                  h1(textOutput("semanticdistance_output"), style = "font-size:150px;", style = "color:orange;", align = "center"),
              )),
            
            # Word Vector Calculations
            # Infobox
            fluidRow(
              box(title = strong("Word Vector Calculations"), width = 12, background = "yellow", collapsible = T, collapsed = F,
                  h4("Below you can perform calculations with word vectors. This means that you can subtract or add vector representations of words and see which words are created. Vectors are essentially a list of numbers, and each word has a particular vector associated with it which determines where the words are situated in the vector space. Try subtracting 'mand' from 'konge' and add 'kvinde' and see what happens."))
            ),
            
            # Infobox
            fluidRow(
              box(title = span(strong("Calculations"), style = "color:orange",
                               bsButton("model_info3", label = "", icon = icon("info-circle", lib = "font-awesome"), size = "extra-small"),
                               bsPopover(id = "model_info3", title = "",
                                         content = paste0("Insert the words you wish to perform calculations with, and choose whihc kind of calculation you wish to make. One word per box."),
                                         trigger = "hover", 
                                         placement = "right",
                                         options = list(container = "body")
                               )), width = 6, height = 900,
                  
                  # Define where the user should input the words
                  textInput(inputId = "word1_sm",
                            label = "Word 1",
                            placeholder = "Insert the first word here, e.g. 'konge'"),
                  textInput(inputId = "word2_sm",
                            label = "Word 2",
                            placeholder = "Insert the second word here, e.g. 'mand'"),
                  textInput(inputId = "word3_sm",
                            label = "Word 3",
                            placeholder = "Insert the third word here, e.g. 'kvinde'"),
                  br(),
                  tags$hr(),
                  
                  # Let the user choose how many words they wish to see in the output
                  radioButtons(
                    inputId = "calc_n", 
                    label = "The number of resulting words displayed in the output", 
                    choices = c("5" = 5, "10" = 10, "20" = 20), 
                    selected = "5", 
                    inline = TRUE),
                  tags$hr(),
                  
                  # Let the user choose which kind of calculation to make
                  radioButtons(
                    inputId = "calc_source",
                    label = "Which kind of calculation do you want to perform?",
                    choices = c(
                      "Word 1 + Word 2" = "calc_option1",
                      "Word 1 - Word 2" = "calc_option2",
                      "Word 1 - Word 2 + Word 3" = "calc_option3",
                      "Word 1 + Word 2 + Word 3" = "calc_option4",
                      "Word 1 + Word 2 - Word 3" = "calc_option5",
                      "Word 1 - Word 2 - Word 3" = "calc_option6"
                    )),
                  tags$hr(),
                  
                  # Define calculation button
                  column(12,
                         actionButton("calc_action", "Calculate", style = "color: #fff; background-color: orange; border-color: orange"),
                         align = "center"
                  ),
                  tags$hr()
              ),
              
              # Output: showing the resulting words
              box(title = span(strong("Results"), style = "color:orange"), width = 6, height = 900,
                  withSpinner(div(tableOutput("new_word_vector"), style = "font-size:130% ; height:150%"), type = 1), align = "center")
            )
    ),
    
    # === Sentiment Analysis Page === #
    # === Credit: the sentiment calculations are based on the Sentida-package developed by Lauridsen, Svendsen and Dalsgaard (2020)
    tabItem(tabName = "sa",
            
            # Infobox
            fluidRow(
              box(title = strong("Sentiment Analysis"), width = 12, background = "yellow",
                  h4("Within this panel you can get an overview of the sentiment within a text. You can both assess the total sentiment score of a text as well as the mean sentiment score. The sentiment scores are based on the Danish sentiment lexicon, SENTIDA, which is a Danish lexicon of words rated in terms of their sentiment. This lexicon has been developed specifically for Danish sentiment analysis purposes.")),
            ),
            
            # Making it possible for the user to upload a text file or insert the text manually
            fluidRow(
              box(title = span(strong("Begin your sentiment analysis"), style = "color:orange",
                               bsButton("info14_sa", label = "", icon = icon("info-circle", lib = "font-awesome"), size = "extra-small"),
                               bsPopover(id = "info14_sa", title = "",
                                         content = paste0("If you choose to upload your text as a file, this file should either be a txt-file or a CSV-file."),
                                         trigger = "hover",
                                         placement = "bottom",
                                         options = list(container = "body")
                               )), width = 12,
                  radioButtons(
                    inputId = "sa_source",
                    label = "Text Input",
                    choices = c(
                      "Insert your text" = "sa_own",
                      "Upload a text file" = "sa_file"
                    )),
                  
                  # Option 1: Manually insert text
                  conditionalPanel(
                    condition = "input.sa_source == 'sa_own'",
                    textAreaInput(inputId = "sa_text", "Insert text here", rows = 7)
                  ),
                  
                  # Option 2: Upload a text file
                  conditionalPanel(
                    condition = "input.sa_source == 'sa_file'",
                    fileInput("sa_file", "Choose your file")
                  )
              )
            ),
            
            # Output: Total sentiment score
            fluidRow(
              box(title = span(strong("Total Sentiment Score"), style = "color:orange",
                               bsButton("info1_sa", label = "", icon = icon("info-circle", lib = "font-awesome"), size = "extra-small"),
                               bsPopover(id = "info1_sa", title = "",
                                         content = paste0("The total sentiment score is the sum of each word’s sentiment score. Context words are taken into account when computing the total sentiment score, which means that a negative word will become even more negative if it has the word ’very’ in front of it."),
                                         trigger = "hover",
                                         placement = "bottom",
                                         options = list(container = "body")
                               )), width = 6,
                  withSpinner(verbatimTextOutput("sentiment_score_total"), type = 1)),
              
              # Output: Mean sentiment score
              box(title = span(strong("Mean Sentiment Score"), style = "color:orange",
                               bsButton("info2_sa", label = "", icon = icon("info-circle", lib = "font-awesome"), size = "extra-small"),
                               bsPopover(id = "info2_sa", title = "",
                                         content = paste0("The mean sentiment score is the total sentiment score divided by the number of words within the text input. The mean sentiment score lies between -5 (very negative) and +5 (very positive), with 0 indicating a neutral tone."),
                                         trigger = "hover",
                                         placement = "bottom",
                                         options = list(container = "body")
                               )), width = 6,
                  withSpinner(verbatimTextOutput("sentiment_score_mean"), type = 1))
            ),
            
            # Polarity Plot: visualizing the distribution of positive and negative words within the text
            fluidRow(
              box(title = span(strong("Polarity Plot"), style = "color:orange",
                               bsButton("info3_sa", label = "", icon = icon("info-circle", lib = "font-awesome"), size = "extra-small"),
                               bsPopover(id = "info3_sa", title = "",
                                         content = paste0("The Polarity Plot displays the distribution of positive and negative words in your text. Neutral words are excluded, given that some words do not exist in the SENTIDA lexicon, and are therefore given a sentiment score of 0. Hence, displaying the neutral words would give a distorted image of the distribution."),
                                         placement = "top", 
                                         trigger = "hover", 
                                         options = list(container = "body")
                               )), width = 12,
                  tags$style(type="text/css",
                             ".shiny-output-error { visibility: hidden; }",
                             ".shiny-output-error:before { visibility: hidden; }"
                  ),
                  withSpinner(highchartOutput("sentiment_polarity_plot", height = 210), type = 1)
              )
            )
    ),
    
    # === Word Cloud Panel === # 
    tabItem(tabName = "wc",
            fluidRow(
              box(title = strong("The Word Cloud Generator"), width = 12, background = "yellow",
                  h4("Within this panel, you can generate a word cloud, which is a visual representation of the contents of a text. The most frequent words will appear larger in font size and more centralised in the cloud. When you generate a word cloud, the text will automatically go through a series of preprocessing steps: stopwords (commonly used words with no apparent meaning), numbers, punctuations, will be removedord, and all letters will be converted to lower case. You have the opportunity to color the words in the word cloud by their sentiment, which means that positive words will appear green and negative words will appear red. A word cloud is a good place to start a text analysis, because it provides you with a general overview of the text you are analysing.")),
            ),
            
            #Infobox
            fluidRow(
              box(title = span(strong("Generate Your Word Cloud"), style = "color:orange",
                               bsButton("info11_wc", label = "", icon = icon("info-circle", lib = "font-awesome"), size = "extra-small"),
                               bsPopover(id = "info11_wc", title = "",
                                         content = paste0("If you choose to upload a file, it has to be either a txt-file or a CSV-file."),
                                         trigger = "hover", 
                                         placement = "right",
                                         options = list(container = "body")
                               )), width = 12,
                  
                  # Make it possible for the user to choose between uploading a file or inserting text manually
                  radioButtons(
                    inputId = "source",
                    label = "Tekstinput",
                    choices = c(
                      "Insert your text" = "own",
                      "Upload a file" = "file"
                    )
                  ),
                  
                  # Option 1: Insert text manually
                  conditionalPanel(
                    condition = "input.source == 'own'",
                    textAreaInput("text", "Insert your text here", rows = 7)
                  ),
                  
                  # Option 2: Upload text-file
                  conditionalPanel(
                    condition = "input.source == 'file'",
                    fileInput("file", "Choose your file")
                  ),
                  
                  # Make it possible for the user to choose if they want to color the words by sentiment 
                  checkboxInput(inputId = "sentiment_wc", span("Color negative and positive words",
                                                               bsButton("info1_wc", label = "", icon = icon("info-circle", lib = "font-awesome"), size = "extra-small"),
                                                               bsPopover(id = "info1_wc", title = "",
                                                                         content = paste0("If you choose to color the words in your word cloud by their sentiment, positive words will appear green while negative words will appear red. Neutral words or words that do not exist in the lexicon will appear grey."),
                                                                         trigger = "hover", 
                                                                         placement = "right",
                                                                         options = list(container = "body")
                                                               )), value = FALSE
                  ),
                  
                  # Make it possible for the user to remove specific words from the word cloud
                  conditionalPanel(
                    condition = "input.sentiment_wc == 1"),
                  checkboxInput(inputId = "wc_remove_words", label = span("Remove words",
                                                                          bsButton("info2_wc", label = "", icon = icon("info-circle", lib = "font-awesome"), size = "extra-small"),
                                                                          bsPopover(id = "info2_wc", title = "",
                                                                                    content = paste0("If you notice words in your word cloud that your are not particularly interested in, you can remove them. The word should not start with a capital letter."),
                                                                                    trigger = "hover", 
                                                                                    placement = "right",
                                                                                    options = list(container = "body")
                                                                          )), value = FALSE
                  ),

                  # Make a box for each word they want to remove
                  conditionalPanel(
                    condition = "input.wc_remove_words == 1",
                    textAreaInput("wc_words_to_remove1", "Insert words you want to remove. One word per box.", rows = 1)
                  ),
                  conditionalPanel(
                    condition = "input.wc_remove_words == 1 && input.wc_words_to_remove1.length > 0",
                    textAreaInput("wc_words_to_remove2", "", rows = 1)
                  ),
                  conditionalPanel(
                    condition = "input.wc_remove_words == 1 && input.wc_words_to_remove2.length > 0",
                    textAreaInput("wc_words_to_remove3", "", rows = 1)
                  ),
                  conditionalPanel(
                    condition = "input.wc_remove_words == 1 && input.wc_words_to_remove3.length > 0",
                    textAreaInput("wc_words_to_remove4", "", rows = 1)
                  ),
                  conditionalPanel(
                    condition = "input.wc_remove_words == 1 && input.wc_words_to_remove4.length > 0",
                    textAreaInput("wc_words_to_remove5", "", rows = 1)
                  ),
                  conditionalPanel(
                    condition = "input.wc_remove_words == 1 && input.wc_words_to_remove5.length > 0",
                    textAreaInput("wc_words_to_remove6", "", rows = 1)
                  ),
                  conditionalPanel(
                    condition = "input.wc_remove_words == 1 && input.wc_words_to_remove6.length > 0",
                    textAreaInput("wc_words_to_remove7", "", rows = 1)
                  ),
                  conditionalPanel(
                    condition = "input.wc_remove_words == 1 && input.wc_words_to_remove7.length > 0",
                    textAreaInput("wc_words_to_remove8", "", rows = 1)
                  ),
                  conditionalPanel(
                    condition = "input.wc_remove_words == 1 && input.wc_words_to_remove8.length > 0",
                    textAreaInput("wc_words_to_remove9", "", rows = 1)
                  ),
                  conditionalPanel(
                    condition = "input.wc_remove_words == 1 && input.wc_words_to_remove9.length > 0",
                    textAreaInput("wc_words_to_remove10", "", rows = 1)
                  ),
                  conditionalPanel(
                    condition = "input.wc_remove_words == 1 && input.wc_words_to_remove10.length > 0",
                    textAreaInput("wc_words_to_remove11", "", rows = 1)
                  ),
                  
                  # Make it possible for the user to change the background color of the word cloud
                  colourInput(inputId = "col", "Background Color", value = "white"),
                  numericInput("num", "Number of words in your word cloud",
                               value = 100, min = 5),
              )
            ),
            
            # Infobox
            fluidRow(
              box(title = span(strong("Word Cloud"), style = "color:orange",
                               bsButton("info12_wc", label = "", icon = icon("info-circle", lib = "font-awesome"), size = "extra-small"),
                               bsPopover(id = "info12_wc", title = "",
                                         content = paste0("Words that appear frequently in your text will appear larger in font size and more centralised. If you choose to color the words in your word cloud by their sentiment, positive words will appear green while negative words will appear red. Neutral words or words that do not exist in the lexicon will appear grey. When you hover your mouse over a word, the frequency of that word will appear."),
                                         trigger = "hover", 
                                         placement = "right",
                                         options = list(container = "body")
                               )),
                  
                  # Generate the actual word cloud
                  withSpinner(wordcloud2Output("cloud"), type = 1), width = 12)
            ),
            
            # Table output: Shows the most frequent words
            fluidRow(
              box(title = span(strong("Most Frequent Words"), style = "color:orange"), 
                  withSpinner(plotlyOutput("wc_word_frequency_plot"), type = 1), width = 12
              )
            )
    )
  )
)


ui <- dashboardPage(header, sidebar, body, skin = "yellow")

## ------------------------------------------------ DEFINE SERVER ------------------------------------------ ##
server <- function(input, output, session) {
  
  # === INTRODUCTION PAGE === #
  output$semantic_models <- renderInfoBox({
    infoBox(
      "Tool 1", "EXPLORE SEMANTIC RELATIONS", "Explore semantic relationships", icon = icon("project-diagram", lib = "font-awesome"),
      color = "yellow", fill = TRUE,
    )
  })
  
  output$WordCloud <- renderInfoBox({
    infoBox(
      "Tool 2", "WORD CLOUD GENERATOR", "Visualize your text in a cloud", icon = icon("cloud", lib = "font-awesome"),
      color = "yellow", fill = TRUE,
    )
  })
  
  output$SentimentAnalysis <- renderInfoBox({
    infoBox(
      "Tool 3", "SENTIMENT ANALYSIS", "Explore the sentiment of your texts", icon = icon("theater-masks", lib = "font-awesome"),
      color = "yellow", fill = TRUE,
    )
  })
  
  # === SEMANTIC MODELS OUTPUTS === # 
  
  # Create empty placeholder for data
  react_closest_words <- reactiveValues(closest_words = NULL)

  # Find the most semantically similar words to the input word using the semantic model
  output$closest_words <- renderTable({
    closest_words <- predict(coNLL17_model, newdata = req(input$sm_text1), type = "nearest")
    closest_words <- as.data.frame(closest_words)
    names(closest_words)[1] <- "Your word"
    names(closest_words)[2] <- "Closest Word"
    names(closest_words)[3] <- "Semantic Similarity"
    names(closest_words)[4] <- "Rank"
    closest_words
    react_closest_words$closest_words <- closest_words
  })
  
  # Semantic Network Plot 1
  output$semantics_plot1 <- renderPlot({
      x <- predict(coNLL17_model, req(c(react_closest_words$closest_words[1,1], react_closest_words$closest_words[,2])), type = "embedding")
      y <- predict(coNLL17_model, req(c(react_closest_words$closest_words[1,1], react_closest_words$closest_words[,2])), type = "embedding")
      df <- word2vec_similarity(x, y)
      df <- as.data.frame(df)
      df2 <- data.matrix(df)
      dd <- dist(scale(df2), method = "euclidian")
      hc <- hclust(dd, method = "ward.D2")
      plot(as.phylo(hc), type = "unrooted", main = "Semantic Network")
  })
  
  # Semantic Network Plot 2
  output$semantics_plot2 <- renderPlot({
      x <- predict(coNLL17_model, req(c(react_closest_words$closest_words[1,1], react_closest_words$closest_words[,2])), type = "embedding")
      y <- predict(coNLL17_model, req(c(react_closest_words$closest_words[1,1], react_closest_words$closest_words[,2])), type = "embedding")
      df <- word2vec_similarity(x, y)
      df <- as.data.frame(df)
      df2 <- data.matrix(df)
      dd <- dist(scale(df2), method = "euclidian")
      hc <- hclust(dd, method = "ward.D2")
      plot(as.phylo(hc), type = "fan", main = "Semantic Network")
  })
  
  # Semantic Network Plot 3
  output$semantics_plot3 <- renderPlot({
      x <- predict(coNLL17_model, req(c(react_closest_words$closest_words[1,1], react_closest_words$closest_words[,2])), type = "embedding")
      y <- predict(coNLL17_model, req(c(react_closest_words$closest_words[1,1], react_closest_words$closest_words[,2])), type = "embedding")
      df <- word2vec_similarity(x, y)
      df <- as.data.frame(df)
      df2 <- data.matrix(df)
      dd <- dist(scale(df2), method = "euclidian")
      hc <- hclust(dd, method = "ward.D2")
      hcd <- as.dendrogram(hc)
      plot(hcd, type = "triangle", axes = F, main = "Semantic Network")
  })
  
  # Calculate the semantic similarity between two two words
  output$semanticdistance_output <- renderText({
    x <- predict(coNLL17_model, req(input$semanticdistance_word1), type = "embedding")
    y <- predict(coNLL17_model, req(input$semanticdistance_word2), type = "embedding")
    z <- word2vec_similarity(x, y)
    round(z[1,1], 2)
  })
  
  # Word vector calculation outputs:
  observeEvent(input$calc_action, {
    vector_word1 <- predict(coNLL17_model, newdata = req(input$word1_sm), type = "embedding")
    vector_word2 <- predict(coNLL17_model, newdata = req(input$word2_sm), type = "embedding")
    vector_word3 <- predict(coNLL17_model, newdata = input$word3_sm, type = "embedding")
    
    # Word 1 + Word 2
    if (input$calc_source == "calc_option1"){
      new_vector <- vector_word1[,] + vector_word2[,]
    }
    
    # Word 1 - Word 2
    if (input$calc_source == "calc_option2"){
      new_vector <- vector_word1[,] - vector_word2[,]
    }
    
    # Word 1 - Word 2 + Word 3
    if (input$calc_source == "calc_option3"){
      new_vector <- vector_word1[,] - vector_word2[,] + vector_word3[,]
    }
    
    # Word 1 + Word 2 + Word 3
    if (input$calc_source == "calc_option4"){
      new_vector <- vector_word1[,] + vector_word2[,] + vector_word3[,]
    }
    
    # Word 1 + Word 2 - Word 3
    if (input$calc_source == "calc_option5"){
      new_vector <- vector_word1[,] + vector_word2[,] - vector_word3[,]
    }
    
    # Word 1 - Word 2 - Word 3
    if (input$calc_source == "calc_option6"){
      new_vector <- vector_word1[,] - vector_word2[,] - vector_word3[,]
    }
    
    # Make sure that the chosen number of ouput words is displayed
    n <- as.numeric(input$calc_n) + 3
    predicted_word <- predict(coNLL17_model, newdata = new_vector, type = "nearest", top_n = n)
    predicted_word <- as.data.frame(predicted_word)
    
    # Change names of the output table
    names(predicted_word)[1] <- "Word"
    names(predicted_word)[2] <- "Semantic Similarity"
    names(predicted_word)[3] <- "Rank"
    
    # Make sure that the input words do not show up in the output table
    df <- predicted_word %>% 
      filter(Word != req(input$word1_sm), Word != req(input$word2_sm), Word != input$word3_sm) %>% 
      head(input$calc_n)
    df$Rank <- c(1:input$calc_n)
    
    # Render output table
    output$new_word_vector <- renderTable({df})
    
  })
  
  # === WORD CLOUD OUTPUTS === # 
  
  # First we need to determine whether the use wrote the textual input or uploaded a text file
  data_source <- reactive({
    if (input$source == "own") {
      wc_data <- input$text
    } else if (input$source == "file") {
      wc_data <- input_file()
    }
    return(wc_data)
  })
  
  # If nothing is inputted return nothing
  input_file <- reactive({
    if (is.null(input$file)) {
      return("")
    }
    
    # Read the text with the UTF-8 encoding format, which recognizes Danish letters (æøå)
    readLines(input$file$datapath, encoding = "UTF-8")
  })
  
  # Create empty placeholder for word cloyd data
  reactive_wc_data <- reactiveValues(data = NULL)
  
  # Define a function that generates a word cloud
  create_wordcloud <- function(wc_data, num_words = 500, background = "white") {
    
    # If text is provided, convert it to a dataframe of word frequencies
    if (is.character(wc_data)) {
      corpus <- Corpus(VectorSource(wc_data))
      corpus <- tm_map(corpus, tolower) # make lowercase
      corpus <- tm_map(corpus, removePunctuation) # remove puntuation
      corpus <- tm_map(corpus, removeNumbers) # remove numbers  
      corpus <- tm_map(corpus, removeWords, stopwords(tolower("Danish"))) # remove Danish stopwords
      corpus <- tm_map(corpus, removeWords, c(input$wc_words_to_remove1)) # the following lines removes words that the user does not want in the word cloud
      corpus <- tm_map(corpus, removeWords, c(input$wc_words_to_remove2))
      corpus <- tm_map(corpus, removeWords, c(input$wc_words_to_remove3))
      corpus <- tm_map(corpus, removeWords, c(input$wc_words_to_remove4))
      corpus <- tm_map(corpus, removeWords, c(input$wc_words_to_remove5))
      corpus <- tm_map(corpus, removeWords, c(input$wc_words_to_remove6))
      corpus <- tm_map(corpus, removeWords, c(input$wc_words_to_remove7))
      corpus <- tm_map(corpus, removeWords, c(input$wc_words_to_remove8))
      corpus <- tm_map(corpus, removeWords, c(input$wc_words_to_remove9))
      corpus <- tm_map(corpus, removeWords, c(input$wc_words_to_remove10))
      tdm <- as.matrix(TermDocumentMatrix(corpus)) # Convert the corpus to a matrix
      wc_data <- sort(rowSums(tdm), decreasing = TRUE) # Sort the frequencies from highest to lowest
      wc_data <- data.frame(word = names(wc_data), freq = as.numeric(wc_data)) # convert data to a dataframe
    }
    
    # Put the data inside a data frame
    reactive_wc_data$data <- wc_data
    
    # Make sure a proper num_words is provided
    if (!is.numeric(num_words) || num_words < 3) {
      num_words <- 3
    }
    
    # Grab the top n most common words
    wc_data <- head(wc_data, n = num_words)
    if (nrow(wc_data) == 0) {
      return(NULL)
    }
    
    # If the user wants to color by sentiment we have to calculate the total sentiment of each word 
    if (input$sentiment_wc == TRUE){
      words_as_list <- list(as.character(wc_data[,1])) %>% 
        unlist()
      wc_data$sentiment <- NA
      for (i in 1:length(words_as_list)) {
        words_as_list[[i]][1]
        wc_data$sentiment[i] <- sentida(words_as_list[i], output = "total")
      }
      
      # Color words according to their sentiment (postive word = green, negative words = red)
      # Credit: https://www.displayr.com/sentiment-word-clouds-r/ 
      n = nrow(wc_data)
      colors = rep("grey", n)
      colors[wc_data$sentiment < 0 & wc_data$sentiment > -1] = "#FFCACA"
      colors[wc_data$sentiment < -1 & wc_data$sentiment > -2] = "#FF9696"
      colors[wc_data$sentiment < -2 & wc_data$sentiment > -3] = "#FF5D5D"
      colors[wc_data$sentiment < -3 & wc_data$sentiment > -4] = "#FF2D2D"
      colors[wc_data$sentiment < -4 & wc_data$sentiment > -5] = "#FF0000"
      colors[wc_data$sentiment > 0 & wc_data$sentiment < 1] =  "#C9FFCB"
      colors[wc_data$sentiment > 1 & wc_data$sentiment < 2] =  "#9EFFA1"
      colors[wc_data$sentiment > 2 & wc_data$sentiment < 3] =  "#76FF7B"
      colors[wc_data$sentiment > 3 & wc_data$sentiment < 4] = "#3BFF41"
      colors[wc_data$sentiment > 4 & wc_data$sentiment < 5] =  "#00FF08"
      
      # Create word cloud
      wordcloud2(wc_data, backgroundColor = background, color = colors)
      
      # If the user does not want to color by sentiment, the color of the words will not correspond to sentiment
    } else if (input$sentiment_wc == FALSE){
      wordcloud2(wc_data, backgroundColor = background, color = "black")
    }
  }
  
  # Render the word cloud
  output$cloud <- renderWordcloud2({
    create_wordcloud(data_source(),
                     num_words = input$num,
                     background = input$col
    )
  })
  
  # Frequency plot: displaying the most frequent words
  output$wc_word_frequency_plot <- renderPlotly({
    frequency_ggpplot <- reactive_wc_data$data %>%
      filter(freq > 1) %>% # The frequency must be above 1 to be included in the plot
      top_n(10, freq) %>% # Take the 10 most frequent words
      ggplot(aes(word, freq)) + # x-axis contains wordsm y-axis contains frequencies
      geom_col(alpha = 1, show.legend = FALSE) + # transparency and legend
      theme(text = element_text(size = 12)) + # Font size
      coord_flip() + # Flip the x- and y-axis
      scale_x_reordered() + # reorder x-axis
      labs(x = " ", y = "Frequency") # labels
    ggplotly <- ggplotly(frequency_ggpplot)
    return(ggplotly)
  })
  
  # === SENTIMENT ANALYSIS OUTPUTS === #
  
  # First we need to determine whether the use wrote the textual input or uploaded a text file
  sa_data <- reactive({
    if (input$sa_source == "sa_own") {
      data <- input$sa_text
    } else if (input$sa_source == "sa_file") {
      if (is.null(input$sa_file)) {
        return("")
      }
      
      # Read the text with the UTF-8 encoding format that recognizes Danish letters
      data <- readLines(input$sa_file$datapath, encoding = "UTF-8")
    }
    
    # Convert text to a data frame
    data <- as.data.frame(data)
    return(data)
  })
  
  # Calculate the total sentiment score
  # If the user inputted the text manually:
  output$sentiment_score_total <- renderText({
    if (input$sa_source == "sa_own"){
      total_sentiment <- sentida(req(input$sa_text), output = "total")
      return(total_sentiment)
    }
    
  # If the user chose to upload a file:  
    if (input$sa_source == "sa_file"){
      sa_data_string <- readLines(req(input$sa_file$datapath), encoding = "UTF-8")
      sa_data_string <- as.String(sa_data_string)
      total_sentiment <- sentida(sa_data_string, output = "total")
      return(total_sentiment)
    }
  }) 
  
  # Calculate the mean sentiment core
  # If the user inputted the text manually:
  output$sentiment_score_mean <- renderText({
    if (input$sa_source == "sa_own"){
      mean_sentiment <- sentida(req(input$sa_text), output = "mean")
      return(mean_sentiment)
    }
    
  # If the user chose to upload a file:    
    if (input$sa_source == "sa_file"){
      sa_data_string <- readLines(req(input$sa_file$datapath), encoding = "UTF-8")
      sa_data_string <- as.String(sa_data_string)
      mean_sentiment <- sentida(sa_data_string, output = "mean")
      return(mean_sentiment)
    }
  })
  
  # Create a list of words and add column with sentiment
  sa_df <- reactive({
    x <- as.data.frame(sa_data())
    x$data <- tolower(x$data) 
    x$sentiment <- NA
    x <- x %>% unnest_tokens(word, data)
    
    for (row in 1:nrow(x)){
      x$sentiment[row] <- sentida(x$word[row])
    }
    
    x$category_senti <- ifelse(x$sentiment < 0, "Negative", ifelse(x$sentiment > 0, "Positive", "Neutral"))
    return(x)
    
  })
  
  # Create data frame and give the negative and positive words a color
  senti_df <- reactive({
    senti_df <- sa_df() %>% 
      group_by(category_senti) %>% 
      summarise(score = n()) %>% 
      mutate(score_pct = score/sum(score)*100, coloract = c("#d35400", "#2ecc71", "#48a60d")) %>% 
      filter(category_senti != "Neutral")
  })
  
  # Sentiment Polarity Plot
  output$sentiment_polarity_plot <- renderHighchart({
    hc <- highchart() %>%
      hc_chart(type = "bar") %>%
      hc_xAxis(categories = senti_df()$category_senti,
               labels = list(style = list(fontSize= '12px'))
      )    %>%
      hc_colors(colors = senti_df()$coloract) %>% 
      hc_add_series(name="Sentiment", data = senti_df()$score_pct, colorByPoint = TRUE, 
                    type ="column",
                    color = "#4472c4", showInLegend= F) %>% 
      hc_yAxis(labels=list(format = '{value}%'), min=0,
               max=100, showFirstLabel = TRUE, showLastLabel=TRUE)
    return(hc)
  })
  
}

# ===================================== RUN THE SHINY APPLICATION ===========================================
shinyApp(ui = ui, server = server)
