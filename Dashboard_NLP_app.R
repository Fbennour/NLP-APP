# ----------  1. LIBRARIES  ----------
# shiny

library(shiny)
library(shinydashboard)

library(DT)
library(data.table)

library(plotly)

library(reticulate)
library(dplyr)
library(tidytext)
library(quanteda)
library(ggplot2)
library(tm)
library(topicmodels)
library(tidyverse)
library(wordcloud)
library(gutenbergr)
library(textclean)
library(foreach)
library(parallel)
library(textstem)
library(gmodels)
library(tidyquant)

use_condaenv()
source_python("py_script.py")
# source_python("py/pipeline_financial_sentiment.py")

source('Rfunctions.R')

# ----------  2. APP_ UI  ----------

ui <- dashboardPage(
    skin = "black", 
    
        #  2.1 HEADER  ----
    dashboardHeader(title= 
                       h1("axefinance - Demo PDF Mining", 
                       style = "font-weight: 450; color: #4d3a7d; font-size: 24px;"),
                    titleWidth = 450
                    # ,tags$img(src='axefinance.png')
                    ),
    
        #  2.2 SIDEBAR  ----
    dashboardSidebar(
        disable = TRUE, 
        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
        )
        ),
    
        #  2.3 BODY  ----
    dashboardBody(
            #  2.3.1 CSS FILE  ----
        #     tags$head(
        #     tags$head(rel="stylesheet", type= "text/css", href="custom.css")
        # ),
        # br(),
        # br(),
        # tags$h3("This application helps to get useful insights from PDF documents"),

            #  2.3.2. Select PDF File ----
        fluidRow(
            box(
            title = "Select a PDF File", width= 10, 
            background = "navy",
            "here's a list of the available files" ,solidHeader = TRUE,
            collapsible = TRUE,
            uiOutput('sel')
        )
        ),
        # box(
        #     title = "PDF Preview", width = 9 ,status = "primary", tags$h3("The text associated with the selected PDF"),solidHeader = T,
        #     collapsible = T,
        #     textOutput('text')
        # )
        fluidRow(
        box(title = "TOPIC WORD-CLOUD",width= 7, height = 500, status = "primary",
                    solidHeader = T, collapsible = T,
                    column(width = 9,
                           plotOutput("wordCloud")),

                    column(width = 3,
                           br(),
                           uiOutput("minfreq"),
                           br(),
                           uiOutput("maxwords"),
                           br(),
                )
                ),
        box(title = "table of names entites",width= 5, height = 500, status = "primary",
                    solidHeader = T, collapsible = T,
                    dataTableOutput('tab')
                )
        ),
        
        
            #  2.3.3. NAMED ENTITIES TABLE ----
        
        # fluidRow(
        #     infoBox(
        #         "TABLE", paste0("NAMED ENTITES"), width= 3, icon = icon("list"),
        #         color = "navy"
        #     ),
        #     box(
        #         title = "table of names entites",width= 9, status = "primary",
        #         solidHeader = T, collapsible = T, 
        #         dataTableOutput('tab')
        #     )
        #     
        # ),
        
            #  2.3.4 WORD CLOUD PLOT  ----
        
        # fluidRow(
        #     infoBox(
        #         "TOPIC", paste0("WORD CLOUD"), width= 3, icon = icon("cloud", lib = "glyphicon"),
        #         color = "navy"
        #     ),
        #     box(
        #         title = "TOPIC WORD-CLOUD",width= 9, status = "primary",
        #         solidHeader = T, collapsible = T, 
        #         # plotOutput("wordCloud")
        #         
        #         column(width = 8,
        #                plotOutput("wordCloud")),
        # 
        #         column(width = 4,
        #                br(),
        #                uiOutput("minfreq"),
        #                br(),
        #                uiOutput("maxwords"),
        #                br(),
        #     )
        #     )
        # ),
            #  2.3.5 SENTIMENT COMPONENT  ----
        fluidRow(
            # infoBox(
            #     "THE", paste0("SENTIMENT COMPONENT"), width=3, icon = icon("heart", lib = "glyphicon"),
            #     color = "navy"
            # ),
            box(
                title = "SENTIMENT COMPONENT", width=12, status = "primary",
                solidHeader = T, collapsible = T,
                plotlyOutput("sentComponent")
            )
        ),
        #  2.3.5 TEXT PREVIEW  ----
        
        fluidRow(
            box(
                title = "PDF Preview", width = 12 ,status = "success",solidHeader = T,
                collapsible = T,
                textOutput('text')
            )
        )
        
    )
    
    
)


# ----------  3. APP_SERVER  ----------
server <- function(input, output) {
        #  3.1 load the files   ----
    news <- readRDS('data/BBC_Reuters_GoogleNews_articles_business.rds')
    
    #  3.4. PDF selection  ----
    output$sel <- renderUI({
        selectInput(label = 'Title', inputId = 'selectTab', choices = news$Title, selected = news$Title[1])
    })
    
        #  3.2 Named entity table  ----
    output$tab <- renderDataTable({
        
        article<-news[news$Title == input$selectTab,'Body']
        
        datatable(get_NE(article), filter = 'top',  
            options = list(pageLength = 5, autoWidth = TRUE),
            rownames= FALSE 
        )
    })
    
         #  3.3. PDF text output  ----
    output$text<-renderText({
        article <- news[news$Title == input$selectTab,'Body']
    })
    
        #  3.4. PDF selection  ----
    output$sel <- renderUI({
        selectInput(label = 'Title', inputId = 'selectTab', choices = news$Title, selected = news$Title[1])
    })
    
        #  3.5. word cloud output  ----
    
    output$wordCloud <- renderPlot({
        
        article<-news[news$Title == input$selectTab,'Body']


        art_parg= data.frame(
            paragraph_text =unlist(tokenize_sentence(article))
        ) %>%
            rowid_to_column(var = "paragraph_num")




        art_rm_NE = NE_Cleansing(art_parg, 'paragraph_num', 'paragraph_text', group = TRUE, rm=FALSE,    Extract_Named_Entities(art_parg) %>% filter(Label %in% c("GPE", "ORG", "PERSON","LOC",'NORP')) %>% select(-Label) %>% unique())

        df = art_rm_NE %>%
            select(paragraph_num, TEXT) %>%
            unnest_tokens(input = TEXT, output = word)  %>%
            mutate(word = str_remove_all(word,"'s$")) %>%
            mutate(word = str_remove_all(word,"^the"))%>%
            mutate(word = str_remove_all(word,"^The"))%>%
            mutate(word = textstem::lemmatize_words(word)) %>%
            mutate(word= tolower(word)) %>%
            filter(!str_detect(word, '^\\d')) %>%
            filter(!str_detect(word, '^\\d[a-z][a-z]')) %>%
            anti_join(stop_words) %>%
            filter(nchar(word) > 2) %>%
            purrr::set_names('id','word') %>%
            dplyr::count(id, word)


        df_lemma = df %>%
            mutate(word = str_remove_all(word,"'s$")) %>%
            mutate(word = str_remove_all(word,"^the"))%>%
            mutate(word = str_remove_all(word,"^The"))%>%
            mutate(word = textstem::lemmatize_words(word)) %>%
            mutate(word= tolower(word)) %>%
            anti_join(stop_words) %>%
            filter(nchar(word) > 2) %>%
            purrr::set_names('id','word','n') %>%
            dplyr::count(id, word)

        dtm = df_lemma %>%
            tidytext::cast_dtm(document=id, term=word, value=n)

        mod = LDA_optimal(dtm, 2, 10, 5)

        terms = terms(mod$min_perp$model, k=20) %>%
            as.data.frame() %>%
            gather( topic, word,`Topic 1`:`Topic 4`, factor_key=FALSE) %>%
            left_join(
                df
            ) %>%
            filter(!is.na(n)) %>%
            select(-id) %>%
            group_by(topic, word) %>%
            mutate(n = sum(n))%>%
            unique()


        terms(mod$stationary_prep$model, k=20) %>%
            as.data.frame()




        wordcloud(words = terms$word, freq = terms$n, min.freq=input$freq, 
                  max.words=input$max, random.order=FALSE, rot.per=0.1,
                  ordered.colors=TRUE,
                  colors=brewer.pal(8, "Dark2")[factor(terms$topic)])
        

})
    # wordcloud control
    output$minfreq = renderUI({
        sliderInput("freq",
                    em("Minimum Frequency:",style="color:black;font-size:100%"),
                    min = 1,  max = 50, value = 15)
    })
    output$maxwords = renderUI({
        sliderInput("max",
                    em("Maximum Number of Words:",style="color:black;font-size:100%"),
                    min = 1,  max = 300,  value = 200)
    })
    
    
    
        #  3.6. sentiment component  ----

    output$sentComponent <- renderPlotly({
         article<-news[news$Title == input$selectTab,'Body']
        
         source_python("py/pipeline_financial_sentiment.py")
         
         
        paragraph_text_tbl <- tibble(
            # Page Text
            page_text = article
        ) %>%
            rowid_to_column(var = "page_num") %>%

            # Paragraph Text
            mutate(paragraph_text = str_split(page_text, pattern = "\\.\n")) %>%
            select(-page_text) %>%
            unnest(paragraph_text) %>%
            rowid_to_column(var = "paragraph_num") %>%
            select(page_num, paragraph_num, paragraph_text)
         
         
        sentiment_classification <- paragraph_text_tbl %>%
            pull(paragraph_text) %>%
            pipeline_classification()

        sentiment_regression <- paragraph_text_tbl %>%
            pull(paragraph_text) %>%
            pipeline_regression()

        data_prepared_tbl <- paragraph_text_tbl %>%
            mutate(
                sentiment_classification = sentiment_classification,
                sentiment_regression     = sentiment_regression
            ) %>%
            mutate(label = str_glue("Page: {page_num}
                            Paragraph: {paragraph_num}
                            Sentiment: {round(sentiment_regression)}
                            ---
                            {str_wrap(paragraph_text, width = 80)}"))


        g <- data_prepared_tbl %>%
            mutate(sentiment_classification = case_when(
                sentiment_classification == 0  ~ "neutral",
                sentiment_classification == 1  ~ "positive",
                sentiment_classification == -1 ~ "negative"
            ) %>% factor(levels = c("negative", "neutral", "positive"))) %>%
            ggplot(aes(sentiment_classification, sentiment_regression, color = sentiment_regression)) +
            geom_point(aes(text = label,
                           size = abs(sentiment_regression))) +
            scale_color_viridis_c() +
            theme_tq() +
            coord_flip()

        ggplotly(g, tooltip = "text")
    })

    
}


# ----------  RUN_APP  ----------

shinyApp(ui = ui, server = server)
