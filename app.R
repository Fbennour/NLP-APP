
# LIBRARIES ----
# Python
library(reticulate)

source("Rfunctions.R")

use_condaenv(condaenv = 'C:/ProgramData/Miniconda3')
source_python("py_script.py")


# Shiny
library(shiny)
library(shinythemes)

# PDF
library(pdftools)

# Images
library(magick)

# Plotting
library(plotly)

# Core
library(tidyverse)
library(tidyquant)
# dt
library(data.table)
library(DT)

# DATA SETUP ----


# UI ----
ui <- navbarPage(
    title = " PDF NLP Analyzer",
    collapsible = TRUE,
    position    = "static-top", 
    inverse     = FALSE, 
    theme       = shinytheme("spacelab"),
    
    tabPanel(
        title = "PDF DISPLAY",
        sidebarLayout(
            sidebarPanel = sidebarPanel(
                width = 3,
                
                shiny::fileInput(inputId = "pdf_input", label = "Select PDF File", accept = ".pdf"),
                shiny::actionButton(inputId = "submit", "Display", class = "btn-primary"),
                
            ),
            # * MAIN
            mainPanel = mainPanel(
                width = 9,
                div(
                    #class = "col-sm-6 panel",
                    div(class= "panel-heading", h3("PDF Viewer")),
                    div(
                        #class="panel-body", style="height:700px",  
                        uiOutput("page_controls"),
                        imageOutput("img_pdf", width = "50%", height = "600px")
                        
                    )
                ),
                
            )
        )
    ),
    tabPanel(
        title = "NAMED ENTITY TABLE",
        sidebarLayout(
            sidebarPanel = sidebarPanel(
                width = 3),
            mainPanel = mainPanel(
                width = 9,
                DT::dataTableOutput("DataTable"),
                br(),
                uiOutput("text"),
                uiOutput('forsearch'),
                uiOutput('searchbutton'),
                
            ))
    )
)

## --- server ----
server <- function(session, input, output) {
    
    # Limit PDF Files to 10MB
    options(shiny.maxRequestSize = 10*1024^2)
    
    rv <- reactiveValues()
    ### ---- first panel ----
    
    observeEvent(input$submit, {
        
        # Handle Inputs
        req(input$pdf_input)
        
        rv$pdf <- input$pdf_input
        
        # Read Text from PDF
        rv$text_data <- pdf_text(rv$pdf$datapath)
        
        rv$paragraph_text_tbl <- tibble(
            # Page Text
            page_text = rv$text_data
        ) %>%
            rowid_to_column(var = "page_num") %>%
            
            # Paragraph Text
            mutate(paragraph_text = str_split(page_text, pattern = "\\.\n")) %>%
            select(-page_text) %>%
            unnest(paragraph_text) %>%
            rowid_to_column(var = "paragraph_num") %>%
            select(page_num, paragraph_num, paragraph_text)
        
        
        
    })
    
    # Debugging
    output$print <- renderPrint({
        list(
            pdf = rv$pdf,
            text_data = rv$text_data,
            paragraph_text_tbl = rv$paragraph_text_tbl,
        )
    })
    
    # Render PDF Images
    output$img_pdf <- renderImage({
        
        req(rv$pdf)
        
        # Get page num
        page_num <- input$page_num
        
        # Read PDF Images
        rv$img_data <- image_read_pdf(rv$pdf$datapath, pages = page_num)
        
        tmpfile <- rv$img_data %>% 
            image_scale("600") %>%
            image_write(tempfile(fileext='jpg'), format = 'jpg')
        
        # Return a list
        list(src = tmpfile, contentType = "image/jpeg")
    })
    
    # Render PDF Viewer Controls
    output$page_controls <- renderUI({
        
        req(rv$pdf)
        
        n_max <- pdf_length(rv$pdf$datapath)
        
        div(
            class = "row",
            shiny::sliderInput(
                "page_num", 
                label = NULL, 
                value = 1, min = 1, max = n_max, step = 1, 
                width = "100%")
        )
        
    })
    ### ---- second panel ----
    
    output$DataTable <-DT::renderDataTable(
        pdf = rv$pdf,
        text_data = rv$text_data,
        paragraph_text_tbl = rv$paragraph_text_tbl,
        
        
        mydata=get_NE(page_text),
        
        mydata= as.data.table(mydata),
        mydata= unique(mydata[, Freq:= .N, by = .(Entity)]),
        setorder(mydata,-Freq),
        
        
            datatable(
                mydata, filter = 'top',  
                options = list(pageLength = 5, autoWidth = TRUE),
                rownames= FALSE
                
            )
   
    
    )
}

# Run the application 
shinyApp(ui = ui, server = server)