library(googlesheets)
library(ggplot2)

#ui
ui <- fluidPage(
  tabsetPanel(
    
    #data entry tab
    tabPanel("Enter data",
             #select which entry this comes from
             radioButtons("compared_by", "Which entry are you from?",
                          c("entry_1", "entry_2", "entry_3")
             ),
             
             #pairs of radio buttons for the best of each head to head
             radioButtons("comparison_1", "Select the better one:",
                          c("entry_1", "entry_2")
             ),
             radioButtons("comparison_2", "Select the better one:",
                          c("entry_1", "entry_3")
             ),
             radioButtons("comparison_3", "Select the better one:",
                          c("entry_2", "entry_3")
             ),
             
             #button to submit choices
             actionButton("submit", "Submit choices")
    ),
    #results tab
    tabPanel("Check results",
             plotOutput("full_download_plot")
    )
  )
)

#server
server <- function(input, output) {
  
  battle_names <- c("entry_1 v entry_2",
                    "entry_1 v entry_3",
                    "entry_2 v entry_3")
  
  datasetInput <- eventReactive(input$submit,
                                data.frame(data_from = input$compared_by,
                                           battle = battle_names,
                                           winners =  c(input$comparison_1,
                                                        input$comparison_2,
                                                        input$comparison_3)
                                )
  )
  
  #create the output for showing in the ui
  output$this_enty <- renderTable({
    datasetInput()
  })
  
  #save the output from this entry to google sheets
  sheet_obj <- gs_title("ida_plate_sheet")
  observeEvent(input$submit, gs_add_row(sheet_obj,
                                        input = datasetInput()
                                        
  )
  )
  
  #load the full sheet into shiny
  full_download <- gs_read_csv(gs_title("ida_plate_sheet"))
           
           output$full_download_plot <- renderPlot({
             ggplot(data = full_download,
                                                          aes(x = winners)) +
                                                     geom_bar()
           })
  
}

#app
shinyApp(ui = ui, server = server)