
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(DT)
source("helper.R")

# Define UI for application that draws a histogram
ui <- navbarPage("2018 Philly Primary Vote Explorer", id="nav", 
                 
      tabPanel("Interactive Map",
        div(class="outer",
            leafletOutput('voteMap', width="100%", height="100%"),
            
            absolutePanel(id="controls", class = "panel panel-default", fixed = TRUE,
                          draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                          width = 330, height = "auto",
                          style = "z-index: 1000;", ## z-index modification
                          
                          selectInput("office", label = h4("Office"),
                                      choices = offices
                                      ),
                          uiOutput("candidateSelect")
                          
            ),
            tags$div(id="cite",
                     'Created by ', tags$a(href="mailto:hollander@gmail.com", "Michael Hollander"), "Available on ", tags$a(href="asdf","GitHub")
            )
        )
      ),
      
      tabPanel("Data Explorer", 
        htmlOutput("DTTitle",container= tags$h2),
        dataTableOutput("voteTable"),
        tags$div(id="cite",
                 'Created by ', tags$a(href="mailto:hollander@gmail.com", "Michael Hollander"), "Available on ", tags$a(href="https://github.com/mhollander/2017PhillyDAPrimary","GitHub")
        )
        
      ),
      tags$head( tags$style(HTML("
          div.outer {
                                 position: fixed;
                                 top: 41px;
                                 left: 0;
                                 right: 0;
                                 bottom: 0;
                                 overflow: hidden;
                                 padding: 0;
                                 }
                                 
                                 #controls {
                                 /* Appearance */
                                 background-color: white;
                                 padding: 0 20px 20px 20px;
                                 cursor: move;
                                 /* Fade out while not hovering */
                                 opacity: 0.65;
                                 zoom: 0.9;
                                 transition: opacity 500ms 1s;
                                 }
                                 #controls:hover {
                                 /* Fade in while hovering */
                                 opacity: 0.95;
                                 transition-delay: 0;
                                 }
                                 
                                 /* Position and style citation */
                                 #cite {
                                 position: absolute;
                                 bottom: 10px;
                                 left: 10px;
                                 font-size: 14px;
                                 }           
                                 "))
      )
      
  )

server <- function(input, output, session) {
  
  session$userData$tempVotes <- c()
  session$userData$tempPercents <- c()
  
  # This is the dropdown of the candidates, which changes, depending on the office at stake.
  output$candidateSelect <- renderUI({
    selectInput("candidate", label= h4("Candidate"), choices=make.names(sort(unique(votes$Tape_Text[votes$Office_Prop.Name == input$office]))))
  })

  
  output$voteMap <- renderLeaflet({
    return(VoteMap)
  })
  
  output$DTTitle <- renderUI({
    
    HTML(paste(input$office,"Primary Results, by Ward and Division"))

  })
  
    output$voteTable <- renderDataTable({
    
      
    outputTable <- session$userData$tempPercents 

    aVoteTable <- DT::datatable(outputTable[,-1],
                                options=list(
                                  pageLength = 10,
                                  lengthMenu = list(c(10, 30, 60, -1),c("10", "30", "60", 'All')),
                                  order = list(0,'asc'),
                                  searching=TRUE
                                ), 
                                class="stripe",
                                rownames=FALSE
    ) 
    return(aVoteTable)
  })
  
    observe({
      session$userData$tempVotes <- getOfficeVotes(votes, input$office)
      session$userData$tempPercents <- getOfficePercent(session$userData$tempVotes)
    
    })

   observe({
     if (!is.null(input$candidate))
     {
       tempPrecincts <- precincts
       tempPrecincts@data = tempPrecincts@data %>%
          left_join(session$userData$tempPercents, by=c("DIVISION_N"="Precinct_Name"), copy=TRUE)
       columnData = tempPrecincts@data[[input$candidate]]
       #session$userData$r184th <- input$candidate
  
          leafletProxy("voteMap", data=tempPrecincts) %>%
            clearShapes() %>%
            addPolygons(fillColor = ~pal(columnData),
                      fillOpacity = 0.8,
                      color="#BDBDC3",
                      weight = 1,
                      popup = getPopup(votes, tempPrecincts,input$office))
     }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

