#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Histogramy"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput("parametr", "Dane:", 
                    choices=colnames(liczby)),
         hr(),
         uiOutput("pasek"),
         hr(),
         helpText("Kacper Pietraszewski, 2016")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("histogram"),
         verbatimTextOutput("opis"),
         hr(),
         h3("Podsumowanie"),
         tableOutput("podsumowanie"))
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

   output$histogram <- renderPlot({
     x       <- liczby[,input$parametr][!is.na(liczby[,input$parametr])]
     kolumny <- seq(min(x), max(x), length.out = input$kolumny + 1)
     hist(x, main=input$parametr, breaks = kolumny, col = 'blue', border = 'white')
   })
   output$pasek <- renderUI({
     x       <- liczby[,input$parametr][!is.na(liczby[,input$parametr])]
     y       <- dane[,input$parametr][!is.na(dane[,input$parametr])]
     sliderInput("kolumny", "Liczba przedziałów:",
                 min=1,
                 max=length(unique(x)),
                 value=length(unique(y)),
                 step=1)
   })
   output$podsumowanie <- renderTable({
     x       <- liczby[,input$parametr][!is.na(liczby[,input$parametr])]
     kolumny <- seq(min(x), max(x), length.out = input$kolumny + 1)
     h       <- hist(x, breaks = kolumny)
     podsumowanie[input$parametr,]
   })
   output$opis <- renderPrint({
     x       <- liczby[,input$parametr][!is.na(liczby[,input$parametr])]
     kolumny <- seq(min(x), max(x), length.out = input$kolumny + 1)
     h       <- hist(x, breaks = kolumny)
     opis = h$counts
     opis = rbind(opis, round(h$counts*100/sum(h$counts),3))
     opis = rbind(opis, round(seq(0, 1, length.out=length(h$counts)),3))
     row.names(opis) <- c("Odpowiedzi", "Udział proc.", "Stos. do max.")
     print(opis)
   })

}

# Run the application 
shinyApp(ui = ui, server = server)

