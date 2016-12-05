#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
source("statystyki.R", encoding = "UTF-8")
# Define UI for application that draws a histogram
ui <- navbarPage("PlantFoods",
             tabPanel("Histogramy",
                      titlePanel("Histogramy"),
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("parametr", "Dane:",
                                      choices=colnames(liczby)),
                                      hr(),
                                      uiOutput("pasek"),
                                      hr(),
                                      helpText("Kacper Pietraszewski, 2016")),
                        mainPanel(
                          plotOutput("histogram"),
                          verbatimTextOutput("opis"),
                          hr(),
                          h3("Podsumowanie"),
                          tableOutput("podsumowanie"))
                        )
                      ),
             tabPanel("Zestawienie",
                      titlePanel("Zestawienie"),
                      DT::dataTableOutput("zestawienie")
                      ), 
             tabPanel("Statystyki",
                       titlePanel("Statystyki"),
                       sidebarLayout(
                         sidebarPanel(
                           numericInput("p", "Poziom istotności (p)", value = 0.05, step = 0.01, min = 0, max=0.1),
                           numericInput("r", "Zakres korelacji (r)", value = 0.5, step = 0.05, min = 0, max=1),
                           hr(),
                           helpText("Kacper Pietraszewski, 2016")
                         ),
                         mainPanel(
                           DT::dataTableOutput("statystyki")
                           )
                         )
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
     y       <- dane[,input$parametr][!is.na(dane[,input$kolumny])]
     sliderInput("kolumny", "Liczba przedziałów:",
                 min=1,
                 max=length(unique(x)),
                 value=length(unique(y)),
                 step=1)
   })
   output$podsumowanie <- renderTable({
     x       <- liczby[,input$parametr][!is.na(liczby[,input$parametr])]
     kolumny <- seq(min(x), max(x), length.out = input$kolumny + 1)
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
   output$statystyki <- DT::renderDataTable({
     wyniki <- c()
     for (i in 1:dim(wyniki.total)[2]){
       wyniki = cbind(wyniki,
                      wyniki.total[,i][as.numeric(wyniki.total$p) < input$p & (as.numeric(wyniki.total$r) > input$r | as.numeric(wyniki.total$r) < -input$r)])
       wyniki
     }
     wyniki = as.data.frame(wyniki)
     names(wyniki) = names(wyniki.total)
     DT::datatable(
       wyniki[,c(2,6,9:12)], options = list(
         lengthMenu = list(c(5, 10, 15, 20, 30, 50, 75, 100, -1), c('5', '10', '15', '20', '30', '50', '75', '100', 'Wszystkie')),
         pageLength = 20
         )
       )
   })
   output$zestawienie <- DT::renderDataTable({
     DT::datatable(
       podsumowanie, rownames = F, options = list(
         lengthMenu = list(c(5, 10, 15, 20, 30, 50, 75, 100, -1), c('5', '10', '15', '20', '30', '50', '75', '100', 'Wszystkie')),
         pageLength = 20
         )
     )
   })
}
# Run the application 
shinyApp(ui = ui, server = server)