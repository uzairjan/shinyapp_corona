#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(httr)
devtools::install_github('uzairjan/lab05_1')
library("coronaApi")
library(dplyr)
library(tidyr)
library(stringr)
library(tidyverse)
library(ggplot2)

obj <- coronaApi()

newData <- obj$getDailyReportedData('daily')
newData
sweden <- as.data.frame(obj$getSingleCountryList('sweden'))



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Covid 19 data "),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          h5("This application used to retrieve covid-19 data from r package coronaApi
             which provide daily, overAll result of corona about corona")
        ),
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                type = 'tab',
                tabPanel("plots",  plotOutput("distPlot")),
                        
                tabPanel("corona in Sweden", plotOutput('swed')),
                tabPanel("Summary", verbatimTextOutput("summary")),
                tabPanel("Structure",verbatimTextOutput("str")),
                tabPanel("Data", tableOutput("data"))
                
            )
          
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        
        pl <- ggplot(head(newData, 50), aes(x=reportDate, reportDate,  y=totalConfirmed)) +
            geom_boxplot(aes(fill = totalConfirmed))+
            theme(axis.text.x = element_text(angle = 90))
        pl
        
    })
    
    output$summary <- renderPrint({
        summary(newData)
    })
    
    output$data <- renderTable({
        newData
    })
    
    output$str <- renderPrint({
        str(newData)
    })
    
    output$swed <- renderPlot({


        sweden <- sweden %>%
            gather(key=Type, value=Score, recovered,deaths,confirmed)

       bar<- ggplot(data=sweden, aes(x=Type, y=Score)) +  geom_bar(stat="identity" )
       bar
    })
}
# Run the application 
shinyApp(ui = ui, server = server)




