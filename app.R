#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
rsconnect::setAccountInfo(name='jhubiostatistics',
                          token='41969E2AC0F047B42111176450B2ADB8',
                          secret='vzjG1Ab7kdLIzR7mFTQW3uJdJnzhcsv5NBcanzXs')


library(shiny)
library(ggplot2)
library(lubridate)

full <- readRDS("full.rds")
data_1<- full
unique_topic_list<-as.list(unique(data_1$topic))
unique_justice_list<-as.list(unique(data_1$opinion))



# browser()
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Supreme Court Decisions"),
   
   # Sidebar with a dropdown menu for supreme court justices
   sidebarLayout(
      sidebarPanel(
        tabsetPanel(
        tabPanel("Topic Menu",selectInput(c("topic"),label="Select topic",
                    choices=unique_topic_list,
                    selected=1)),
        
        tabPanel("Justice Menu",selectInput("justice", label="Select justice",
                  choices=unique_justice_list,selected=1))
      )
      ), 
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
         tabPanel("Plot by Topic",plotOutput(outputId = "Plot_Topic")),
         tabPanel("Plot by Justice",plotOutput("Plot_Justices"))
        )
      )
   )
)

   



# Define server logic required to draw a histogram
server <- function(input, output) {
  data_1<- full
  
  unique_topic_list<-as.list(unique(data_1$topic))
  unique_justice_list<-as.list(unique(data_1$opinion))
  
  data_1$decided<- as.Date(data_1$decided,format="%B %d, %Y")
  
   output$Plot_Justices <- renderPlot({
     # subset data according to justice and topic
     data_plotted_justice<- subset(data_1, opinion==input$justice)
     
     #create bins for histogram
     bins_just <- seq(min(data_plotted_justice$decided), max(data_plotted_justice$decided),by="days")
     
     
     
     hist(data_plotted_justice$decided,breaks=bins_just,main=c("Histogram of Supreme Court cases with opinion written by",input$justice,"over time"),
          xlab= "Date of court decision")
     
     
      
      
      
   })
   
   output$Plot_Topic <- renderPlot({
     #subset data according to justice and topic
     data_plotted_topic<- subset(data_1, topic==input$topic)
     
     #create bins for histogram
     bins <- seq(min(data_plotted_topic$decided), max(data_plotted_topic$decided),by="days")
     
     
     
     hist(data_plotted_topic$decided,breaks=bins,
          main=c("Histogram of Supreme Court cases pertaining to",input$topic,"over time"),
          xlab= "Date of court decision")
     
     
 
     
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

