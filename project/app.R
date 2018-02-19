#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)

A2010<- read.csv('PB Apprehensions 2010.csv', header=TRUE, stringsAsFactors = FALSE)
A2017<- read.csv('PB Apprehensions 2017.csv', header=TRUE, stringsAsFactors = FALSE)
Monthly<- read.csv('PB monthly summaries.csv',header=TRUE, stringsAsFactors = FALSE)



# Define UI for application that draws a time series plot
ui <- fluidPage(
  
  # Application title
  titlePanel(" Border Patrol Apprehensions "),
  

  
    # Show a plot of the generated distribution
    mainPanel(
      
     
      tabsetPanel(type = "tabs",
                tabPanel("Time Series Plot", plotOutput("plot")),
                tabPanel("2010 Bar Plot", plotOutput("barplot1")),
                tabPanel("2017 Bar Plot", plotOutput("barplot2")),
                tabPanel("Summary", verbatimTextOutput("summary")),
                tabPanel("Citation", tableOutput("Citation"))
                

    )
  )
)


# Define server logic required to draw plots
server <- function(input, output) {
  
  output$plot <- renderPlot({
    ts8 <- as.vector(t(Monthly[,-1]))
    ts9 <- ts(rev(ts8), start= c(2000,1), frequency=12)
    ts10 <- ts.plot(ts9, gpars=list(xlab="year", ylab="Apprehensions", lty=c(1:3)),col='purple')
    #let the Monthly data turns into a matrix
      ts11 <-as.matrix(Monthly)
    #the function to calculate the average of each year's apprehensions
      ts12 <- rev(sapply(1:18, function(i) sum(ts11[i,])/12))
      namebank <- as.character(c(2000:2017))
    #label the years and lines on the averaged position
     text(c(2000:2017), ts12, namebank,cex=0.9)
     text(c(2000:2017), ts12, labels="----", cex=0.9,pos=2, col="red" )
    
  })
  #Generate 2010 monthly bar plot 
  output$barplot1 <- renderPlot({
    barplot(as.matrix(A2010), names.arg = colnames(A2010), 
                 las=2,
                 axisnames=TRUE,
                 main="2010 Border Patrol Apprehensions by Sector",
                 ylab="Apprehensions",
                 xlab="Year",
                 border="light blue",
                 col="light blue")
      
  })
  #Generate 2017 monthly bar plot 
  output$barplot2 <- renderPlot({
    barplot(as.matrix(A2017), names.arg = colnames(A2017), 
            las=2,
            axisnames=TRUE,
            main="2017 Border Patrol Apprehensions by Sector",
            ylab="Apprehensions",
            xlab="Year",
            border="pink",
            col="pink")
    
  })
  # Generate a summary of the data ----
  output$summary <- renderPrint({
    HTML(
      '      Apprehension statistics record the number of foreigners who are caught while 
      illegally entering the United States. These people are charged with violation of the 
      Immigration and Nationality Act, and are subject to removal by the US Border Patrol. 
      According to CNN politics, US Border Patrol reported a historic low of apprehensions under
      the Trump administration. 
      Our project uses the 2010 and 2017 Illegal Alien Apprehension statistical report and the 
      Border Patrol Monthly Summary dataset to prove the decreasing number of apprehension across
      9 main sectors of the US. With simple statistical tests and data analytic methods in R, 
      our graphical analysis also shows the decreasing trend in apprehensions from 2000 to 2017.'
    
         )
      })

  # Generate a citation of the data ----
  output$Citation <- renderPrint({
    paste('Kopan, Tal. “US-Mexico Border Apprehensions Hit 17-Year Lows.” CNN, Cable News Network, 9 May 2017, www.cnn.com/2017/05/09/politics/border-crossings-apprehensions-down-trump/index.html.')
  })
  
  
}




# Run the application 
shinyApp(ui = ui, server = server)

