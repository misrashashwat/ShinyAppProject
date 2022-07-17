library(shiny)
library(mlbench)
library(MASS)
library(pROC)
library(randomForest)
library(ROCR)
library(rsconnect)
library(verification)
ui <- fluidPage(
  headerPanel('Random Forest Model for Testing Diabetes'),
  sidebarPanel(
    sliderInput(inputId = "node",label = 'Choose number of max nodes:',step = 1,value = 5,min = 1, max = 50),
    sliderInput(inputId = "tree",label = "choose Number of trees:",step=10, value = 1000,min = 10, max = 1000),
  ),
  mainPanel(
    plotOutput('plot1',  width = "100%"),
    plotOutput('plot2',  width = "100%"),
    htmlOutput("printacc"),
    tableOutput('table')
  )
)

server <- function(input, output) {
 
  db<- read.csv("./diabetes.csv") 
  newdb <- na.omit(db)
  newdb$outcome[newdb$outcome=="true"]<-1
  newdb$outcome[newdb$outcome=="false"]<-0
  newdb$outcome <- as.factor(newdb$outcome)
  rnd<- reactive({randomForest(data=newdb,outcome~.,ntree=input$tree, maxnodes=input$node)})
  output$plot1= renderPlot({
    plot(rnd())
    rnd.legend <- if (is.null(rnd()$test$err.rate)) {colnames(rnd()$err.rate)}
    else {colnames(rnd()$test$err.rate)}
    legend("top", cex =1, legend=rnd.legend, lty=c(1,2,3), col=c(1,2,3), horiz=T)
    })
  output$plot2= renderPlot({
  rf.predicted<- predict(rnd(),type = 'prob',predict.all=TRUE)
  th<-seq(0,1,0.001)
  roc.plot(x=newdb$outcome == "1", pred =rf.predicted[,2] ,thresholds = th)$roc.vol})
  output$printacc <- renderUI({ 
  options('digits'=3)
  cmatrix=rnd()$confusion[,0:2]
  t=(cmatrix[1,1]+cmatrix[1,2]+cmatrix[2,1]+cmatrix[2,2])
  p=(cmatrix[1,1]+cmatrix[2,2])
  acc<-round(p/t,2)
  s1<-paste("The accuracy of the model is", acc)
  s2<-paste("The Class Error Values are")
  HTML(paste(s1, s2, sep = '<br/>'))
  })
  output$table <- renderTable(as.table(rnd()$confusion))

}

shinyApp(ui = ui, server = server)

