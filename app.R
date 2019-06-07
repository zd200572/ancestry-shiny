#install.packages("radmixture")
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
# download.file(url = 'https://github.com/wegene-llc/radmixture/raw/master/data/K7b.alleles.RData', destfile = 'K7b.alleles.RData')
# download.file(url = 'https://github.com/wegene-llc/radmixture/raw/master/data/globe4.4.F.RData', destfile = 'globe4.4.F.RData')


library(shiny)
library(radmixture)
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("祖源分析小工具"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       fileInput("file1", "Choose txt File",
                multiple = FALSE,
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".txt")),
      tags$hr(),
      selectInput("variable", "请选择一种参考数据模型:",
                  list("K4(亚、欧、非和美洲印第安人模型)" = "globe4.4-K4", 
                       "K7b（南亚、西亚、西伯利亚、非洲、Southern？、大西洋波罗的海和东亚人模型）" = "K7b.7-K7b", 
                       "world9（美洲印第安人、东亚、非洲、西伯利亚、大西洋波罗的海、澳大利亚、高佳索人模型）" = "world9.9-World9",
                       "E11（非洲、欧洲、印度、马来、傣族、彝族、中国东部和日本人模型）" = "e11.11-E11",
                       "K12b（格德罗西亚、西伯利亚、西北非、南亚、大西洋地中海、北欧、南亚、东非、东南亚、撒哈拉以南和东亚人模型）" = "K12b.12-K12b",
                       "K13（西伯利亚、美洲印第安人、西非、古非洲、西南亚、东亚、地中海、澳大利亚、北极附近、西亚、北欧、南亚和东非人模型）" = "globe13.13-K13"
                  ))
      #checkboxInput("outliers", "Show outliers", FALSE)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h3("你的祖源分析结果为:"),
      h4("这可能要花上一两分钟，请耐心等待，谢谢！。。。"),
      tableOutput(outputId = 'test')
  )
))

# Define server logic required to calculate ancestry
server <- function(input, output) {
  options(shiny.maxRequestSize=30*1024^2)
  #data1 <- 'globe4.4.alleles.RData'#input$var1.alleles.RData
  #load(data1)
  #data2 <- 'globe4.4.F.RData'#input$var1.F.RData
  #load(data2)
  output$test <- renderTable({ 
    "You have selected this"
    inFile <- input$file1
    if(is.null(inFile))     
      return(NULL) 
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 0, 
                 expr = {
                   for (i in 1:15) {
                     incProgress(1/15)
                     Sys.sleep(0.25)
                   }
                 })
  d <- unlist(strsplit(input$variable, '[.]'))
  d0 <- d[1]
  d1 <- unlist(strsplit(d[2], "[-]"))[1]
  d2 <- unlist(strsplit(input$variable, '[-]'))[1]
  d3 <- unlist(strsplit(d[2], "[-]"))[2]
  d4 <- paste(d0,sep = "",".alleles")
  d5 <- paste(d2,sep = "",".F")
  load(paste(d4,sep="",".RData"))
  load(paste(d5,sep="",".RData"))
  res <- tfrdpub(read.table(inFile$datapath), d1, get(d4), get(d5))
  ances <- fFixQN(res$g, res$q, res$f, tol = 1e-4, method = "BR", pubdata = d3)
  ances$q
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
