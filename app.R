#install.packages("radmixture")
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
# download.file(url = 'https://github.com/wegene-llc/radmixture/raw/master/data/globe4.alleles.RData', destfile = 'globe4.alleles.RData')
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
                  list("K4(亚、欧、非和美洲印第安人模型)" = "globe4.4", 
                       "K7b（南亚、西亚、西伯利亚、非洲、Southern？、大西洋波罗的海和东亚人模型）" = "K7b", 
                       "world9（美洲印第安人、东亚、非洲、西伯利亚、大西洋波罗的海、澳大利亚、高佳索人模型）" = "world9",
                       "E11（非洲、欧洲、印度、马来、傣族、彝族、中国东部和日本人模型）" = " E11",
                       "K12b（格德罗西亚、西伯利亚、西北非、南亚、大西洋地中海、北欧、南亚、东非、东南亚、撒哈拉以南和东亚人模型）" = "K12b",
                       "K13（西伯利亚、美洲印第安人、西非、古非洲、西南亚、东亚、地中海、澳大利亚、北极附近、西亚、北欧、南亚和东非人模型）" = "K13"
                  ))
      #checkboxInput("outliers", "Show outliers", FALSE)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h4("你的祖源分析结果为:"),
      tableOutput(outputId = 'test')
  )
))

# Define server logic required to calculate ancestry
server <- function(input, output) {
  options(shiny.maxRequestSize=30*1024^2)
  data1 <- 'globe4.4.alleles.RData'#input$var1.alleles.RData
  load(data1)
  data2 <- 'globe4.4.F.RData'#input$var1.F.RData
  load(data2)
  output$test <- renderTable({ 
    "You have selected this"
    inFile <- input$file1
    if(is.null(inFile))     
      return(NULL) 
  res <- tfrdpub(read.table(inFile$datapath), 4, globe4.alleles, globe4.4.F)
  ances <- fFixQN(res$g, res$q, res$f, tol = 1e-4, method = "BR", pubdata = "K4")
  ances$q
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
