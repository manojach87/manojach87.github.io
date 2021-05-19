install.packages("shiny")
install.packages("quantmod")

library("quantmod")
library("shiny")
library("ggplot2")

#runApp("stockVis")

loadCounter = 0
# Read the source file
df <-read.csv("./data/minutes_per_view_stb.csv")
df$brdcstwkenddtsk<-as.Date(as.character(df$brdcstwkenddtsk), "%Y%m%d")
x=unique(df$mso)

returnDMA<-function(mso){
  #dmaList <= 
  if(mso == "")
    return(unique(df$dma_nm))
  else
    return(unique(df[df$mso==mso,]$dma_nm))
}

returnSpectrumGuide<-function(mso,dma){
  if(mso == "" && dma == "")
    return(unique(df$spectrum_guide))
  else
    return(unique(df[df$mso==mso & df$dma_nm == dma,]$spectrum_guide))
}


# df1<- df[df$mso=='TWC' & df$spectrum_guide == 'All' & df$dma_nm == 'Albany et al, NY',]
# df1<- df[df$mso=='TWC' & df$spectrum_guide == 'All' & df$dma_nm == 'Nashville, TN',]

#summary(df1$pct)

#paste(df1$q1," = ",df1$q3)

plot.diagram<-function(df2){
  ggplot(df2, aes(x = brdcstwkenddtsk)) + 
    geom_line(aes(y = pct), colour="black") + 
    geom_line(aes(y = LCL), colour = "red") + 
    geom_line(aes(y = UCL), colour = "red") + 
    geom_line(aes(y = q1), colour = "orange") +
    geom_line(aes(y = q3), colour = "orange") + 
    ylab(label="Minutes") + 
    xlab(paste("Week\n",unique(df2$mso), "\n" ,unique(df2$dma_nm), "\n" ,unique(df2$spectrum_guide)))
}



# User interface ----
ui <- fluidPage(
  titlePanel("Minutes Per View - STB"),
  
  sidebarLayout(
    sidebarPanel(
      #helpText("Select MSO"),
      selectInput("MSO", "Select MSO:",as.list(x)
                  #c("Cylinders" = "cyl",
                  #  "Transmission" = "am",
                  #  "Gears" = "gear")
      ),
      selectInput("DMA", "Select DMA:",returnDMA("")
                  #c("Cylinders" = "cyl",
                  #  "Transmission" = "am",
                  #  "Gears" = "gear")
      ),
      selectInput("SpectrumGuide", "Select Spectrum Guide:",returnSpectrumGuide("","")
                  #c("Cylinders" = "cyl",
                  #  "Transmission" = "am",
                  #  "Gears" = "gear")
      )
    ),
    mainPanel(plotOutput("plot"))
  ),   
  verbatimTextOutput("value")
)



server <- function(input, output, session) {
  observe({
    x <- input$MSO

    # Can use character(0) to remove all choices
    if (is.null(x))
      x <- character(0)

    # Can also set the label and select items
    updateSelectInput(session, "DMA",
                      label = "DMA",
                      choices = returnDMA(x)
                      ,selected = "All"
    )

  })
  
  observe({
    y <- input$DMA
    
    # Can use character(0) to remove all choices
    if (is.null(y))
      y <- character(y)

    # Can also set the label and select items

    updateSelectInput(session, "SpectrumGuide",
                      label = "Spectrum Guide",
                      choices = returnSpectrumGuide(x,y)
                      #,selected = "All"
    )
  })
  
  output$plot  <- renderPlot({
    df1 <- df[df$mso==input$MSO & df$spectrum_guide == input$SpectrumGuide & df$dma_nm == input$DMA,]
    plot.diagram(df1)
  })
  
}


# Run the app
shinyApp(ui, server)
