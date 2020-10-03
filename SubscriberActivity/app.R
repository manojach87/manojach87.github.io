# install.packages("shiny")
# install.packages("quantmod")
# install.packages("shinythemes")

library("shinythemes")
library(plotly)
library(shiny)
library("quantmod")
library("shiny")
library("ggplot2")
library("plotly")
#install.packages('forecast')
#install.packages("zoo")

#install.packages("imputeTS")

# test if there is at least one argument: if not, return an error
#if (length(args)==0) {
#  stop("At least one argument[ ENV = DEV or QA or PRD] must be supplied", call.=FALSE)
#}

#ENV=args[1]

#print(paste("ENV=",ENV,sep=""))



library(pracma)
library(forecast)
library(dplyr)
# Read data+
library(readxl)
library(lubridate)
library (RODBC)
library(sqldf)
library(ggplot2)
library(imputeTS)

library(rmarkdown)

currentDate<-format(Sys.Date(), "%Y%m%d")
#dfFile<-paste("./data/df.",currentDate,".csv",sep="")
dfFile<-paste("./data/summary.csv",sep="")
#df<-read.csv(dfFile)
#df$Id=NULL
#write.csv(df,dfFile, row.names = FALSE)
mask<-TRUE

doMasking<-function(df){
  # df$Company<-ifelse(df$Company == 'BHN',"Company 1",
  #                     ifelse(df$Company == 'CHR',"Company 2",
  #                            ifelse(df$Company == 'TWC',"Company 3",df$Company)))
  # 
  #df$Account_Class<-ifelse(df$Account_Class == 'Resi',"Residential",
  #                         ifelse(df$Account_Class == 'SMB',"Commercial",df$Account_Class))
  df$Company<-as.factor(df$Company)
  df$Account_Class<-as.factor(df$Account_Class)
  return(df)
}

readFile<-function(dfFile){
  if(file.exists(dfFile)){
    df<-read.csv(dfFile)
    if(mask==TRUE){
      df<-doMasking(df)
    }
    df$CalDate <- as.Date(df$CalDate)
  } else
  {
    
    
  }
  return(df)
}

df<-readFile(dfFile)
df$Id=NULL
df$MSO=NULL
datesDf<-data.frame(seq.Date(min(df$CalDate), max(df$CalDate), by = "days"))
names(datesDf) <-c("CalDate")



distinctAttributes<-distinct(df,
                             MetricDesc
                             , Company
                             , Service
                             , Account_Class
)
# Create Missing Records
CleanDataDf<-merge(merge(distinctAttributes,datesDf,all=TRUE),df,all=TRUE)


#replace Null by Zero

CleanDataDf[is.na(CleanDataDf)] <- 0

summary(CleanDataDf)
summary(df)

dfOriginal<-df

df<-CleanDataDf

readFileAndFilter<-function(dfFile, MetricDesc, Company, Service, Account_Class){
  #df<-readFile(dfFile)
  
  return(df[df$MetricDesc == MetricDesc 
            & df$Company==Company 
            & df$Service == Service 
            & df$Account_Class == Account_Class,]
  )
}



#df$Company <- df$MSO





#print(df)

mockDays<-30

finalDf<-data.frame()

allModelDf<-data.frame()
gg_list <<- list()

#runModel("Upgrades", "Large Company", "Video", "SMB")

runModel<-function(MetricDesc, Company, Service, Account_Class){
  if(!(is.null(MetricDesc) | is.null(Company) | is.null(Service) | is.null(Account_Class)))
  {
    print(paste("These Are not Null ", MetricDesc, Company, Service, Account_Class,sep="|"))
    
    if(is.null(gg_list[[paste(MetricDesc, Company, Service, Account_Class,sep="|")]])==TRUE)
    {
      #print(gg_list[["Downgrade|BHN|Internet|SMB"]])
      print(paste("Running Model for ", MetricDesc, Company, Service, Account_Class,sep="|"))
      # df<-read.csv("./data/df.csv")
      # df$CalDate <- as.Date(df$CalDate)
      # data<-df[df$MetricDesc == MetricDesc & df$Company==Company & df$Service == Service & df$Account_Class == Account_Class,]
      
      data<-readFileAndFilter(dfFile, MetricDesc, Company, Service, Account_Class)
      
      #print(data)
      
      par(mfrow = c(1,1))
      data$Date <- as.Date(data$CalDate)
      
      MinCalDate <- min(data$CalDate)
      MaxCalDate <- max(data$CalDate)
      
      print(paste(MinCalDate, MaxCalDate))
      
      #data$ma      <- movavg(data$Metric, 50, "s")
      data$mean    <- mean (data$Metric)
      data$sd      <- sd   (data$Metric)
      data$Metric1 <- ifelse(data$Metric>data$mean+3*data$sd | data$Metric<data$mean-3*data$sd,data$mean,data$Metric)
      ## Forecasting with tbats
      msts <- msts(data$Metric1,seasonal.periods = c(7,30,365),start = decimal_date(MinCalDate))
      #print("Test")
      #train <- window(msts,start=decimal_date(MinCalDate), end=decimal_date(MaxCalDate-mockDays-1))
      train <- window(msts,start=decimal_date(MinCalDate), end=decimal_date(MaxCalDate-mockDays-1))
      #print("Test2")
      #test <- window(msts, start=decimal_date(MaxCalDate-mockDays), end=decimal_date(MaxCalDate))
      
      # jpeg(paste("./data/plots20191111/rplot",Account_Class,Company,gsub("/", "", MetricDesc),Service,"__.jpg",sep="_"), width = 1280, height = 768)
      
      plot(msts)
      #print("Test3")
      
      #plot(data$Metric, type = "l", col = "red")
      #lines(data$Metric1)
      
      # dev.off()
      
      s <- tbats(train)
      sp<- predict(s,h=33)
      
      
      sp <- data.frame(sp)
      sp$MetricDesc    <-MetricDesc
      sp$Company      <-Company
      sp$Service           <-Service
      sp$Account_Class <-Account_Class
      ##sp$Dates         <-format(date_decimal(row.names(sp$Dates)), "%Y-%m-%d")
      sp$Dates         <-round_date(date_decimal(as.numeric(row.names(sp))),"day")
      sp$CalDate         <-round_date(date_decimal(as.numeric(row.names(sp))),"day")
      
      
      # 1. Open jpeg file
      #jpeg(paste("./data/plots20191111/rplot",Account_Class,Company,gsub("/", "", MetricDesc),Service,".jpg",sep="_"), width = 1280, height = 768)
      # 2. Create the plot
      #plot(sp, main = paste(MetricDesc,"TBATS Forecast For",Company,Service,Account_Class), include=30)
      #lines(msts)
      
      
      XXXX<-subset.data.frame(data, CalDate>MaxCalDate-90)
      
      XXXX=merge(XXXX,sp,all=TRUE)
      
      XXXX$Point.Forecast<-ifelse(
        sapply(XXXX$Point.Forecast, function(x) all(is.na(x))) == TRUE 
        ,XXXX$Metric
        ,XXXX$Point.Forecast)
      
      XXXX$Lo.80<-ifelse(
        sapply(XXXX$Lo.80, function(x) all(is.na(x))) == TRUE 
        ,XXXX$Metric
        ,XXXX$Lo.80)
      
      XXXX$Hi.80<-ifelse(
        sapply(XXXX$Hi.80, function(x) all(is.na(x))) == TRUE 
        ,XXXX$Metric
        ,XXXX$Hi.80)
      
      XXXX$Lo.95<-ifelse(
        sapply(XXXX$Lo.95, function(x) all(is.na(x))) == TRUE 
        ,XXXX$Metric
        ,XXXX$Lo.95)
      
      XXXX$Hi.95<-ifelse(
        sapply(XXXX$Hi.95, function(x) all(is.na(x))) == TRUE 
        ,XXXX$Metric
        ,XXXX$Hi.95)
      
      
      
      #print(sp)
      fig <- plot_ly(XXXX
                     , x = ~CalDate
                     , y = ~Point.Forecast
                     , name = 'Forecast', type = 'scatter', mode = 'lines', showlegend = TRUE,
                     line = list(color = 'rgb(10,20,190)', width = 2))
      
      fig <- fig %>% add_trace(data = XXXX, y = ~Lo.80, type = 'scatter', mode = 'lines',
                               fill = 'tonexty', fillcolor='rgba(0,80,100,0.5)', line = list(color = 'transparent'),
                               showlegend = TRUE, name = 'Lo.80') 
      
      fig <- fig %>% add_trace(data = XXXX, y = ~Hi.80, type = 'scatter', mode = 'lines',
                               fill = 'tonexty', fillcolor='rgba(0,80,100,0.5)', line = list(color = 'transparent'),
                               showlegend = TRUE, name = 'Hi.80') 
      
      fig <- fig %>% add_trace(data = XXXX, y = ~Lo.95, type = 'scatter', mode = 'lines',
                               fill = 'tonexty', fillcolor='rgba(0,80,100,0.2)', line = list(color = 'transparent'),
                               showlegend = TRUE, name = 'Lo.95') 
      
      fig <- fig %>% add_trace(data = XXXX, y = ~Hi.95, type = 'scatter', mode = 'lines',
                               fill = 'tonexty', fillcolor='rgba(0,80,100,0.2)', line = list(color = 'transparent'),
                               showlegend = TRUE, name = 'Hi.95') 
      
      fig <- fig %>% add_trace(data = XXXX, y = ~Metric, type = 'scatter', mode = 'lines',
                               line = list(color = 'rgb(12, 12, 24)', width = 2),
                               showlegend = TRUE, name = 'Actual') 
      
      #fig

      #str(sp)
      # gg_list[[paste(MetricDesc, Company, Service, Account_Class,sep="|")]]<<-ggplot( data = sp) +
      #   geom_line(aes(Dates, Point.Forecast),color="blue") +
      #   geom_line(data = subset.data.frame(data, CalDate>MaxCalDate-90), aes(x=as.POSIXct(CalDate, format="%Y-%m-%d"), y=Metric)) +
      #   geom_ribbon(aes(x=Dates,ymin=Lo.80,ymax=Hi.80), fill="blue", alpha="0.3") +
      #   geom_ribbon(aes(x=Dates,ymin=Lo.95,ymax=Hi.95), fill="blue", alpha="0.2") +
      #   #geom_ribbon(aes(x=Dates,ymin=Lo.99,ymax=Hi.99), fill="blue", alpha="0.1") +
      #   ggtitle(label = paste("TBATS 30 day Forecast - ", Company)) +
      #   labs(y = paste(Account_Class,Service, MetricDesc))
      gg_list[[paste(MetricDesc, Company, Service, Account_Class,sep="|")]]<<-fig
      
      # p<-plot_ly(sp, x = ~Dates, y = ~Hi.80, type = 'scatter', mode = 'lines',
      #         line = list(color = 'transparent'),
      #         showlegend = FALSE, name = 'High')%>%
      #   add_trace(y = ~Lo.80, type = 'scatter', mode = 'lines',
      #             fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', line = list(color = 'transparent'),
      #             showlegend = FALSE, name = 'Low') %>%
      #   add_trace(y = ~Hi.95, type = 'scatter', mode = 'lines',
      #             fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', line = list(color = 'transparent'),
      #             showlegend = FALSE, name = 'Low') %>%
      #   add_trace(y = ~Lo.95, type = 'scatter', mode = 'lines',
      #             fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', line = list(color = 'transparent'),
      #             showlegend = FALSE, name = 'Low') %>%
      #   add_trace(x = ~Dates, y = ~Point.Forecast, type = 'scatter', mode = 'lines',
      #             line = list(color='rgb(0,100,80)'),
      #             name = 'Forecast')  %>%
      #   add_trace(subset.data.frame(data, CalDate>MaxCalDate-90), x = ~CalDate, y = ~Metric, type = 'scatter', mode = 'lines',
      #             line = list(color='rgb(0,100,80)'),
      #             name = 'Forecast') 
      # print(p)
      
    }
    print(gg_list[[paste(MetricDesc, Company, Service, Account_Class,sep="|")]])
    #modelDf<-data.frame(MetricDesc, Company, Service, Account_Class,p)
    #names(modelDf)<-c("Downgrade","BHN","Internet","SMB","Model")
    # 3. Close the file
    #dev.off()
    #allModelDf <- rbind(allModelDf,modelDf)
  }
  else{
    print(paste("Null Found ", MetricDesc, Company, Service, Account_Class,sep="|"))
  }
}


#runApp("stockVis")

loadCounter = 0

#df <-read.csv("./data/minutes_per_view_stb.csv")
###########################

#df$CalDate<-as.Date(as.character(df$CalDate), "%Y%m%d")
###########################
x=unique(df$Company)

returnService<-function(Company){
  #ServiceList <= 
  if(Company == "")
    return(unique(df$Service))
  else
    return(unique(df[df$Company==Company,]$Service))
}

returnAccountClass<-function(Company,Service){
  if(Company == "" && Service == "")
    return(unique(df$Account_Class))
  else
    return(unique(df[df$Company==Company & df$Service == Service,]$Account_Class))
}


returnMetricDesc<-function(Company,Service, Account_Class){
  if(Company == "" && Service == "" && Account_Class == "" )
    return(unique(df$MetricDesc))
  else
    return(unique(df[df$Company==Company & df$Service == Service & df$Account_Class == Account_Class,]$MetricDesc))
}

# returnMetricDesc(Company, Service, Account_Class)

# df1<- df[df$Company=='TWC' & df$Account_Class == 'All' & df$Service == 'Albany et al, NY',]
# df1<- df[df$Company=='TWC' & df$Account_Class == 'All' & df$Service == 'Nashville, TN',]

#summary(df1$Metric)

#paste(df1$q1," = ",df1$q3)

plot.diagram<-function(MetricDesc, Company, Service, Account_Class){
  # ggplot(df2, aes(x = CalDate)) +
  #   geom_line(aes(y = Metric), colour="black") +
  #   #geom_line(aes(y = LCL), colour = "red") +
  #   #geom_line(aes(y = UCL), colour = "red") +
  #   #geom_line(aes(y = q1), colour = "orange") +
  #   #geom_line(aes(y = q3), colour = "orange") +
  #   ylab(label=unique(df2$Company)) +
  #   xlab(paste("Week\n",unique(df2$Company), "\n" ,unique(df2$Service), "\n" ,unique(df2$Account_Class)))
  runModel(MetricDesc, Company, Service, Account_Class)
}



# User interface ----
ui <- fluidPage(
  theme = shinytheme("darkly"),
  includeMarkdown(file.path("rmd","about.rmd")),
  
  #titlePanel("Subscriber Activity Report"),
  
  sidebarLayout(
    sidebarPanel(
      #helpText("Select Company"),
      selectInput("Company", "Select Company:",as.list(x)),
      selectInput("Service", "Select Service:",returnService("")),
      selectInput("AccountClass", "Select Account Class:",returnAccountClass("","")),
      selectInput("MetricDesc", "Select MetricDesc:",returnMetricDesc("","",""))
    ),
    mainPanel(plotlyOutput("plot"))
  ),   
  verbatimTextOutput("value")
)

server <- function(input, output, session) {
  observe({
    x <- input$Company
    
    # Can use character(0) to remove all choices
    if (is.null(x))
      x <- character(0)
    
    # Can also set the label and select items
    updateSelectInput(session, "Service",
                      label = "Service",
                      choices = returnService(x)
                      #,selected = "All"
    )
    
  })
  
  observe({
    y <- input$Service
    
    # Can use character(0) to remove all choices
    if (is.null(y))
      y <- character(y)
    
    # Can also set the label and select items
    
    updateSelectInput(session, "AccountClass",
                      label = "Account Class",
                      choices = returnAccountClass(x,y)
                      #,selected = "All"
    )
  })
  
  observe({
    x <- input$Company
    y <- input$Service
    z <- input$AccountClass
    
    # Can use character(0) to remove all choices
    if (is.null(z))
      z <- character(z)
    
    # Can also set the label and select items
    
    updateSelectInput(session, "MetricDesc",
                      label = "Metric Desc",
                      choices = returnMetricDesc(x,y,z)
                      #,selected = "All"
    )
  })
  
  output$plot  <- renderPlotly({
    #df1 <- df[df$Company==input$Company & df$Account_Class == input$AccountClass & df$Service == input$Service,]
    print(paste(input$MetricDesc, input$Company, input$Service, input$AccountClass))
    plot.diagram(input$MetricDesc, input$Company, input$Service, input$AccountClass)
  })
}


# Run the app
shinyApp(ui, server)
