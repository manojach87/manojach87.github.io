library(shiny)

library(dplyr)

library(leaflet)

library(DT)

shinyServer(function(input, output) {
  # Import Data and clean it
  
  bb_data <- read.csv("data/blood-banks.csv", stringsAsFactors = FALSE )
  bb_data1<- read.csv("data/data_mod.csv")
  
  bb_data <- data.frame(bb_data)
  bb_data1 <- data.frame(bb_data1)
  
  print(bb_data1)
  bb_data$Latitude <-  as.numeric(bb_data$Latitude)
  bb_data$Longitude <-  as.numeric(bb_data$Longitude)
  
  bb_data1$Latitude <-  as.numeric(bb_data1$lat)
  bb_data1$Longitude <-  as.numeric(bb_data1$lon)
  
  bb_data=filter(bb_data, Latitude != "NA") # removing NA values
  bb_data1=filter(bb_data1, Latitude != "NA") # removing NA values
  
  # new column for the popup label
  
  bb_data <- mutate(bb_data, cntnt=paste0('<strong>Name: </strong>',Blood.Bank.Name,
                                          '<br><strong>State:</strong> ', State,
                                          '<br><strong>Time:</strong> ', Service.Time,
                                          '<br><strong>Mobile:</strong> ',Mobile,
                                          '<br><strong>HelpLine:</strong> ',Helpline,
                                          '<br><strong>Contact1:</strong> ',Contact.No.1,
                                          '<br><strong>Contact2:</strong> ',Contact.No.2,
                                          '<br><strong>Contact3:</strong> ',Contact.No.3,
                                          '<br><strong>Contact4:</strong> ',Contact.No.4,
                                          '<br><strong>Contact5:</strong> ',Contact.No.5,
                                          '<br><strong>Contact6:</strong> ',Contact.No.6,
                                          '<br><strong>Contact7:</strong> ',Contact.No.7,
                                          '<br><strong>Email:</strong> ',Email,
                                          '<br><strong>Website:</strong> ',Website)) 

  bb_data1 <- mutate(bb_data1, cntnt=paste0(
    '<strong>name: </strong>',name,
    '<strong>imo: </strong>',imo,
    '<strong>mmsi: </strong>',mmsi,
    '<strong>callsign: </strong>',callsign,
    '<strong>length: </strong>',length,
    '<strong>width: </strong>',width,
    '<strong>dwt: </strong>',dwt,
    '<strong>grt: </strong>',grt,
    '<strong>nrt: </strong>',nrt,
    '<strong>timestamp_position: </strong>',timestamp_position,
    '<strong>source_position: </strong>',source_position,
    '<strong>lon: </strong>',lon,
    '<strong>lat: </strong>',lat,
    '<strong>speed: </strong>',speed,
    '<strong>course: </strong>',course,
    '<strong>heading: </strong>',heading,
    '<strong>nav_status: </strong>',nav_status,
    '<strong>timestamp_voyage: </strong>',timestamp_voyage,
    '<strong>time1: </strong>',time1,
    '<strong>source_voyage: </strong>',source_voyage,
    '<strong>draught: </strong>',draught,
    '<strong>destination: </strong>',destination,
    '<strong>country: </strong>',country,
    '<strong>eta: </strong>',eta,
    '<strong>time: </strong>',time,
    '<strong>aisshiptype: </strong>',aisshiptype,
    '<strong>shiptype: </strong>',shiptype,
    '<strong>dimA: </strong>',dimA,
    '<strong>dimB: </strong>',dimB,
    '<strong>dimC: </strong>',dimC,
    '<strong>dimD: </strong>',dimD,
    '<strong>type: </strong>',type))

  
  # create a color paletter for category type in the data file
  
  pal <- colorFactor(pal = c("#1b9e77", "#d95f02", "#7570b3"), domain = c("Charity", "Government", "Private"))
  pal1 <- colorFactor(pal = c("#1b9e77", "#d95f02"), domain = c("S-AIS","T-AIS"))
  
  # create the leaflet map  

  
  output$bbmap <- renderLeaflet({
    leaflet(bb_data1) %>% 
      addCircles(lng = ~Longitude, lat = ~Latitude) %>% 
      addTiles() %>%
      addCircleMarkers(data = bb_data1, lat =  ~Latitude, lng =~Longitude, 
                       radius = 3, popup = ~as.character(name), 
                       color = ~pal1(source_position),
                       stroke = FALSE, fillOpacity = 0.8)%>%
      addLegend(pal=pal1, values=bb_data1$source_position,opacity=1, na.label = "Not Available")%>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="ME",
        onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
  })
  
  #create a data object to display data
  
  output$data <-DT::renderDataTable(datatable(
      bb_data1[,c(-1,-35,-36,-37)],filter = 'top',
      colnames = c("","name","imo","mmsi","callsign","length","width","dwt","grt","nrt","timestamp_position","source_position",
                   "lon","lat","speed","course","heading","nav_status","timestamp_voyage","time1","source_voyage","draught","de
stination","country","eta","time","aisshiptype","shiptype","dimA","dimB","dimC","dimD","type")
  ))

  
})
