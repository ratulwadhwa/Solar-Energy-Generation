library(readr)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(leaflet)
library(ggmap)
library(shiny)
library(plotly)

PE2_Solar_Data_Generation_2020_Full <- read_csv("DATA/PE2_Solar_Data_Generation_2020_Full.csv")
PE2_Solar_Panels <- read_csv("DATA/PE2_Solar_Panels.csv")

total_energy_map <- PE2_Solar_Data_Generation_2020_Full %>%
  group_by(BuildingNum, MonSolarMeter)%>%
  summarise(`Real Energy Into the Load (kWh)` = sum(`Real Energy Into the Load (kWh)`, na.rm = TRUE) )

map <- merge(y = total_energy_map, x = PE2_Solar_Panels, by = "BuildingNum", all.x = TRUE)

# shiny

ui <- fluidPage(
  titlePanel(h1("Solar Energy Generation in Monash University Clayton Campus", align = "center", style = "font-size: 30px;")),
  fluidRow(
    column(width=1),
    column(width=4,
           fluidRow(tags$h2(strong("VIS 1: Top 5 energy generating solar panels on buildings", style = "font-size:18px;"))),
           fluidRow("The Top 5 buildings with most energy generating solar panels are N1 Carpark, Learning and Teaching Building, Sport, Robert Blackwood Hall and East_41, with N1 Carpark leading the bar plot with maximum energy of 814,844kwh. The spatial map on the right shows the location of all the solar panels on 27 buildings. The Slider bar can be used to filter with respect to the capacity(kW) of the panels. Additionally the tooltip is an user interface element which displays the name and the total energy generated for that building.", style = "font-size:15px; text-align:justify; background-color:#F4FAFB;"),
           fluidRow(style = "height:50px;"),
           fluidRow(plotOutput("bar_chart")),
           fluidRow(tags$h2(strong("VIS 2: Energy generation throughout 2020", style = "font-size:18px;"))),
           fluidRow("The line chart on the right provides the information on energy generated in the top building throughout the year. It can be observed that all top 5 buildings follow a similar trend. More energy was generated during summertime, that is November to January followed by a decrease around May and June. It is perhaps because May and June are in the winter season in Australia. The generation was enormously decreased, due to the shorter daytime and less sunlight in winter, the generation was enormously decreased.. Carpark West provided the maximum energy throughout the year in comparison to the other four building.", style = "font-size:15px; text-align:justify; background-color:#F4FAFB;")),
    column(width = 1),
    column(width = 6,
           fluidRow(tags$h1(strong("Spatial Positions of all 27 buildings", style = "font-size:18px;"))),
           fluidRow (leafletOutput("mymap", height = "450px"),
                     absolutePanel(top = 0, right = 10,
                                   sliderInput("range", "Capacity (kW)", min(PE2_Solar_Panels$`Capacity (kW)`), max(PE2_Solar_Panels$`Capacity (kW)`),
                                               value = range(PE2_Solar_Panels$`Capacity (kW)`), step = 0.1))),
           fluidRow(plotOutput("line_plot")))
  ))



server <- function(input, output, session) {
  
  output$bar_chart <- renderPlot({
    total_energy <- PE2_Solar_Data_Generation_2020_Full %>%
      group_by(BuildingNum, MonSolarMeter)%>%
      summarise(`Real Energy Into the Load (kWh)` = sum(`Real Energy Into the Load (kWh)`, na.rm = TRUE) )%>%
      arrange(desc(`Real Energy Into the Load (kWh)`))%>%
      head(5)
    ggplot(total_energy,  aes(x= MonSolarMeter , y = `Real Energy Into the Load (kWh)`, fill= BuildingNum))+
      geom_col()+
      scale_y_continuous(labels = scales::comma)+
      theme_solarized()+
      theme(axis.text.x = element_text())+
      coord_flip()+
      theme(legend.position="none")
    
  })
  
  observeEvent(input$range, {
    map <- map %>% filter(`Capacity (kW)` >= input$range[1] & `Capacity (kW)` <= input$range[2])
    m <- leaflet(map) %>% addTiles() 
    
    output$mymap <- renderLeaflet({ m %>%  addCircleMarkers(lng = ~ map$Longitude, lat = ~map$Latitude, radius = ~map$`Real Energy Into the Load (kWh)`/10000, color = "#000000",
                                                            popup = paste("Building:", map$BuildingName, "<br>",
                                                                          "Energy (kWh):", map$`Real Energy Into the Load (kWh)`, "<br>"),
                                                            
                                                            label = paste("Building:", map$BuildingName, 
                                                                          ", Energy (kWh):", map$`Real Energy Into the Load (kWh)`))
    })
  })
  
  output$line_plot <- renderPlot({
    total_energy <- PE2_Solar_Data_Generation_2020_Full
    total_energy$Year <- format(total_energy$Timestamp, format="%Y")
    total_energy$Month <- format(total_energy$Timestamp, format="%m")
    total_energy %>%
      filter(BuildingNum %in%c("80","92","1","2" ,"41"))%>%
      group_by(BuildingNum, MonSolarMeter, Year, Month)%>%
      summarise(`Real Energy Into the Load (kWh)` = sum(`Real Energy Into the Load (kWh)`, na.rm = TRUE) )%>%
      
      ggplot(aes(x= Month , y = `Real Energy Into the Load (kWh)`, color= MonSolarMeter, group=MonSolarMeter))+
      geom_line()+
      theme_solarized()+
      
      theme(legend.position="bottom")+
      guides(color = guide_legend(nrow = 3, byrow = TRUE))+
      theme(legend.background = element_rect(fill="black",
                                             size=0.5, linetype="solid", 
                                             colour ="black"))+
      theme(legend.title = element_text(size = 10, color= "white"),
            legend.text = element_text(size = 12, color= "white"))
  })
}

shinyApp(ui, server)