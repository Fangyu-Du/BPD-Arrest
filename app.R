library(shiny)
library(shinydashboard) 
library(shinydashboardPlus) 
library(dplyr)
library(stringr)
library(ggplot2)
library(readr)
library(leaflet)
library(DT)
library(plotly)


ui <- dashboardPagePlus(
    header = dashboardHeaderPlus(title = "BPD Arrest", 
                                 enable_rightsidebar = TRUE, rightSidebarIcon = "gears"),
    sidebar = dashboardSidebar(
        sidebarMenu(
            menuItem("Plotly", tabName = "page1", icon = icon("line-chart")),
            menuItem("Density", tabName = "page2", icon = icon("area-chart")),
            menuItem("Map", tabName = "page3", icon = icon("map-o")),
            menuItem("Data", tabName = "page4", icon = icon("fas fa-database"))
        )
    ),
    body = dashboardBody(
        tabItems(
            tabItem(tabName = "page1",
                    checkboxInput("holiday", label = "Show holidays", value = FALSE),
                    plotlyOutput("plot2", height = 500)
                    ),
            tabItem(tabName = "page2",
                    sliderInput("year", "Year:", min = 2014, max = 2020, value = 1, 
                                step = 1, animate = animationOptions(interval = 2000, loop = FALSE)),
                    plotOutput("plot1")
                    ),
            tabItem(tabName = "page3",
                    leafletOutput("myMap", width="100%")
                    ),
            tabItem(tabName = "page4",
                    DT::dataTableOutput("myTable")
            )
        )
    ),
    rightsidebar = rightSidebar(
        tags$a(href="https://data.baltimorecity.gov/Public-Safety/BPD-Arrests/3i3v-ibrt",
               target="_blank","Data Source")
    ),
    title = "DashboardPage"
)


server <- function(input, output, session) {

    data <- read_csv("data.csv")
    head(data)
    data$ArrestDate <- as.Date(data$ArrestDate,format = "%m/%d/%Y")
    data$Longitude <- round(data$Longitude,digits = 5)
    data$Latitude <- round(data$Latitude,digits = 5)
    
    #clean holiday data 
    holidays <- read_csv("usholidays.csv")
    holidays <- holidays %>% 
        select(-c(X1))
    #convert Date column to Date
    holidays$Date <-as.Date(holidays$Date)
    #create  abbreviations
    words=unique(holidays$Holiday)
    Abb=c("NYD","MLKB","WaB", "MeD", "InD", "LaD", "CoD", "VeD", "ThD", "ChD",
          "NYD","MLKB","WaB")
    holidays$Abb=holidays$Holiday
    for (i in 1:length(words)) {
        holidays$Abb=str_replace(holidays$Abb,words[i],Abb[i])
    }
    #the number of crimes
    data1 <-data %>% group_by(Date=ArrestDate) %>% summarise(N=n())
    #creat data_hol merged by two dataset
    data_hol <-merge(data1,holidays,all.x=TRUE)
    
    output$plot1 = renderPlot({
        newdata <- data %>%
            filter(as.numeric(format(ArrestDate,'%Y')) == input$year)
        
        ggplot(newdata,aes(color=Sex))+
            geom_density(aes(x=Age),size=1)+
            scale_color_discrete(labels = c("Female", "Male"))+
            annotate(geom="text",label=input$year,size=15,x=60,y=0.03,color="grey80")+
            labs(color="Gender",title="Age distribution of crimes reported within each gender")+
            xlim(10,70)+ylim(0,0.05)+
            theme(axis.text.y=element_blank(), axis.ticks.y = element_blank(),
                  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(),plot.title = element_text(size = rel(1)))
    })
    
    output$plot2 = renderPlotly({
        f <-ggplot(data_hol,aes(x=Date,y=N))+
            geom_line()+
            geom_smooth()+
            labs(y="Number of arrests",title="Arrests in Baltimore")
        # if(input$holiday==TRUE)
        #     {f=f+geom_point(data=subset(data_hol, !is.na(Holiday)), color="purple")+
        #     geom_text(data=subset(data_hol,!is.na(Holiday)), aes(x=Date, y=N, label=Abb))}else{
        #         f}
        ggplotly(f)
    })
    
    output$myMap = renderLeaflet({
        #find the frequency 
        loc_data= data %>%
            group_by(lng=round(Longitude,3),lat=round(Latitude,3)) %>%
            summarise(N=n())
        #creat the range of longtitude and latitude
        loc_data$latL<-loc_data$lat-0.0005
        loc_data$latH<-loc_data$lat+0.0005
        loc_data$lngL<-loc_data$lng-0.0005
        loc_data$lngH<-loc_data$lng+0.0005
        #create map on the leaflet
        m=loc_data %>% leaflet() %>% addTiles() %>%
            setView(-76.6,39.31, zoom=12) %>%
            addProviderTiles(providers$Stamen.Toner, group = "Toner")%>%
            addLayersControl(baseGroups = c("Toner", "OSM"),
                             options = layersControlOptions(collapsed = FALSE)) %>%
            addRectangles(
                lng1=~lngL, lat1=~latL,
                lng2=~lngH, lat2=~latH,
                fillOpacity = ~N/150, opacity = 0, fillColor = "red", label = ~N
            )#add rectangels
        
    })
    
    output$myTable = DT::renderDataTable({
        return(datatable(data, rownames= FALSE)) 
        
    })

}

shinyApp(ui = ui, server = server)
