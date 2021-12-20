library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(DT)
library(data.table)
library(shinyalert)
library(dashboardthemes)
Tb=read.csv("D:/aafaq/datascience/Travelbehaviour.csv",header=T)
Tb=Tb %>% mutate(Reason_Travel=case_when(Reason.for.travel %in% c("Umrah",
                                                                    "Family Vacation",
                                                                    "Getaway(Vacation)",
                                                                    "Bachelor ",
                                                                    "Honeymoon",
                                                                    "Family vacation is best and it's get a chance to adore Nature to see the creation of Allah ",
                                                                    "Valentine's Day",
                                                                    "Marriage ")~"personal",
                                           Reason.for.travel %in% c("With friends.. ",
                                                                    "Friends (vacation)",
                                                                    "Friends vacation",
                                                                    "Travel with friends",
                                                                    "Bike rides",
                                                                    "Trips",
                                                                    "With friends ")~"Friends",
                                           Reason.for.travel %in% c("Business",
                                                                    "Job",
                                                                    "Education",
                                                                    "Flim Shouting ",
                                                                    "Flim shouting",
                                                                    "Coaching",
                                                                    "Sports",
                                                                    "Coach",
                                                                    "Flim Shooting ")~"professional"))

Tb=Tb %>% mutate(Season_Travel=case_when(Month.Of.Travel=="January-February"~"winter",
                                         Month.Of.Travel=="January-February"~"winter",
                                         Month.Of.Travel=="November-December"~"winter",
                                         Month.Of.Travel=="September-October"~"rain",
                                         Month.Of.Travel=="September-October"~"rain",
                                         Month.Of.Travel=="Feburay-March "~"spring",
                                         Month.Of.Travel=="Feburay-March"~"spring",
                                         Month.Of.Travel=="May-June"~"rain" ,
                                         Month.Of.Travel==" March-April"~"summer",
                                         Month.Of.Travel=="July-August" ~"spring",
                                         Month.Of.Travel=="March-April"~"summer"))
Tb$Mode.of.transport=gsub("Flight ","Flight",Tb$Mode.of.transport,ignore.case=T)
Tb=Tb %>% filter(Over.all.Budget>1000&Over.all.Budget<=100000)
Tb$previous.budget=Tb$previous.budget/1000
Tb$Over.all.Budget=Tb$Over.all.Budget/1000
Tb=Tb %>% mutate(Hotel.Room.Preference=replace(Hotel.Room.Preference,Hotel.Room.Preference=="Roll- Away","Roll- Away Bed"))
Tb=Tb %>% mutate(No.of.Adults=replace(No.of.Adults,No.of.Adults=="5","More than 4"))
Tb$Gender=as.factor(Tb$Gender)
Tb$Mode.of.transport=as.factor(Tb$Mode.of.transport)
Tb$Reason.for.travel=as.factor(Tb$Reason.for.travel)
Tb$Month.Of.Travel=as.factor(Tb$Month.Of.Travel)
Tb$Traveler.Type=as.factor(Tb$Traveler.Type)
Tb$Type.of.accommodation=as.factor(Tb$Type.of.accommodation)
Tb$Hotel.Room.Preference=as.factor(Tb$Hotel.Room.Preference)
Tb$No.of.Adults=as.factor(Tb$No.of.Adults)
Tb$No.of.childrens=as.factor(Tb$No.of.childrens)
Tb$Place.preference=as.factor(Tb$Place.preference)
Tb$No.of.days=as.factor(Tb$No.of.days)
Tb$Reason_Travel=as.factor(Tb$Reason_Travel)
Tb$Season_Travel=as.factor(Tb$Season_Travel)
ui<-dashboardPage(skin="green",
  dashboardHeader(title="Travel Behaviour "),
  dashboardSidebar(sidebarMenu(id = "tab1",selected="home",
                               menuItem("Home",tabName="home"),
                               menuItem("Travel Preference", tabName = "Travel"),
                               menuItem("Stay preference", tabName = "stay"),
                               menuItem("Spend", tabName = "spend"))),
  
  dashboardBody(tags$head( 
    tags$style(HTML(".main-sidebar { font-size: 24px; }")) 
  ),
    tabItems(
      tabItem("home",
              fluidRow(column(12, h2("Welcome to the Presentation of Travel Behaviour"))),
              
              
              fluidRow(column(12, box(radioButtons(inputId = "radio1",selected = character(0),
                                                   label = "Please choose for Travel Preference  stay preference  or  or spend",
                                                   choices = c("Travel Preference", "Stay preference","Spend"))))),
              img(src = "travelbehaviour.png", height =570, width = 650,style="display: block; margin-left: auto; margin-right: auto;")
  ),
              tabItem("Travel",
                      tabsetPanel(type = "tabs", selected = "Mode of transport", id = "intabset1",
                                  tabPanel("Mode of transport", value = "Mode of transport",
                                           h1(" Overall budget based on Mode of transport "),
                                           
                                           
                                           
                                           
                                             
                                                    fluidRow(column(10,offset=1,box(width=6,sliderInput(
                                                      "Slider1", "Over.all.Budget",
                                                      min = round(min(Tb$Over.all.Budget)),
                                                      max = round(max(Tb$Over.all.Budget)),
                                                      value = c(min = min(Tb$Over.all.Budget), max = max(Tb$Over.all.Budget)))
                                                    ))),
                                                    fluidRow(column(10,offset=1 ,box(selectizeInput("mo","select plot of your choice", choice=c("boxplot","density"))))),
                                                    
                                             
                                             fluidRow(column(10,offset=1,plotlyOutput("Mode.of.transportTravelbehaviour"))),
                                             fluidRow(h1(" ")),
                                             fluidRow(h1(" ")),
                                           fluidRow(column(10,offset=1,dataTableOutput("Mode.of.transport"))),
                                           fluidRow(offset=1,downloadButton("download1", label = "Download")),
                                           fluidRow(h1(" ")),
                                           fluidRow(h1(" ")),
                                           fluidRow(column(1,offset = 11,actionButton(inputId = "button1", label = "Next")))
                                          
                                  ),
                                  tabPanel("Reason for travel", value = "Reason for travel",
                                           h1(" Overall Budget  based on Reason for travel"),
                                           fluidRow(column(10,offset=1,box (selectizeInput("re","select plot of your choice", choice=c("boxplot","density"))))),
                                           fluidRow(column(10,offset=1,plotlyOutput("Reason_TravelTravelbehaviour"))),
                                           fluidRow(h1(" ")),
                                           fluidRow(h1(" ")),
                                           fluidRow(column(10,offset=1,dataTableOutput("Reason_Travel"))),
                                           fluidRow(offset=1,downloadButton("download2", label = "Download")),
                                           fluidRow(h1(" ")),
                                           fluidRow(h1(" ")),
                                           fluidRow(column(1,offset = 11,actionButton(inputId = "button2", label = "Back"))),
                                           fluidRow(h1(" ")),
                                           fluidRow(h1(" ")),
                                           fluidRow(column(1,offset = 11,actionButton(inputId = "button3", label = "Next")))
                                           
                                  ),
                                  tabPanel("Season Of Travel", value = "Season Of Travel",
                                           h1(" Overall Budget based on Season Of Travel "),
                                           fluidRow(column(10,offset=1,box ( selectizeInput("st","select plot of your choice", choice=c("boxplot","density"))))),
                                           fluidRow(column(10,offset=1,plotlyOutput("Season_TravelTravelbehaviour"))),
                                           fluidRow(h1(" ")),
                                           fluidRow(h1(" ")),
                                           fluidRow(column(10,offset=1,dataTableOutput("Season_Travel"))),
                                           fluidRow(offset=1,downloadButton("download3", label = "Download")),
                                           fluidRow(h1(" ")),
                                           fluidRow(h1(" ")),
                                           fluidRow(column(1,offset = 11,actionButton(inputId = "button4", label = "Back"))),
                                           fluidRow(h1(" ")),
                                           fluidRow(h1(" ")),
                                           fluidRow(column(1,offset = 11,actionButton(inputId = "button5", label = "Next")))
                                           
                                  ),
                                  tabPanel("Traveler Type", value = "Traveler Type",
                                           h1(" Overall Budget based on Over Traveler Type"),
                                           fluidRow(column(10,offset=1,box ( selectizeInput("tt","select plot of your choice", choice=c("boxplot","density"))))),
                                           fluidRow(column(10,offset=1,plotlyOutput("Traveler.TypeTravelbehaviour"))),
                                           fluidRow(h1(" ")),
                                           fluidRow(h1(" ")),
                                           fluidRow(column(10,offset=1,dataTableOutput("Traveler.Type"))),
                                           fluidRow(offset=1,downloadButton("download4", label = "Download")),
                                           fluidRow(h1(" ")),
                                           fluidRow(h1(" ")),
                                           fluidRow(column(1,offset = 11,actionButton(inputId = "button6", label = "Back")))
                                           
                                  ))),
                                  
                                  
                      
      
                                  
                                  tabItem("stay",
                                          tabsetPanel(type = "tabs", selected = "Place preference", id = "intabset2",
                                                      tabPanel("Place preference", value = "Place preference",
                                                               h1(" Overall Budget based on Place preference"),
                                                               
                                                               fluidRow(column(10,offset=1,box ( selectizeInput("pe","select plot of your choice", choice=c("boxplot","density"))))),
                                                             
                                                               fluidRow(column(10,offset=1,plotlyOutput("Place.preferenceTravelbehaviour"))),
                                                               fluidRow(h1(" ")),
                                                               fluidRow(h1(" ")),
                                                               fluidRow(column(10,offset=1,dataTableOutput("Place.preference"))),
                                                               fluidRow(offset=1,downloadButton("download5", label = "Download")),
                                                               fluidRow(h1(" ")),
                                                               fluidRow(h1(" ")),
                                                               fluidRow(column(1,offset = 11,actionButton(inputId = "button7", label = "Next")))
                                                               
                                                      ),
                                  tabPanel("No of days", value = "No of days",
                                           h1(" Overall Budget based on No of days "),
                                           fluidRow(column(10,offset=1,box ( selectizeInput("nd","select plot of your choice", choice=c("histogram","density"))))),
                                           fluidRow(column(10,offset=1,plotlyOutput("No.of.daysTravelbehaviour"))),
                                           fluidRow(h1(" ")),
                                           fluidRow(h1(" ")),
                                           fluidRow(column(10,offset=1,dataTableOutput("No.of.days"))),
                                           fluidRow(column(6,offset=1,downloadButton("download6", label = "Download"))),
                                           fluidRow(h1(" ")),
                                           fluidRow(h1(" ")),
                                           fluidRow(column(1,offset = 11,actionButton(inputId = "button8", label = "Back"))),
                                           fluidRow(h1(" ")),
                                           fluidRow(h1(" ")),
                                           fluidRow(column(1,offset = 11,actionButton(inputId = "button9", label = "Next")))
                                           
                                  ),
                                  tabPanel("Type of accommodation", value = "Type of accommodation",
                                           h1(" Overall Budget Based  on Type of accommodation"),
                                           fluidRow(column(10,offset=1,box ( selectizeInput("ta","select plot of your choice", choice=c("boxplot","density"))))),
                                           fluidRow(column(10,offset=1,plotlyOutput("Type.of.accommodationTravelbehaviour"))),
                                           fluidRow(h1(" ")),
                                           fluidRow(h1(" ")),
                                           fluidRow(column(10,offset=1,dataTableOutput("Type.of.accommodation"))),
                                           fluidRow(offset=1,downloadButton("download7", label = "Download")),
                                           fluidRow(h1(" ")),
                                           fluidRow(h1(" ")),
                                           fluidRow(column(1,offset = 11,actionButton(inputId = "button10", label = "Back"))),
                                           fluidRow(h1(" ")),
                                           fluidRow(h1(" ")),
                                           fluidRow(column(1,offset = 11,actionButton(inputId = "button11", label = "Next")))
                                          
                                  ),
                                  tabPanel("Hotel Room Preference", value = "Hotel Room Preference",
                                           h1(" Overall Budget based on Hotel Room Preference"),
                                           fluidRow(column(10,offset=1,box ( selectizeInput("hr","select plot of your choice", choice=c("histogram","density"))))),
                                           fluidRow(column(10,offset=1,plotlyOutput("Hotel.Room.PreferenceTravelbehaviour"))),
                                           fluidRow(h1(" ")),
                                           fluidRow(h1(" ")),
                                           fluidRow(column(10,offset=1,dataTableOutput("Hotel.Room.Preference"))),
                                           fluidRow(column(6,offset=1,downloadButton("download8", label = "Download"))),
                                           fluidRow(h1(" ")),
                                           fluidRow(h1(" ")),
                                           fluidRow(column(1,offset = 11,actionButton(inputId = "button12", label = "Back")))
        
                                         
                                  ))),
                                  tabItem("spend",
                                          tabsetPanel(type = "tabs", selected = "previous budget", id = "intabset3",
                                                      tabPanel("previous budget", value = "previous budget",
                                                               h1(" Overall Budget based on Previous Budget "),
                                                            
                                                               fluidRow(column(10,offset=1,plotlyOutput("previous.budgetTravelbehaviour")))
                                                               
                                                               )
                                                               
                                                      )))))
    
                                           
                                                   
    
  
  






server <- function(input, output, session){
  observeEvent(input$radio1,{
    if(input$radio1 == "Travel Preference"){
      updateTabItems(session = session, inputId = "tab1", selected = "Travel")
    }
    if(input$radio1 == "Stay preference"){
      updateTabItems(session = session, inputId = "tab1", selected = "stay")
    }
    if(input$radio1 == "Spend"){
      updateTabItems(session = session, inputId = "tab1", selected = "spend")
    }
  })
  observeEvent(input$button1,{
    updateTabsetPanel(session = session, "intabset1", selected = "Reason for travel")
  })
  observeEvent(input$button2,{
    updateTabsetPanel(session = session, "intabset1", selected = "Mode of transport")
  })
  observeEvent(input$button3,{
    updateTabsetPanel(session = session, "intabset1", selected = "Season Of Travel")
  })
  observeEvent(input$button4,{
    updateTabsetPanel(session = session, "intabset1", selected = "Reason for travel")
  })
  observeEvent(input$button5,{
    updateTabsetPanel(session = session, "intabset1", selected = "Traveler Type")
  })
  observeEvent(input$button6,{
    updateTabsetPanel(session = session, "intabset1", selected = "Season Of Travel")
  })
  observeEvent(input$button7,{
    updateTabsetPanel(session = session, "intabset2", selected = "No of days")
  })
  observeEvent(input$button8,{
    updateTabsetPanel(session = session, "intabset2", selected = "Place preference")
  })
  observeEvent(input$button9,{
    updateTabsetPanel(session = session, "intabset2", selected = "Type of accommodation")
  })
  observeEvent(input$button10,{
    updateTabsetPanel(session = session, "intabset2", selected = "No of days")
  })
  observeEvent(input$button11,{
    updateTabsetPanel(session = session, "intabset2", selected = "Hotel Room Preference")
  })
  observeEvent(input$button12,{
    updateTabsetPanel(session = session, "intabset2", selected = "Type of accommodation")
  })
  output$Mode.of.transportTravelbehaviour <- renderPlotly({
    if(input$mo=="boxplot")
    {pdata0=Tb 
    Tb1=pdata0 %>% filter(Over.all.Budget>=input$Slider1[1] & Over.all.Budget<=input$Slider1[2])
    
    fig<-plot_ly(Tb1 ,y=~Over.all.Budget,color=~Mode.of.transport,type = "box")
    }else{pdata0=Tb 
    Tb1=pdata0 %>% filter(Over.all.Budget>=input$Slider1[1] & Over.all.Budget<=input$Slider1[2])
      dens<-with(Tb1,tapply(Over.all.Budget,INDEX=Mode.of.transport,density))
    df <- data.frame(
      x = unlist(lapply(dens, "[[", "x")),
      y = unlist(lapply(dens, "[[", "y")),
      Mode.of.transport = rep(names(dens), each = length(dens[[1]]$x))
    )
    fig <- plot_ly(df, x = ~x, y = ~y, color = ~Mode.of.transport,fill = 'tozeroy')
    fig <- fig %>% add_lines()
     }
    fig
    
  })  
  output$Reason_TravelTravelbehaviour <- renderPlotly({
    if(input$re=="boxplot")
      {pdata0=Tb
      pdata0=Tb %>% filter(Tb$Over.all.Budget%in% input$select1)
      plot_ly(Tb ,y=~Over.all.Budget,color=~Reason_Travel,type = "box")
    }else{pdata0=Tb
    pdata0=Tb %>% filter(Tb$Over.all.Budget%in% input$select1)
    dens<-with(Tb,tapply(Over.all.Budget,INDEX=Reason_Travel,density))
    df <- data.frame(
      x = unlist(lapply(dens, "[[", "x")),
      y = unlist(lapply(dens, "[[", "y")),
      Reason_Travel= rep(names(dens), each = length(dens[[1]]$x))
    )
    fig <- plot_ly(df, x = ~x, y = ~y, color = ~Reason_Travel,fill = 'tozeroy')
    fig <- fig %>% add_lines()
    }
    
    
  })
  output$Season_TravelTravelbehaviour <- renderPlotly({
    if(input$st=="boxplot")
    {pdata0=Tb
    pdata0=Tb %>% filter(Tb$Over.all.Budget%in% input$select1)
    plot_ly(Tb ,y=~Over.all.Budget,color=~Season_Travel,type = "box")
    }else{pdata0=Tb
    dens<-with(Tb,tapply(Over.all.Budget,INDEX=Season_Travel,density))
    df <- data.frame(
      x = unlist(lapply(dens, "[[", "x")),
      y = unlist(lapply(dens, "[[", "y")),
      Season_Travel= rep(names(dens), each = length(dens[[1]]$x))
    )
    fig <- plot_ly(df, x = ~x, y = ~y, color = ~Season_Travel,fill = 'tozeroy')
    fig <- fig %>% add_lines()
    }
    

    
  })
  output$Traveler.TypeTravelbehaviour <- renderPlotly({
    if(input$tt=="boxplot")
    {pdata0=Tb
    pdata0=Tb %>% filter(Tb$Over.all.Budget%in% input$select1)
    
    pdata0=Tb %>% filter(Tb$Over.all.Budget%in% input$select1)
    plot_ly(Tb ,y=~Over.all.Budget,color=~Traveler.Type,type = "box")
    }else{pdata0=Tb
    dens<-with(Tb,tapply(Over.all.Budget,INDEX=Traveler.Type,density))
    df <- data.frame(
      x = unlist(lapply(dens, "[[", "x")),
      y = unlist(lapply(dens, "[[", "y")),
      Traveler.Type= rep(names(dens), each = length(dens[[1]]$x))
    )
    fig <- plot_ly(df, x = ~x, y = ~y, color = ~Traveler.Type,fill = 'tozeroy')
    fig <- fig %>% add_lines()
    }
  })
  output$Place.preferenceTravelbehaviour <- renderPlotly({
     if (input$pe=="boxplot") 
    {pdata0=Tb %>% filter(Tb$Over.all.Budget%in% input$select1)
    fig<-plot_ly(Tb ,y=~Over.all.Budget,color=~Place.preference,type = "box")
      
    }else{dens<- with(Tb, tapply(Over.all.Budget, INDEX =Place.preference , density))
    df <- data.frame(
      x = unlist(lapply(dens, "[[", "x")),
      y = unlist(lapply(dens, "[[", "y")),
      Place.preference = rep(names(dens), each = length(dens[[1]]$x))
    )
     # Data Visualize
    
    fig <- plot_ly(df, x = ~x, y = ~y, color = ~Place.preference,fill = 'tozeroy')
    fig <- fig %>% add_lines()
      
    } 
    fig
  })
   output$No.of.daysTravelbehaviour <- renderPlotly({
     if(input$nd=="histogram")
     {pdata0=Tb %>% filter(Tb$Over.all.Budget%in% input$select1)
     plot_ly(Tb ,x=~Over.all.Budget,color=~No.of.days,type = "histogram")
     }else{dens<- with(Tb, tapply(Over.all.Budget, INDEX =No.of.days, density))
     df <- data.frame(
       x = unlist(lapply(dens, "[[", "x")),
       y = unlist(lapply(dens, "[[", "y")),
       No.of.days = rep(names(dens), each = length(dens[[1]]$x))
     )
     # Data Visualize
     
     fig <- plot_ly(df, x = ~x, y = ~y, color= ~No.of.days,fill = 'tozeroy')
     fig <- fig %>% add_lines()
     
     } 
  })
  output$Type.of.accommodationTravelbehaviour <- renderPlotly({
    if(input$ta=="boxplot")
    {pdata0=Tb %>% filter(Tb$Over.all.Budget%in% input$select1)
    plot_ly(Tb ,y=~Over.all.Budget,color=~Type.of.accommodation,type = "box")
    }else{dens<- with(Tb, tapply(Over.all.Budget, INDEX = Type.of.accommodation, density))
    df <- data.frame(
      x = unlist(lapply(dens, "[[", "x")),
      y = unlist(lapply(dens, "[[", "y")),
      Type.of.accommodation = rep(names(dens), each = length(dens[[1]]$x))
    )
    # Data Visualize
    
    fig <- plot_ly(df, x = ~x, y = ~y, color = ~Type.of.accommodation,fill = 'tozeroy')
    fig <- fig %>% add_lines()
    
    } 
  })
  output$Hotel.Room.PreferenceTravelbehaviour <- renderPlotly({
    if(input$hr=="histogram")
    {pdata0=Tb %>% filter(Tb$Over.all.Budget%in% input$select1)
    plot_ly(Tb ,x=~Over.all.Budget,color=~Hotel.Room.Preference,type = "histogram")
    }else{dens<- with(Tb, tapply(Over.all.Budget, INDEX =Hotel.Room.Preference , density))
    df <- data.frame(
      x = unlist(lapply(dens, "[[", "x")),
      y = unlist(lapply(dens, "[[", "y")),
      Hotel.Room.Preference = rep(names(dens), each = length(dens[[1]]$x))
    )
    # Data Visualize
    
    fig <- plot_ly(df, x = ~x, y = ~y, color = ~Hotel.Room.Preference,fill = 'tozeroy')
    fig <- fig %>% add_lines()
    
    } 
    })
  
  output$previous.budgetTravelbehaviour <- renderPlotly({
    
    pdata0=Tb %>% filter(Tb$Over.all.Budget%in% input$select1)
    plot_ly(Tb ,x=~previous.budget,y=~Over.all.Budget,type = "scatter",mode="markers")
    })
  output$download1 <- downloadHandler(
    filename = function() {
      paste("Mode", ".csv", sep = "")
    },
    content = function(file) {
      DF=Tb %>% group_by(Mode.of.transport) %>% summarise(
        average=round(mean(Over.all.Budget)),std.dev=round(sd(Over.all.Budget)),
        minimum=round(min(Over.all.Budget)),maximum=round(max(Over.all.Budget))
      )
      write.csv(DF, file, row.names = FALSE)
      
    }
  )  
  output$download2 <- downloadHandler(
    filename = function() {
      paste("Reason", ".csv", sep = "")
    },
    content = function(file) {
      DF=Tb %>% group_by(Reason_Travel) %>% summarise(
        average=round(mean(Over.all.Budget)),std.dev=round(sd(Over.all.Budget)),
        minimum=round(min(Over.all.Budget)),maximum=round(max(Over.all.Budget))
      )
      write.csv(DF, file, row.names = FALSE)
      
    }
  )
  output$download3 <- downloadHandler(
    filename = function() {
      paste("Month", ".csv", sep = "")
    },
    content = function(file) {
      DF=Tb %>% group_by(Season_Travel) %>% summarise(
        average=round(mean(Over.all.Budget)),std.dev=round(sd(Over.all.Budget)),
        minimum=round(min(Over.all.Budget)),maximum=round(max(Over.all.Budget))
      )
      write.csv(DF, file, row.names = FALSE)
      
    }
  )  
  output$download4 <- downloadHandler(
    filename = function() {
      paste("Traveler", ".csv", sep = "")
    },
    content = function(file) {
      DF=Tb %>% group_by(Traveler.Type) %>% summarise(
        average=round(mean(Over.all.Budget)),std.dev=round(sd(Over.all.Budget)),
        minimum=round(min(Over.all.Budget)),maximum=round(max(Over.all.Budget))
      )
      write.csv(DF, file, row.names = FALSE)
      
    }
  )  
  output$download5 <- downloadHandler(
    filename = function() {
      paste("place", ".csv", sep = "")
    },
    content = function(file) {
      DF=Tb %>% group_by(Place.preference) %>% summarise(
        average=round(mean(Over.all.Budget)),std.dev=round(sd(Over.all.Budget)),
        minimum=round(min(Over.all.Budget)),maximum=round(max(Over.all.Budget))
      )
      write.csv(DF, file, row.names = FALSE)
      
    }
  )  
  output$download6 <- downloadHandler(
    filename = function() {
      paste("No of days", ".csv", sep = "")
    },
    content = function(file) {
      DF=Tb %>% group_by(No.of.days) %>% summarise(
        average=round(mean(Over.all.Budget)),std.dev=round(sd(Over.all.Budget)),
        minimum=round(min(Over.all.Budget)),maximum=round(max(Over.all.Budget))
      )
      write.csv(DF, file, row.names = FALSE)
      
    }
  )  
  output$download7 <- downloadHandler(
    filename = function() {
      paste("accomodation", ".csv", sep = "")
    },
    content = function(file) {
      DF=Tb %>% group_by(Type.of.accommodation) %>% summarise(
        average=round(mean(Over.all.Budget)),std.dev=round(sd(Over.all.Budget)),
        minimum=round(min(Over.all.Budget)),maximum=round(max(Over.all.Budget))
      )
      write.csv(DF, file, row.names = FALSE)
      
    }
  )  
  output$download8 <- downloadHandler(
    filename = function() {
      paste("Hotel room", ".csv", sep = "")
    },
    content = function(file) {
      DF=Tb %>% group_by(Hotel.Room.Preference) %>% summarise(
        average=round(mean(Over.all.Budget)),std.dev=round(sd(Over.all.Budget)),
        minimum=round(min(Over.all.Budget)),maximum=round(max(Over.all.Budget))
      )
      write.csv(DF, file, row.names = FALSE)
      
    }
  )  
  output$Mode.of.transport <- renderDT({
    DF=Tb %>% group_by(Mode.of.transport) %>% summarise(
      average=round(mean(Over.all.Budget)),std.dev=round(sd(Over.all.Budget)),
      minimum=min(Over.all.Budget),maximum=max(Over.all.Budget)
    )
    datatable(DF)
  })
  output$Reason_Travel <- renderDT({
    DF=Tb %>% group_by(Reason_Travel) %>% summarise(
      average=round(mean(Over.all.Budget)),std.dev=round(sd(Over.all.Budget)),
      minimum=round(min(Over.all.Budget)),maximum=round(max(Over.all.Budget))
    )
    datatable(DF)
  })
  output$Season_Travel <- renderDT({
    DF=Tb %>% group_by(Season_Travel) %>% summarise(
      average=round(mean(Over.all.Budget)),std.dev=round(sd(Over.all.Budget)),
      minimum=round(min(Over.all.Budget)),maximum=round(max(Over.all.Budget))
    )
    datatable(DF)
  })
  output$Traveler.Type<- renderDT({
    DF=Tb %>% group_by(Traveler.Type) %>% summarise(
      average=round(mean(Over.all.Budget)),std.dev=round(sd(Over.all.Budget)),
      minimum=round(min(Over.all.Budget)),maximum=round(max(Over.all.Budget))
    )
    datatable(DF)
  })
  output$Place.preference<- renderDT({
    DF=Tb %>% group_by(Place.preference) %>% summarise(
      average=round(mean(Over.all.Budget)),std.dev=round(sd(Over.all.Budget)),
      minimum=round(min(Over.all.Budget)),maximum=round(max(Over.all.Budget))
    )
    datatable(DF)
  })
  output$No.of.days<- renderDT({
    DF=Tb %>% group_by(No.of.days) %>% summarise(
      average=round(mean(Over.all.Budget)),std.dev=round(sd(Over.all.Budget)),
      minimum=round(min(Over.all.Budget)),maximum=round(max(Over.all.Budget))
    )
    datatable(DF)
  })
  output$Type.of.accommodation<- renderDT({
    DF=Tb %>% group_by(Type.of.accommodation) %>% summarise(
      average=round(mean(Over.all.Budget)),std.dev=round(sd(Over.all.Budget)),
      minimum=round(min(Over.all.Budget)),maximum=round(max(Over.all.Budget))
    )
    datatable(DF)
  })
  output$Hotel.Room.Preference<- renderDT({
    DF=Tb %>% group_by(Hotel.Room.Preference) %>% summarise(
      average=round(mean(Over.all.Budget)),std.dev=round(sd(Over.all.Budget)),
      minimum=round(min(Over.all.Budget)),maximum=round(max(Over.all.Budget))
    )
    datatable(DF)
  })
  
  
}
shinyApp(ui, server)