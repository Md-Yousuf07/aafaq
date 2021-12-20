library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(DT)
library(data.table)
library(shinyalert)
library(dashboardthemes)

wage_data <- ISLR::Wage

ui <- dashboardPage(skin="green",
                    dashboardHeader(title = "Learning Shiny"),
                    dashboardSidebar(sidebarMenu(id = "tab1", selected = "home",
                                                 menuItem("Home", tabName = "home"),
                                                 menuItem("Personal Info", tabName = "personal"),
                                                 menuItem("Professional Info", tabName = "prof"))),
                    dashboardBody(
                      tabItems(
                        tabItem("home",
                                fluidRow(column(12, h2("Welcome to the Presentation of Wage Data"))),
                                fluidRow(column(12, box(radioButtons(inputId = "radio1",selected = character(0),
                                                                     label = "Please choose for Personal or Professional",
                                                                     choices = c("Personal Info", "Professional Info"))))),
                                fluidRow(column(12, box(fileInput("file1", "Please choose CSV File to upload",
                                                                  accept = c(".xls",".xlsx",".csv")),width = "100%"))),
                                fluidRow(column(12, DTOutput("InputData")))
                        ),
                        tabItem("personal",
                                tabsetPanel(type = "tabs", selected = "age", id = "intabset1",
                                            tabPanel("Age", value = "age",
                                                     h1(" Age vs Wage"),
                                                     fluidRow(column(8, plotlyOutput("AgeWage")),
                                                              column(4, fluidRow(selectizeInput(
                                                                "select1", "Please select the years of choice", multiple = TRUE,
                                                                choices = sort(unique(wage_data$year)),
                                                                selected = sort(unique(wage_data$year))
                                                              )),
                                                              fluidRow(sliderInput(
                                                                "Slider1", "Please select the age group",
                                                                min = min(wage_data$age),
                                                                max = max(wage_data$age),
                                                                value = c(min = min(wage_data$age), max = max(wage_data$age))
                                                              )),
                                                              fluidRow(sliderInput(
                                                                "Slider2", "Please select the Wage group",
                                                                min = min(wage_data$wage),
                                                                max = max(wage_data$wage),
                                                                value = c(min = min(wage_data$wage), max = max(wage_data$wage))
                                                              )))
                                                     ),
                                                     fluidRow(h1(" ")),
                                                     fluidRow(h1(" ")),
                                                     fluidRow(column(1,offset = 11,actionButton(inputId = "button1", label = "Next")))
                                            ),
                                            tabPanel("Race", value = "race",
                                                     h1(" race vs Wage"),
                                                     fluidRow(column(12, plotlyOutput("RaceWage"))),
                                                     
                                                     fluidRow(h2(" ")),
                                                     fluidRow(h2(" ")),
                                                     fluidRow(column(1,offset = 0, actionButton(inputId = "button2", label = "Back"))),
                                                     fluidRow(h2(" ")),
                                                     fluidRow(h2(" ")),
                                                     fluidRow(column(1,offset = 0, actionButton(inputId = "button3", label = "Next"))),
                                            ),
                                            tabPanel("Marital Status", value = "maritl",
                                                     h1(" Martital Status vs Wage"),
                                                     fluidRow(column(8,plotlyOutput("marital_plot")),
                                                              column(4, fluidRow(selectizeInput("MaritalInp", "Select the type of plot",
                                                                                                choices = c("Box Plot", "Density Plot"))),
                                                                     fluidRow(dataTableOutput("marital")),
                                                                     fluidRow(downloadButton("download1", label = "Download")))),
                                                     fluidRow(h2(" ")),
                                                     fluidRow(h2(" ")),
                                                     fluidRow(column(1,offset = 0, actionButton(inputId = "button4", label = "Back"))),
                                                     fluidRow(h2(" ")),
                                                     fluidRow(h2(" ")),
                                                     fluidRow(column(1,offset = 0, actionButton(inputId = "button5", label = "Next")))
                                                     
                                                     
                                            ),
                                            tabPanel("Region", value = "region",
                                                     h1(" Region  vs Wage"),
                                                     fluidRow(column(12,plotlyOutput("RegionWage"))),
                                                     fluidRow(h2(" ")),
                                                     fluidRow(h2(" ")),
                                                     fluidRow(column(1,offset = 0, actionButton(inputId = "button6", label = "Back")))
                                                    
                                            )
                                )
                                
                        ),
                        tabItem("prof",
                                tabsetPanel(type = "tabs", selected = "edu", id = "intabset2",
                                            tabPanel("Education", value = "edu",
                                                     h1(" Education vs Wage"),
                                                     fluidRow(column(12,plotlyOutput("EducationWage"))),
                                                     fluidRow(h2(" ")),
                                                     fluidRow(h2(" ")),
                                                     fluidRow(column(1,offset = 0, actionButton(inputId = "button7", label = "Next")))
                                                     
                                            ),
                                            tabPanel("Jobclass", value = "job",
                                                     h1(" Job class vs Wage"),
                                                     fluidRow(column(12,plotlyOutput("Jobclasswage"))),
                                                     fluidRow(h2(" ")),
                                                     fluidRow(h2(" ")),
                                                     fluidRow(column(1,offset = 0, actionButton(inputId = "button8", label = "Back"))),
                                                     fluidRow(h2(" ")),
                                                     fluidRow(h2(" ")),
                                                     fluidRow(column(1,offset = 0, actionButton(inputId = "button9", label = "Next")))
                                                     
                                            ),
                                            tabPanel("Health", value = "health",
                                                     h1(" Health vs Wage"),
                                                     fluidRow(column(12,plotlyOutput("Healthwage"))),
                                                     fluidRow(h2(" ")),
                                                     fluidRow(h2(" ")),
                                                     fluidRow(column(1,offset = 0, actionButton(inputId = "button10", label = "Back"))),
                                                     fluidRow(h2(" ")),
                                                     fluidRow(h2(" ")),
                                                     fluidRow(column(1,offset = 0, actionButton(inputId = "button11", label = "Next")))
                                                     
                                                     
                                            ),
                                            tabPanel("Health Insurance", value = "HlthIns",
                                                     h1(" Health Insurance  vs Wage"),
                                                     fluidRow(column(12,plotlyOutput("HealthInsurancewage"))),
                                                     fluidRow(h2(" ")),
                                                     fluidRow(h2(" ")),
                                                     fluidRow(column(1,offset = 0, actionButton(inputId = "button12", label = "Back"))),
                                                     
                                                     
                                            )
                                )
                        )
                      )
                    )
)

server <- function(input, output, session){
  
  observeEvent(input$file1,{
    read_excel_allsheets <- read.csv(input$file1$datapath)
    
    #fwrite(read_excel_allsheets,"D:/aafaq/datascience/Iris.csv")
    #save(read_excel_allsheets, file = "D:/aafaq/datascience/Input_data.rda")
    
    output$InputData <- renderDT({
      datatable(read_excel_allsheets)
    })
    
    
  })
  
  observeEvent(input$button1,{
    updateTabsetPanel(session = session, "intabset1", selected = "race")
  })
  
  observeEvent(input$button2,{
    updateTabsetPanel(session = session, "intabset1", selected = "age")
  })
  observeEvent(input$button3,{
    updateTabsetPanel(session = session, "intabset1", selected = "maritl")
  })
  
  observeEvent(input$button4,{
    updateTabsetPanel(session = session, "intabset1", selected = "race")
  })
  observeEvent(input$button5,{
    updateTabsetPanel(session = session, "intabset1", selected = "region")
  })
  observeEvent(input$button6,{
    updateTabsetPanel(session = session, "intabset1", selected = "maritl")
  })
  observeEvent(input$button7,{
    updateTabsetPanel(session = session, "intabset2", selected = "job")
  })
  observeEvent(input$button8,{
    updateTabsetPanel(session = session, "intabset2", selected = "edu")
  })
  observeEvent(input$button9,{
    updateTabsetPanel(session = session, "intabset2", selected = "health")
  })
  observeEvent(input$button10,{
    updateTabsetPanel(session = session, "intabset2", selected = "job")
  })
  observeEvent(input$button11,{
    updateTabsetPanel(session = session, "intabset2", selected = "HlthIns")
  })
  observeEvent(input$button12,{
    updateTabsetPanel(session = session, "intabset2", selected = "health")
  })
  observeEvent(input$radio1,{
    if(input$radio1 == "Personal Info"){
      updateTabItems(session = session, inputId = "tab1", selected = "personal")
    }
    if(input$radio1 == "Professional Info"){
      updateTabItems(session = session, inputId = "tab1", selected = "prof")
    }
  })
  
  output$AgeWage <- renderPlotly({
    pdata0 <- wage_data[which(wage_data$year %in% input$select1),]
    pdata <- pdata0[which(pdata0$age %in% (input$Slider1[1]:input$Slider1[2])),]
    pdata <- pdata[which(pdata$wage > input$Slider2[1] & pdata$wage <= input$Slider2[2]),]
    plot_ly(data = pdata, 
            x = ~age, y = ~wage)
  })
  output$RaceWage <- renderPlotly({
    pdata1 <-wage_data [which(wage_data$year %in% input$select1),]
    pdata <- pdata1[which(pdata1$race %in% (input$Slider1[1]:input$Slider1[2])),]
    plot_ly(wage_data, y = ~wage, type = 'box',color=~race,stroke=I('goldenrod'))
  })
  output$RegionWage <- renderPlotly({
    pdata1 <-wage_data [which(wage_data$year %in% input$select1),]
    pdata <- pdata1[which(pdata1$region %in% (input$Slider1[1]:input$Slider1[2])),]
    plot_ly(wage_data, y = ~wage, type = 'box',color=~region,stroke=I('goldenrod'))
  })
  output$EducationWage<- renderPlotly({
    pdata1 <-wage_data [which(wage_data$year %in% input$select1),]
    pdata <- pdata1[which(pdata1$education %in% (input$Slider1[1]:input$Slider1[2])),]
    plot_ly(wage_data, y = ~wage, type = 'box',color=~education,stroke=I('goldenrod'))
  })
  output$Jobclasswage<-renderPlotly({
    pdata1 <-wage_data [which(wage_data$year %in% input$select1),]
    pdata <- pdata1[which(pdata1$jobclass %in% (input$Slider1[1]:input$Slider1[2])),]
    plot_ly(wage_data, y = ~wage, type = 'box',color=~jobclass,stroke=I('goldenrod'))
  })
  output$Healthwage<-renderPlotly({
    pdata1 <-wage_data [which(wage_data$year %in% input$select1),]
    pdata <- pdata1[which(pdata1$health %in% (input$Slider1[1]:input$Slider1[2])),]
    plot_ly(wage_data, y = ~wage, type = 'box',color=~health,stroke=I('goldenrod'))
  })
  output$HealthInsurancewage<-renderPlotly({
    pdata1 <-wage_data [which(wage_data$year %in% input$select1),]
    pdata <- pdata1[which(pdata1$health_ins %in% (input$Slider1[1]:input$Slider1[2])),]
    plot_ly(wage_data, y = ~wage, type = 'box',color=~health_ins,stroke=I('goldenrod'))
  })
  output$marital <- renderDataTable({
    df = as.data.frame(wage_data %>% 
                         group_by(maritl) %>% select(wage, maritl) %>% 
                         summarise(mean_wage = mean(wage),
                                   max_wage = max(wage)))
    datatable(df, options = list(searching = FALSE))
  })
  
  output$marital_plot <- renderPlotly({
    if(input$MaritalInp == "Box Plot"){
      plot_ly(wage_data, y = ~wage, x= ~maritl, type = "box",
              color = ~maritl)%>%
        layout(xaxis = list(showticklabels = FALSE, title = "Marital Status"),
               yaxis = list(title = 'Wage'))
    } else{
      Req_data2 <- wage_data[which(wage_data$maritl == "1. Never Married"),]
      den1 <- density(Req_data2$wage)
      Req_data3 <- wage_data[which(wage_data$maritl == "2. Married"),]
      den2 <- density(Req_data3$wage)
      Req_data4 <- wage_data[which(wage_data$maritl == "3. Widowed"),]
      den3 <- density(Req_data4$wage)
      Req_data5 <- wage_data[which(wage_data$maritl == "4. Divorced"),]
      den4 <- density(Req_data5$wage)
      Req_data6 <- wage_data[which(wage_data$maritl == "5. Separated"),]
      den5 <- density(Req_data6$wage)
      
      plot_ly(x = ~den1$x, y = ~den1$y, type = 'scatter', 
              mode = 'lines', name = 'Never Married', fill = 'tozeroy') %>% 
        add_trace(x = ~den2$x, y = ~den2$y, name = 'Married', 
                  fill = 'tozeroy')%>% 
        add_trace(x = ~den3$x, y = ~den3$y, name = 'Widowed', 
                  fill = 'tozeroy')%>% 
        add_trace(x = ~den4$x, y = ~den4$y, name = 'Divorced', 
                  fill = 'tozeroy') %>%
        add_trace(x = ~den5$x, y = ~den5$y, name = 'Separated', 
                  fill = 'tozeroy') %>% layout(xaxis = list(title = 'Marital Status'),
                                               yaxis = list(title = 'Wage'))
      
    }
  })
  
  output$download1 <- downloadHandler(
    filename = function() {
      paste("Marital Status", ".csv", sep = "")
    },
    content = function(file) {
      df = as.data.frame(wage_data %>% 
                           group_by(maritl) %>% select(wage, maritl) %>% 
                           summarise(mean_wage = mean(wage),
                                     max_wage = max(wage)))
      write.csv(df, file, row.names = TRUE)
    }
  )
}

shinyApp(ui, server)