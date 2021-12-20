library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(DT)
library(data.table)
library(shinyalert)
library(dashboardthemes)
ed=read.csv("D:/aafaq/datascience/ElectionData.csv",header=T)
ui <- dashboardPage(
  dashboardHeader(title="Portugal Election Data 2019"),
  dashboardSidebar(sidebarMenu(id = "tab1",selected="home",
                               menuItem("Home",tabName="home"),
                               menuItem("Previous Election", tabName = "Previous"),
                               menuItem("Current Election", tabName = "Current"),
                               menuItem("Local Election", tabName = "Local"))),
  dashboardBody(
    tabItems(
      tabItem("home",
              fluidRow(column(12, h2("Welcome to the Presentation of Portugal Election Data"))),
              fluidRow(column(12, box(radioButtons(inputId = "radio1",selected = character(0),
                                                   label = "Please choose for Previous or Current or Local",
                                                   choices = c("Previous Election", "Current Election","Local Election")))))),
                       
              
      tabItem("Previous",
              tabsetPanel(type = "tabs", selected = "totalMandates", id = "intabset1",
                          tabPanel("totalMandates", value = "totalMandates",
                                   h1(" totalMandates VS FinalMandates "),
                                   
                                   fluidRow(h1(" ")),
                                   fluidRow(h1(" ")),
                                   fluidRow(column(1,offset = 11,actionButton(inputId = "button1", label = "Next"))),
                                   fluidRow(column(8,plotlyOutput("totalMandatesElectionData"))),
                                   column(4, fluidRow(selectizeInput(
                                     "select1", "Final Mandates", multiple = TRUE,
                                     choices = sort(unique(ed$FinalMandates)),
                                     selected = sort(unique(ed$FinalMandates))
                                   )),
                                   fluidRow(sliderInput(
                                     "Slider1", "total Mandates",
                                     min = min(ed$totalMandates),
                                     max = max(ed$totalMandates),
                                     value = c(min = min(ed$totalMandates), max = max(ed$totalMandates))
                                   ))
                                   
                                   )
                                   
                        
                          ),
                          tabPanel("pre.totalVoters", value = "pre.totalVoters",
                                   h1(" previous total vote VS Final Mandates"),
                                   fluidRow(h1(" ")),
                                   fluidRow(h1(" ")),
                                   fluidRow(column(1,offset = 11,actionButton(inputId = "button2", label = "Back"))),
                                   fluidRow(h1(" ")),
                                   fluidRow(h1(" ")),
                                   fluidRow(column(1,offset = 11,actionButton(inputId = "button3", label = "Next"))),
                                   fluidRow(column(12,plotlyOutput("pre.totalVotersElectionData")))
                          ),
                          tabPanel("pre.subscribedVoters", value = "pre.subscribedVoters",
                                   h1(" pre.subscribed Voters VS Final Mandate"),
                                   fluidRow(h1(" ")),
                                   fluidRow(h1(" ")),
                                   fluidRow(column(1,offset = 11,actionButton(inputId = "button4", label = "Back"))),
                                   fluidRow(h1(" ")),
                                   fluidRow(h1(" ")),
                                   fluidRow(column(1,offset = 11,actionButton(inputId = "button5", label = "Next"))),
                                   fluidRow(column(12,plotlyOutput("pre.subscribedVotersElectionData")))
                          ),
                          tabPanel("pre.votersPercentage", value = "pre.votersPercentage",
                                   h1(" pre.voters Percentage VS  Final Mandate"),
                                   fluidRow(h1(" ")),
                                   fluidRow(h1(" ")),
                                   fluidRow(column(1,offset = 11,actionButton(inputId = "button6", label = "Back"))),
                                   fluidRow(h1(" ")),
                                   fluidRow(h1(" ")),
                                   fluidRow(column(1,offset = 11,actionButton(inputId = "button7", label = "Next"))),
                                   fluidRow(column(12,plotlyOutput("pre.votersPercentageElectionData")))
                          ),
                          tabPanel("pre.blankVotes", value = "pre.blankVotes",
                                   h1(" pre.blank Votes VS Final Mandate"),
                                   fluidRow(h1(" ")),
                                   fluidRow(h1(" ")),
                                   fluidRow(column(1,offset = 11,actionButton(inputId = "button8", label = "Back"))),
                                   fluidRow(h1(" ")),
                                   fluidRow(h1(" ")),
                                   fluidRow(column(1,offset = 11,actionButton(inputId = "button9", label = "Next"))),
                                   fluidRow(column(12,plotlyOutput("pre.blankVotesElectionData")))
                          ),
                          tabPanel("pre.blankVotesPercentage", value = "pre.blankVotesPercentage",
                                   h1("pre.blank Votes Percentage VS Final Mandate"),
                                   fluidRow(h1(" ")),
                                   fluidRow(h1(" ")),
                                   fluidRow(column(1,offset = 11,actionButton(inputId = "button10", label = "Back"))),
                                   fluidRow(column(12,plotlyOutput("pre.blankVotesPercentageElectionData")))
                          )
                          
              )
              
      ),
      tabItem("Current",
              tabsetPanel(type = "tabs", selected = "totalVoters", id = "intabset2",
                          tabPanel("totalVoters", value = "totalVoters",
                                   fluidRow(column (4, selectizeInput("pe","select plot of your choice", choice=c("boxplot","density")))),
                             h1(" total Voters VS Final Mandate"),
                             fluidRow(h1(" ")),
                             fluidRow(h1(" ")),
                             fluidRow(column(1,offset = 11,actionButton(inputId = "button11", label = "Next"))),
                             fluidRow(column(12,plotlyOutput("totalVotersElectionData")))
                          ),
                          tabPanel("Votes", value = "Votes",
                                   h1(" Votes VS Final Mandate"),
                                   fluidRow(h1(" ")),
                                   fluidRow(h1(" ")),
                                   fluidRow(column(1,offset = 11,actionButton(inputId = "button12", label = "Back"))),
                                   fluidRow(h1(" ")),
                                   fluidRow(h1(" ")),
                                   fluidRow(column(1,offset = 11,actionButton(inputId = "button13", label = "Next"))),
                                   fluidRow(column(12,plotlyOutput("VotesElectionData")))
                          ),
                          tabPanel("validVotesPercentage", value = "validVotesPercentage",
                                   h1(" valid Votes Percentage VS Final Mandate"),
                                   fluidRow(h1(" ")),
                                   fluidRow(h1(" ")),
                                   fluidRow(column(1,offset = 11,actionButton(inputId = "button14", label = "Back"))),
                                   fluidRow(h1(" ")),
                                   fluidRow(h1(" ")),
                                   fluidRow(column(1,offset = 11,actionButton(inputId = "button15", label = "Next"))),
                                   fluidRow(column(12,plotlyOutput("validVotesPercentageElectionData")))
                          ),
                          tabPanel("blankVotes", value = "blankVotes",
                                   h1(" blank Votes VS Final Mandate"),
                                   fluidRow(h1(" ")),
                                   fluidRow(h1(" ")),
                                   fluidRow(column(1,offset = 11,actionButton(inputId = "button16", label = "Back"))),
                                   fluidRow(h1(" ")),
                                   fluidRow(h1(" ")),
                                   fluidRow(column(1,offset = 11,actionButton(inputId = "button17", label = "Next"))),
                                   fluidRow(column(12,plotlyOutput("blankVotesElectionData")))
                          ),
                          tabPanel("blankVotesPercentage", value = "blankVotesPercentage",
                                   h1(" blank Votes Percentage VS Final Mandate"),
                                   fluidRow(h1(" ")),
                                   fluidRow(h1(" ")),
                                   fluidRow(column(1,offset = 11,actionButton(inputId = "button18", label = "Back"))),
                                   fluidRow(column(12,plotlyOutput("blankVotesPercentageElectionData")))
                          )
              )
      ),
      tabItem("Local",
              tabsetPanel(type = "tabs", selected = "numParishes", id = "intabset3",
                          tabPanel("numParishes", value = "numParishes",
                                   h1(" num parishes VS Final Mandate"),
                                   fluidRow(h1(" ")),
                                   fluidRow(h1(" ")),
                                   fluidRow(column(1,offset = 11,actionButton(inputId = "button19", label = "Next"))),
                                   fluidRow(column(12,plotlyOutput("numParishesElectionData")))
                          ),
                          tabPanel("numParishesApproved", value = "numParishesApproved",
                                   h1(" num Parishes Approved VS Final Mandate"),
                                   fluidRow(h1(" ")),
                                   fluidRow(h1(" ")),
                                   fluidRow(column(1,offset = 11,actionButton(inputId = "button20", label = "Back"))),
                                   fluidRow(column(12,plotlyOutput("numParishesApprovedElectionData")))
                          )
              )
      )
    ))
    
  )

server <- function(input, output, session){
  observeEvent(input$radio1,{
    if(input$radio1 == "Previous Election"){
      updateTabItems(session = session, inputId = "tab1", selected = "Previous")
    }
    if(input$radio1 == "Current Election"){
      updateTabItems(session = session, inputId = "tab1", selected = "Current")
    }
    if(input$radio1 == "Local Election"){
      updateTabItems(session = session, inputId = "tab1", selected = "Local")
    }
  })
  observeEvent(input$button1,{
    updateTabsetPanel(session = session, "intabset1", selected = "pre.totalVoters")
  })
  observeEvent(input$button2,{
    updateTabsetPanel(session = session, "intabset1", selected = "totalMandates")
  })
  observeEvent(input$button3,{
    updateTabsetPanel(session = session, "intabset1", selected = "pre.subscribedVoters")
  })
  observeEvent(input$button4,{
    updateTabsetPanel(session = session, "intabset1", selected = "pre.totalVoters")
  })
  observeEvent(input$button5,{
    updateTabsetPanel(session = session, "intabset1", selected = "pre.votersPercentage")
  })
  observeEvent(input$button6,{
    updateTabsetPanel(session = session, "intabset1", selected = "pre.subscribedVoters")
  })
  observeEvent(input$button7,{
    updateTabsetPanel(session = session, "intabset1", selected = "pre.blankVotes")
  })
  observeEvent(input$button8,{
    updateTabsetPanel(session = session, "intabset1", selected = "pre.subscribedVoters")
  })
  observeEvent(input$button9,{
    updateTabsetPanel(session = session, "intabset1", selected = "pre.blankVotesPercentage")
  })
  observeEvent(input$button10,{
    updateTabsetPanel(session = session, "intabset1", selected = "pre.blankVotes")
  })
  observeEvent(input$button11,{
    updateTabsetPanel(session = session, "intabset2", selected = "Votes")
  })
  observeEvent(input$button12,{
    updateTabsetPanel(session = session, "intabset2", selected = "totalVoters")
  })
  observeEvent(input$button13,{
    updateTabsetPanel(session = session, "intabset2", selected = "validVotesPercentage")
  })
  observeEvent(input$button14,{
    updateTabsetPanel(session = session, "intabset2", selected = "Votes")
  })
  observeEvent(input$button15,{
    updateTabsetPanel(session = session, "intabset2", selected = "validVotesPercentage")
  })
  observeEvent(input$button15,{
    updateTabsetPanel(session = session, "intabset2", selected = "blankVotes")
  })
  observeEvent(input$button16,{
    updateTabsetPanel(session = session, "intabset2", selected = "validVotesPercentage")
  })
  observeEvent(input$button17,{
    updateTabsetPanel(session = session, "intabset2", selected = "blankVotesPercentage")
  })
  observeEvent(input$button18,{
    updateTabsetPanel(session = session, "intabset2", selected = "blankVotes")
  })
  observeEvent(input$button19,{
    updateTabsetPanel(session = session, "intabset3", selected = "numParishesApproved")
  })
  observeEvent(input$button20,{
    updateTabsetPanel(session = session, "intabset3", selected = "numParishes")
  })
  output$totalMandatesElectionData <- renderPlotly({
    pdata0=ed %>% filter(ed$FinalMandates%in% input$select1)
    ed1=pdata0 %>% filter(totalMandates>=input$Slider1[1] & totalMandates<=input$Slider1[2])
    plot_ly(ed1 ,x = ~totalMandates, y = ~FinalMandates,type = "scatter",mode="markers")
  })
  output$pre.totalVotersElectionData <- renderPlotly({
    pdata0=ed %>% filter(ed$FinalMandates%in% input$select1)
    ed1=pdata0 %>% filter(pre.totalVoters>=input$Slider1[1] & pre.totalVoters<=input$Slider1[2])
    plot_ly(ed,y=~pre.totalVoters,type="box",color=~FinalMandates)
})
  output$pre.subscribedVotersElectionData<- renderPlotly({
    pdata0=ed %>% filter(ed$FinalMandates%in% input$select1)
    ed1=pdata0 %>% filter(pre.subscribedVoters>=input$Slider1[1] & pre.subscribedVoters<=input$Slider1[2])
    plot_ly(ed,x=~pre.subscribedVoters,type="histogram")
  })
  output$pre.votersPercentageElectionData<- renderPlotly({
    pdata0=ed %>% filter(ed$FinalMandates%in% input$select1)
    ed1=pdata0 %>% filter(pre.votersPercentage>=input$Slider1[1] & pre.blankVotesPercentage<=input$Slider1[2])
    plot_ly(ed,x=~pre.blankVotesPercentage,y=~FinalMandates,type="scatter",mode="markers")
  })
  output$pre.blankVotesElectionData<- renderPlotly({
    pdata0=ed %>% filter(ed$FinalMandates%in% input$select1)
    ed1=pdata0 %>% filter(pre.blankVotes>=input$Slider1[1] & pre.blankVotes<=input$Slider1[2])
    plot_ly(ed,y=~pre.blankVotes,color=~FinalMandates,type="box")
  })

output$pre.blankVotesPercentageElectionData<- renderPlotly({
  pdata0=ed %>% filter(ed$FinalMandates%in% input$select1)
  ed1=pdata0 %>% filter(pre.blankVotesPercentage>=input$Slider1[1] &pre.blankVotesPercentage<=input$Slider1[2])
  plot_ly(ed,x=~pre.blankVotesPercentage,color=~FinalMandates,type="histogram")
})
output$totalVotersElectionData<- renderPlotly({
  pdata0=ed %>% filter(ed$FinalMandates%in% input$select1)
  ed1=pdata0 %>% filter(totalVoters>=input$Slider1[1] &totalVoters<=input$Slider1[2])
  plot_ly(ed,y=~totalVoters,color=~FinalMandates,type="box")
})
output$VotesElectionData<- renderPlotly({
  pdata0=ed %>% filter(ed$FinalMandates%in% input$select1)
  ed1=pdata0 %>% filter(Votes>=input$Slider1[1] &Votes<=input$Slider1[2])
  plot_ly(ed,x=~Votes,color=~FinalMandates,type="histogram")
})
output$validVotesPercentageElectionData<- renderPlotly({
  pdata0=ed %>% filter(ed$FinalMandates%in% input$select1)
  ed1=pdata0 %>% filter(validVotesPercentage>=input$Slider1[1] &validVotesPercentage<=input$Slider1[2])
  plot_ly(ed,x=~validVotesPercentage,y=~FinalMandates,type="scatter",mode="markers")
})
output$blankVotesElectionData<- renderPlotly({
  pdata0=ed %>% filter(ed$FinalMandates%in% input$select1)
  ed1=pdata0 %>% filter(blankVotes>=input$Slider1[1] &blankVotes<=input$Slider1[2])
  plot_ly(ed,y=~blankVotes,color=~FinalMandates,type="box")
})
output$blankVotesPercentageElectionData<- renderPlotly({
  pdata0=ed %>% filter(ed$FinalMandates%in% input$select1)
  ed1=pdata0 %>% filter(blankVotesPercentage>=input$Slider1[1] &blankVotesPercentage<=input$Slider1[2])
  plot_ly(ed,x=~blankVotesPercentage,type="histogram")
})
output$numParishesElectionData<- renderPlotly({
  pdata0=ed %>% filter(ed$FinalMandates%in% input$select1)
  ed1=pdata0 %>% filter(numParishes>=input$Slider1[1] &numParishes<=input$Slider1[2])
  plot_ly(ed,x=~numParishes,y=~FinalMandates,type="scatter",mode="markers")
})
output$numParishesApprovedElectionData<- renderPlotly({
  pdata0=ed %>% filter(ed$FinalMandates%in% input$select1)
  ed1=pdata0 %>% filter(numParishesApproved>=input$Slider1[1] &numParishesApproved<=input$Slider1[2])
  plot_ly(ed,y=~numParishesApproved,color=~FinalMandates,type="box")
})
}
shinyApp(ui, server)
