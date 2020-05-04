library(shiny)
library(tidyverse)
library(shinyWidgets)
library(plotly)
library(scales)
library(shinydashboard)
library(DT)
library(shinyjs)
library(shinydashboardPlus)
setwd("~/R/Projects/Beach Volley Finish History")
df <- read.csv('beach_volley_finish_history.csv')
df$Date <- as.Date(df$Date)
df$Tournament <- as.character(df$Tournament)
df$Gender <- as.character(df$Gender)
df$Player <- as.character(df$Player)
df$Country <- as.character(df$Country)
df <- df %>% filter(!is.na(Finish))
df$Year <- as.integer(str_sub(df$Date, 1, 4))
df$Winnings[is.na(df$Winnings)] <- 0
df$Points[is.na(df$Points)] <- 0
df$Circut <- as.character(df$Circut)
df <- df %>% rename(Win = Winnings)


body <-
  dashboardBody(
    useShinyjs(),
    tabBox(title = '',id = 'tab1',width = '150%',
           tabPanel("Player Finish",
                    fluidRow(
                      column(6,selectInput('Select1', 'Player', sort(unique(df$Player)), selected = 'Emanuel Rego', width = '400')),
                      column(6,pickerInput('Select2','Circuit',choices = sort(unique(df$Circuit)),options = list('actions-box' = T, size = 12),multiple = T,selected = c('FIVB', 'AVP'), width = '400'))),
                    fluidRow(
                      column(6,DT::dataTableOutput('table2')),
                      column(6,plotlyOutput('plot1.1'))),
                    fluidRow(
                      plotlyOutput('plot1'))),
           tabPanel("Country Finish",
                    fluidRow(
                      column(6,selectInput('SelectA', 'Country', choices = sort(unique(df$Country)), selected = 'United States', width = '400')),
                      column(6,pickerInput('SelectB','Type',choices = sort(unique(df$Type)),options = list('actions-box' = T, size = 12),multiple = T,selected = unique(df$Type), width = '400'))),
                    fluidRow(
                      column(6,DT::dataTableOutput('table3')),
                      column(6,plotlyOutput('plot2'))),
                    fluidRow(
                      plotlyOutput('plot4')))))

shinyApp(
  ui <- dashboardPage(
    dashboardHeader(title = "BVB Finish Exploration"),
    dashboardSidebar(),
    body),
  
  
  
  server <- function(input, output, session) {
    addClass(selector = "body", class = "sidebar-collapse")
    ##
    current_in2 <- reactiveVal()
    observe({
      current_in2(input$Select2)  
    })
    
    observeEvent(input$Select1,{
      updatePickerInput(session,'Select2',selected = current_in2(),
                        choices=unique(sort(df$Circuit[df$Player==input$Select1])))
    })
    ##
    
    #tab1
    output$plot1 <- renderPlotly({
      g <- df %>% 
        filter(Player == input$Select1, Circuit %in% input$Select2) %>%
        ggplot(aes(x = Date, y = Finish, label = Tournament)) + 
        geom_point() + 
        geom_smooth(se = F) + 
        scale_y_continuous(trans = 'reverse', breaks = seq(min(1), max(120))) + 
        scale_x_date(date_breaks = "1 year", date_labels = "%Y", guide = guide_axis(n.dodge = 2)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      g <- ggplotly(g)
    })
    
    output$plot1.1 <- renderPlotly({
      g <- df %>% 
        filter(Player == input$Select1, Circuit %in% input$Select2) %>% 
        arrange(Date) %>% 
        mutate(Winnings = cumsum(Win/2)) %>% 
        ggplot(aes(Date, Winnings, colour = Gender)) + 
        geom_point() + 
        geom_smooth(se = F) + 
        scale_y_continuous('Winnings', label = comma)
      g <- ggplotly(g)
    })
    
    output$table2 <- DT::renderDataTable({
      
      DT::datatable(t1 <- 
                      df %>% 
                      filter(Player == input$Select1, Circuit %in% input$Select2) %>% 
                      group_by(Circuit, Partner) %>% 
                      summarise('Total Played' = n(), 
                                'Avg Finish' = sprintf("%0.1f",mean(Finish, na.rm=T))) %>%
                      arrange(desc(`Total Played`)),
                    class = 'disply nowrap compact',
                    filter = 'top',
                    rownames = FALSE,
                    options = list(
                      lengthChange = FALSE,
                      columnDefs = list(list(className = 'dt-center', targets ="_all")),
                      sDom  = '<"top">lrpt<"bottom">i',# options
                      scrollX = TRUE, # allow user to scroll wide tables horizontally
                      stateSave = FALSE,
                      search = list(regex = FALSE, caseInsensitive = FALSE)))
    })
    #tab2
    output$plot2 <- renderPlotly({
      g <- df %>% 
        filter(Country != '' & Country == input$SelectA, Type %in% input$SelectB) %>% 
        group_by(Gender) %>% arrange(Date) %>% 
        mutate(Winnings = ifelse(Gender == 'M', cumsum(Win), 
                                 ifelse(Gender == 'W', cumsum(Win), NA))) %>% 
        ggplot(aes(Date, Winnings, colour = Gender)) + 
        geom_point() + 
        geom_smooth(se = F) + 
        scale_y_continuous('Winnings', label = comma)
      g <- ggplotly(g)
    })
    
    output$table3 <- DT::renderDataTable({
      
      DT::datatable(t1 <- 
                      df %>%
                      filter(Country == input$SelectA, Type %in% input$SelectB) %>%
                      arrange(Date) %>%
                      group_by(Year) %>%
                      summarise(First_Place = sum(Finish == 1)/2, 
                                Second_Place = sum(Finish == 2)/2, 
                                Third_Place = sum(Finish == 3)/2, 
                                Fourth_Place = sum(Finish == 4)/2),
                    class = 'disply nowrap compact',
                    filter = 'top',
                    rownames = FALSE,
                    options = list(
                      lengthChange = FALSE,
                      columnDefs = list(list(className = 'dt-center', targets ="_all")),
                      sDom  = '<"top">lprt<"bottom">i',# options
                      scrollX = TRUE, # allow user to scroll wide tables horizontally
                      stateSave = FALSE,
                      search = list(regex = FALSE, caseInsensitive = FALSE)))
    })
    
    output$plot4 <- renderPlotly({
      g <- df %>% 
        filter(Country != '' & Country == input$SelectA, Type %in% input$SelectB) %>% 
        ggplot(aes(x = Date, y = Finish, label = Tournament)) + 
        geom_point() + 
        geom_smooth(se = F) + 
        scale_y_continuous(trans = 'reverse', breaks = seq(min(1), max(120))) + 
        scale_x_date(date_breaks = "1 year", date_labels = "%Y", guide = guide_axis(n.dodge = 2)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      g <- ggplotly(g)
    })
    
  })


