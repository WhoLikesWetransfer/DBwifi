library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)

path3 <- "C:\\Users\\Jonathan\\Hackathon\\realbahnhof.csv"
path4 <- "C:\\Users\\Jonathan\\Hackathon\\qualitaet.csv"
realbahnhof <- read_delim(file = path3, delim = ",") %>%
  separate(streckenname, into = c("von", "nach"), sep = " nach ", remove = FALSE)
qualitaet <- read_delim(file = path4, delim = ",") %>%
  separate(streckenname, into = c("von", "nach"), sep = " nach ", remove = FALSE)

body <- dashboardBody(
    tabBox(width = 12,
      title = "DB Wifi Qualität",
      # The id lets us use input$tabset1 on the server to find the current tab
      id = "tabset1",
      tabPanel("Overview", "Deine DB",
              DT::dataTableOutput("tabelle")),
      tabPanel("Details", "Detailierte Informationen",   
              fluidRow(
                selectInput("von", label = "VON", 
                            choices = sort(unique(realbahnhof$von))),
                        selectInput("nach", label = "NACH", 
                           choices = sort(unique(realbahnhof$nach))),
               textOutput("select_check"),
               plotOutput("strecken_plot")),
              fluidRow(
                valueBoxOutput(
                  outputId = "downloadspeed", width = 4),
                valueBoxOutput(
                outputId = "uploadspeed", width = 4), 
                valueBoxOutput(
                  outputId = "ping", width = 4)
              )
              )
      )
)

      


shinyApp(
  ui = dashboardPage(
    dashboardHeader(title = "tabBoxes"),
    dashboardSidebar(type = "aside"),
    body
  ),
  server = function(input, output) {
    # The currently selected tab from the first box
    output$strecken_plot <- renderPlot({
      five <- realbahnhof %>%
        filter(streckenname %in% qualitaet$streckenname) %>%
        filter(von == input$von & nach == input$nach) %>%
        group_by(fahrt) %>%
        summarize(datenpunkte = n()) %>%
        head(5)

      realbahnhof %>%
        filter(fahrt %in% five$fahrt) %>%
        filter(von == input$von & nach == input$nach) %>%
        filter(downloadppavg < 10000) %>%
        ungroup() %>%
        group_by(fahrt) %>%
        mutate(minute = 1:n()) %>% 
        ggplot() +
          aes(x = minute, y = downloadppavg, colour = fahrt) +
          geom_smooth(se = FALSE)
    })
    
    output$downloadspeed <-  renderValueBox({
      result <- qualitaet %>%
        filter(von == input$von & nach == input$nach) %>%
        pull(download)
      if(result < 2000){
        color = "red"
      } else if (result < 5000) {
        color = "yellow"
      } else {
        color = "green"
      }
       valueBox(value = result, "Upspeed", color = color)
    })
    output$uploadspeed <-  renderValueBox({
      result <- qualitaet %>%
        filter(von == input$von & nach == input$nach) %>%
        pull(upload)
      if(result < 2000){
        color = "red"
      } else if (result < 5000) {
        color = "yellow"
      } else {
        color = "green"
      }
      valueBox(value = result, "Downloadspeed", color = color)
    })
    output$ping <-  renderValueBox({
      result <- qualitaet %>%
        filter(von == input$von & nach == input$nach) %>%
        pull(ping) %>% round(0)
      if(result < 275){
        color = "green"
      } else if (result < 350) {
        color = "yellow"
      } else {
        color = "red"
      }
      valueBox(value = result, "ping", color = color)
    })
    output$tabelle <- DT::renderDataTable({
      datatable(qualitaet, options = list(paging = FALSE))
    })
  }
)

