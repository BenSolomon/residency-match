library(shiny);library(dplyr);library(tidyr);library(DT); library(ggplot2); library(shinycssloaders)

df <- read.csv("matchData.csv", header=T, stringsAsFactors = F) %>% mutate(Stat = ifelse(Stat == "SOAP", "Unmatched", Stat))
states <- levels(factor(df$State))
years <- levels(factor(df$Year))
specialties <- levels(factor(df$simpleSpecialty))

ui <- navbarPage("Match Data",
  header = 
    div(
      tags$head(
        includeHTML("google-analytics.js"),
        includeCSS("closableFooterCSS.css")
      ), 
      includeHTML("closableFooterHTML.html"),

      style="margin-left:2%; margin-right:2%;",
      titlePanel("Match Data"),
      div(style="border:solid; background-color:#f5f7fa; border-radius: 25px",
          column(4,selectizeInput(inputId = "specialty",
                      label = "Choose specialty",
                      choices = c("All specialties", sort(specialties)),
                      selected = "All specialties")),      
          column(4,selectInput(inputId = "state",
                      label = "Choose state",
                      choices = c("All states", states),
                      selected = "All states")),
          column(4,selectInput(inputId = "year",
                      label = "Choose year",
                      choices = c("All years", rev(years)),
                      selected = "2020")),
      fluidRow(
        column(6, align = "center", checkboxInput("unmatched", label = "Only show unmatched positions?", value = FALSE)),
        column(6, align = "center", downloadButton('downloadData', 'Download filtered data'))
        )
      ),
      br()
  ),
  footer = div(column(12,
                      br(),
    p("Source:", a(href="https://mk0nrmp3oyqui6wqfm.kinstacdn.com/wp-content/uploads/2020/05/Program-Results_2016_2020.pdf", 
                   "NRMP 2020 Main Match Results"), align = "right"),
    p("Source:", a(href="https://mk0nrmpcikgb8jxyd19h.kinstacdn.com/wp-content/uploads/2019/04/Program-Result-2015-2019.pdf", 
                   "NRMP 2019 Main Match Results"), align = "right"),
    p("Source:", a(href="http://www.nrmp.org/wp-content/uploads/2018/04/Program-Results-2014-2018.pdf", 
                   "NRMP 2018 Main Match Results"), align = "right"),
    p("Source:", a(href="http://www.nrmp.org/wp-content/uploads/2017/06/Main-Match-Program-Results-2013-2017.pdf", 
                   "NRMP 2017 Main Match Results"), align = "right")
  )),
  tabPanel("Data Browser",
    div(DT::dataTableOutput("mytable"))
    ),
  tabPanel("Plots",
    div(style="border:solid; border-radius: 25px; border-width: 1px",
      fluidRow(h4(textOutput("countText"), align = "center")),
      fluidRow(style="height=25%;",
        column(12, withSpinner(plotOutput("countPlot")) )
      ),
      fluidRow(br())
    ),
    div(fluidRow(br())),
    div(style="border:solid; border-radius: 25px; border-width: 1px",
      fluidRow(h4(textOutput("rateText"), align = "center")),
      fluidRow(style="height=25%;",
        column(12, withSpinner(plotOutput("ratePlot")) )
      ),
      fluidRow(br())
    ),
    includeHTML("closableFooterJS.html")
  )
)

server <- function(input, output) {

  
  ##DATA TABLE DISPLAY##
  data.browse <- reactive({
    if (input$state != "All states") {df <- df %>% filter(State == input$state)} 
    if (input$year != "All years") {df <- df %>% filter(Year == input$year)}
    if (input$specialty != "All specialties"){df <- df %>% filter(simpleSpecialty == input$specialty)}
    else {df <- df %>% select(-simpleSpecialty) %>% distinct()}
    df <- df %>% spread(Stat, value, fill = 0) %>% select(Program, Specialty, Code, State, City, Year, Quota, Matched, Unmatched)
    if (input$unmatched == TRUE) {df <- df %>% filter(Unmatched > 0)}
    df
  })
  
  output$mytable = DT::renderDataTable({data.browse()})
  
  output$downloadData <- downloadHandler(
    filename = "filteredMatchData.csv",
    content = function(file) {
      write.csv(data.browse(), file,row.names = FALSE)
    }
  )
  
  
  ##PLOT - POSITION COUNTS OVER TIME##
  output$countText <- renderText({paste("Displaying stats for", input$specialty, "in", input$state)}) 
  data.count <- reactive({
    if (input$state != "All states") {df <- df %>% filter(State == input$state)} 
    if (input$specialty != "All specialties"){df <- df %>% filter(simpleSpecialty == input$specialty)}
    else {df <- df %>% select(-simpleSpecialty) %>% distinct()}
    df <- df %>% mutate(Stat = factor(Stat, levels=c("Quota", "Matched", "Unmatched")))
    df
  })
  output$countPlot <- renderPlot({
    data.count() %>% 
      ggplot(aes(x = Year, y = value)) + 
      stat_summary(fun.y = sum, geom = "path", aes(color = Stat), size = 2) +
      stat_summary(fun.y = sum, geom = "point", size = 3) +
      facet_wrap(~Stat, scales = "free") +
      theme_bw() +
      scale_color_brewer(palette = "Set1") +
      theme(legend.position = "none",
            axis.title = element_blank(),
            axis.text.x = element_text(angle = 90, vjust = 0.5),
            axis.text = element_text(size = 12),
            strip.text = element_text(size = 14, face = "bold"))
  })
  
  
  ##PLOT - MATCH RATE OVER TIME##
  output$rateText <- renderText({paste("Fill rates for ", input$state, "in", input$year)})
  data.rate <- reactive({
    if (input$state != "All states") {df <- df %>% filter(State == input$state)} 
    if (input$year != "All years") {df <- df %>% filter(Year == input$year)}
    df <- df %>% 
      spread(Stat, value, fill = 0) %>%
      group_by(simpleSpecialty) %>%
      summarise(Quota = sum(Quota), Matched = sum(Matched)) %>%
      filter(Quota != 0) %>%
      mutate(PercentMatched = 100*Matched/Quota) %>%
      arrange(desc(PercentMatched)) %>%
      mutate(simpleSpecialty = factor(simpleSpecialty, levels = simpleSpecialty),
             simpleSpecialty = droplevels(simpleSpecialty))
    df
  })
  output$ratePlot <- renderPlot({
    data.rate() %>% {
      ggplot(., aes(x = simpleSpecialty, y = PercentMatched)) +
      geom_bar(stat = "identity") +
      theme_bw() +
      coord_cartesian(ylim=c(min(.$PercentMatched, na.rm = T), 100)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
            axis.text = element_text(size = 12)) +
      labs(x = "",
           y = "Percent positions filled")
      }
  })
}

shinyApp(ui, server)