# Load packages
library(shiny)
library(shinythemes)
#library(dplyr)
#library(readr)
#library(readxl)
#library(reshape)
library(plotly)
#library(foreign)

# remove the following line for production environment
# NOTE: when it is remove the default data connection is used
# Sys.setenv(R_CONFIG_ACTIVE = "development") 

source('ui-extension.R')
source('common.R')

ui <- fluidPage(
  theme = shinytheme("lumen")
  , sidebarLayout(
    sidebarPanel(
      verticalLayout(
        h4("Segmentação de Usuário")
        , htmlOutput("schoolSelector")
        , htmlOutput("gradeSelector")
        , htmlOutput("classroomSelector")
        , htmlOutput("userSelector")
      )
      , verticalLayout(
        h4("Segmentação de Conteúdo")
        , htmlOutput("domainSelector")
        , htmlOutput("curriculumSelector")
        , htmlOutput("topicSelector")
        , htmlOutput("moreContentSegmentation")
      )
      , htmlOutput("temporalSegmentation")
      , fluidRow(
        actionButton("updateDataGraphButton", "Update Data/Graph", class = "btn-primary")
        , downloadButton("downloadCSVButton", "Download .csv"))
    )
    # Output: Description, lineplot, and reference
    , mainPanel(
      navbarPage(
        "Learning Analytics"
        , id = "mainNavigation"
        , tabPanel(
          "Desempenho"
          , value = "learning-performance"
          , verticalLayout(
            h3("Desempenho da Aprendizagem")
            , fillRow(
              radioButtons(
                "typeLearningPerformance"
                , strong("Percentagem de:")
                , list("completude"="pmc", "acertos"="pma", "erros"="pme")
                , inline = T
              )
              , height = "65px"
            )
            , tabsetPanel(
              type = "tabs"
              , tabPanel("Radar Chart", plotlyOutput(outputId = "learningPerformanceRadarChart", height = "800px"))
              , tabPanel("Dot Chart", plotlyOutput("learningPerformanceDotChart"), height = "800px")
              , tabPanel("Table", dataTableOutput("learningPerformanceTable"))
              , tabPanel("SQL",  verbatimTextOutput("learningPerformanceSQL"))
            )
          )
        )
        , tabPanel(
          "Desempenho:Séries Temporais"
          , value = "learning-performance-temporal-series"
          , verticalLayout(
            h3("Séries Temporais de Desempenho")
            , fillRow(
              radioButtons(
                "typeLearningPerformanceTemporalSerie"
                , strong("Percentagem de:")
                , list("completude"="pmc", "acertos"="pma", "erros"="pme")
                , inline = T
              )
              , height = "65px"
            )
            , tabsetPanel(
              type = "tabs"
              , tabPanel("Line Chart", plotlyOutput(outputId="learningPerformanceTemporalSerieLineChart", height = "800px"))
              , tabPanel("Table", dataTableOutput("learningPerformanceTemporalSerieTable"))
              , tabPanel("SQL",  verbatimTextOutput("learningPerformanceTemporalSerieSQL"))
            )
          )
        )
        , tabPanel(
          "Engajamento:Séries Temporais"
          , value = "learning-engagement-temporal-series"
          , verticalLayout(
            h3("Séries Temporais de Engajamento")
            , fillRow(
              radioButtons(
                "typeLearningEngagementTemporalSerie"
                , strong("Tempo:")
                , list("efetivo"="te", "interacao"="ti", "observacao"="to")
                , inline = T
              )
              , radioButtons(
                "formulaLearningEngagementTemporalSerie"
                , strong("Formula:")
                , list("tempo medio"="tm", "tempo acumulado"="ta")
                , inline = T
              )
              , height = "65px"
            )
            , tabsetPanel(
              type = "tabs"
              , tabPanel("Line Chart", plotlyOutput(outputId="learningEngagementTemporalSerieLineChart", height = "800px"))
              , tabPanel("Table", dataTableOutput("learningEngagementTemporalSerieTable"))
              , tabPanel("SQL",  verbatimTextOutput("learningEngagementTemporalSerieSQL"))
            )
          )
        )
      )
    )
    , position = "right"
  )
)

# Define server for app
server <- function(input, output) {
  
  # rendering UI for user segmentation
  output$schoolSelector <- renderUI({
    choices <- get_choices("school")
    selectInput(
      inputId = "school"
      , multiple = T
      , label = strong("Unidades de ensino:")
      , choices = choices)
  })
  
  output$gradeSelector <- renderUI({
    if (length(input$school) > 0) {
      choices <- get_choices("grade", list(school=input$school))
      selectInput(
        inputId = "grade"
        , multiple = T
        , label = strong("Grau educacionais:")
        , choices = choices)
    }
  })
  
  output$classroomSelector <- renderUI({
    if (length(input$grade) > 0) {
      choices <- get_choices("classroom", list(school=input$school, grade=input$grade))
      selectInput(
        inputId = "classroom"
        , multiple = T
        , label = strong("Turmas:")
        , choices = choices)
    }
  })
  
  output$userSelector <- renderUI({
    if (length(input$classroom) > 0) {
      choices <- get_choices("user", list(school=input$school, grade=input$grade, classroom=input$classroom))
      selectInput(
        inputId = "user"
        , multiple = T
        , label = strong("Usuários:")
        , choices = choices)
    }
  })
  
  # rendering UI for resources segmentation
  output$domainSelector <- renderUI({
    choices <- get_choices("domain")
    selectInput(
      inputId = "domain"
      , multiple = T
      , label = strong("Disciplinas:")
      , choices = choices)
  })
  
  output$curriculumSelector <- renderUI({
    if (length(input$domain) > 0) {
      choices <- get_choices("curriculum", list(domain=input$domain))
      selectInput(
        inputId = "curriculum"
        , multiple = T
        , label = strong("Curriculums:")
        , choices = choices)
    }
  })
  
  output$topicSelector <- renderUI({
    if (length(input$curriculum) > 0) {
      choices <- get_choices("topic", list(domain=input$domain, curriculum=input$curriculum))
      selectInput(
        inputId = "topic"
        , multiple = T
        , label = strong("Tópicos:")
        , choices = choices)
    }
  })
  
  # rendering UI for more fine content segmentation
  output$moreContentSegmentation <- renderUI({
    if ((input$mainNavigation != "learning-performance")
        & (length(input$topic) > 0)) {
      verticalLayout(
        checkboxInput("moreContentCheckbox", label="Mostrar mais", value=F)
        , htmlOutput("resourceSelector")
      )
    }
  })
  
  output$resourceSelector <- renderUI({
    if ((input$mainNavigation != "learning-performance")
        & (length(input$topic) > 0)
        & input$moreContentCheckbox) {
      choices <- get_choices("resource", list(domain=input$domain, curriculum=input$curriculum, topic=input$topic))
      selectInput(
        inputId = "resource"
        , multiple = T
        , label = strong("Recurso:")
        , choices = choices)
    }
  })
  
  # rendering UI for temporal segmentation
  output$temporalSegmentation <- renderUI({
    if (input$mainNavigation != "learning-performance") {
      verticalLayout(
        h4("Segmentação Temporal")
        , radioButtons("typeDateInput"
                       , strong("Tipo de segmentação:")
                       , list("diaria"="daily", "mensal"="monthly")
                       , inline = T)
        , htmlOutput("dateInputSelector")
      )
    }
  })
  
  output$dateInputSelector <- renderUI({
    if (input$typeDateInput == "daily") {
      dateRangeInput("dateInput"
                     , strong("Date range")
                     , format = "dd-mm-yyyy"
                     , start = "2018-04-01"
                     , end = format(Sys.Date(),"%Y-%m-%d")
                     , max = format(Sys.Date(),"%Y-%m-%d")
                     , language = "pt-BR")
    } else {
      dateRangeMonthsInput("dateInput"
                           , strong("Date range")
                           , format = "dd-mm-yyyy"
                           , start = "2018-04-01"
                           , end = format(Sys.Date(),"%Y-%m-%d")
                           , max = format(Sys.Date(),"%Y-%m-%d")
                           , language = "pt-BR")
    }
  })
  
  # get params for the application
  app_params <- eventReactive(input$updateDataGraphButton, {
    filters <- list(school = input$school, domain = input$domain)
    
    if (length(input$domain) > 0) filters[["curriculum"]] <- input$curriculum
    if (length(input$curriculum) > 0) filters[["topic"]] <- input$topic
    if (length(input$school) > 0) filters[["grade"]] <- input$grade
    if (length(input$grade) > 0) filters[["classroom"]] <- input$classroom
    if (length(input$classroom) > 0) filters[["user"]] <- input$user
    
    if (input$mainNavigation != "learning-performance") {
      if (length(input$topic) > 0) {
        if (input$moreContentCheckbox) filters[["resource"]] <- input$resource
      }
      filters[["endDate"]] <- tryCatch(
        as.numeric(as.POSIXct(input$dateInput[2]), format = "dd-mm-yyyy")
        , error = function(e) { as.numeric(as.POSIXct(Sys.Date())) })
      filters[["startDate"]] <- tryCatch(
        as.numeric(as.POSIXct(input$dateInput[1]), format = "dd-mm-yyyy")
        , error = function(e) { as.numeric(as.POSIXct(Sys.Date())) })
    }
    
    options <- list(
      typeDate = input$typeDateInput
      , typeLearningPerformance = ifelse(
          input$mainNavigation == "learning-performance"
          , input$typeLearningPerformance
          , input$typeLearningPerformanceTemporalSerie)
      , typeLearningEngagement = input$typeLearningEngagementTemporalSerie
      , formulaLearningEngagement = input$formulaLearningEngagementTemporalSerie
    )
    
    list(dtype = input$mainNavigation, filters = filters, options = options)
  })
  
  # rendering UI for learning performance
  output$learningPerformanceRadarChart <- renderPlotly({
    ctype <- 'radar'
    params <- app_params()
    get_plotly(params$dtype, ctype, params$filters, params$options)
  })
  
  output$learningPerformanceDotChart <- renderPlotly({
    ctype <- 'dot'
    params <- app_params()
    get_plotly(params$dtype, ctype, params$filters, params$options)
  })
  
  output$learningPerformanceTable <- renderDataTable({
    params <- app_params()
    get_data.as.df(params$dtype, params$filters, params$options)
  })
  
  output$learningPerformanceSQL <- renderText({
    params <- app_params()
    get_data(params$dtype, params$filters, params$options)$SQL
  })
  
  # rendering UI plots for learning engagement
  output$learningEngagementTemporalSerieLineChart <- renderPlotly({
    ctype <- 'tserie'
    params <- app_params()
    get_plotly(params$dtype, ctype, params$filters, params$options)
  })
  
  output$learningEngagementTemporalSerieTable <- renderDataTable({
    params <- app_params()
    get_data.as.df(params$dtype, params$filters, params$options)
  })
  
  output$learningEngagementTemporalSerieSQL <- renderText({
    params <- app_params()
    get_data(params$dtype, params$filters, params$options)$SQL
  })
  
  # rendering UI plots for learning performance
  output$learningPerformanceTemporalSerieLineChart <- renderPlotly({
    ctype <- 'tserie'
    params <- app_params()
    get_plotly(params$dtype, ctype, params$filters, params$options)
  })
  
  output$learningPerformanceTemporalSerieTable <- renderDataTable({
    params <- app_params()
    get_data.as.df(params$dtype, params$filters, params$options)
  })
  
  output$learningPerformanceTemporalSerieSQL <- renderText({
    params <- app_params()
    get_data(params$dtype, params$filters, params$options)$SQL
  })
  
  # setting other UI events: download, 
  output$downloadCSVButton <- downloadHandler(
    filename = paste0(input$mainNavigation,"-data.csv")
    , content = function(file) {
      params <- app_params()
      df <- get_data.as.df(params$dtype, params$filters, params$options)
      write.csv(df, file, row.names = F)
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)

