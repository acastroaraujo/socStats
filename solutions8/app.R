
library(shiny)
## remotes::install_github("datalorax/equatiomatic")
library(equatiomatic)
library(dplyr)
library(tidyr)
library(bslib)

formatEq <- equatiomatic:::format.equation

input <- readRDS("data.rds")

d <- input[["df"]]
dict <- input[["dict"]]

ui <- fluidPage(
  
  theme = bslib::bs_theme( 
    base_font = bslib::font_google("Arsenal"),
    heading_font = bslib::font_google("Arsenal"),
    version = "3", 
  ),
  
  titlePanel("socStats — Solutions 8", windowTitle = "socStats8"),
  
  withMathJax(),
  fluidRow(
    column(
      width = 4,
      wellPanel(
      selectInput("Y", "Y", choices = dict[["id"]], selected = sample(dict[["id"]], 1)),
      uiOutput("descriptionY"),
      uiOutput("choicesY"),
      selectInput("X", "X", choices = dict[["id"]], selected = sample(dict[["id"]], 1)),
      uiOutput("descriptionX"),
      uiOutput("choicesX"),
      actionButton("solve", "Show Answer")
      )
    ),
    column(
      width = 3,
      verbatimTextOutput("crosstab")
    ),
    column(
      width = 5,
      uiOutput("models")
    )
  )
)

server <- function(input, output, session) {
  
  df <- reactive({
    drop_na(d[ , c(input$Y, input$X)])
  })
  
  ## Y ----
  
  output$descriptionY <- renderUI({
    subset <- dict[dict[["id"]] == input$Y, ]
    helpText(subset[["description"]], br(), br(), style = "font-size:1.2em;")
  })
  
  output$choicesY <- renderUI({
    req(df())
    checkboxGroupInput("yvals", "Y = 1:", choices = levels(df()[[input$Y]]), selected = isolate(input$yvals))
  })
  
  ## X ----
  
  output$descriptionX <- renderUI({
    subset <- dict[dict[["id"]] == input$X, ]
    helpText(subset[["description"]], br(), br(), style = "font-size:1.2em;")
  })
  
  output$choicesX <- renderUI({
    req(df())
    checkboxGroupInput("xvals", "X = 1:", choices = levels(df()[[input$X]]), selected = isolate(input$xvals))
  })
  
  ## CREATE ----
  
  out <- eventReactive(input$solve, ignoreNULL = TRUE, {
    
    Y <- sym(input$Y)
    X <- sym(input$X)
    
    df() |> 
      mutate(
        Y = as.integer({{Y}} %in% input$yvals), 
        X = as.integer({{X}} %in% input$xvals)
      )
  })
  
  output$crosstab <- renderPrint({
    req(out())
    
    tab <- with(out(), table(X, Y))
    cat("Contingency Table:\n")
    print(addmargins(tab))
    cat("\n\n")
    cat("Row Percentages:\n")
    round(prop.table(tab, margin = 1), 4)
  })
  
  output$models <- renderUI({
    req(out())
    
    ols <- lm(Y ~ X, data = out())
    logit <- glm(Y ~ X, data = out(), family = binomial(link = "logit"))
    
    tagList(
      column(6, h3("Linear Probability"), br(),
      withMathJax(helpText(formatEq(extract_eq(ols, use_coefs = TRUE))))),
      column(6, HTML(modelsummary::msummary(ols, gof_map = "none", estimate = "{estimate}{stars}"))),
      column(6, h3("Logistic Regression"), br(),
      withMathJax(helpText(formatEq(extract_eq(logit, use_coefs = TRUE))))),
      column(6, HTML(modelsummary::msummary(logit, gof_map = "none", estimate = "{estimate}{stars}")))
    )
    
  })
  
  
}

shinyApp(ui, server)