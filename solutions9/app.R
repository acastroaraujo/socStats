
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
  
  titlePanel("socStats — Solutions 9", windowTitle = "socStats9"),
  
  withMathJax(),
  fluidRow(
    column(
      width = 3,
      wellPanel(
      selectInput("Y", "Y", choices = dict[["id"]], selected = dict[["id"]][[1]]),
      uiOutput("descriptionY"),
      uiOutput("choicesY"),
      selectInput("X", "X", choices = dict[["id"]], selected = dict[["id"]][[2]]),
      uiOutput("descriptionX"),
      uiOutput("choicesX"),
      selectInput("Z", "Z", choices = dict[["id"]], selected = dict[["id"]][[3]]),
      uiOutput("descriptionZ"),
      uiOutput("choicesZ"),
      actionButton("solve", "Show Answer")
      )
    ),
    column(
      width = 3,
      verbatimTextOutput("crosstab")
    ),
    column(
      width = 6,
      uiOutput("models")
    )
  )
)

server <- function(input, output, session) {
  
  df <- reactive({
    drop_na(d[ , c(input$Y, input$X, input$Z)])
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
  
  ## Z ----
  
  output$descriptionZ <- renderUI({
    subset <- dict[dict[["id"]] == input$Z, ]
    helpText(subset[["description"]], br(), br(), style = "font-size:1.2em;")
  })
  
  output$choicesZ <- renderUI({
    req(df())
    checkboxGroupInput("zvals", "Z = 1:", choices = levels(df()[[input$Z]]), selected = isolate(input$zvals))
  })
  
  ## CREATE ----
  
  out <- eventReactive(input$solve, ignoreNULL = TRUE, {
    
    Y <- sym(input$Y)
    X <- sym(input$X)
    Z <- sym(input$Z)
    
    df() |> 
      mutate(
        Y = as.integer({{Y}} %in% input$yvals), 
        X = as.integer({{X}} %in% input$xvals),
        Z = as.integer({{Z}} %in% input$zvals)
      )
  })
  
  output$crosstab <- renderPrint({
    req(out())
    
    tab <- with(out(), table(X, Y, Z))
    cat("Contingency Table:\n")
    print(addmargins(tab, margin = c(1, 2)))
    cat("\n\n")
    cat("Row Percentages:\n")
    round(prop.table(tab, margin = c(1, 3)), 4)
  })
  
  output$models <- renderUI({
    req(out())
    
    ols <- lm(Y ~ X*Z, data = out())
    logit <- glm(Y ~ X*Z, data = out(), family = binomial(link = "logit"))
    
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