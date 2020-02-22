#' Shiny App Interface with rpart::rpart
#' 
#' User selects response variable and predictors, app displays optimal tree in 
#' terms of cross-validation error along with a summary table and importance 
#' plot. Relies on \code{\link[rpart]{rpart}} for fitting and 
#' \code{\link[rattle]{fancyRpartPlot}} for plotting.
#'
#' @param data Data frame.
#' 
#' @return Shiny app.
#'
#' @export
cart_app <- function(data = mtcars) {
  
  ui <- fluidPage(
    
    fluidRow(
      
      column(
        width = 12,
        plotOutput("pruned")
      )
      
    ), 
    
    br(), 
    br(), 
    
    fluidRow(
      
      column(
        width = 3, 
        selectInput("response", "Response variable", choices = names(data), selected = names(data)[1])
      ), 
      column(
        width = 6, 
        uiOutput("predictor_choices")
      ), 
      column(
        width = 3, 
        uiOutput("method_choices")
      )
      
    ), 
    
    br(), 
    
    
    
    br(),
    br(), 
    
    hr(), 
    
    fluidRow(
      
      column(
        width = 6, plotOutput("importance")
      ),
      
      column(
        width = 6, tableOutput("table")
      )
      
      
      
    )
    
  )
  
  
  server <- function(input, output) {
    
    output$predictor_choices <- renderUI({
      checkboxGroupInput("predictors", "Predictors", choices = setdiff(names(data), input$response), 
                         selected = setdiff(names(data), input$response), inline = TRUE)
    })
    
    output$method_choices <- renderUI({
      class.y <- class(data[[input$response]])
      method.choices <- c("anova", "poisson", "class")
      if (length(unique(data[[input$response]])) == nrow(data)) method.choices <- c("anova", "poisson")
      if (class.y == "factor") method.choices <- "class"
      radioButtons("method", "Method", choices = method.choices, inline = TRUE)
    })
    
    fit <- reactive({
      data.subset <- data[, c(input$response, input$predictors)]
      rpart(paste(input$response, ".", sep = " ~ "), data = data.subset, method = input$method)
    })
    
    output$table <- renderTable({
      req(input$predictors)
      as.data.frame(fit()$cptable)
    })
    
    output$pruned <- renderPlot({
      
      req(input$predictors)
      cps <- fit()$cptable[, 1]
      xerrors <- fit()$cptable[, 4]
      fit.optimal <- prune(fit(), cp = cps[which.min(xerrors)] + 1e-6)
      fancyRpartPlot(fit.optimal, main = "Pruned Tree (Minimum Cross-Validation Error)")
      
    })
    
    output$importance <- renderPlot({
      
      req(input$predictors)
      df.imp <- data.frame(
        Variable = names(fit()$variable.importance), 
        Importance = fit()$variable.importance
      )
      ggplot(df.imp, aes(x = reorder(Variable, Importance), y = Importance)) + 
        geom_col() + 
        coord_flip() + 
        theme_gray(base_size = 16) + 
        scale_y_continuous(limits = c(0, max(df.imp$Importance) * 1.1), expand = c(0, 0)) + 
        labs(title = "Variable Importance", 
             y = "", 
             x = "")
      
    })
    
  }
  
  shinyApp(ui = ui, server = server)
  
}
