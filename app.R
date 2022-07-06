library(shiny)
library(tidyverse)
library(vroom)

ui <- fluidPage(
    fluidRow(
        column(6,uiOutput("prod_codes"))
        ),
    fluidRow(
        column(4, tableOutput("diag")),
        column(4, tableOutput("body_part")),
        column(4, tableOutput("location"))
    ),
    fluidRow(
        column(12, plotOutput("agesex"))
    )
)

server <- function(input, output, session) {
    # store app state using reactiveValues() ?
    db <- reactiveValues()

    # observe() is run on startup
    observe({
        db$injuries <- vroom("injuries.tsv.gz");
        db$products <- vroom("products.tsv");
        db$population <- vroom("population.tsv");
        # use renderUI() to generate dynamic ui elements
        output$prod_codes <- renderUI({
            selectInput("code", "Product", choices=setNames(db$products$prod_code, db$products$title))
            })
    })

    # Create reactive expressions
    prodx <- reactive(filter(db$injuries, prod_code==input$code))
    # Use a reactive expression by calling it like a function
    diag <- reactive(count(prodx(), diag, wt=weight, sort=TRUE))
    body_part <- reactive(count(prodx(), body_part, wt=weight, sort=TRUE))
    location <- reactive(count(prodx(), location, wt=weight, sort=TRUE))
    agesex <- reactive({
        count(prodx(), age, sex, wt=weight) %>%
        left_join(db$population, by=c("age","sex")) %>%
        mutate(rate = n/population)
    })
    agesexplot <- reactive({
        ggplot(agesex()) + geom_line(aes(age,rate,color=sex),na.rm=TRUE)
    })

    #render outputs
    output$diag = renderTable(diag())
    output$body_part = renderTable(body_part())
    output$location = renderTable(location())
    output$agesex = renderPlot(agesexplot(), res=96)
}

shinyApp(ui, server)
