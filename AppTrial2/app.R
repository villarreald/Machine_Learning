library(shiny)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Linear Modeling of CSV Data"),

    # Sidebar with a slider input for number of bins, document uploader 
    sidebarLayout(
        sidebarPanel(
            
            #Side bar document uploader
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Checkbox if file has header ----
            checkboxInput("header", "Header", TRUE),
            
            # Input: Select separator ----
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            
            # Input: Select quotes ----
            radioButtons("quote", "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"'),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Select number of rows to display ----
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head"),
            actionButton("go", "Create Linear Model"),
        
        # Horizontal line ----
        tags$hr(),
        
        htmlOutput("RSquared"),
        htmlOutput("coefficient"),
        htmlOutput("Intercept"),
        htmlOutput("Slope")
),
          

# Show a plot of the generated distribution

mainPanel(
    
    fluidRow(
                splitLayout(cellWidths = c("30%", "70%"), tableOutput("contents"), plotlyOutput("distPlot")),
           plotlyOutput("distLine"),
        )
    )
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    dataInput <- reactive({
        req(input$file1)
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
        return(df)
        
    })
    
    #Render CSV Data Table
    output$contents <- renderTable({
        
        if(input$disp == "head") {
            return(head(dataInput()))
        }
        else {
            return(dataInput())
        }
        
    })
    
    line <- eventReactive(input$go, {lm(dataInput()$y ~ dataInput()$x)})
    
    #Render Graph
    output$distPlot <- renderPlotly({
        plot_ly(dataInput(), x = ~x, y = ~y, type = 'scatter', mode = 'markers')
    })
    
    output$distLine <- renderPlotly({
        plot_ly(dataInput(), x = ~x, y = ~y, type = 'scatter', mode = 'markers')%>%
        add_trace(dataInput(), x = ~x, y = fitted(line()), mode = "lines", showlegend = F)
    })
    
    output$RSquared <- renderUI({
        str1 <- paste("R", tags$sup(2)," - ", sep = "")
        str2 <- paste(format(round(summary(line())$r.squared, 3)))
        HTML(paste(str1, str2))
    })
    
    output$Slope <- renderUI({
        str1 <- paste("Slope - ")
        str2 <- paste(format(round(summary(line())$coefficients[2], 3)))
        HTML(paste(str1, str2))
    })
    
    output$Intercept <- renderUI({
        str1 <- paste("Intercept - ")
        str2 <- paste(format(round(summary(line())$coefficients[1], 3)))
        HTML(paste(str1, str2))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)