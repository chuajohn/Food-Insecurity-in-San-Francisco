
library(shiny)
library(tidyverse)
library(shinythemes)


data <- read_rds("shiny_data/data.rds")
district_level <- read_rds("shiny_data/data_new.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
    shinythemes::themeSelector(),
    navbarPage("Food Insecurity in San Francisco",
               tabPanel("The Neighborhood",
                        titlePanel("Visualizing Needs in the Neighborhood of Alemany Farm"),
                        
                            # Show a plot of the generated distribution
                            mainPanel(plotOutput("distPlot"), 
                            p("The most common ethnic group represented among aid recipients is Hispanic"),
                            plotOutput("histPlot"), 
                             p("They are concentrated among low-income neighborhoods in SF."),
                            plotOutput("newPlot"),
                            leafletOutput("mapPlot")
                            ))),
               
               
               tabPanel("Demographics",
                        titlePanel("By the Numbers: Assessing Needs in SF"),
                        
                        # Sidebar with a slider input for number of bins 
                        sidebarLayout(
                            sidebarPanel(selectInput("district_choice", "Supervisorial District", unique(district_level$supervisor_district)), 
                                         selectInput("fiscal_year", "Fiscal Year", unique(district_level$fiscal_year)),
                            selectInput(inputId = "y", 
                                        label = "Y-axis:",
                                        choices = c("Reading" = "read", "Writing" ="write", "Math" = "math", "Science" = "science", "Social Studies" = "socst"), 
                                        selected = "math")),
                            
                            
                            # Show a plot of the generated distribution
                            mainPanel(plotOutput("distPlot"), 
                                      p("The most common ethnic group represented among aid recipients is Hispanic"),
                                      plotOutput("histPlot"), 
                                      p("They are concentrated among low-income neighborhoods in SF."),
                                      plotOutput("newPlot"),
                                      leafletOutput("mapPlot")
                            ))),
               
               
               tabPanel("About",
                        titlePanel("About"),
                        br(),
                        h4("About the data"),
                        p("This dataset is from the SF Mayor's Office of Housing and Community Development, which contains data about low-income SF residents participating in the Community Development Public Service Programs. The plot is a simple visualization of the racial composition of these recipients. I will definitely be playing around with the data more to create different types of plots."),
                        br(),
                        h4("About me"),
                        p("I am a student at Harvard College studying Gov."),
                        p("The source code for this Shiny App can be found at my GitHub", 
                          a("HERE", href="https://github.com/chuajohn/sffoodinsecurity"))
                        )))


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        data %>%
            ggplot(aes(x = race_ethnicity)) +
            geom_bar() +
            labs(title = "SF Mayor's Office Aid Recipients According to Racial Group", x = "Race/Ethnicity", y = "Number of Individuals") +
            theme_minimal()
        })
    
    output$histPlot <- renderPlot({
        data %>%
            ggplot(aes(x = neighborhood)) +
            geom_bar() +
            labs(title = "SF Mayor's Office Aid Recipients According to Neighborhood", x = "Neighborhood", y = "Number of Individuals") +
            coord_flip() +
            theme_minimal()
    })
    
    output$newPlot <- renderPlot({
        district_level %>%
            filter(supervisor_district == input$district_choice) %>%
            filter(fiscal_year == input$fiscal_year) %>%
            ggplot(aes(x = race_ethnicity)) +
            geom_bar() +
            labs(title = paste("SF Mayor's Office Aid Recipients According to Race In District ", input$district_choice), x = "Race/Ethnicity", y = "Number of Individuals") +
            coord_flip() +
            theme_minimal()
    })
    
    output$mapPlot <- renderLeaflet({
        
        
        
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
