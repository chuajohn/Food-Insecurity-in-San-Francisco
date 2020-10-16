#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)


data <- read_rds("shiny_data/data.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    navbarPage("Food Insecurity in San Francisco",
               tabPanel("Model",
                        titlePanel("Racial Breakdown of SF Residents on Aid"),
                        
                        # Sidebar with a slider input for number of bins 
                        sidebarLayout(
                            sidebarPanel(
                                sliderInput("bins",
                                            "Number of bins:",
                                            min = 1,
                                            max = 50,
                                            value = 30)),
                            
                            # Show a plot of the generated distribution
                            mainPanel(plotOutput("distPlot")))),
               
               tabPanel("About",
                        titlePanel("About"),
                        br(),
                        h4("About the data"),
                        p("This dataset is from the SF Mayor's Office of Housing and Community Development, which contains data about low-income SF residents participating in the Community Development Public Service Programs. The plot is a simple visualization of the racial composition of these recipients. I will definitely be playing around with the data more to create different types of plots. 
"),
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
            geom_bar()
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
