
library(shiny)
library(tidyverse)
library(shinythemes)
library(tigris)
library(leaflet)
library(haven)
library(gganimate)


data <- read_rds("shiny_data/data.rds")
district_level <- read_rds("shiny_data/data_new.rds")
opp_zones <- read_rds("shiny_data/opp_zones.rds")
food_banks <- read_rds("shiny_data/food_banks.rds")
cases <- read_rds("shiny_data/cases.rds")
calfresh_monthly <- read_rds("shiny_data/calfresh_monthly.rds")
PRI_calfresh <- read_rds("shiny_data/PRI_calfresh.rds")
calfresh_total <- read_rds("shiny_data/calfresh_total.rds")
newer_mrfei <- read_rds("shiny_data/newer_mrfei.rds")
income <- read_rds("shiny_data/income.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("journal"),
    navbarPage("Food Insecurity in San Francisco",
               tabPanel("The Neighborhood",
                        titlePanel("Visualizing Needs in the Neighborhood of Alemany Farm"),
                        
                            # Show a plot of the generated distribution
                            mainPanel(h3("Proximity to Federal Opportunity Zones"),
                                      p("Alemany Farm is within walking distance of five federally-designated “opportunity zones” in the Portola and Excelsior neighborhoods of San Francisco. Its network of partner organizations extends well into the Hunters Point and Bayview neighborhoods, home to more than five additional opportunity zones marked by poverty, homelessness, and food scarcity."),
                                leafletOutput("mapPlot"),
                                br(), 
                                h3("Income Distribution in San Francisco"),
                                p("Alemany Farm is surrounded by the lowest-income neighborhoods of San Francisco. It is the main food bank supplier in southeastern San Francisco."),
                                leafletOutput("incomePlot"),
                                br(), 
                                h3("Minority Populations in San Francisco"),
                                p("Alemany Farm is located at the heart of the Mission District, a historically Latinx community where many undocumented city residents and their families still reside in San Francisco. It is adjacent to neighborhoods that are disproportionately represented by minority communities."),
                                leafletOutput("minorityPlot"),
                                br(), 
                                h3("COVID-19 in San Francisco"),
                                p("Adjacent neighborhoods have been disproportionately hard hit by COVID-19, with the highest rates of COVID-19 cases in the city."),
                                leafletOutput("covidPlot"),
                                br(),
                                h3("Access to Healthy Food"),
                                p("Communities in the neighbourhood of Alemany Farm have less access to affordable fruits, vegetables, whole grains, low-fat/non-fat milk or dairy alternatives, and other foods that make up the full range of a healthy diet, based on CDC Modified Retail Food Environment Index (MRFEI) calculations."),
                                leafletOutput("mrfeiPlot")
                            )),
               
               
               tabPanel("Demographics",
                        titlePanel("By the Numbers: Assessing Needs in SF"),
                        
                        # Sidebar with a slider input for number of bins 
                        sidebarLayout(
                            sidebarPanel(selectInput("district_choice", "Fig. 1 Supervisorial District", unique(district_level$supervisor_district), selected = 8), 
                                         varSelectInput("dem_variable", "Fig. 1 Demographic Variable:", district_level[, c(5:7)]),
                                         varSelectInput("variable", "Fig. 2 Variable:", calfresh_monthly[, c(4:6)]),
                                         varSelectInput("calfreshdem_variable", "Fig. 4 Variable:", calfresh_total[, c(3:8)])),
                            
                            #selectInput(inputId = "y", 
                                        #label = "Variable - Calfresh Monthly:",
                                        #choices = unique(names(calfresh_monthly)), 
                                        #selected = "cal_fresh_persons"
     
                            
                            # Show a plot of the generated distribution
                            mainPanel(h3("Demographics of Aid Recipients in District 8"),
                                      plotOutput("newPlot"),
                                      p("They are concentrated among low-income neighborhoods in SF."),
                                      br(), 
                                      h3("Trends in Food Insecurity in terms of Enrollment in Federal Food Stamps Program"),
                                      plotOutput("calfreshmonthlyPlot"),
                                      p("There is a sharp rise in the number of individuals and households in San Francisco enrolled in the federal food stamps program Calfresh after the outbreak of COVID-19, a trend reflected in the monthly unemployment rates as well. This suggests that food insecurity is on the rise as a result of the pandemic."),
                                      br(), 
                                      h3("Federal Food Stamps Program Reach in San Francisco"),
                                      plotOutput("PRI_calfreshPlot"),
                                      p("The Calfresh program in San Franciso has consistently had a lower program reach among food insecure individuals compared to statewide levels. Almost 50% of at-risk individuals are not enrolled in the Calfresh program (see About for details about the Calfresh Program Reach Index calculations.)"),
                                      br(),
                                      h3("Demographics of Recipients on Food Stamps in San Francisco"),
                                      plotOutput("calfresh_totalPlot")
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


    output$newPlot <- renderPlot({
        district_level %>%
            filter(supervisor_district == input$district_choice) %>%
            filter(fiscal_year == "2018-19") %>%
            ggplot(aes(x = !!input$dem_variable)) +
            geom_bar() +
            labs(title = paste("SF Mayor's Office Aid Recipients According to Race In District ", input$district_choice), x = "Race/Ethnicity", y = "Number of Individuals") +
            coord_flip() +
            theme_bw()
    })
    
    output$calfreshmonthlyPlot <- renderPlot({
        calfresh_monthly %>%
            filter(county == "San Francisco") %>%
            ggplot(aes(x = date, y = !!input$variable)) +
            geom_line() +
            theme_bw()})
    
    output$PRI_calfreshPlot <- renderPlot({
         PRI_calfresh %>%
            filter(county %in% c("San Francisco", "Statewide")) %>%
            ggplot(aes(y = pri, x = calendar_year, color = county)) +
            geom_point() +
            labs(title = "Calfresh Program Reach - San Francisco vs Statewide", x = "Year", y = "Program Reach Index (%)") + 
            theme_bw()
    })
    
    output$calfresh_totalPlot <- renderPlot({
        calfresh_total %>%
            ggplot(aes(y = !!input$calfreshdem_variable, x = calendar_year, color = county)) +
            geom_line() +
            labs(title = "Variable - San Francisco vs Statewide", x = "Year", y = "Count") + 
            theme_bw()
    })
    
    
    output$mapPlot <- renderLeaflet({
        tract_data <- tracts(state = "CA", county = "San Francisco", cb = TRUE) %>%
            mutate(NAME = as.numeric(NAME)) %>%
            geo_join(opp_zones, by = "NAME") 
        
        pal <- colorNumeric(
            palette = "Greens",
            domain = tract_data$Value)
        bins <- c(0, 5, 10)
        pal <- colorBin("YlOrRd", domain = tract_data$Value, bins = bins)
        
        getColor <- function(food_banks) {
            sapply(food_banks$lat, function(lat) {
                if(lat == 37.73230) {
                    "green"
                } else {
                    "orange"
                } })
        }
        
        icons <- awesomeIcons(
            icon = 'ios-close',
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(food_banks)
        )
        
        
        tract_data %>%      
            sf::st_transform(4326) %>% 
            leaflet() %>%
            addTiles() %>%
            addProviderTiles("CartoDB") %>% 
            addAwesomeMarkers(data = food_banks, ~as.numeric(long), ~as.numeric(lat), icon=icons, label=~as.character(name)) %>%
            setView(-122.409981, 37.773972, zoom = 12) %>% 
            setMaxBounds(lng1 = -122.517412, lat1 = 37.706829, lng2 = -122.347559, lat2 = 37.811813) %>%
            addTiles() %>%
            addPolygons(fillColor = ~pal(tract_data$Value), weight = 2,
                        opacity = 1,
                        color = "white",
                        dashArray = "3",
                        fillOpacity = 0.7) 
    })
    
    output$incomePlot <- renderLeaflet({
        income_data <- tracts(state = "CA", county = "San Francisco", cb = TRUE) %>%
            mutate(NAME = as.numeric(NAME)) %>%
            geo_join(income, by = "NAME") %>%
            mutate(tract_median_family_income = tract_median_family_income *1000)
        
        income_bins <- c(0, 40000, 80000, 120000, 160000, 200000, 240000, 280000, Inf)
        income_pal <- colorBin("Greens", domain = income_data$tract_median_family_income, bins = income_bins)
        
        income_data %>%
            sf::st_transform(4326) %>% 
            mutate(popup = str_c("Census Tract: ",  "<strong>", NAME, "</strong>",
                                 "<br/>",
                                 "Median Household Income (2019) ", "<strong>", tract_median_family_income, "</strong>") %>%
                       map(htmltools::HTML)) %>%
            leaflet() %>% 
            addTiles() %>%
            addProviderTiles("CartoDB") %>% 
            addAwesomeMarkers(data = food_banks, ~as.numeric(long), ~as.numeric(lat), icon=icons, label=~as.character(name)) %>%
            setView(-122.409981, 37.773972, zoom = 12) %>% 
            addPolygons(label = ~popup, fillColor = ~income_pal(income_data$tract_median_family_income), weight = 2,
                        opacity = 1,
                        color = "white",
                        dashArray = "3",
                        fillOpacity = 0.7) %>%
            addLegend(pal = income_pal, 
                      values = income_data$tract_median_family_income, 
                      position = "bottomright", 
                      title = "Median household incomes")
    })
    
    output$minorityPlot <- renderLeaflet({
        minority_bins <- c(0, 20, 40, 60, 80, 100)
        minority_pal <- colorBin("Blues", domain = income_data$minority_population, bins = minority_bins)
        
        income_data %>%
            sf::st_transform(4326) %>% 
            mutate(popup = str_c("Census Tract: ",  "<strong>", NAME, "</strong>",
                                 "<br/>",
                                 "Percent Minority Population ", "<strong>", minority_population, "</strong>",
                                 "<br/>",
                                 "Total Minority Population ", "<strong>", tract_minority_percent, "</strong>") %>%
                       map(htmltools::HTML)) %>%
            leaflet() %>% 
            addTiles() %>%
            addProviderTiles("CartoDB") %>% 
            addAwesomeMarkers(data = food_banks, ~as.numeric(long), ~as.numeric(lat), icon=icons, label=~as.character(name)) %>%
            setView(-122.409981, 37.773972, zoom = 12) %>% 
            addPolygons(label = ~popup, fillColor = ~minority_pal(income_data$minority_population), weight = 2,
                        opacity = 1,
                        color = "white",
                        dashArray = "3",
                        fillOpacity = 0.7) %>%
            addLegend(pal = minority_pal, 
                      values = income_data$minority_population, 
                      position = "bottomright", 
                      title = "Percent Minority Population")
    })
    
    output$covidPlot <- renderLeaflet({
        cases_data <- tracts(state = "CA", county = "San Francisco", cb = TRUE) %>%
            mutate(NAME = as.numeric(NAME)) %>%
            geo_join(cases, by = "NAME") 
        
        other_bins <- c(0, 20, 40, 60, 80, 100, 200, 300, Inf)
        pal <- colorBin("YlOrRd", domain = cases_data$count, bins = other_bins)
        
        getColor <- function(food_banks) {
            sapply(food_banks$lat, function(lat) {
                if(lat == 37.73230) {
                    "green"
                } else {
                    "orange"
                } })
        }
        
        icons <- awesomeIcons(
            icon = 'ios-close',
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(food_banks)
        )
        
        cases_data %>%
            sf::st_transform(4326) %>% 
            mutate(popup = str_c("Census Tract: ",  "<strong>", NAME, "</strong>",
                                 "<br/>",
                                 "COVID-19 Cases: ", "<strong>", count, "</strong>") %>%
                       map(htmltools::HTML)) %>%
            leaflet() %>% 
            addTiles() %>%
            addProviderTiles("CartoDB") %>% 
            addAwesomeMarkers(data = food_banks, ~as.numeric(long), ~as.numeric(lat), icon=icons, label=~as.character(name)) %>%
            setView(-122.409981, 37.773972, zoom = 12) %>% 
            addPolygons(label = ~popup, fillColor = ~pal(cases_data$count), weight = 2,
                        opacity = 1,
                        color = "white",
                        dashArray = "3",
                        fillOpacity = 0.7) %>%
            addLegend(pal = pal, 
                      values = cases_data$count, 
                      position = "bottomright", 
                      title = "COVID Cases")
    })
    
    output$mrfeiPlot <- renderLeaflet({
        merged_mrfei <- tracts(state = "CA", county = "San Francisco", cb = TRUE) %>%
            mutate(TRACTCE = as.numeric(TRACTCE)) %>%
            geo_join(newer_mrfei, by = "TRACTCE") 
        
        mrfei_bins <- c(0, 5, 10, 15, 20, 25, 30, 40, 65)
        mrfei_pal <- colorBin("Oranges", domain = merged_mrfei$mrfei, bins = mrfei_bins)
        
        getColor <- function(food_banks) {
            sapply(food_banks$lat, function(lat) {
                if(lat == 37.73230) {
                    "green"
                } else {
                    "orange"
                } })
        }
        
        icons <- awesomeIcons(
            icon = 'ios-close',
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(food_banks)
        )
        
        
        mrfei_map <- merged_mrfei %>%
            sf::st_transform(4326) %>% 
            mutate(popup = str_c("Census Tract: ",  "<strong>", NAME, "</strong>",
                                 "<br/>",
                                 "Modified Retail Food Environment Index: ", "<strong>", mrfei, "</strong>") %>%
                       map(htmltools::HTML)) %>%
            leaflet() %>% 
            addTiles() %>%
            addProviderTiles("CartoDB") %>% 
            addAwesomeMarkers(data = food_banks, ~as.numeric(long), ~as.numeric(lat), icon=icons, label=~as.character(name)) %>%
            setView(-122.409981, 37.773972, zoom = 12) %>% 
            addPolygons(label = ~popup, fillColor = ~mrfei_pal(merged_mrfei$mrfei), weight = 2,
                        opacity = 1,
                        color = "white",
                        dashArray = "3",
                        fillOpacity = 0.7) %>%
            addLegend(pal = mrfei_pal, 
                      values = merged_mrfei$mrfei, 
                      position = "bottomright", 
                      title = "CDC MRFEI Index")
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
