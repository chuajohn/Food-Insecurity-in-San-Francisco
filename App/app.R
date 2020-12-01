
library(shiny)
library(tidyverse)
library(shinythemes)
library(tigris)
library(leaflet)
library(haven)
library(gganimate)
library(rstanarm)
library(gtsummary)
library(gt)
library(broom.mixed)



data <- read_rds("shiny_data/data.rds")
district_level <- read_rds("shiny_data/data_new.rds") %>%
    rename("Race/Ethnicity" = race_ethnicity, "Income Level" = income_level, 
           "Age Category" = age_category)
food_banks <- read_rds("shiny_data/food_banks.rds")
cases <- read_rds("shiny_data/cases.rds")
calfresh_monthly <- read_rds("shiny_data/calfresh_monthly.rds") %>%
    rename("Unemployment" = unemployment_monthly, "Households Enrolled in Calfresh" = cal_fresh_households,
           "Individuals Enrolled in Calfresh" = cal_fresh_persons)
PRI_calfresh <- read_rds("shiny_data/PRI_calfresh.rds")
calfresh_total <- read_rds("shiny_data/calfresh_total.rds") %>%
    rename("Individuals Aged between 18 and 59" = age_18_to_59_cal_fresh_july,
           "Children" = children_cal_fresh_july,
           "Total Population" = total_population_cy,
           "Elderly above Age 60" = total_elderly_60plus_cy)
newer_mrfei <- read_rds("shiny_data/newer_mrfei.rds")
income <- read_rds("shiny_data/income.rds")
income_new <- read_rds("shiny_data/income_new.rds")
opp_zones_new <- read_rds("shiny_data/opp_zones_new.rds")
filtered_CIH <- read_rds("shiny_data/filtered_CIH.rds")

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
                        tabsetPanel(tabPanel("Demographics by District",
                                             titlePanel("By the Numbers: Assessing Needs in SF"),
                                             
                                             # Sidebar to select different variables 
                                             sidebarLayout(
                                                 sidebarPanel(selectInput("district_choice", "Fig. 1 Supervisorial District", unique(district_level$supervisor_district), selected = 8), 
                                                              varSelectInput("dem_variable", "Fig. 1 Demographic Variable:", district_level[, c(5:7)])),

                                                 # Show a plot of the generated distribution
                                                 mainPanel(h3("Demographics of Aid Recipients in District 8"),
                                                           plotOutput("newPlot"),
                                                           p("Aid recipients in District 8 are predominantly Hispanic/Latino, most are classified as 'extremely low income', and they mainly fall in age categories between 25 and 64.")))),
                                    tabPanel("Calfresh Program Data",
                                             titlePanel("By the Numbers: Assessing Needs in SF"),
                                             
                                             # Sidebar to select different variables 
                                             sidebarLayout(
                                                 sidebarPanel(varSelectInput("variable", "Fig. 2 Variable:", calfresh_monthly[, c(4:6)])),

                                                 # Show a plot of the generated distribution
                                                 mainPanel(
                                                           h3("Trends in Food Insecurity in terms of Enrollment in Federal Food Stamps Program"),
                                                           plotOutput("calfreshmonthlyPlot"),
                                                           p("There is a sharp rise in the number of individuals and households in San Francisco enrolled in the federal food stamps program Calfresh after the outbreak of COVID-19, a trend reflected in the monthly unemployment rates as well. This suggests that food insecurity is on the rise as a result of the pandemic."),
                                                           br(), 
                                                           h3("Federal Food Stamps Program Reach in San Francisco"),
                                                           plotOutput("PRI_calfreshPlot"),
                                                           p("The Calfresh program in San Franciso has consistently had a lower program reach among food insecure individuals compared to statewide levels. Almost 50% of at-risk individuals are not enrolled in the Calfresh program.")))),
                                    tabPanel("Demographics of Food Stamp Recipients",                         titlePanel("By the Numbers: Assessing Needs in SF"),
                                             
                                             # Sidebar to select different variables 
                                             sidebarLayout(
                                                 sidebarPanel(varSelectInput("calfreshdem_variable", "Fig. 4 Variable:", calfresh_total[, c(4:7)])),
                                                 
                                                 # Show a plot of the generated distribution
                                                 mainPanel(
                                                           h3("Demographics of Recipients on Food Stamps in San Francisco"),
                                                           plotOutput("calfresh_totalPlot"),
                                                           p("San Francisco has fewer elderly, children and adults enrolled in the Calfresh program than the statewide average, which could partially be a function of the lower program reach in the city."))))
                            )),
               
               tabPanel("Model",
                        titlePanel("Model"),
                        br(),
                        h3("Predicting Food Insecurity in California"),
                        gt_output(outputId = "table"),
                        br(),
                        h3("Analysis"),
                        h4("Dataset and Variables"),
                        p("The dataset is from the adult sample of the California Health Interview Survey, conducted in 2019. The CHIS is the nation's largest state health survey, with population-based data on representative Californians, including under-surveyed racial, ethnic and sexual-minority groups. I am using a subset of the dataset that contains 4744 observations, drawn from a universe of those who responded to the question on food insecurity and whose income level is less than 200% of the federal poverty level."),
                        br(),
                        p("The variables that I am regressing are:"),
                        p("fslevcb - a measure of food security. A 1 indicates that the respondent is food secure, a 2 indicates that they are food insecure."),
                        p("ak22_p1 - a measure of household total annual income before taxes. This is a 19-point scale from less than $10000 to over $18000, with each 1 point increase representing an increase in annual income of $10000."),
                        p("ahedc_p1 - a measure of educational attainment. This is a 9 point scale, ranging from no formal education/grades 1-8 to a Ph.D to equivalent."),
                        p("ae2 - a measure of the number of times the respondent consumed fruits in the past month."),
                        p("ak28 - a measure of how often the respondent feels safe in their neighborhood. This is a 4 point scale, ranging from 'all of the time' to 'none of the time.'"),
                        p("ins - a measure of whether the respondent is insured. '1' is coded 'Yes' and '2' is coded 'No'."),
                        p("numcig - a measure of the number of cigarettes smoked by the respondent each day. This is a 6 point scale, ranging from 'none' to '20 or more'."),
                        p("srage_p1 - a measure of self-reported age."),
                        br(),
                        h4("Analysis"),
                        p("This model is a predictive one that attempts to determine how any given variable might predict food insecurity in a given individual. We can interpret the following from the regression:"),
                        p("1) The 'intercept' is the food security status when all the other variables are held at 0. It is 1.5, indicating the midpoint between food security and food insecurity."),
                        p("2) Annual household income is correlated with food security status. For every increase in annual household income of $10,000, the individual is 2% less likely to be food insecure."),
                        p("3) Educational attainment is correlated with food security status. For every increase in educational attainment of 1 on the 9 point scale, the individual is 2% less likely to be food insecure."),
                        p("4) There is no correlation between the number of times a respondent consumed fruit in the past month with their food security status."),
                        p("5) Perception of insecurity is negatively correlated with food security status. For every increase of 1 on the 4 point scale of self-reported perception of safety in the neighborhood (ie the respondent feels more insecure), the individual is 11% more likely to be food insecure."),
                        p("6) Current insurance status is not correlated with food security status."),
                        p("7) The number of cigarettes smoked per month is negatively correlated with food security status. For every increase of 1 in the 6 point scale (approximately 5 cigarettes), the individual is 4% more likely to be food insecure."),
                        p("8) Self-reported age is not correlated with food security status.")),
               
               tabPanel("About",
                        titlePanel("About"),
                        br(),
                        h4("About the project"),
                        p("This project was commissioned by Alemany Farm, the largest urban farm in San Francisco, in support of a $400,000 US Department of Agriculture grant. Alemany Farm is seeking to expand its Community Food Project Program (CFP) and Agroecology Internship Program (AIP), projects that seek to curb negative physical and mental health outcomes related to poor nutrition, limited exercise, and the absence of green spaces. My project seeks to leverage data to obtain insights about the demographics of Southeast SF and the current state of food insecurity in the city."),
                        tags$ul(
                            tags$li("By visualizing the surrounding neighborhoods of Alemany Farm and District 8, we find that residents lie at the intersection of different forms of vulnerability: they are disproportionately low-income, from minority groups, lacking access to healthy and affordable food options, and the most hard hit in terms of COVID-19 cases."), 
                            tags$li("Using statewide federal welfare program data as a proxy, we show that food insecurity has sharply increased after the outbreak of the pandemic, in parallel with increased rates of unemployment."), 
                            tags$li("We also show that the federal food stamp program (Calfresh) has consistently had a much lower reach in San Francisco relative to the state-wide average, with almost 50% of individuals at risk of food insecurity in the city not enrolled in the program."),
                            tags$li("Using 2019 data from the nation’s largest state health survey, we find that risk factors for food insecurity in California include low income, low educational attainment, low self-reported perceptions of neighborhood safety and greater number of cigarettes smoked monthly.")),
                        br(),
                        h4("Dataset sources"),
                        tags$ul(
                            tags$li("Opportunity zones proximate to Alemany Farm: CA State Integrated OZ Map, accessed at https://opzones.ca.gov/oz-map/"), 
                            tags$li("COVID-19 cases in SF: Data SF, COVID-19 Cases and Deaths Summarized by Geography (as of 11/23/2020), accessed at: https://data.sfgov.org/COVID-19/COVID-19-Cases-and-Deaths-Summarized-by-Geography/tpyr-dvnc/data"), 
                            tags$li("Modified Retail Food Environment Index (mRFEI) data: CDC Census Tract Level State Maps of the Modified Retail Food Environment Index (mRFEI), accessed at: http://www.cdc.gov/obesity/downloads/2_16_mrfei_data_table.xls"),
                            tags$li("Income and census tracts in SF:  2019 FFIEC Census Report - Summary Census Demographic Information, accessed at https://www.ffiec.gov/census/report.aspx?year=2019&state=06&msa=&county=075&tract=&report=demographic&pdf=true"),
                            tags$li("District 8 aid recipient demographics: DataSF, SF Mayor's Office of Housing and Community Development. Public Service Program Activities by Supervisor District, https://data.sfgov.org/Economy-and-Community/Community-Development-Public-Service-Program-Activ/3ggv-tzb3/data"),
                            tags$li("Calfresh program data: CalFresh Data Dashboard, accessed at: https://www.cdss.ca.gov/inforesources/data-portal/research-and-data/calfresh-data-dashboard"),
                            tags$li("Data for model: 2019 California Health Interview Survey, Adult Public Use Dataset, accessed at https://healthpolicy.ucla.edu/chis/data/public-use-data-file/Documents/2019/CHIS%202019%20Data%20Dictionary%20-%20PUF%20-%20Adult.pdf")),
                        br(),
                        h4("About me"),
                        p("I am a sophomore at Harvard College majoring in Government. I am interested in learning about the intersection between data science and public policy."),
                        p("The source code for this Shiny App can be found at my GitHub", 
                          a("here.", href="https://github.com/chuajohn/sffoodinsecurity"))
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
            ggplot(aes(y = pri, x = calendar_year, color = county, group = county)) +
            geom_point() +
            geom_line() +
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
            geo_join(opp_zones_new, by = "NAME") 
        
        tract_data$Value[is.na(tract_data$Value)] <- 20
        
        
        bins <- c(0, 20, 40, 60, 80, 100)
        pal <- colorBin("Reds", domain = tract_data$Value, bins = bins)
        
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
            mutate(popup = str_c("Census Tract: ",  "<strong>", NAME, "</strong>",
                                 "<br/>",
                                 "Agency: ", "<strong>", agency, "</strong>",
                                 "<br/>",
                                 "Project Name: ", "<strong>", project_name, "</strong>",
                                 "<br/>",
                                 "Funding: ", "<strong>", funding, "</strong>") %>%
                       map(htmltools::HTML)) %>%
            leaflet() %>%
            addTiles() %>%
            addProviderTiles("CartoDB") %>% 
            addAwesomeMarkers(data = food_banks, ~as.numeric(long), ~as.numeric(lat), icon=icons, label=~as.character(name)) %>%
            setView(-122.409981, 37.773972, zoom = 12) %>% 
            addPolygons(label = ~popup, fillColor = ~pal(tract_data$Value), weight = 2,
                        opacity = 1,
                        color = "white",
                        dashArray = "3",
                        fillOpacity = 0.7)})
    
    output$incomePlot <- renderLeaflet({
        income_data <- tracts(state = "CA", county = "San Francisco", cb = TRUE) %>%
            mutate(NAME = as.numeric(NAME)) %>%
            geo_join(income_new, by = "NAME") 
        
        income_bins <- c(0, 40000, 80000, 120000, 160000, 200000, 240000, 280000, Inf)
        income_pal <- colorBin("Greens", domain = income_data$tract_median_family_income, bins = income_bins)
        
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

        income_data <- tracts(state = "CA", county = "San Francisco", cb = TRUE) %>%
            mutate(NAME = as.numeric(NAME)) %>%
            geo_join(income_new, by = "NAME") 
        
        minority_bins <- c(0, 20, 40, 60, 80, 100)
        minority_pal <- colorBin("Blues", domain = income_data$minority_population, bins = minority_bins)
        
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
    
    output$table <- render_gt({
        set.seed(10)
        CIH_mod <- stan_glm(data = filtered_CIH, 
                            formula = fslevcb ~ ak22_p1 + ahedc_p1 + ae2 + ak28 + ins + numcig + srage_p1,
                            refresh = 0)

        tbl_regression(CIH_mod, intercept = TRUE) %>%
            as_gt() %>%
            tab_header(title = md("*Regression of Demographic Characteristics on Food Insecurity Status among Individuals in California*"), 
                       subtitle = "The Effect of Annual Income, Educational Attainment, Fruit Consumption, Perceptions of Safety, Insurance Status, Number of Cigarettes Smoked Monthly and Age on Food Security") %>%
            tab_source_note("Source: California Health Interview Survey (2019)")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
