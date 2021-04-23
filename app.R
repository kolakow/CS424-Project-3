#Pat Kolakowski, pkolak3@uic.edu
#Project 3: We've Got the Power
#CS 424, Spring 2020
#Data sourced from: https://www.kaggle.com/chicago/chicago-energy-usage-2010

#libraries to include
library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(leaflet)
library(stringr)
library(DT)
library(jpeg)
library(grid)
library(scales)
library(mapview)
library(tigris)


data <- read.csv("energy-usage-2010.csv", header = TRUE, stringsAsFactors = FALSE) #Read in data
chicago <- blocks(state = "IL", county = "Cook", year = "2010")
colnames(chicago)[5] <- "BLOCK"
'%notin%' <- Negate('%in%')

communityOptions <- unique(data$COMMUNITY) #Grab unique values in community column

filterOptions <- list("Electric Usage" = "KWH.", #Used for dropdowns
                    "Gas Usage" = "THERM.",
                    "Building Age" = "AVERAGE.BUILDING.AGE",
                    "Building Height" = "AVERAGE.STORIES",
                    "Total Population" = "TOTAL.POPULATION"
                )

monthOptions <- list(
                    "Total" = "TOTAL",
                    "January" = "JANUARY.2010",
                    "February" = "FEBRUARY.2010",
                    "March" = "MARCH.2010",
                    "April" = "APRIL.2010",
                    "May" = "MAY.2010",
                    "June" = "JUNE.2010",
                    "July" = "JULY.2010",
                    "August" = "AUGUST.2010",
                    "September" = "SEPTEMBER.2010",
                    "October" = "OCTOBER.2010",
                    "November" = "NOVEMBER.2010",
                    "December" = "DECEMBER.2010"
                )

typeOptions <- list(
                    "All" = "All",
                    "Residential" = "Residential",
                    "Commercial" = "Commercial",
                    "Industrial" = "Industrial"
                )

chicagoFilterOptions <- list(
                    "Oldest Buildings",
                    "Newest Buildings",
                    "Highest Electricity Usage",
                    "Highest Gas Usage",
                    "Highest Population",
                    "Highest Occupancy Percentage",
                    "Highest Percentage of Renters"
                )



ui <- dashboardPage(
    dashboardHeader(title = "CS 424: Project 3\n"),  
    dashboardSidebar(disable = FALSE, collapsed = FALSE,
                    sidebarMenu(
                        id = "tab",
                        menuItem("Near West Side Overview", tabName = "1"),
                        menuItem("Compare Communities", tabName = "2"),
                        menuItem("Chicago Overview", tabName = "3"),
                        menuItem("About", tabName = "4")
                    ),
                    uiOutput("tabMenu")
                    ),
dashboardBody(
    tabItems(
        tabItem(tabName = "1", #Page 1, Near West Side Heatmap
        fluidRow(
            column(8, 
                fluidRow(
                box(title = "Near West Side Heatmap", status = "primary", width = 12,
                        leafletOutput("nwsMap", height = 800)
                  )
                 ),
            ),
            column(4,
                fluidRow(
                box(title = "Electric vs. Gas Usage", status = "primary", width = 12,
                        plotOutput("nwsGraph")
                  )
                 ),
                fluidRow(
                box(title = "Electric and Gas Usage", status = "primary", width = 12,
                        dataTableOutput("nwsTable"),  style = "overflow-y: scroll;overflow-x: scroll; height: 340px;"
                  )
                 ),
            )
        )
        ),
    tabItem(tabName = "2", #Page 2, Compare Communities
        fluidRow(
            column(6, #Column 1
                fluidRow(
                tags$head(tags$style('.selectize-dropdown {z-index: 10000}')), 
                  box(status = "primary", width = 12,
                    column(3, #Community dropdown
                        fluidRow(
                            selectInput("community1", label = "Community", choices = communityOptions, selected = "Near West Side"),
                            )
                    ),
                    column(3, #Filter dropdown
                        fluidRow(
                            selectInput("filter1", label = "Filter By", choices = filterOptions, selected = "Electrical Usage"),
                            )
                        ),
                    column(3, #Month dropdown
                        fluidRow(
                            selectInput("month1", label = "Month", choices = monthOptions, selected = "Default"),
                            )
                        ),
                    column(3, #Building type dropdown
                        fluidRow(
                            selectInput("type1", label = "Building Type", choices = typeOptions, selected = "All"),
                            )
                    ),
                    leafletOutput("map1", height = 400)

                  )
                 ),
                 fluidRow( #Output graph based on community selection
                     box(title = "", status = "primary", width = 12,
                        plotOutput("graph1", height = 300)
                  )
                 )
            ),
            column(6,
                fluidRow(
                tags$head(tags$style('.selectize-dropdown {z-index: 10000}')),
                  box(status = "primary", width = 12,
                    column(3, #Community dropdown
                        fluidRow(
                            selectInput("community2", label = "Community", choices = communityOptions, selected = "Loop"),
                            )
                    ),
                    column(3, #Filter dropdown
                        fluidRow(
                            selectInput("filter2", label = "Filter By", choices = filterOptions, selected = "Electrical Usage"),
                            )
                        ), 
                    column(3, #Month dropdown
                        fluidRow(
                            selectInput("month2", label = "Month", choices = monthOptions, selected = "Default"),
                            )
                        ),
                    column(3, #Building type dropdown
                        fluidRow(
                            selectInput("type2", label = "Building Type", choices = typeOptions, selected = "All"),
                            )
                    ),
                    leafletOutput("map2", height = 400)
                  )
                 ),
                 fluidRow( #Output graph based on community selection
                     box(title = "", status = "primary", width = 12,
                        plotOutput("graph2", height = 300)
                  ))

            ),
            )
           ),
    tabItem(tabName = "3", #Page 3, US Leaflet Map
        fluidPage(
              box(title = "Chicago Overview", status = "primary", width = 12,
                    leafletOutput("chicagoMap", height = 800)
            )
            )
         ),
    tabItem(tabName = "4", #Page 4, About page
        h3("Project 3: We've Got the Power"),
        h4("by Pat Kolakowski (pkolak3@uic.edu)"),
        h4("for UIC CS 424, Spring 2020"),
        h4("Data sourced from https://www.kaggle.com/chicago/chicago-energy-usage-2010"),
        h4("Information on app creation and installation can be found at: https://pkolak3.people.uic.edu/project3.html")
        )
    )
)
)


server <- function(input, output, session) {
    output$tabMenu <- renderUI({ #Render UI based on tab selected

    #Page 1, Near West Side Overview tab options
    if(input$tab == "1") {
        dyn_ui <- list(
            selectInput("filter",
                label = h5("Filter By"),
                choices = filterOptions,
                selected = "Electric Usage"
                ),
            selectInput("month",
                label = h5("Month"),
                choices = monthOptions,
                selected = "Total"
                ),
            selectInput("type",
                label = h5("Building Type"),
                choices = typeOptions,
                selected = "All"
                ),
            actionButton("reset_button1", "Reset View")
        )
        }
    
    #Page 2, Community Comparison
    if(input$tab == "2") {
        dyn_ui <- actionButton("reset_button2", "Reset View")

        }
    
    #Page 3, Chicago Overview
    if(input$tab == "3") {
        dyn_ui <- list(
            selectInput("chicagoFilter",
                label = h5("Filter By"),
                choices = chicagoFilterOptions,
                selected = "Oldest Buildings"
                ),
            actionButton("reset_button3", "Reset View")
            )
        }
    
    #Page 4, About page
    if(input$tab == "4") {
        dyn_ui <- NULL
    }
    
    return(dyn_ui)
    })



    #Mapview, page 1
    output$nwsMap <- renderLeaflet({
        data <- data[data$COMMUNITY == "Near West Side", ] #Filter for blocks Near West Side and then filter tigris data by those blocks

        chicago <- subset(chicago, BLOCK %in% data$BLOCK)

        m <- mapview(chicago)

        if("All" %notin% input$type) { #Filter by building type
            chicago <- merge(chicago, data[c("BUILDING.TYPE", "BLOCK")], by = "BLOCK")
            chicago <- chicago[chicago$BUILDING.TYPE == input$type,]
        }

        if("KWH." %notin% input$filter & "THERM." %notin% input$filter) {
            chicago <- merge(chicago, data[c(input$filter, "BLOCK")], by = "BLOCK")
            m <- mapview(chicago, z = input$filter)
        }
        
        if("TOTAL" %in% input$month & "KWH." %in% input$filter) { #Total energy usage
            chicago <- merge(chicago, data[c("TOTAL.KWH", "BLOCK")], by = "BLOCK")
            m <- mapview(chicago, z = "TOTAL.KWH")
        } else if("TOTAL" %in% input$month & "THERM." %in% input$filter) { #Total gas usage
            chicago <- merge(chicago, data[c("TOTAL.THERM", "BLOCK")], by = "BLOCK")
            m <- mapview(chicago, z = "TOTAL.THERM")
        } else if("TOTAL" %notin% input$month) { #Or filter for gas/electric usage for a specific month
            value <- paste0(input$filter, input$month)
            chicago <- merge(chicago, data[c(value, "BLOCK")], by = "BLOCK")
            m <- mapview(chicago, z = value)
        }

        m@map
    })

    #Line graph of energy data per month, page 1
    output$nwsGraph <- renderPlot({
        data <- data[data$COMMUNITY == "Near West Side", ] #Filter for blocks Near West Side and then filter tigris data by those blocks        

        total <- data.frame("Jan", sum(as.numeric(data$KWH.JANUARY.2010), na.rm = TRUE), "Electrical (kWh)")
        total <- rbind(total, c("Jan", sum(as.numeric(data$THERM.JANUARY.2010), na.rm = TRUE), "Gas (thm)"))
        total <- rbind(total, c("Feb", sum(as.numeric(data$KWH.FEBRUARY.2010), na.rm = TRUE), "Electrical (kWh)"))
        total <- rbind(total, c("Feb", sum(as.numeric(data$THERM.FEBRUARY.2010), na.rm = TRUE), "Gas (thm)"))
        total <- rbind(total, c("Mar", sum(as.numeric(data$KWH.MARCH.2010), na.rm = TRUE), "Electrical (kWh)"))
        total <- rbind(total, c("Mar", sum(as.numeric(data$THERM.MARCH.2010), na.rm = TRUE), "Gas (thm)"))
        total <- rbind(total, c("Apr", sum(as.numeric(data$KWH.APRIL.2010), na.rm = TRUE), "Electrical (kWh)"))
        total <- rbind(total, c("Apr", sum(as.numeric(data$THERM.APRIL.2010), na.rm = TRUE), "Gas (thm)"))
        total <- rbind(total, c("May", sum(as.numeric(data$KWH.MAY.2010), na.rm = TRUE), "Electrical (kWh)"))
        total <- rbind(total, c("May", sum(as.numeric(data$THERM.MAY.2010), na.rm = TRUE), "Gas (thm)"))
        total <- rbind(total, c("Jun", sum(as.numeric(data$KWH.JUNE.2010), na.rm = TRUE), "Electrical (kWh)"))
        total <- rbind(total, c("Jun", sum(as.numeric(data$THERM.JUNE.2010), na.rm = TRUE), "Gas (thm)"))
        total <- rbind(total, c("Jul", sum(as.numeric(data$KWH.JULY.2010), na.rm = TRUE), "Electrical (kWh)"))
        total <- rbind(total, c("Jul", sum(as.numeric(data$THERM.JULY.2010), na.rm = TRUE), "Gas (thm)"))
        total <- rbind(total, c("Aug", sum(as.numeric(data$KWH.AUGUST.2010), na.rm = TRUE), "Electrical (kWh)"))
        total <- rbind(total, c("Aug", sum(as.numeric(data$THERM.AUGUST.2010), na.rm = TRUE), "Gas (thm)"))
        total <- rbind(total, c("Sep", sum(as.numeric(data$KWH.SEPTEMBER.2010), na.rm = TRUE), "Electrical (kWh)"))
        total <- rbind(total, c("Sep", sum(as.numeric(data$THERM.SEPTEMBER.2010), na.rm = TRUE), "Gas (thm)"))
        total <- rbind(total, c("Oct", sum(as.numeric(data$KWH.OCTOBER.2010), na.rm = TRUE), "Electrical (kWh)"))
        total <- rbind(total, c("Oct", sum(as.numeric(data$THERM.OCTOBER.2010), na.rm = TRUE), "Gas (thm)"))
        total <- rbind(total, c("Nov", sum(as.numeric(data$KWH.NOVEMBER.2010), na.rm = TRUE), "Electrical (kWh)"))
        total <- rbind(total, c("Nov", sum(as.numeric(data$THERM.NOVEMBER.2010), na.rm = TRUE), "Gas (thm)"))
        total <- rbind(total, c("Dec", sum(as.numeric(data$KWH.DECEMBER.2010), na.rm = TRUE), "Electrical (kWh)"))
        total <- rbind(total, c("Dec", sum(as.numeric(data$THERM.DECEMBER.2010), na.rm = TRUE), "Gas (thm)"))

        colnames(total) <- c("Month", "Energy_Generated", "Source")
        total$Month <- factor(total$Month, levels = unique(total$Month))
        total$Energy_Generated <- as.numeric(total$Energy_Generated)

        plot <- ggplot(total, aes(x = Month, y = Energy_Generated, group = Source, color = Source)) +
                geom_line() +
                xlab("Usage") +
                ylab("Month") +
                scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-7))

        plot
    })

    #Data table, page 1
    output$nwsTable <- renderDataTable(
        datatable({
            data <- data[data$COMMUNITY == "Near West Side", ] #Filter for blocks Near West Side and then filter tigris data by those blocks        

            #Lazily create dataframe with sum of each month per source (i'm sorry that you had to see this)
            total <- data.frame("January", paste(sum(as.numeric(data$KWH.JANUARY.2010), na.rm = TRUE), "kWh"), "Electrical")
            total <- rbind(total, c("January", paste(sum(as.numeric(data$THERM.JANUARY.2010), na.rm = TRUE), "thm"), "Gas"))
            total <- rbind(total, c("February", paste(sum(as.numeric(data$KWH.FEBRUARY.2010), na.rm = TRUE), "kWh"), "Electrical"))
            total <- rbind(total, c("February", paste(sum(as.numeric(data$THERM.FEBRUARY.2010), na.rm = TRUE), "thm"), "Gas"))
            total <- rbind(total, c("March", paste(sum(as.numeric(data$KWH.MARCH.2010), na.rm = TRUE), "kWh"), "Electrical"))
            total <- rbind(total, c("March", paste(sum(as.numeric(data$THERM.MARCH.2010), na.rm = TRUE), "thm"), "Gas"))
            total <- rbind(total, c("April", paste(sum(as.numeric(data$KWH.APRIL.2010), na.rm = TRUE), "kWh"), "Electrical"))
            total <- rbind(total, c("April", paste(sum(as.numeric(data$THERM.APRIL.2010), na.rm = TRUE), "thm"), "Gas"))
            total <- rbind(total, c("May", paste(sum(as.numeric(data$KWH.MAY.2010), na.rm = TRUE), "kWh"), "Electrical"))
            total <- rbind(total, c("May", paste(sum(as.numeric(data$THERM.MAY.2010), na.rm = TRUE), "thm"), "Gas"))
            total <- rbind(total, c("June", paste(sum(as.numeric(data$KWH.JUNE.2010), na.rm = TRUE), "kWh"), "Electrical"))
            total <- rbind(total, c("June", paste(sum(as.numeric(data$THERM.JUNE.2010), na.rm = TRUE), "thm"), "Gas"))
            total <- rbind(total, c("July", paste(sum(as.numeric(data$KWH.JULY.2010), na.rm = TRUE), "kWh"), "Electrical"))
            total <- rbind(total, c("July", paste(sum(as.numeric(data$THERM.JULY.2010), na.rm = TRUE), "thm"), "Gas"))
            total <- rbind(total, c("August", paste(sum(as.numeric(data$KWH.AUGUST.2010), na.rm = TRUE), "kWh"), "Electrical"))
            total <- rbind(total, c("August", paste(sum(as.numeric(data$THERM.AUGUST.2010), na.rm = TRUE), "thm"), "Gas"))
            total <- rbind(total, c("September", paste(sum(as.numeric(data$KWH.SEPTEMBER.2010), na.rm = TRUE), "kWh"), "Electrical"))
            total <- rbind(total, c("September", paste(sum(as.numeric(data$THERM.SEPTEMBER.2010), na.rm = TRUE), "thm"), "Gas"))
            total <- rbind(total, c("October", paste(sum(as.numeric(data$KWH.OCTOBER.2010), na.rm = TRUE), "kWh"), "Electrical"))
            total <- rbind(total, c("October", paste(sum(as.numeric(data$THERM.OCTOBER.2010), na.rm = TRUE), "thm"), "Gas"))
            total <- rbind(total, c("November", paste(sum(as.numeric(data$KWH.NOVEMBER.2010), na.rm = TRUE), "kWh"), "Electrical"))
            total <- rbind(total, c("November", paste(sum(as.numeric(data$THERM.NOVEMBER.2010), na.rm = TRUE), "thm"), "Gas"))
            total <- rbind(total, c("December", paste(sum(as.numeric(data$KWH.DECEMBER.2010), na.rm = TRUE), "kWh"), "Electrical"))
            total <- rbind(total, c("December", paste(sum(as.numeric(data$THERM.DECEMBER.2010), na.rm = TRUE), "thm"), "Gas"))


            colnames(total) <- c("Month", "Energy Generated", "Source")
            total
            }, 
            options = list(paging = FALSE), rownames = FALSE 
        )
    )

    #Left map on page 2, compare leaflet maps
    output$map1 <- renderLeaflet({
        data <- data[data$COMMUNITY == input$community1, ] #Filter for blocks Near West Side and then filter tigris data by those blocks

        chicago1 <- subset(chicago, BLOCK %in% data$BLOCK)

        m <- mapview(chicago1)

        if("All" %notin% input$type1) { #Filter by building type
            chicago1 <- merge(chicago1, data[c("BUILDING.TYPE", "BLOCK")], by = "BLOCK")
            chicago1 <- chicago1[chicago1$BUILDING.TYPE == input$type1,]
        }

        if("KWH." %notin% input$filter1 & "THERM." %notin% input$filter1) {
            chicago1 <- merge(chicago1, data[c(input$filter1, "BLOCK")], by = "BLOCK")
            m <- mapview(chicago1, z = input$filter1)
        }
        
        if("TOTAL" %in% input$month1 & "KWH." %in% input$filter1) { #Total energy usage
            chicago1 <- merge(chicago1, data[c("TOTAL.KWH", "BLOCK")], by = "BLOCK")
            m <- mapview(chicago1, z = "TOTAL.KWH")
        } else if("TOTAL" %in% input$month1 & "THERM." %in% input$filter1) { #Total gas usage
            chicago1 <- merge(chicago1, data[c("TOTAL.THERM", "BLOCK")], by = "BLOCK")
            m <- mapview(chicago1, z = "TOTAL.THERM")
        } else if("TOTAL" %notin% input$month1) { #Or filter for gas/electric usage for a specific month
            value <- paste0(input$filter1, input$month1)
            chicago1 <- merge(chicago1, data[c(value, "BLOCK")], by = "BLOCK")
            m <- mapview(chicago1, z = value)
        }

        m@map
    })

    #Right map on page 2, compare leaflet maps
    output$map2 <- renderLeaflet({
        data <- data[data$COMMUNITY == input$community2, ] #Filter for blocks Near West Side and then filter tigris data by those blocks

        chicago2 <- subset(chicago, BLOCK %in% data$BLOCK)

        m <- mapview(chicago2)

        if("All" %notin% input$type2) { #Filter by building type
            chicago2 <- merge(chicago2, data[c("BUILDING.TYPE", "BLOCK")], by = "BLOCK")
            chicago2 <- chicago2[chicago2$BUILDING.TYPE == input$type2,]
        }

        if("KWH." %notin% input$filter2 & "THERM." %notin% input$filter2) {
            chicago2 <- merge(chicago2, data[c(input$filter2, "BLOCK")], by = "BLOCK")
            m <- mapview(chicago2, z = input$filter2)
        }
        
        if("TOTAL" %in% input$month2 & "KWH." %in% input$filter2) { #Total energy usage
            chicago2 <- merge(chicago2, data[c("TOTAL.KWH", "BLOCK")], by = "BLOCK")
            m <- mapview(chicago2, z = "TOTAL.KWH")
        } else if("TOTAL" %in% input$month2 & "THERM." %in% input$filter2) { #Total gas usage
            chicago2 <- merge(chicago2, data[c("TOTAL.THERM", "BLOCK")], by = "BLOCK")
            m <- mapview(chicago2, z = "TOTAL.THERM")
        } else if("TOTAL" %notin% input$month2) { #Or filter for gas/electric usage for a specific month
            value <- paste0(input$filter2, input$month2)
            chicago2 <- merge(chicago2, data[c(value, "BLOCK")], by = "BLOCK")
            m <- mapview(chicago2, z = value)
        }

        m@map
    })

    #Line graph of energy data per month, page 1
    output$graph1 <- renderPlot({
        data <- data[data$COMMUNITY == input$community1, ] #Filter for blocks Near West Side and then filter tigris data by those blocks        

        #Lazily create dataframe with sum of each month per source (i'm sorry that you had to see this)
        total <- data.frame("Jan", sum(as.numeric(data$KWH.JANUARY.2010), na.rm = TRUE), "Electrical (kWh)")
        total <- rbind(total, c("Jan", sum(as.numeric(data$THERM.JANUARY.2010), na.rm = TRUE), "Gas (thm)"))
        total <- rbind(total, c("Feb", sum(as.numeric(data$KWH.FEBRUARY.2010), na.rm = TRUE), "Electrical (kWh)"))
        total <- rbind(total, c("Feb", sum(as.numeric(data$THERM.FEBRUARY.2010), na.rm = TRUE), "Gas (thm)"))
        total <- rbind(total, c("Mar", sum(as.numeric(data$KWH.MARCH.2010), na.rm = TRUE), "Electrical (kWh)"))
        total <- rbind(total, c("Mar", sum(as.numeric(data$THERM.MARCH.2010), na.rm = TRUE), "Gas (thm)"))
        total <- rbind(total, c("Apr", sum(as.numeric(data$KWH.APRIL.2010), na.rm = TRUE), "Electrical (kWh)"))
        total <- rbind(total, c("Apr", sum(as.numeric(data$THERM.APRIL.2010), na.rm = TRUE), "Gas (thm)"))
        total <- rbind(total, c("May", sum(as.numeric(data$KWH.MAY.2010), na.rm = TRUE), "Electrical (kWh)"))
        total <- rbind(total, c("May", sum(as.numeric(data$THERM.MAY.2010), na.rm = TRUE), "Gas (thm)"))
        total <- rbind(total, c("Jun", sum(as.numeric(data$KWH.JUNE.2010), na.rm = TRUE), "Electrical (kWh)"))
        total <- rbind(total, c("Jun", sum(as.numeric(data$THERM.JUNE.2010), na.rm = TRUE), "Gas (thm)"))
        total <- rbind(total, c("Jul", sum(as.numeric(data$KWH.JULY.2010), na.rm = TRUE), "Electrical (kWh)"))
        total <- rbind(total, c("Jul", sum(as.numeric(data$THERM.JULY.2010), na.rm = TRUE), "Gas (thm)"))
        total <- rbind(total, c("Aug", sum(as.numeric(data$KWH.AUGUST.2010), na.rm = TRUE), "Electrical (kWh)"))
        total <- rbind(total, c("Aug", sum(as.numeric(data$THERM.AUGUST.2010), na.rm = TRUE), "Gas (thm)"))
        total <- rbind(total, c("Sep", sum(as.numeric(data$KWH.SEPTEMBER.2010), na.rm = TRUE), "Electrical (kWh)"))
        total <- rbind(total, c("Sep", sum(as.numeric(data$THERM.SEPTEMBER.2010), na.rm = TRUE), "Gas (thm)"))
        total <- rbind(total, c("Oct", sum(as.numeric(data$KWH.OCTOBER.2010), na.rm = TRUE), "Electrical (kWh)"))
        total <- rbind(total, c("Oct", sum(as.numeric(data$THERM.OCTOBER.2010), na.rm = TRUE), "Gas (thm)"))
        total <- rbind(total, c("Nov", sum(as.numeric(data$KWH.NOVEMBER.2010), na.rm = TRUE), "Electrical (kWh)"))
        total <- rbind(total, c("Nov", sum(as.numeric(data$THERM.NOVEMBER.2010), na.rm = TRUE), "Gas (thm)"))
        total <- rbind(total, c("Dec", sum(as.numeric(data$KWH.DECEMBER.2010), na.rm = TRUE), "Electrical (kWh)"))
        total <- rbind(total, c("Dec", sum(as.numeric(data$THERM.DECEMBER.2010), na.rm = TRUE), "Gas (thm)"))

        colnames(total) <- c("Month", "Energy_Generated", "Source")
        total$Month <- factor(total$Month, levels = unique(total$Month))
        total$Energy_Generated <- as.numeric(total$Energy_Generated)

        plot <- ggplot(total, aes(x = Month, y = Energy_Generated, group = Source, color = Source)) +
                geom_line() +
                xlab("Usage") +
                ylab("Month") +
                scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-7))

        plot

    })

    #Line graph of energy data per month, page 1
    output$graph2 <- renderPlot({
        data <- data[data$COMMUNITY == input$community2, ] #Filter for blocks Near West Side and then filter tigris data by those blocks        

        #Lazily create dataframe with sum of each month per source (i'm sorry that you had to see this)
        total <- data.frame("Jan", sum(as.numeric(data$KWH.JANUARY.2010), na.rm = TRUE), "Electrical (kWh)")
        total <- rbind(total, c("Jan", sum(as.numeric(data$THERM.JANUARY.2010), na.rm = TRUE), "Gas (thm)"))
        total <- rbind(total, c("Feb", sum(as.numeric(data$KWH.FEBRUARY.2010), na.rm = TRUE), "Electrical (kWh)"))
        total <- rbind(total, c("Feb", sum(as.numeric(data$THERM.FEBRUARY.2010), na.rm = TRUE), "Gas (thm)"))
        total <- rbind(total, c("Mar", sum(as.numeric(data$KWH.MARCH.2010), na.rm = TRUE), "Electrical (kWh)"))
        total <- rbind(total, c("Mar", sum(as.numeric(data$THERM.MARCH.2010), na.rm = TRUE), "Gas (thm)"))
        total <- rbind(total, c("Apr", sum(as.numeric(data$KWH.APRIL.2010), na.rm = TRUE), "Electrical (kWh)"))
        total <- rbind(total, c("Apr", sum(as.numeric(data$THERM.APRIL.2010), na.rm = TRUE), "Gas (thm)"))
        total <- rbind(total, c("May", sum(as.numeric(data$KWH.MAY.2010), na.rm = TRUE), "Electrical (kWh)"))
        total <- rbind(total, c("May", sum(as.numeric(data$THERM.MAY.2010), na.rm = TRUE), "Gas (thm)"))
        total <- rbind(total, c("Jun", sum(as.numeric(data$KWH.JUNE.2010), na.rm = TRUE), "Electrical (kWh)"))
        total <- rbind(total, c("Jun", sum(as.numeric(data$THERM.JUNE.2010), na.rm = TRUE), "Gas (thm)"))
        total <- rbind(total, c("Jul", sum(as.numeric(data$KWH.JULY.2010), na.rm = TRUE), "Electrical (kWh)"))
        total <- rbind(total, c("Jul", sum(as.numeric(data$THERM.JULY.2010), na.rm = TRUE), "Gas (thm)"))
        total <- rbind(total, c("Aug", sum(as.numeric(data$KWH.AUGUST.2010), na.rm = TRUE), "Electrical (kWh)"))
        total <- rbind(total, c("Aug", sum(as.numeric(data$THERM.AUGUST.2010), na.rm = TRUE), "Gas (thm)"))
        total <- rbind(total, c("Sep", sum(as.numeric(data$KWH.SEPTEMBER.2010), na.rm = TRUE), "Electrical (kWh)"))
        total <- rbind(total, c("Sep", sum(as.numeric(data$THERM.SEPTEMBER.2010), na.rm = TRUE), "Gas (thm)"))
        total <- rbind(total, c("Oct", sum(as.numeric(data$KWH.OCTOBER.2010), na.rm = TRUE), "Electrical (kWh)"))
        total <- rbind(total, c("Oct", sum(as.numeric(data$THERM.OCTOBER.2010), na.rm = TRUE), "Gas (thm)"))
        total <- rbind(total, c("Nov", sum(as.numeric(data$KWH.NOVEMBER.2010), na.rm = TRUE), "Electrical (kWh)"))
        total <- rbind(total, c("Nov", sum(as.numeric(data$THERM.NOVEMBER.2010), na.rm = TRUE), "Gas (thm)"))
        total <- rbind(total, c("Dec", sum(as.numeric(data$KWH.DECEMBER.2010), na.rm = TRUE), "Electrical (kWh)"))
        total <- rbind(total, c("Dec", sum(as.numeric(data$THERM.DECEMBER.2010), na.rm = TRUE), "Gas (thm)"))

        colnames(total) <- c("Month", "Energy_Generated", "Source")
        total$Month <- factor(total$Month, levels = unique(total$Month))
        total$Energy_Generated <- as.numeric(total$Energy_Generated)

        plot <- ggplot(total, aes(x = Month, y = Energy_Generated, group = Source, color = Source)) +
                geom_line() +
                xlab("Usage") +
                ylab("Month") +
                scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-7))

        plot

    })

    #Chicago Overview, page 3
        output$chicagoMap <- renderLeaflet({
            data1 <- data
            chicago1 <- subset(chicago, BLOCK %in% data1$BLOCK)

            if(input$chicagoFilter == "Oldest Buildings") {
                chicago1 <- merge(chicago1, data1[c("AVERAGE.BUILDING.AGE", "BLOCK")], by = "BLOCK")
                chicago1 <- head(chicago1[order(chicago1$AVERAGE.BUILDING.AGE, decreasing = TRUE, na.last = TRUE),], .1 * nrow(chicago1))
                m <- mapview(chicago1, z = "AVERAGE.BUILDING.AGE")
            } else if(input$chicagoFilter == "Newest Buildings") {
                chicago1 <- merge(chicago1, data1[c("AVERAGE.BUILDING.AGE", "BLOCK")], by = "BLOCK")
                chicago1 <- head(chicago1[order(chicago1$AVERAGE.BUILDING.AGE, decreasing = FALSE, na.last = TRUE),], .1 * nrow(chicago1))
                m <- mapview(chicago1, z = "AVERAGE.BUILDING.AGE")
            } else if(input$chicagoFilter == "Highest Electricity Usage") {
                chicago1 <- merge(chicago1, data1[c("TOTAL.KWH", "BLOCK")], by = "BLOCK")
                chicago1 <- head(chicago1[order(chicago1$TOTAL.KWH, decreasing = TRUE, na.last = TRUE),], .1 * nrow(chicago1))
                m <- mapview(chicago1, z = "TOTAL.KWH")
            } else if(input$chicagoFilter == "Highest Gas Usage") {
                chicago1 <- merge(chicago1, data1[c("TOTAL.THERM", "BLOCK")], by = "BLOCK")
                chicago1 <- head(chicago1[order(chicago1$TOTAL.THERM, decreasing = TRUE, na.last = TRUE),], .1 * nrow(chicago1))
                m <- mapview(chicago1, z = "TOTAL.THERM")
            } else if(input$chicagoFilter == "Highest Population") {
                chicago1 <- merge(chicago1, data1[c("TOTAL.POPULATION", "BLOCK")], by = "BLOCK")
                chicago1 <- head(chicago1[order(chicago1$TOTAL.POPULATION, decreasing = TRUE, na.last = TRUE),], .1 * nrow(chicago1))
                m <- mapview(chicago1, z = "TOTAL.POPULATION")
            } else if(input$chicagoFilter == "Highest Occupancy Percentage") {
                chicago1 <- merge(chicago1, data1[c("OCCUPIED.UNITS.PERCENTAGE", "BLOCK")], by = "BLOCK")
                chicago1 <- head(chicago1[order(chicago1$OCCUPIED.UNITS.PERCENTAGE, decreasing = TRUE, na.last = TRUE),], .1 * nrow(chicago1))
                m <- mapview(chicago1, z = "OCCUPIED.UNITS.PERCENTAGE")
            } else if(input$chicagoFilter == "Highest Percentage of Renters") {
                chicago1 <- merge(chicago1, data1[c("RENTER.OCCUPIED.HOUSING.PERCENTAGE", "BLOCK")], by = "BLOCK")
                chicago1 <- head(chicago1[order(chicago1$RENTER.OCCUPIED.HOUSING.PERCENTAGE, decreasing = TRUE, na.last = TRUE),], .1 * nrow(chicago1))
                m <- mapview(chicago1, z = "RENTER.OCCUPIED.HOUSING.PERCENTAGE")
            }

            m@map
        })


    #Used to reset settings on page 1, Near West Sideo verview
    observeEvent(input$reset_button1, {
        updateSelectInput(session, "filter", choices = filterOptions)
        updateSelectInput(session, "month", choices = monthOptions)
        updateSelectInput(session, "type", choices = typeOptions)
    })
    

    
    #Used to reset settings on page 2, Compare Communities
    observeEvent(input$reset_button2, {
        updateSelectInput(session, "community1", choices = communityOptions, selected = "Near West Side")
        updateSelectInput(session, "filter1", choices = filterOptions)
        updateSelectInput(session, "month1", choices = monthOptions)
        updateSelectInput(session, "type1", choices = typeOptions)

        updateSelectInput(session, "community2", choices = communityOptions, selected = "Loop")
        updateSelectInput(session, "filter2", choices = filterOptions)
        updateSelectInput(session, "month2", choices = monthOptions)
        updateSelectInput(session, "type2", choices = typeOptions)
    })

    #Used to reset settings on page 3, Chicago Overview
    observeEvent(input$reset_button3, {
        updateSelectInput(session, "chicagoFilter", choices = chicagoFilterOptions)
    })
}



shinyApp(ui = ui, server = server)
