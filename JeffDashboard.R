library(shiny)
library(ggplot2)
library(shinydashboard)
library(tidyverse)

# Load Data
Data <- read.csv("/Users/grantrehling/Downloads/ThruFL.csv")

Data <- subset(Data, PitchCall %in% c("BallCalled", "StrikeCalled"))
Data <- mutate(Data, Umpire = Batter)
Data <- mutate(Data, PitchType = case_when(
  TaggedPitchType %in% c("Fastball", "Cutter", "Sinker") ~ "Fastball",
  TaggedPitchType %in% c("Slider", "Curveball") ~ "Breaking Ball",
  TaggedPitchType %in% c("ChangeUp", "Splitter", "Knuckleball") ~ "Offspeed",
  TRUE ~ as.character(TaggedPitchType)  # for any other pitch type
))


ui <- dashboardPage(
  dashboardHeader(
    title = "JEFF Dashboard",
    tags$li(class = "dropdown", tags$style(".navbar { background-color: #808080; }")),
    tags$li(class = "dropdown", tags$style(".logo { color: #000000; }")),
    tags$li(class = "dropdown", tags$style(".navbar-header { background-color: #808080; }"))
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Umpire Evaluations", tabName = "plot", icon = icon("chart-bar"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .title-center {
          text-align: center;
          font-family: 'Trebuchet MS', Helvetica, sans-serif;
        }
        body {
          background-color: #000000 !important;
          color: #000000;
        }
        .skin-blue .main-header .navbar {
          background-color: #000000;
          border-bottom: 2px solid #FFD700;
        }
        .skin-blue .main-header .logo {
          background-color: #000000;
          color: #FFFFFF;
          border: none;
          padding-bottom: 5px;
        }
        .content-wrapper,
        .right-side,
        .main-sidebar {
          background-color: #ffffff !important;
          color: #000000;
        }
        .shiny-output {
          background-color: #ffffff !important;
          border-radius: 10px;
        }
        .custom-sidebar {
          background-color: #000000;
          border: 3px solid #000000;
        }
      "))
    ),
    tags$img(src = "BlackAnalyticsLogo.png", height = 100, width = 100, style = "float: right;"),
    tabItems(
      tabItem(tabName = "plot",
              sidebarPanel(
                selectInput("player", "Select Umpire", choices = unique(Data$Umpire)),
                selectInput("pitchType", "Pitch Type", choices = c("All", "Fastball", "Breaking Ball", "Offspeed")),  
              ),  # Added closing parenthesis here
              mainPanel(
                plotOutput("scatterplot")  
              ),
              fluidRow(
                style = "margin-left: 20px;",  
                column(4, offset = 1.75,  
                       selectInput("BatterSide", "Batter Side",
                                   choices = c("Left", "Right"))),  
                column(4, offset = 3,  
                       selectInput("PitcherThrows", "Pitcher Side",
                                   choices = c("Left", "Right")))
              ),
              fluidRow(
                column(6, plotOutput("newGraph1")), 
                column(6, plotOutput("newGraph2"))   
              )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  filtered_data <- reactive({
    req(input$player) 
    filtered <- Data %>%
      filter(Umpire == input$player) %>%
      mutate(
        PitchType = case_when(
          TaggedPitchType %in% c("Fastball", "Cutter", "Sinker") ~ "Fastball",
          TaggedPitchType %in% c("Slider", "Curveball") ~ "Breaking Ball",
          TaggedPitchType %in% c("ChangeUp", "Splitter", "Knuckleball") ~ "Offspeed",
          TRUE ~ as.character(TaggedPitchType)  
        )
      )
    
    return(filtered)
  })

  # Function to calculate accuracy
  calculate_accuracy <- function(data) {
    high_strikes <- sum(data$PlateLocHeight >= 3.5 & data$PitchCall == "StrikeCalled")
    high_balls <- sum(data$PlateLocHeight <= 3.5 & data$PitchCall == "BallCalled")
    low_strikes <- sum(data$PlateLocHeight <= 1.5 & data$PitchCall == "StrikeCalled")
    low_balls <- sum(data$PlateLocHeight >= 1.5 & data$PitchCall == "BallCalled")
    outside_strikes <- sum(data$PlateLocSide >= 0.83 & data$PitchCall == "StrikeCalled")
    outside_balls <- sum(data$PlateLocSide <= 0.83 & data$PitchCall == "BallCalled")
    inside_strikes <- sum(data$PlateLocSide <= -0.83 & data$PitchCall == "StrikeCalled")
    inside_balls <- sum(data$PlateLocSide >= 0.83 & data$PitchCall == "BallCalled")
    
    accuracy_high_strikes <- high_strikes / (high_strikes + high_balls)
    accuracy_high_balls <- high_balls / (high_strikes + high_balls)
    accuracy_low_strikes <- low_strikes / (low_strikes + low_balls)
    accuracy_low_balls <- low_balls / (low_strikes + low_balls)
    accuracy_outside_strikes <- outside_strikes / (outside_strikes + outside_balls)
    accuracy_outside_balls <- outside_balls / (outside_strikes + outside_balls)
    accuracy_inside_strikes <- inside_strikes / (inside_strikes + inside_balls)
    accuracy_inside_balls <- inside_balls / (inside_strikes + inside_balls)
    
    return(list(accuracy_high_strikes = accuracy_high_strikes, 
                accuracy_high_balls = accuracy_high_balls,
                accuracy_low_strikes = accuracy_low_strikes,
                accuracy_low_balls = accuracy_low_balls,
                accuracy_outside_strikes = accuracy_outside_strikes,
                accuracy_outside_balls = accuracy_outside_balls,
                accuracy_inside_strikes = accuracy_inside_strikes,
                accuracy_inside_balls = accuracy_inside_balls))
  }
  
  output$scatterplot <- renderPlot({
    filtered <- filtered_data()
    
    if (!is.null(input$pitchType) && input$pitchType != "All") {
      filtered <- filtered[filtered$PitchType == input$pitchType, ]
    }
    
    # Generate the ggplot visualization of the strike zone using filtered data
    ggplot(filtered, aes(x = PlateLocSide, y = PlateLocHeight, shape = PitchType, color = PitchCall)) +
      geom_point() + 
      scale_shape_manual(values = c(
        "Fastball" = 16,
        "Breaking Ball" = 17,
        "Offspeed" = 18
      )) +
      geom_rect(xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5, 
                color = "black", fill = "transparent",  size = 1.1) +   #Home Plate
      geom_rect(xmin = -1.2, xmax = 1.2, ymin = 1.2, ymax = 3.8, 
                color = "black", linetype = "dashed", fill = "transparent", size = 1.1) +
      geom_rect(xmin = -0.6, xmax = 0.6, ymin = 1.8, ymax = 3.2, 
                color = "black", linetype = "dashed", fill = "transparent", size = 1.1) +
      geom_segment(aes(x = -0.708, y = 0.15, xend= 0.708, yend = 0.15), size = 0.5, color = "black") +
      geom_segment(aes(x = -0.708, y = 0.3, xend = 0, yend = 0.5), size = 0.5, color = "black")+
      geom_segment(aes(x = 0, y = 0.5, xend = 0.708, yend = 0.3), size = 0.5, color = "black")+
      geom_segment(aes(x = 0.708, y = 0.3, xend = 0.708, yend = 0.15), size = 0.5, color = "black")+
      scale_color_manual(name = "Pitch Call", values = c("StrikeCalled" = "gold", "BallCalled" = "black")) +
      coord_equal() +
      theme_bw() +
      xlim(-2, 2) +
      ylim(0, 4)
    
  })
  
  output$newGraph1 <- renderPlot({
    filtered <- filtered_data()
    filtered = filtered %>% filter(BatterSide == input$BatterSide)
    ggplot(filtered, aes(x = PlateLocSide, y = PlateLocHeight, color = PitchCall)) +
      geom_point() + 
      geom_rect(xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5, 
                color = "black", fill = "transparent",  size = 1.1) +   #Home Plate
      geom_rect(xmin = -1.2, xmax = 1.2, ymin = 1.2, ymax = 3.8, 
                color="black", linetype ="dashed", fill="transparent", size = 1.1) +
      geom_rect(xmin = -0.6, xmax = 0.6, ymin = 1.8, ymax = 3.2, 
                color = "black", linetype = "dashed", fill = "transparent", size = 1.1) +
      geom_segment(aes(x = -0.708, y = 0.15, xend= 0.708, yend = 0.15), size = 0.5, color = "black") +
      geom_segment(aes(x = -0.708, y = 0.3, xend = 0, yend = 0.5), size = 0.5, color = "black")+
      geom_segment(aes(x = 0, y = 0.5, xend = 0.708, yend = 0.3), size = 0.5, color = "black")+
      geom_segment(aes(x = 0.708, y = 0.3, xend = 0.708, yend = 0.15), size = 0.5, color = "black")+
      scale_color_manual(name = "Pitch Call", values = c("StrikeCalled" = "gold", "BallCalled" = "black")) +
      coord_equal() +
      theme_bw() +
      xlim(-2, 2) +
      ylim(0, 4)
  }, width = 400, height = 300)
  
  
  output$newGraph2 <- renderPlot({
    filtered <- filtered_data()
    filtered = filtered %>% filter(PitcherThrows == input$PitcherThrows)
    
    ggplot(filtered, aes(x = PlateLocSide, y = PlateLocHeight, color = PitchCall)) +
      geom_point() + 
      geom_rect(xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5, 
                color = "black", fill = "transparent",  size = 1.1) +   #Home Plate
      geom_rect(xmin = -1.2, xmax = 1.2, ymin = 1.2, ymax = 3.8, 
                color="black", linetype ="dashed", fill="transparent", size = 1.1) +
      geom_rect(xmin = -0.6, xmax = 0.6, ymin = 1.8, ymax = 3.2, 
                color = "black", linetype = "dashed", fill = "transparent", size = 1.1) +
      geom_segment(aes(x = -0.708, y = 0.15, xend= 0.708, yend = 0.15), size = 0.5, color = "black") +
      geom_segment(aes(x = -0.708, y = 0.3, xend = 0, yend = 0.5), size = 0.5, color = "black")+
      geom_segment(aes(x = 0, y = 0.5, xend = 0.708, yend = 0.3), size = 0.5, color = "black")+
      geom_segment(aes(x = 0.708, y = 0.3, xend = 0.708, yend = 0.15), size = 0.5, color = "black")+
      scale_color_manual(name = "Pitch Call", values = c("StrikeCalled" = "gold", "BallCalled" = "black")) +
      coord_equal() +
      theme_bw() +
      xlim(-2, 2) +
      ylim(0, 4)
  }, width = 400, height = 300)
}

# Run the application
shinyApp(ui, server)
