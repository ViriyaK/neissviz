#### @author: Viriya Keo
#### @date: December 14, 2018
#### Install R and  R Studio. Load the app.R file, then click Run App and the application will run
####    as a pop-up which you can then optionally open in a browser.
#### If the app does not run, it's most likely bcause you do not have the required packages installed.
#### Install the required packages by running `install.packages("package_name")` with the package_name in parenthesis.
#### For the neiss package, extra steps are needed.
#### `install.packages("devtools")` and then `devtools::install_github("hadley/neiss")`.
#### Note that this might take a while to download because of the big amount of data.

library(shiny)
library(shinyWidgets)
library(shinythemes)
library(plyr)
library(dplyr)
library(neiss)  
library(ggplot2)
library(readr)
library(DT)
library(lubridate)
library(scales)

injuries <- left_join(injuries, products, by = c("prod1" = "code"))
injuries <- as.data.frame(injuries)
colnames(injuries)[colnames(injuries) == "title"] <- "product"
injuries$year <- as.factor(year(as.Date(injuries$trmt_date, "%m/%d/%Y")))
injuries$iage <- cut(injuries$age, breaks = seq(0, 120, by = 10), include.lowest = TRUE)
factors <- c(4:15)
injuries[factors] <- lapply(injuries[factors], as.factor)

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("NEISSviz: NEISS Injury Data Explorer", windowTitle = "NEISSviz"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      tags$h4("Case Definition"),
      pickerInput(inputId = "year",
                  label = "Year",
                  choices = levels(injuries$year),
                  selected = 2017,
                  options = list(
                    `actions-box` = TRUE,
                    size = 10,
                    `selected-text-format` = "count > 3"
                  ),
                  multiple = TRUE),
      
      sliderInput("age", label = "Age",
                  min = 0, max = max(injuries$age), value = c(0, max(injuries$age))),
      
      checkboxGroupInput(inputId = "sex",
                         label = "Sex",
                         choices = c("Female" = "Female", "Male" = "Male"),
                         selected = c("Female", "Male")),
      
      pickerInput(inputId = "race",
                  label = "Race",
                  choices = levels(injuries$race),
                  selected = levels(injuries$race),
                  options = list(
                    `actions-box` = TRUE,
                    size = 10,
                    `selected-text-format` = "count > 3"
                  ),
                  multiple = TRUE),
      
      pickerInput(inputId = "body_part",
                  label = "Body Parts",
                  choices = levels(injuries$body_part),
                  selected = levels(injuries$body_part),
                  options = list(
                    `actions-box` = TRUE,
                    size = 10,
                    `selected-text-format` = "count > 3"
                  ),
                  multiple = TRUE),
      
      pickerInput(inputId = "diagnosis",
                  label = "Diagnosis",
                  choices = levels(injuries$diag),
                  selected = levels(injuries$diag),
                  options = list(
                    `actions-box` = TRUE,
                    size = 10,
                    `selected-text-format` = "count > 3"
                  ),
                  multiple = TRUE),
      
      pickerInput(inputId = "disposition",
                  label = "Disposition",
                  choices = levels(injuries$disposition),
                  selected = levels(injuries$disposition),
                  options = list(
                    `actions-box` = TRUE,
                    size = 10,
                    `selected-text-format` = "count > 3"
                  ),
                  multiple = TRUE),
      
      pickerInput(inputId = "location",
                  label = "Location",
                  choices = levels(injuries$location),
                  selected = levels(injuries$location),
                  options = list(
                    `actions-box` = TRUE,
                    size = 10,
                    `selected-text-format` = "count > 3"
                  ),
                  multiple = TRUE),
      
      pickerInput(inputId = "fire",
                  label = "Fire Department Involvement",
                  choices = levels(injuries$fmv),
                  selected = levels(injuries$fmv),
                  options = list(
                    `actions-box` = TRUE,
                    size = 10,
                    `selected-text-format` = "count > 3"
                  ),
                  multiple = TRUE),
      
      pickerInput(inputId = "product",
                  label = "Product",
                  choices = levels(injuries$prod1),
                  selected = levels(injuries$prod1),
                  options = list(
                    `actions-box` = TRUE, 
                    size = 10,
                    `selected-text-format` = "count > 3"
                  ),
                  multiple = TRUE),
      
      pickerInput(inputId = "stratum",
                  label = "Stratum",
                  choices = levels(injuries$stratum),
                  selected = levels(injuries$stratum),
                  options = list(
                    `actions-box` = TRUE, 
                    size = 10,
                    `selected-text-format` = "count > 3"
                  ),
                  multiple = TRUE),
      
      checkboxInput(inputId = "show_data",
                    label = "Show Data Table",
                    value = TRUE),
      width = 3,
      
      h5("Built with",
         img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
         "by",
         img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "30px"),
         ".")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs",
                  id = "tabsetpanel",
                  tabPanel("Summary Graphics", 
                           selectInput(inputId = "x",
                                       label = "Choose X-axis",
                                       choices = c("Age" = "iage", "Sex" = "sex", "Race" = "race", "Body Part" = "body_part", "Diagnosis" = "diag",
                                                   "Disposition" = "disposition", "Location" = "location", "Fire" = "fmv", "Product" = "fmv",
                                                   "Product" = "prod1", "Stratum" = "stratum"),
                                       selected = "sex"),
                           plotOutput(outputId = "histogram"),
                           tableOutput(outputId = "info")),
                  tabPanel("Data", 
                           DT::dataTableOutput(outputId = "dataTable"),
                           downloadButton(outputId = "download_data", label = "Download data")
                  ),
                  tabPanel("Product Description", 
                           DT::dataTableOutput(outputId = "prodTable"))
                 ),
      width = 9
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  case_definition <- reactive({
    req(input$year, input$age, input$sex, input$race, input$body_part, input$diagnosis, input$disposition, input$location, input$fire, input$product, input$stratum)
    injuries %>% 
      filter(year %in% input$year, 
             age >= input$age[1] & age <= input$age[2],
             sex %in% input$sex, 
             race %in% input$race,
             body_part %in% input$body_part,
             diag %in% input$diagnosis,
             disposition %in% input$disposition,
             location %in% input$location,
             fmv %in% input$fire,
             prod1 %in% input$product,
             stratum %in% input$stratum
      )
  })
  
  output$histogram <- renderPlot({
      req(input$x)
      case_definition() %>% 
        ggplot(aes_string(x = input$x)) + 
        geom_bar(aes(y = (..count..)/sum(..count..), fill = factor(..x..)), stat="count") +
        geom_text(aes( label = scales::percent((..count..)/sum(..count..)),
                       y= (..count..)/sum(..count..) ), stat= "count") +
        labs(y = "Percent") +
        coord_flip() +
        scale_y_continuous(labels = scales::percent) +
        theme(legend.position="none")
  })
  
  output$info <- renderTable({
    case_definition() %>%
      group_by_(input$x) %>% 
      count()
  })
  
  output$dataTable <- DT::renderDataTable({
    selected <- case_definition() %>%
      select(age, sex, race, body_part, product, narrative, year)
    DT::datatable(data = selected,
                  options = (list(pageLength = 5)),
                  rownames = FALSE)
  })
  
  output$prodTable <- DT::renderDataTable({
    DT::datatable(data = products,
                  options = (list(pageLength = 10)),
                  rownames = FALSE)
  })
  
  observeEvent(input$show_data, {
    if(input$show_data){
      showTab(inputId = "tabsetpanel", target = "Data")
    } else {
      hideTab(inputId = "tabsetpanel", target = "Data")
    }
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("Case_Definition.tsv")
    },
    content = function(file) {
      write_tsv(case_definition(), file)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

