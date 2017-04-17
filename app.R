setwd('~/Documents/MSAN/DataViz/HW2')
library(ggvis)
library(shiny)
library(reshape2)


### LOAD and PREP ###
data <- read.csv('pop_fert_le.csv', stringsAsFactors = F)
drop <- c('Indicator.Code')
data <- data[,!(names(data) %in% drop)]
names(data)[4:ncol(data)] <- 1960:2014

meta <- read.csv('meta.csv', stringsAsFactors = F)[,1:2]
names(meta) <- c('Country.Code', 'Region')

data <-  merge(data, meta, "Country.Code")
data <- data[,2:ncol(data)]
data$Region <- as.factor(data$Region)
data$Country.Name <- as.factor(data$Country.Name)
data$Indicator.Name <- as.factor(data$Indicator.Name)
data <- melt(data, c("Indicator.Name", "Country.Name", "Region"), 3:(ncol(data)-1))
names(data) <- c('Indicator', 'Country', 'Region', 'Year', 'Value')
data <- dcast(data, Country+Region+Year~Indicator, value.var = "Value")
names(data)[4:5] <- c('Fertility', 'Life_Expectancy')
data <- subset(data, Region != '')
regions <- c('All Regions', as.character(unique(data$Region)))
####



ui <- shinyUI(
              navbarPage("Spencer's Homework Two",
                tabPanel("World Health Statistics",
                  sidebarLayout(
                    sidebarPanel(
                      selectInput('region', 'Select Region', choices = regions)
                                ),
                    mainPanel(
                      fluidRow(ggvisOutput('plot')),
                      fluidRow(sliderInput('slider', 'Year',
                                           min=1960, max=2014, value=1960,
                                           animate=animationOptions(interval = 100), sep = ""))
                                    )))))


server <- function(input, output) {
  
  output$region <- renderUI( selectInput('region', 'Select Region', choices = regions,  multiple = T))
  
  dataByYear <- reactive({
    d <- subset(data, Year == input$slider)
    d <- subset(d, !is.na(d$Life_Expectancy))
    d <- subset(d, !is.na(d$Fertility))
  
    region <- input$region
    if(region != "All Regions"){
      d <- subset(d, Region == region, drop = T)
    }
    
    return(d)
  })

  
  #regions <- reactive({
    
  #})

  visualization <- reactive({

    dataByYear %>%
      ggvis(~Life_Expectancy, ~Fertility, fill = ~Region,
            fillOpacity := 0.75, fillOpacity.hover := 1,
            strokeOpacity := 0.5) %>%
      scale_numeric("x", domain = c(10, 90), nice = FALSE) %>%
      scale_numeric("y", domain = c(0, 10), nice = FALSE) %>%
      set_options(width = 900, height = 600, renderer = "svg") %>%
      add_axis("x", title = "Life expectancy", properties = axis_props(title = list(fontSize = 16))) %>%
      add_axis("y", title = "Fertility rate", properties = axis_props(title = list(fontSize = 16))) %>%
      layer_points(size := ~Population/500000, key := ~Country) %>%
      hide_legend("size") %>%
      add_tooltip(function(data){
        paste0("Country: <b>", as.character(data$Country), "</b><br>",
               "Region: <b>", as.character(data$Region), "</b><br>",
               "Population: <b>", prettyNum(data$"Population"*500000, big.mark=",", scientific=FALSE), "</b><br>",
               "Life Expectancy: <b>", as.character(round(data$Life_Expectancy, 2)), "</b><br>",
               "Fertility Rate: <b>", as.character(round(data$Fertility, 2)), "</b>")
      }, "hover")
  })

  visualization %>% bind_shiny("plot")
}

shinyApp(ui = ui, server = server)


