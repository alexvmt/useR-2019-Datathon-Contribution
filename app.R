##### Shiny App #####

# load libraries
library(shiny)
library(tidyverse)
library(plotly)

# disable scientific notation
options(scipen=999)

# load data from csv
country_data_final <- read.csv("country_data_final.csv", header=TRUE, sep=",", dec=".")

# prepare data for geograhical plots for undernourishment prevalence in 2000 and 2016
year_map1 <- 2000
year_map2 <- 2016

map_data1 <- country_data_final %>% 
  filter(Year==year_map1)

map_data2 <- country_data_final %>% 
  filter(Year==year_map2)

# prepare data for geographical plot for percentage point change in undernourishment prevalence
map_data_change <- country_data_final %>% 
  filter(Year==year_map1 | Year==year_map2) %>% 
  group_by(Country.Name, Country.Code) %>% 
  summarize(first=first(Prevalence.of.undernourishment.per.cent.of.population),
            last=last(Prevalence.of.undernourishment.per.cent.of.population)) %>% 
  mutate(change=last-first)

# light grey boundaries for map
l <- list(color = toRGB("grey"), width = 0.5)

# specify map projection/options
g <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = 'Mercator')
)



### ui
ui <- fluidPage(
  
  titlePanel("useR! 2019 Datathon Contribution in relation to the UN Sustainable Development Goal 2 - Zero Hunger"),

    navbarPage(
      
      "Plots on indicators related to nutrition",
      
      tabPanel("Line plots",
               
               fluidRow(
                 
                 column(3, offset=1,
                        selectInput("indicator", "Select indicator",
                                    colnames(country_data_final)[7:(ncol(country_data_final))])
                        ),
                 
                 column(3,
                        selectInput("country1", "Select left country",
                                    unique(country_data_final$Country.Name))
                        ),
                 
                 column(3, 
                        selectInput("country2", "Select right country",
                                    unique(country_data_final$Country.Name))
                        )
                 
                 ),
                 
                 fluidRow(
                   
                   column(6,
                          plotlyOutput("line_plot1")
                          ),
                   
                   column(6,
                          plotlyOutput("line_plot2")
                          )
                   
                   )
               
               ),
      
      tabPanel("Scatter plot", 
               
               fluidRow(
                 
                 column(3, offset=1,
                        selectInput("indicator_y_axis", "Select indicator for y-axis",
                                    colnames(country_data_final)[7:(ncol(country_data_final))])),
                 
                 column(3,
                        selectInput("indicator_x_axis", "Select indicator for x-axis",
                                    colnames(country_data_final)[7:(ncol(country_data_final))])),
                 
                 column(3,
                        sliderInput("year", "Select year",
                                    min=min(unique(country_data_final$Year)),
                                    max=max(unique(country_data_final$Year)),
                                    step=1,
                                    value=2000,
                                    animate=TRUE)
                        )
                 
                 ),
               
                 plotlyOutput("scatter_plot")
                 
               ),
      
      tabPanel("Geographic plots",
               
               fluidRow(
                 
                 column(6, 
                        plotlyOutput("map1")
                        ),
                 
                 column(6,
                        plotlyOutput("map2")
                        )
                 
                 ),
               
               fluidRow(
                 
                 column(6, offset=3,
                        plotlyOutput("map3")
                        )
                 
                 )
               )
      )
  )



### server
server <- function(input, output) {
  
  # preparing reactive data for line plot 1
  line_plot_data1 <- reactive({
    
    line_plot_data1 <- country_data_final %>%
      filter(Country.Name==input$country1)
    
    })
  
  # rendering line plot 1
  output$line_plot1 <- renderPlotly({
    
    x <- list(title="Year")
    y <- list(title=input$indicator)
    
    plot_ly(line_plot_data1(), 
            x=~Year, 
            y=~get(input$indicator), 
            type="scatter", 
            mode="lines+markers", 
            text=~Income.Group,
            color=~Country.Name, 
            colors="Set1") %>% 
      layout(xaxis=x, yaxis=y, title="Development of a certain indicator for a certain country over time")
    
    })
  
  # preparing reactive data for line plot 2
  line_plot_data2 <- reactive({

    line_plot_data2 <- country_data_final %>%
      filter(Country.Name==input$country2)

  })

  # rendering line plot 2
  output$line_plot2 <- renderPlotly({

    x <- list(title="Year")
    y <- list(title=input$indicator)
    
    plot_ly(line_plot_data2(), 
            x=~Year, 
            y=~get(input$indicator), 
            type="scatter", 
            mode="lines+markers", 
            text=~Income.Group,
            color=~Country.Name, 
            colors="Set2") %>% 
      layout(xaxis=x, yaxis=y, title="Development of a certain indicator for a certain country over time")

  })
  
  # preparing reactive data for scatter plot
  scatter_plot_data <- reactive({

    scatter_plot_data <- country_data_final %>%
      filter(Year==input$year)

    })

  # rendering scatter plot
  output$scatter_plot <- renderPlotly({

    x <- list(title=input$indicator_x_axis)
    y <- list(title=input$indicator_y_axis)
    
    plot_ly(scatter_plot_data(), 
            x=~get(input$indicator_x_axis), 
            y=~get(input$indicator_y_axis), 
            text=~Country.Name, 
            color=~Region,
            size=~Population.total,
            type="scatter", 
            mode="markers") %>% 
      layout(xaxis=x, yaxis=y, title="Scatter plot to compare relationship between nutrion and socio-demographic indicators")

  })
  
  # rendering geographical plot 1
  output$map1 <- renderPlotly({

    plot_geo(map_data1) %>%
      add_trace(
        z=~Prevalence.of.undernourishment.per.cent.of.population, color=~Prevalence.of.undernourishment.per.cent.of.population, colors="Reds",
        text=~Country.Name, locations=~Country.Code, marker=list(line=l)
      ) %>%
      colorbar(title="% of population", ticksuffix="%") %>%
      layout(
        title="Prevalence of undernourishment (% of population), Year==2000",
        geo=g
      )

  })

  # rendering geographical plot 2
  output$map2 <- renderPlotly({
    
    plot_geo(map_data2) %>%
      add_trace(
        z=~Prevalence.of.undernourishment.per.cent.of.population, color=~Prevalence.of.undernourishment.per.cent.of.population, colors="Reds",
        text=~Country.Name, locations=~Country.Code, marker=list(line=l)
      ) %>%
      colorbar(title="% of population", ticksuffix="%") %>%
      layout(
        title="Prevalence of undernourishment (% of population), Year==2016",
        geo=g
      )
    
  })
 
  # rendering geographical plot 3
  output$map3 <- renderPlotly({
    
    plot_geo(map_data_change) %>%
      add_trace(
        z=~change, color=~change, colors="RdYlBu",
        text=~Country.Name, locations=~Country.Code, marker=list(line=l)
      ) %>%
      colorbar(title=" % point change", ticksuffix="%") %>%
      layout(
        title="Percentage point change in prevalence of undernourishment between 2000 and 2016",
        geo=g
      )
    
  })
  }



shinyApp(ui = ui, server = server)