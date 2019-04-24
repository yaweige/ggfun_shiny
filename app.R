library(shiny)

# set up package
library(dplyr)
library(ggplot2)
library(ggfun)

# Define UI for application that draws a histogram
ui <- fluidPage(
  navbarPage("ggfun",
             tabPanel("geom_image",
               sidebarLayout(
                 sidebarPanel(
                   selectInput("x_variable",
                               label = "Choose variable on x axis",
                               choices = list("mpg" = "mpg", "cyl" = "cyl", "disp" = "disp",
                                              "drat" = "drat", "wt" = "wt", "qsec" = "qsec",
                                              "vs" = "vs", "am" = "am", "gear" = "gear",
                                              "carb" = "carb"),
                               selected = "mpg"
                   ),
                   selectInput("y_variable",
                               label = "Choose variable on y axis",
                               choices = list("mpg" = "mpg", "cyl" = "cyl", "disp" = "disp",
                                              "drat" = "drat", "wt" = "wt", "qsec" = "qsec",
                                              "vs" = "vs", "am" = "am", "gear" = "gear",
                                              "carb" = "carb"),
                               selected = "mpg"
                   ),
                   sliderInput("size", label = "Choose image size",
                               min = 0.01, max = 0.3, value = 0.1)
                   
                 ),
                 
                 # Show a plot of the generated distribution
                 mainPanel(
                   plotOutput("geomimage"),
                   helpText("Example code:",
                            "ggplot(data = mtcars, aes(x = mpg, y = wt)) + 
                            geom_image()")
                 )
               )
             ),
             
             tabPanel("stat_star",
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("datasize", "Data size",
                                      min = 2, max = 200, value = 50),
                          selectInput("x_dist", label = "Choose distribution on x axis",
                                      choices = list("Normal" = "normal", "f-distribution" = "f"),
                                      selected = "f"),
                          selectInput("y_dist", label = "Choose distribution on y axis",
                                      choices = list("Normal" = "normal", "f-distribution" = "f"),
                                      selected = "normal"),
                          selectInput("starcolor", "Line color",
                                      choices = list("black" = "black", "red" = "red", "yellow" = "yellow",
                                                     "green" = "green", "blue" = "blue", "pink" = "pink"),
                                      selected = "black"),
                          sliderInput("starsize", "Lline size",
                                      min = 0.1, max = 3, value = 0.5),
                          
                          conditionalPanel("input.x_dist === 'normal'",
                                           sliderInput("xnorm_mean", "x axis mean",
                                           min = -10, max = 10, value = 0),
                                           sliderInput("xnorm_var", "x axis variance",
                                                       min = 0.01, max = 10, value = 1)),
                          
                          conditionalPanel("input.y_dist === 'normal'",
                                           sliderInput("ynorm_mean", "y axis mean",
                                                       min = -10, max = 10, value = 0),
                                           sliderInput("ynorm_var", "y axis variance",
                                                       min = 0.01, max = 10, value = 1))
                          
                          
                        ),
                       
                        # Show a plot of the generated distribution
                        mainPanel(
                          plotOutput("statstar"),
                          helpText("Example code:",
                                   "ggplot(data, aes(x, y)) + geom_point() + 
                                   stat_star()")
                        )
                      )),
             
             tabPanel("stat_arrowmap",
                      sidebarLayout(
                        sidebarPanel(
                        textInput("saysomething", "Do you want to say something?",
                                  value = ""),
                        verbatimTextOutput("text"),
                        radioButtons("arrowtype", "Arrow type",
                                     choices = list("closed" = "closed", open = "open"),
                                     selected = "open"),
                        conditionalPanel("input.arrowtype === 'closed'",
                                         selectInput("arrowfill", "Arrow head fill color",
                                                     choices = list("black" = "black", "red" = "red", "yellow" = "yellow",
                                                                    "green" = "green", "blue" = "blue", "pink" = "pink"),
                                                     selected = "black")),
                        sliderInput("arrowlength", "Arrow head length",
                                    min = 0.05, max = 0.5, value = 0.01),
                        sliderInput("arrowsize", "Arrow line size",
                                    min = 0.05, max = 2, value = 0.5)
                          
                        ),
                        
                        
                        
                        # Show a plot of the generated distribution
                        mainPanel(
                          plotOutput("statarrowmap"),
                          helpText("Example code:",
                                   "ggplot(certain_data) + geom_path(aes(long, lat, group)) +
                                    stat_arrowmap(aes(long, lat, change, group))")
                        )
                      )),
             
             tabPanel("layer_PersHomo",
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("eqDate", "Set a Time Range of observation in AD", min = -70, max = 2019, value = c(-70,2019)),
                          sliderInput("MAG", "Set minimum Magnitude of earthquake in Ms", min = 0, max = 15, value = 0),
                          sliderInput("d", "Set the Persistent Homology Radius in km", min = 0, max = 1000000, value = 150000)
                          ),
                        
                        # Show a plot of the generated world map with linkage
                        mainPanel(
                          plotOutput("PersHomoMap"),
                          helpText("Example code:",
                                   "ggplot(worldMap) + layer_PersHomo(data= eq, mapping = aes(x=LONGITUDE, y=LATITUDE), d=input$d, colour = 'blue') +
                                   geom_point()")
                        )
                      ))
             
  )
  # Sidebar with a slider input for number of bins 
  
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$geomimage <- renderPlot({
    ggplot(data = mtcars, aes_string(x = input$x_variable, y = input$y_variable)) + 
      geom_image(size = input$size) +
      ggtitle("mtcars data set")
  })
  
  output$statstar <- renderPlot({
    if (input$x_dist == "normal") x <- rnorm(n = input$datasize, mean = input$xnorm_mean, sd = input$xnorm_var)
    if (input$x_dist == "f") x <- rf(n = input$datasize, df1 = 5, df2 = 2)
    if (input$y_dist == "normal") y <- rnorm(n = input$datasize, mean = input$ynorm_mean, sd = input$ynorm_var)
    if (input$y_dist == "f") y <- rf(n = input$datasize, df1 = 5, df2 = 2)
    
    data <- data.frame(x = x, y = y)
    ggplot(data = data, aes(x = x, y = y)) + 
      geom_point() + 
      stat_star(size = input$starsize, color = input$starcolor)
  })
  
  usmap <- map_data("state")
  output$statarrowmap <- renderPlot({
    madedata_standard <- data.frame(region = unique(usmap$region), change = (runif(49)-0.5)*2,
                                    stringsAsFactors = F)
    madedata_standard <- madedata_standard %>%
      left_join(usmap,by = "region")
    
    madedata_standard %>%
      ggplot() +
      geom_path(aes(x = long, y = lat, group = group)) +
      stat_arrowmap(aes(x = long, y = lat, change = change, group = region), 
                    curvature = 0.3, angle = 60, arrow.fill = input$arrowfill,
                    size = input$arrowsize,
                    arrow = arrow(type = input$arrowtype,
                                  length = unit(input$arrowlength, "inches")))
  })
  
  url <- "https://www.ngdc.noaa.gov/nndc/struts/results?type_0=Exact&query_0=$ID&t=101650&s=13&d=189&dfn=signif.txt"
  eq.raw <- read.delim(url, as.is=T) %>%
    filter(!is.na(LONGITUDE) & !is.na(LATITUDE)) %>%
    filter(LONGITUDE > 110 | LONGITUDE < -45) %>%  
    mutate(LONGITUDE = ifelse(LONGITUDE < 0, LONGITUDE + 360, LONGITUDE)) %>%
    select(YEAR, MONTH,DAY, EQ_MAG_MS, COUNTRY, LOCATION_NAME, LATITUDE, LONGITUDE)
  
  output$PersHomoMap <- renderPlot({
    eq <- eq.raw %>% 
      filter(EQ_MAG_MS > input$MAG) %>% 
      filter(YEAR > input$eqDate[1] | YEAR < input$eqDate[2])
    ## plot base map
    worldmap <- map_data("world2")
    p <- ggplot() +
      geom_polygon(data=worldmap, aes(x=long, y=lat, group = group),fill="white", colour="#7f7f7f", size=0.5) +
      ggtitle("Earthquake around Pacific Plate") +
      theme(axis.line=element_blank(),
            axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            legend.position="none",
            panel.background=element_blank(),
            panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            plot.background=element_blank()); p
    ## add layer_PersHomo
    fp <- p + layer_PersHomo(data= eq, mapping = aes(x=LONGITUDE, y=LATITUDE), d=input$d, colour = "blue") +
      geom_point(); fp
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

