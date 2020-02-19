library(tmap)
library(sf)

if (require("shiny")) {
  
  result = st_read('data/result.gpkg')
  ## Remove variables that will not be options for plotting
  result_vars = setdiff(names(result), c("id", "name", "geom"))
  
  
  ui <- fluidPage(
    # Application title
    titlePanel("Salzburg District population and touristic attractions"),
    
    # Top panel with municipality name
    verticalLayout(
      
      wellPanel(textOutput("municipality")),
      
      # the map itself
      tmapOutput("map"),
      selectInput("attribute", label = "Select variable: ", result_vars)
    )
  )
  
  server <- function(input, output, session) {
    output$map <- renderTmap({
      tm_shape(result) +
        tm_polygons(col = result_vars[1], zindex = 401, style = 'jenks', palette = 'viridis')
    })
    
    observe({
      var <- input$attribute
      tmapProxy("map", session, {
        tm_remove_layer(401) +
          tm_shape(result) +
          tm_polygons(var, zindex = 401, style = 'jenks', palette = 'viridis')
      })
    })
    
    observe({
      event <- input$map_shape_click
      output$municipality <- renderText(as.character(result$name[paste0('X',result$id) == event$id]))

    })
    

  }	
  
  
  app <- shinyApp(ui, server)
  if (interactive()) app
}