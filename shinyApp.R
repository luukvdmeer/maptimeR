library(tmap)
library(sf)

if (require("shiny")) {
  
  ## Load the result just created 
  result = st_read('data/result.gpkg')
  ## Remove variables that will not be options for plotting
  result_vars = setdiff(names(result), c("id", "name", "geom"))
  
  # Prepare the basic layout of the User Interface
  ui <- fluidPage(
    # Application title
    titlePanel("Salzburg District population and touristic attractions"),
    
    # Include a vertical layout to add bith the text box and the map
    verticalLayout(
      
      # Top panel with municipality name
      wellPanel(textOutput("municipality")),
      
      # the map itself
      tmapOutput("map"),
      selectInput("attribute", label = "Select variable: ", result_vars)
    )
  )
  
  # Prepare the server 
  server <- function(input, output, session) {
    
    # Here comes the code for the tmap (remember the interactive version?)
    output$map <- renderTmap({
      tm_shape(result) +
        tm_polygons(
          # To plot the first variable 
          col = result_vars[1],
          # This sets the layer number for the municipalities to plot. 
          # Always set as 400 + n
          zindex = 401, 
          style = 'jenks', 
          palette = 'viridis'
        )
    })
    
    # Adding some interaction, this section will change the attribute 
    # plotted when the user selects it on the drop-down list
    observe({
      var <- input$attribute
      tmapProxy("map", session, {
        tm_remove_layer(401) +
          tm_shape(result) +
          tm_polygons(var, zindex = 401, style = 'jenks', palette = 'viridis')
      })
    })
    
    # This will display the name of the municipality on the 
    # text box on top of the map when its polygon is clicked on the map
    observe({
      event <- input$map_shape_click
      output$municipality <- renderText(
        # We had to do some workarounds for this section to work properly,
        # due to data classes mistmatches 
        # Essentially we obtain the name of the municipality that corresponds 
        # to the id clicked on the map by matching it to the municipality id 
        as.character(result$name[paste0('X',result$id) == event$id])
      )
      
    })
    
    
  }	
  
  # Save the app
  app <- shinyApp(ui, server)
  if (interactive()) app
}