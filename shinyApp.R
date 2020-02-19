library(tmap)

if (require("shiny")) {
  
  result = st_read('data/result.gpkg')
  # data(World)
  result_vars = setdiff(names(result), c("id", "name", "geom"))
  
  
  ui <- fluidPage(
    tmapOutput("map"),
    selectInput("attribute", label = "Select variable: ", result_vars)
  )
  
  server <- function(input, output, session) {
    output$map <- renderTmap({
      tm_shape(result) +
        tm_polygons(col = result_vars[1], zindex = 401, style = 'jenks')
    })
    
    observe({
      var <- input$attribute
      tmapProxy("map", session, {
        tm_remove_layer(401) +
          tm_shape(result) +
          tm_polygons(var, zindex = 401, style = 'jenks')
      })
    })
    
    observeEvent(input$map_shape_click, {
      
      #create object for clicked polygon
      click <- input$map_shape_click
      
      castles %>% filter(GEMNR == click$id)
      
      print(click$id)
      
    })
    
    # output$table <- renderTable({
    #   df()
    # })
  }	
  
  
  app <- shinyApp(ui, server)
  if (interactive()) app
}