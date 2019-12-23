library(shiny)
library(odbc)
library(tidyverse)
#library(DT)
#library(data.table)

#set database connection
conn <- odbc::dbConnect(odbc(), dsn = "mars_testing", uid = Sys.getenv("shiny_uid"), pwd = Sys.getenv("shiny_pwd"))
#query all SMP IDs
smp_id <- odbc::dbGetQuery(conn, paste0("select distinct smp_id from smpid_facilityid_componentid")) %>% dplyr::arrange(smp_id)

#create dataframe with new wells, names and codes
asset_type <- c("Shallow Well", "Groundwater Well", "Groundwater Control Well", "Forebay")
ow_code <- c("SW", "GW", "CW", "FB")
new_wells <- data.frame(asset_type, ow_code)
new_wells$asset_type <- new_wells$asset_type %>% as.character()
new_wells$ow_code <- new_wells$ow_code %>% as.character()
rm(asset_type, ow_code)

ui <- fluidPage(
  
  titlePanel("MARS Observation Wells"),
  
  sidebarPanel(
  
    width = 6, 
  selectInput("smp_id", "Select an SMP ID", choices = smp_id),
  selectInput("component_id", "Select a Component ID", choices = c("a",
                                                                   "OW2")),
  
  textInput("ow_suffix", "OW Suffix"), 
  actionButton("add_ow", "Add Observation Well")
  ), 
  
  mainPanel(
  h4("Observation Well Name"),
  textOutput("text"),
  h4("List of Observation Wells at this SMP"), 
  tableOutput("table"), 
  h4("Facility ID"), 
  textOutput("facility")
  # h4("test"),
  # textOutput("blah")
  )
  
)

server <- function(input, output, session){
  # anything commented out is likely for adding "EDIT" buttons to the table, which has been put on hold for the time being (12/23/2019 - NM)
  
  
  # shinyInput <- function(FUN, len, id, ...) {
  #   inputs <- character(len)
  #   for (i in seq_len(len)) {
  #     inputs[i] <- as.character(FUN(paste0(id, i), ...))
  #   }
  #   inputs
  # }
  
  #set component IDs based on SMP ID
  component_id <- reactive(odbc::dbGetQuery(conn, paste0(
    "select distinct component_id, asset_type from smpid_facilityid_componentid where smp_id = '", input$smp_id, "' 
        AND component_id IS NOT NULL")))
  
  #set ow code based on asset comp
  #this could be a dataframe merge if needed
  #set "asset comp code" to be shown in component ID combo box
  asset_comp <- reactive(component_id() %>% 
                           mutate(ow_code = case_when(asset_type == "Inlet" ~ "GI",
                                                      asset_type == "Fitting" ~ "OW",
                                                      asset_type == "Cleanout" ~ "CO",
                                                      asset_type == "Observation Well" ~ "OW",
                                                      asset_type == "Groundwater Control Well" ~ "CW",
                                                      asset_type == "Groundwater Welll" ~ "GW",
                                                      asset_type == "Shallow Well" ~ "SW", 
                                                      asset_type == "Control Structure" ~ "CS")) %>% 
                           dplyr::bind_rows(new_wells) %>% 
                           mutate(asset_comp_code = paste(component_id, asset_type, ow_code, sep = " | ")))

  asset_combo <- reactive(asset_comp()$asset_comp_code)
  
  #update component ID box based on the SMP chosen
  observe(updateSelectInput(session, "component_id", choices = asset_combo()))
  
  #update well name (SMP ID + OW SUFFIX)
  output$text <- renderText({
    paste(input$smp_id, input$ow_suffix, sep = "_")
   })
  
  #see which wells already exist at the SMP
  result_table <- reactive(odbc::dbGetQuery(conn, paste0(
        "SELECT * FROM ow_testing WHERE smp_id = '", input$smp_id, "'")))
  
  #actions <- reactive(shinyInput(actionButton, nrow(result_table), 'button', label = "Edit"))
  
  # df <- reactive(reactiveValues(data = data.frame(
  #   smp_id = result_table()$smp_id, 
  #   ow_suffix = result_table()$ow_suffix, 
  #   facility_id = result_table()$facility_id, 
  #   actions = shinyInput(actionButton, nrow(result_table()), 'button', label = "Edit", onclick = 'Shiny.onInputChange(select_button, this.id'), 
  #   stringsAsFactors = FALSE, 
  #   row.names = 1:nrow(result_table())
  # )))
  
  # output$table <- DT::renderDataTable(
  #   df()$data, server = FALSE, escape = FALSE, selection = 'none'
  # )
  
  #render result table
  output$table <- renderTable(
    result_table()
  )
  
  # observeEvent(input$select_button, {
  #   asset_combo() <- c(7, 5, 3)
  #   
  # })
  
  select_component_id <- reactive(asset_comp() %>% 
                                     dplyr::filter(asset_comp_code == input$component_id) %>% 
                                     dplyr::select(component_id) %>% 
                                     dplyr::pull())
   
  select_ow_code <- reactive(asset_comp() %>% 
                                dplyr::filter(asset_comp_code == input$component_id) %>% 
                                dplyr::select(ow_code) %>% 
                                dplyr::pull())
  
  #get facility ID. Either use SMP footprint (for a new well) or the facility ID of the existing component
  facility_id <- reactive(if(select_ow_code() %in% new_wells$ow_code) odbc::dbGetQuery(conn, paste0(
       "SELECT facility_id FROM smpid_facilityid_componentid WHERE component_id IS NULL AND smp_id = '", input$smp_id, "'")) else 
      odbc::dbGetQuery(conn, paste0(
      "SELECT facility_id from smpid_facilityid_componentid WHERE component_id = '", select_component_id(), "'"))
  )
  
  #show facility ID
  output$facility <- renderText({
    paste(facility_id()[1,1])
  })
  
  #get existing OW codes, without the digits
  existing_ow_codes <- reactive(gsub('\\d+', '', result_table()$ow_suffix))
  
  #count how many existing observation wells of the selected type already exist at the SMP
  well_count <- reactive(length(existing_ow_codes()[existing_ow_codes() == select_ow_code()]))
  
  #OW + (Count + 1) for suggested name of the new well 
  ow_suggested_pre <- reactive(paste0(select_ow_code(), (well_count() + 1))) 
  
  # output$blah <- renderText({
  #   paste(asset_combo(), collapse = ",")
  # })
  
  #Suggest new well name
  observe(updateTextInput(session, "ow_suffix", value = ow_suggested_pre()))
  
  #Write to database when button is clicked
  observeEvent(input$add_ow, {
    odbc::dbGetQuery(conn, paste0(
    "INSERT INTO ow_testing (smp_id, ow_suffix, facility_id) 
	      VALUES ('", input$smp_id, "','", input$ow_suffix, "','",  facility_id()[1,1], "')"
    ))
  })
  
}

shinyApp(ui, server)