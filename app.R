library(shiny)
#library(pool)
library(odbc)
library(tidyverse)
library(shinythemes)
library(lubridate)
library(shinyjs)
#library(DT)
#library(data.table)


# Setup for adding observation well ---------------------------------------

  #set database connection
  conn <- odbc::dbConnect(odbc(), dsn = "mars_testing", uid = Sys.getenv("shiny_uid"), pwd = Sys.getenv("shiny_pwd"))
  #conn <- odbc::dbConnect(odbc(), dsn = "marsDB", uid = "shiny_user", pwd = "bat-hog-verse-fun-crypt-noise")
  #query all SMP IDs
  smp_id <- odbc::dbGetQuery(conn, paste0("select distinct smp_id from smpid_facilityid_componentid")) %>% dplyr::arrange(smp_id) %>% dplyr::pull()
  
  # create dataframe with new wells, names and codes
  asset_type <- c("Shallow Well", "Groundwater Well", "Groundwater Control Well", "Forebay")
  ow_code <- c("SW", "GW", "CW", "FB")
  new_wells <- data.frame(asset_type, ow_code)
  new_wells$asset_type <- new_wells$asset_type %>% as.character()
  new_wells$ow_code <- new_wells$ow_code %>% as.character()
  rm(asset_type, ow_code)

  #disconnect from db on stop 
  #db connection may be replaced with POOL soon 
  onStop(function(){
    odbc::dbDisconnect(conn)
    })
# Setup for Sensor Inventory ----------------------------------------------

  #Sensor Model Number options
  hobo_options <- c("", "U20-001-01", "U20-001-04", "U20L-01", "U20L-04")

# UI ----------------------------------------------------------------------
ui <- navbarPage("Fieldwork DB", theme = shinytheme("cerulean"), id = "inTabset", #create a navigation bar at top of page, called inTabset
    
    #create a tabPanel for each tab
    tabPanel(title = "Add OW", value = "add_ow",  
      titlePanel("Add Observation Well"),
      useShinyjs(), #this function needs to be called anywhere in the UI to use any other Shinyjs() functions
      sidebarPanel(
    
        # width = 6, 
        selectInput("smp_id", "Select an SMP ID", choices = c("", smp_id), selected = NULL),
        selectInput("component_id", "Select a Component ID", choices = c("", "")),
        textInput("ow_suffix", "OW Suffix"), 
        actionButton("add_ow", "Add Observation Well"), 
        actionButton("add_ow_deploy", "Deploy Sensor at this SMP")
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
    ), 
  tabPanel(title = "Add Sensor", value = "add_sensor",
    titlePanel("Add Sensor to Inventory"), 
    
    sidebarPanel(
      textInput("serial_no", "Sensor Serial Number"), 
      selectInput("model_no", "Sensor Model Number", choices = hobo_options, selected = NULL), 
      dateInput("date_purchased", "Purchase Date", value = as.Date(NA)), 
      actionButton("add_sensor", "Add Sensor")
      ),
    
    mainPanel(
      textOutput("testing")
    )
    
    ), 
  tabPanel("Deploy Sensor", value = "deploy_tab",
    titlePanel("Deploy Sensor"), 
    
    fluidRow(
      column(width = 4, 
        sidebarPanel(width = 12, 
      actionButton("create_well", "Create a New Well"), 
      selectInput("smp_id_deploy", "Select an SMP ID", choices = c("", smp_id), selected = NULL), 
      selectInput("well_name", "Well Name", choices = c("a", "b"))
      )
      ),
     
    column(width = 4,
      sidebarPanel(width = 12, 
      actionButton("create_sensor", "Create a New Sensor"),
      selectInput("sensor_id", paste("Sensor ID"), choices = c("", ""), selected = NULL),
      selectInput("sensor_purpose", "Sensor Purpose", choices = c("", "BARO", "LEVEL"), selected = NULL),
      selectInput("interval", "Measurement Interval (min)", choices = c("", 5, 15), selected = NULL),
      dateInput("deploy_date", "Deployment Date", value = as.Date(NA)),
      dateInput("collect_date", "Collection Date", value = as.Date(NA))
      )
      ),
    column(width = 4, 
      actionButton("deploy_sensor", "Deploy Sensor"), 
      #actionButton("test_sensor", "test"),
      conditionalPanel(width = 12, 
        condition = "input.collect_date",
        checkboxInput("redeploy", "Redeploy Sensor?"),
        h6("Redeploy sensor in the same well on this collection date")
      )
    )
    ),
    column(width = 7, 
      h4("Active Deployments at this SMP"),
      tableOutput("current_deployment")
    ), 
    column(width = 5, 
      h4("Previous Deployments at this SMP"),
      tableOutput("prev_deployment")
    )
    )
    
  )



# Server ------------------------------------------------------------------
server <- function(input, output, session){
  

 # Add Observation Well  ----
  #set component IDs based on SMP ID
  component_and_asset <- reactive(odbc::dbGetQuery(conn, paste0(
    "select distinct component_id, asset_type from smpid_facilityid_componentid where smp_id = '", input$smp_id, "' 
        AND component_id IS NOT NULL")))
  
  #set ow code based on asset comp
  #set "asset comp code" to be shown in component ID combo box. asset comp code is a concat of asset type, ow_code, and component id 
  asset_comp <- reactive(component_and_asset() %>% 
                           mutate(ow_code = case_when(asset_type == "Inlet" ~ "GI",
                                                      asset_type == "Fitting" ~ "OW",
                                                      asset_type == "Cleanout" ~ "CO",
                                                      asset_type == "Observation Well" ~ "OW",
                                                      asset_type == "Groundwater Control Well" ~ "CW",
                                                      asset_type == "Groundwater Welll" ~ "GW",
                                                      asset_type == "Shallow Well" ~ "SW", 
                                                      asset_type == "Control Structure" ~ "CS")) %>% 
                           dplyr::bind_rows(new_wells) %>% 
                           mutate(asset_comp_code = paste(asset_type, ow_code, component_id, sep = " | ")))

  asset_combo <- reactive(asset_comp()$asset_comp_code)
  
  #update component ID box (to include the asset combo) based on the SMP chosen
  observe(updateSelectInput(session, "component_id", choices = c("", asset_combo()), selected = NULL))
  
  #update well name (SMP ID + OW SUFFIX)
  output$text <- renderText({
    if(input$smp_id != "" & input$ow_suffix != "") paste(input$smp_id, input$ow_suffix, sep = "_") else ""
   })
  
  #use reactive values to read in table, and see which wells already exist at the SMP
  rv_ow <- reactiveValues()
  ow_table_query <- reactive(paste0("SELECT * FROM ow_testing WHERE smp_id = '", input$smp_id, "'"))
  rv_ow$ow_table <- reactive(odbc::dbGetQuery(conn, ow_table_query()))
  #get the existing ow_codes, but just the two letters. these will be counted and used to suggest and OW Suffix
  rv_ow$existing_ow_codes <- reactive(gsub('\\d+', '', rv_ow$ow_table()$ow_suffix))
  
  #render ow table
  output$table <- renderTable(
    rv_ow$ow_table()
  )
  
  #get the row/case that has the selected asset_comp_code
  select_combo_row <- reactive(asset_comp() %>% 
                                 dplyr::filter(asset_comp_code == input$component_id))
  
  #get component id from the chosen asset_comp_code, then use to get facility id
  select_component_id <- reactive(select_combo_row() %>% 
                                     dplyr::select(component_id) %>% 
                                     dplyr::pull())
   
  #get ow_code/suffix from chosen asset_comp_code, then use to get facility ID and suggest new ow suffix
  select_ow_code <- reactive(select_combo_row() %>% 
                                dplyr::select(ow_code) %>% 
                                dplyr::pull())
  
  #get facility ID. Either use SMP footprint (for a new well) or the facility ID of the existing component
  facility_id <- reactive(if(input$component_id != "" & length(select_ow_code() > 0 )){
    if(select_ow_code() %in% new_wells$ow_code) odbc::dbGetQuery(conn, paste0(
       "SELECT facility_id FROM smpid_facilityid_componentid WHERE component_id IS NULL AND smp_id = '", input$smp_id, "'"))[1,1] else 
      odbc::dbGetQuery(conn, paste0(
      "SELECT facility_id from smpid_facilityid_componentid WHERE component_id = '", select_component_id(), "'"))[1,1]
  }else{
    ""
  }
  )
  
  #show facility ID
  output$facility <- renderText({
    paste(facility_id())
  })
  
  #count how many existing observation wells of the selected type already exist at the SMP
  well_count <- reactive(length(rv_ow$existing_ow_codes()[rv_ow$existing_ow_codes() == select_ow_code()]))
  
  #OW + (Count + 1) for suggested name of the new well 
  ow_suggested_pre <- reactive(paste0(select_ow_code(), (well_count() + 1))) 
  
  #only show suggested suffix if there is a selected component id
  observe(if(input$component_id=="") updateTextInput(session, "ow_suffix", value = NA) else 
                  updateTextInput(session, "ow_suffix", value = ow_suggested_pre()))
  
  #toggle state (enable/disable) buttons based on whether smp id, component id, and ow suffix are selected (this is shinyjs)
  observe({toggleState(id = "add_ow", condition = nchar(input$smp_id) > 0 & nchar(input$component_id) > 0 & nchar(input$ow_suffix) >0)})
  observe({toggleState(id = "add_ow_deploy", nchar(input$smp_id) > 0)})
  
  #Write to database when button is clicked
  observeEvent(input$add_ow, {
    odbc::dbGetQuery(conn, paste0(
    "INSERT INTO ow_testing (smp_id, ow_suffix, facility_id) 
	      VALUES ('", input$smp_id, "','", input$ow_suffix, "','",  facility_id(), "')"
    ))
    #update ow_table with new well
    rv_ow$ow_table <- reactive(odbc::dbGetQuery(conn, ow_table_query()))
    #reset ow_suffix to blank
    updateTextInput(session, "ow_suffix", value = NA)
    #reset component id to blank
    updateSelectInput(session, "component_id", selected = NA)
  })
  
  #upon click of "Deploy Sensor at this SMP" update the smp_id in the Deploy Sensor tab, and switch tabs
  #it needs to switch to NULL and then back to the input$smp_id to make sure the change is registered
  observeEvent(input$add_ow_deploy, {
    updateSelectInput(session, "smp_id_deploy", selected = "")
    updateSelectInput(session, "smp_id_deploy", selected = input$smp_id)
    updateTabsetPanel(session, "inTabset", selected = "deploy_tab")
  })
  
 # Sensor Inventory ----

  #start reactiveValues for this section
  rv_sensor <- reactiveValues()
  
  #Sensor Serial Number List
  hobo_list_query <-  "select sensor_serial, sensor_model, date_purchased from inventory_sensors_testing"
  rv_sensor$hobo_list <- odbc::dbGetQuery(conn, hobo_list_query)

  #update sensor id choices based on the hobo list
  observe(updateSelectInput(session, inputId = "sensor_id", choices = c("", rv_sensor$hobo_list$sensor_serial), selected = NULL))
  
  #if input serial number is already in the list, then suggest the existing model number. if it isn't already there, show NULL
  model_no_select <- reactive(if(input$serial_no %in% rv_sensor$hobo_list$sensor_serial) dplyr::filter(rv_sensor$hobo_list, sensor_serial == input$serial_no) %>% dplyr::select(sensor_model) %>% dplyr::pull() else "")
  
  observe(updateSelectInput(session, "model_no", selected = model_no_select()))
  
  #if input serial number is already in the list, then suggest the date_purchased. if it isn't already there, show NULL
  date_purchased_select <- reactive(if(input$serial_no %in% rv_sensor$hobo_list$sensor_serial) dplyr::filter(rv_sensor$hobo_list, sensor_serial == input$serial_no) %>% dplyr::select(date_purchased) %>% dplyr::pull() else as.Date(NA))
  
  observe(updateDateInput(session, "date_purchased", value = date_purchased_select()))
  
  #enable/disable the "add sensor button" if all fields are not empty
  observe({toggleState(id = "add_sensor", condition = nchar(input$serial_no) > 0 & nchar(input$model_no) > 0 & nchar(input$date_purchased) >0)})
  #Write to database when button is clicked
  observeEvent(input$add_sensor, { #write new sensor info to db
    if(!(input$serial_no %in% rv_sensor$hobo_list$sensor_serial)){
      odbc::dbGetQuery(conn, paste0(
      "INSERT INTO inventory_sensors_testing (sensor_serial, sensor_model, date_purchased) 
	      VALUES ('", input$serial_no, "','", input$model_no, "','",  input$date_purchased, "')"))
      output$testing <- renderText({
        isolate(paste("Sensor", input$serial_no, "added."))
      })
    }else{ #edit sensor info
      odbc::dbGetQuery(conn, paste0("UPDATE inventory_sensors_testing SET sensor_model = '", input$model_no, 
                                    "', date_purchased = '", input$date_purchased, "' WHERE sensor_serial = '", input$serial_no, "'"))
      output$testing <- renderText({
        isolate(paste("Sensor", input$serial_no, "edited."))
      })
    }
    #update sensor list following addition
    rv_sensor$hobo_list <- odbc::dbGetQuery(conn, hobo_list_query)
  })
  
 # Deploy Sensor ----
  
  #start reactiveValues for this section/tab
  rv_deploy <- reactiveValues()
  ## well panel
    #query ow_suffixes based on smp_id
    rv_deploy$ow_suffixes <- reactive(odbc::dbGetQuery(conn, paste0(
      "select ow_suffix from ow_testing where smp_id = '", input$smp_id_deploy, "'")) %>% dplyr::pull())
    
    observe(updateSelectInput(session, "well_name", choices = rv_deploy$ow_suffixes()))
    observe(updateDateInput(session, "collect_date", min = input$deploy_date))
    
    observeEvent(input$create_well, {
      updateTabsetPanel(session, "inTabset", selected = "add_ow")
      updateSelectInput(session, "smp_id" , selected = input$smp_id_deploy)
    })
  
  ## sensor panel
    observeEvent(input$create_sensor, {
      updateTabsetPanel(session, "inTabset", selected = "add_sensor")
    })
  
    rv_deploy$purpose <- reactive(if(input$sensor_purpose == "LEVEL") 1 else if(input$sensor_purpose == "BARO") 2 else NA)
    rv_deploy$inventory_sensors_uid <- reactive(odbc::dbGetQuery(conn, paste0(
      "SELECT inventory_sensors_uid FROM inventory_sensors_testing WHERE sensor_serial = '", input$sensor_id, "'"
    )))
    rv_deploy$collect_date <- reactive(if(length(input$collect_date) == 0) "NULL" else paste0("'", input$collect_date, "'"))
  
  ## show tables
    #query for active deployments
    active_table_query <- reactive(paste0(
      "SELECT te.deployment_dtime_est, v.smp_id, v.ow_suffix, te.sensor_purpose, te.interval_min FROM deployment_testing te
        LEFT JOIN ow_testing v ON te.ow_uid = v.ow_uid
        WHERE v.smp_id = '", input$smp_id_deploy, "' AND te.collection_dtime_est IS NULL"))
    
    #create table as a reactive value based on query, then add 80% and 100% full dates and relabel table
    rv_deploy$active_table_db <- reactive(odbc::dbGetQuery(conn, active_table_query()))
    rv_deploy$active_table <- reactive(rv_deploy$active_table_db() %>% 
                   mutate(`80% Full Date`= case_when(interval_min == 5 ~ round_date(deployment_dtime_est + days(60), "day"),
                                                     interval_min == 15 ~ round_date(deployment_dtime_est + days(180), "day")), 
                          `100% Full Date` = case_when(interval_min == 5 ~ round_date(deployment_dtime_est + days(75), "day"),
                                                       interval_min == 15 ~ round_date(deployment_dtime_est + days(225), "day"))) %>% 
                   mutate_at(c("deployment_dtime_est", "80% Full Date", "100% Full Date"), as.character) %>% 
                   dplyr::select(deployment_dtime_est, smp_id, ow_suffix, sensor_purpose, interval_min, `80% Full Date`, `100% Full Date`) %>% 
                   dplyr::rename("Deploy Date" = "deployment_dtime_est", "SMP ID" = "smp_id", 
                                 "OW" = "ow_suffix", "Purpose" = "sensor_purpose", "Interval" = "interval_min"))
    
    output$current_deployment <- renderTable({
      rv_deploy$active_table()
    })
    
    #query for previous deployments
    old_table_query <- reactive(paste0(
      "SELECT te.deployment_dtime_est, v.smp_id, v.ow_suffix, te.sensor_purpose, te.interval_min, te.collection_dtime_est
       FROM deployment_testing te
        LEFT JOIN ow_testing v ON te.ow_uid = v.ow_uid
        WHERE v.smp_id = '", input$smp_id_deploy, "' AND te.collection_dtime_est IS NOT NULL"))
    
    #create table as a reactive value based on query, then relabel table
    rv_deploy$old_table_db <- reactive(odbc::dbGetQuery(conn, old_table_query()))
    rv_deploy$old_table <- reactive(rv_deploy$old_table_db() %>% 
        mutate_at(c("deployment_dtime_est", "collection_dtime_est"), as.character) %>% 
        dplyr::select(deployment_dtime_est, smp_id, ow_suffix, sensor_purpose, interval_min, collection_dtime_est) %>% 
        dplyr::rename("Deploy Date" = "deployment_dtime_est", "SMP ID" = "smp_id", "OW" = "ow_suffix", 
                      "Purpose" = "sensor_purpose", "Interval" = "interval_min", "Collection Date" = "collection_dtime_est"))
   
    output$prev_deployment <- renderTable({
      rv_deploy$old_table()
    })
  
  #control for redeploy checkbox. had to add, because after checking it, then removing collect_date, it goes away, but is still TRUE
    rv_deploy$redeploy <- reactive(if(length(input$collect_date > 0) & input$redeploy == TRUE) TRUE else FALSE)
  
  #write to database on click
  #1/7/2020 does not yet do anything different for editing,
  observeEvent(input$deploy_sensor, {
    #write deployment
    odbc::dbGetQuery(conn,
     paste0("INSERT INTO deployment_testing (deployment_dtime_est, ow_uid,
     inventory_sensors_uid, sensor_purpose, interval_min, collection_dtime_est)
        VALUES ('", input$deploy_date, "', get_ow_uid_testing('",input$smp_id_deploy,"', '", input$well_name, "'), '",
            rv_deploy$inventory_sensors_uid(), "','", rv_deploy$purpose(), "','",input$interval, "',", rv_deploy$collect_date(),")"))
    #write redeployment
    if(rv_deploy$redeploy() == TRUE){
      dbGetQuery(conn, paste0("INSERT INTO deployment_testing (deployment_dtime_est, ow_uid,
     inventory_sensors_uid, sensor_purpose, interval_min, collection_dtime_est)
        VALUES (", rv_deploy$collect_date(), ", get_ow_uid_testing('",input$smp_id_deploy,"', '", input$well_name, "'), '",
                              rv_deploy$inventory_sensors_uid(), "','", rv_deploy$purpose(), "','",input$interval, "', NULL)"))
    }
    #query active table
    rv_deploy$active_table_db  <- reactive(odbc::dbGetQuery(conn, active_table_query())) 
    #query prev table
    rv_deploy$old_table_db <- reactive(odbc::dbGetQuery(conn, old_table_query())) 
    })
    
  #enable/disable deploy sensor button based on whether all inputs (except collect date) are not empty
  observe({toggleState(id = "deploy_sensor", condition = nchar(input$smp_id_deploy) > 0 &
                         nchar(input$well_name) > 0 & nchar(input$sensor_id) > 0 & nchar(input$sensor_purpose) > 0 &
                        nchar(input$interval) > 0 & length(input$deploy_date) > 0 )})
            
   
}

shinyApp(ui, server)