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

  onStop(function(){
    odbc::dbDisconnect(conn)
    })
# Setup for Sensor Inventory ----------------------------------------------

  #Sensor Model Number options
  hobo_options <- c("U20-001-01", "U20-001-04", "U20L-01", "U20L-04")

# UI ----------------------------------------------------------------------
ui <- navbarPage("Fieldwork DB", theme = shinytheme("cerulean"), id = "inTabset",
                
    tabPanel(title = "Add OW", value = "add_ow",  
      titlePanel("Add Observation Well"),
      useShinyjs(),
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
      selectInput("model_no", "Sensor Model Number", choices = hobo_options), 
      dateInput("date_purchased", "Purchase Date"), 
      actionButton("add_sensor", "Add Sensor")
      ),
    
    mainPanel(
      textOutput("testing")
    )
    
    ), 
  tabPanel("Deploy Sensor", value = "deploy_tab",
    titlePanel("Deploy Sensor"), 
    
    fluidRow(
      column(4, 
        sidebarPanel(width = 12, 
      actionButton("create_well", "Create a New Well"), 
      selectInput("smp_id_deploy", "Select an SMP ID", choices = c("", smp_id), selected = NULL), 
      selectInput("well_name", "Well Name", choices = c("a", "b"))
      )
      ),
     
    column(4,
      sidebarPanel(width = 12, 
      actionButton("create_sensor", "Create a New Sensor"),
      selectInput("sensor_id", paste("Sensor ID"), choices = c("", ""), selected = NULL),
      selectInput("sensor_purpose", "Sensor Purpose", choices = c("", "BARO", "LEVEL"), selected = NULL),
      selectInput("interval", "Measurement Interval (min)", choices = c("", 5, 15), selected = NULL),
      dateInput("deploy_date", "Deployment Date", value = as.Date(NA)),
      dateInput("collect_date", "Collection Date", value = as.Date(NA))
      )
      ),
    column(4, 
      actionButton("deploy_sensor", "Deploy Sensor"), 
      #actionButton("test_sensor", "test"),
      conditionalPanel(width = 12, 
        condition = "input.collect_date",
        checkboxInput("redeploy", "Redeploy Sensor?"),
        h6("Redeploy sensor in the same well on this collection date")
      )
    )
    ),
    column(7, 
      h4("Active Deployments at this SMP"),
      tableOutput("current_deployment")
    ), 
    column(5, 
      h4("Previous Deployments at this SMP"),
      tableOutput("prev_deployment")
    )
    )
    
  )



# Server ------------------------------------------------------------------
server <- function(input, output, session){
  

 # Add Observation Well  ----
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
                           mutate(asset_comp_code = paste(asset_type, ow_code, component_id, sep = " | ")))

  asset_combo <- reactive(asset_comp()$asset_comp_code)
  
  #update component ID box based on the SMP chosen
  observe(updateSelectInput(session, "component_id", choices = c("", asset_combo()), selected = NULL))
  
  #update well name (SMP ID + OW SUFFIX)
  output$text <- renderText({
    if(input$smp_id != "" & input$ow_suffix != "") paste(input$smp_id, input$ow_suffix, sep = "_") else ""
   })
  
  ow_table <- reactive(odbc::dbGetQuery(conn, paste0(
    "SELECT * FROM ow_testing WHERE smp_id = '", input$smp_id, "'")))
  
  #us reactive values to read in table, and see which wells already exist at the SMP
  rv_ow <- reactiveValues()
  rv_ow$ow_table <- reactive(odbc::dbGetQuery(conn, paste0(
    "SELECT * FROM ow_testing WHERE smp_id = '", input$smp_id, "'")))
  rv_ow$existing_ow_codes <- reactive(gsub('\\d+', '', rv_ow$ow_table()$ow_suffix))
  
  #render result table
  output$table <- renderTable(
    rv_ow$ow_table()
  )
  
  select_component_id <- reactive(asset_comp() %>% 
                                     dplyr::filter(asset_comp_code == input$component_id) %>% 
                                     dplyr::select(component_id) %>% 
                                     dplyr::pull())
   
  select_ow_code <- reactive(asset_comp() %>% 
                                dplyr::filter(asset_comp_code == input$component_id) %>% 
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
  #output$facility <- observe(if(input$component_id == "") renderText({paste("x")}) else renderText({paste(facility_id()[1,1])}) )
  output$facility <- renderText({
    paste(facility_id())
  })
  
  
  #count how many existing observation wells of the selected type already exist at the SMP
  well_count <- reactive(length(rv_ow$existing_ow_codes()[rv_ow$existing_ow_codes() == select_ow_code()])+1)
  
  #OW + (Count + 1) for suggested name of the new well 
  ow_suggested_pre <- reactive(paste0(select_ow_code(), (well_count()))) 
  
  observe(if(input$component_id=="") updateTextInput(session, "ow_suffix", value = NA) else 
                  updateTextInput(session, "ow_suffix", value = ow_suggested_pre()))
  
  observe({toggleState(id = "add_ow", condition = nchar(input$smp_id) > 0 & nchar(input$component_id) > 0 & nchar(input$ow_suffix) >0)})
  observe({toggleState(id = "add_ow_deploy", nchar(input$smp_id) > 0)})
  
  #Write to database when button is clicked
  observeEvent(input$add_ow, {
    odbc::dbGetQuery(conn, paste0(
    "INSERT INTO ow_testing (smp_id, ow_suffix, facility_id) 
	      VALUES ('", input$smp_id, "','", input$ow_suffix, "','",  facility_id(), "')"
    ))
    rv_ow$ow_table <- reactive(odbc::dbGetQuery(conn, paste0(
      "SELECT * FROM ow_testing WHERE smp_id = '", input$smp_id, "'")))
    updateTextInput(session, "ow_suffix", value = NA)
    updateSelectInput(session, "component_id", selected = NA)
  })
  
  observeEvent(input$add_ow_deploy, {
    updateSelectInput(session, "smp_id_deploy", selected = "1-1-1")
    updateSelectInput(session, "smp_id_deploy", selected = input$smp_id)
    updateTabsetPanel(session, "inTabset", selected = "deploy_tab")
    #updateSelectInput(session, "smp_id_deploy", selected = input$smp_id)
  })
  
  #existing_ow_codes <- eventReactive(input$add_ow, odbc::dbGetQuery(conn, paste0("SELECT ow_suffix FROM ow_testing WHERE smp_id = '", input$smp_id, "'")))
 # Sensor Inventory ----

  #Sensor Serial Number List
  rv_sensor <- reactiveValues()
  rv_sensor$hobo_list <- odbc::dbGetQuery(conn, paste0("select sensor_serial, sensor_model, date_purchased from inventory_sensors_testing"))

  observe(updateSelectInput(session, inputId = "sensor_id", choices = c("", rv_sensor$hobo_list$sensor_serial), selected = NULL))
  
  #autofill model number and purchase data if entered serial number already exists in database
  reactive(if(input$serial_no %in% rv_sensor$hobo_list$sensor_serial){
    model_no <- dplyr::filter(rv_sensor$hobo_list, sensor_serial = serial_no) %>% dplyr::select(sensor_model) %>% dplyr::pull()
    date_purchased <- dplyr::filter(rv_sensor$hobo_list, sensor_serial = serial_no) %>% dplyr::select(date_purchased) %>% dplyr::pull()
    output$testing <-renderText({
                      model_no
                      })
  })
  
  model_no_select <- reactive(if(input$serial_no %in% rv_sensor$hobo_list$sensor_serial) dplyr::filter(rv_sensor$hobo_list, sensor_serial == input$serial_no) %>% dplyr::select(sensor_model) %>% dplyr::pull() else NULL )
  
  observe(updateSelectInput(session, "model_no", selected = model_no_select()))
  
  date_purchased_select <- reactive(if(input$serial_no %in% rv_sensor$hobo_list$sensor_serial) dplyr::filter(rv_sensor$hobo_list, sensor_serial == input$serial_no) %>% dplyr::select(date_purchased) %>% dplyr::pull() else NULL )
  
  observe(updateSelectInput(session, "model_no", selected = model_no_select()))
  observe(updateDateInput(session, "date_purchased", value = date_purchased_select()))
  
  observe({toggleState(id = "add_sensor", condition = nchar(input$serial_no) > 0 & nchar(input$model_no) > 0 & nchar(input$date_purchased) >0)})
  #Write to database when button is clicked
  observeEvent(input$add_sensor, {
    if(!(input$serial_no %in% rv_sensor$hobo_list$sensor_serial)){
      odbc::dbGetQuery(conn, paste0(
      "INSERT INTO inventory_sensors_testing (sensor_serial, sensor_model, date_purchased) 
	      VALUES ('", input$serial_no, "','", input$model_no, "','",  input$date_purchased, "')"))
      output$testing <- renderText({
        isolate(paste("Sensor", input$serial_no, "added."))
      })
    }else{
      odbc::dbGetQuery(conn, paste0("UPDATE inventory_sensors_testing SET sensor_model = '", input$model_no, 
                                    "', date_purchased = '", input$date_purchased, "' WHERE sensor_serial = '", input$serial_no, "'"))
      output$testing <- renderText({
        isolate(paste("Sensor", input$serial_no, "edited."))
      })
    }
    rv_sensor$hobo_list <- odbc::dbGetQuery(conn, paste0("select sensor_serial, sensor_model, date_purchased from inventory_sensors_testing"))
  })
  

  
 # Deploy Sensor ----
  
  rv_deploy <- reactiveValues()
  rv_deploy$ow_suffixes <- reactive(odbc::dbGetQuery(conn, paste0(
    "select ow_suffix from ow_testing where smp_id = '", input$smp_id_deploy, "'")) %>% dplyr::pull())
  
  observe(updateSelectInput(session, "well_name", choices = rv_deploy$ow_suffixes()))
  observe(updateDateInput(session, "collect_date", min = input$deploy_date))
  
  observeEvent(input$create_well, {
    updateTabsetPanel(session, "inTabset", selected = "add_ow")
    updateSelectInput(session, "smp_id" , selected = input$smp_id_deploy)
  })
  
  observeEvent(input$create_sensor, {
    updateTabsetPanel(session, "inTabset", selected = "add_sensor")
  })
  
  observeEvent(input$deploy_tab, {
    updateSelectInput(session, "well_name", choices = ow_suffixes())
  })
  
  rv_deploy$purpose <- reactive(if(input$sensor_purpose == "LEVEL") 1 else if(input$sensor_purpose == "BARO") 2 else NA)
  rv_deploy$inventory_sensors_uid <- reactive(odbc::dbGetQuery(conn, paste0(
    "SELECT inventory_sensors_uid FROM inventory_sensors_testing WHERE sensor_serial = '", input$sensor_id, "'"
  )))
  rv_deploy$collect_date <- reactive(if(length(input$collect_date) == 0) "NULL" else paste0("'", input$collect_date, "'"))
  
  active_table_query <- reactive(paste0(
    "SELECT te.deployment_dtime_est, v.smp_id, v.ow_suffix, te.sensor_purpose, te.interval_min FROM deployment_testing te
      LEFT JOIN ow_validity v ON te.ow_uid = v.ow_uid
      WHERE v.smp_id = '", input$smp_id_deploy, "' AND te.collection_dtime_est IS NULL"))
  
  rv_deploy$active_table <- reactive(odbc::dbGetQuery(conn, active_table_query()) %>% 
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
  
  old_table_query <- reactive(paste0(
    "SELECT te.deployment_dtime_est, v.smp_id, v.ow_suffix, te.sensor_purpose, te.interval_min, te.collection_dtime_est
     FROM deployment_testing te
      LEFT JOIN ow_validity v ON te.ow_uid = v.ow_uid
      WHERE v.smp_id = '", input$smp_id_deploy, "' AND te.collection_dtime_est IS NOT NULL"))
  
  rv_deploy$old_table <- reactive(odbc::dbGetQuery(conn, old_table_query()) %>% 
      mutate_at(c("deployment_dtime_est", "collection_dtime_est"), as.character) %>% 
      dplyr::select(deployment_dtime_est, smp_id, ow_suffix, sensor_purpose, interval_min, collection_dtime_est) %>% 
      dplyr::rename("Deploy Date" = "deployment_dtime_est", "SMP ID" = "smp_id", "OW" = "ow_suffix", 
                    "Purpose" = "sensor_purpose", "Interval" = "interval_min", "Collection Date" = "collection_dtime_est"))
 
  output$prev_deployment <- renderTable({
    rv_deploy$old_table()
  })
  
  rv_deploy$redeploy <- reactive(if(length(input$collect_date > 0) & input$redeploy == TRUE) TRUE else FALSE)
  
  #write to database on click
  #1/7/2020 does not yet do anything different for editing,
  observeEvent(input$deploy_sensor, { 
    odbc::dbGetQuery(conn,
     paste0("INSERT INTO deployment_testing (deployment_dtime_est, ow_uid,
     inventory_sensors_uid, sensor_purpose, interval_min, collection_dtime_est)
        VALUES ('", input$deploy_date, "', get_ow_uid_testing('",input$smp_id_deploy,"', '", input$well_name, "'), '",
            rv_deploy$inventory_sensors_uid(), "','", rv_deploy$purpose(), "','",input$interval, "',", rv_deploy$collect_date(),")"))
    if(rv_deploy$redeploy() == TRUE){
      dbGetQuery(conn, paste0("INSERT INTO deployment_testing (deployment_dtime_est, ow_uid,
     inventory_sensors_uid, sensor_purpose, interval_min, collection_dtime_est)
        VALUES (", rv_deploy$collect_date(), ", get_ow_uid_testing('",input$smp_id_deploy,"', '", input$well_name, "'), '",
                              rv_deploy$inventory_sensors_uid(), "','", rv_deploy$purpose(), "','",input$interval, "', NULL)"))
      print("input$redeploy == TRUE")
    }
    rv_deploy$active_table  <- reactive(odbc::dbGetQuery(conn, active_table_query()) %>% 
              mutate(`80% Full Date`= case_when(interval_min == 5 ~ round_date(deployment_dtime_est + days(60), "day"),
                                                interval_min == 15 ~ round_date(deployment_dtime_est + days(180), "day")), 
                     `100% Full Date` = case_when(interval_min == 5 ~ round_date(deployment_dtime_est + days(75), "day"),
                                                  interval_min == 15 ~ round_date(deployment_dtime_est + days(225), "day"))) %>% 
              mutate_at(c("deployment_dtime_est", "80% Full Date", "100% Full Date"), as.character) %>% 
              dplyr::select(deployment_dtime_est, smp_id, ow_suffix, sensor_purpose, interval_min, `80% Full Date`, `100% Full Date`) %>% 
              dplyr::rename("Deploy Date" = "deployment_dtime_est", "SMP ID" = "smp_id", 
                            "OW" = "ow_suffix", "Purpose" = "sensor_purpose", "Interval" = "interval_min"))
    rv_deploy$old_table <- reactive(odbc::dbGetQuery(conn, old_table_query()) %>% 
        mutate_at(c("deployment_dtime_est", "collection_dtime_est"), as.character) %>% 
        dplyr::select(deployment_dtime_est, smp_id, ow_suffix, sensor_purpose, interval_min, collection_dtime_est) %>% 
        dplyr::rename("Deploy Date" = "deployment_dtime_est", "SMP ID" = "smp_id", "OW" = "ow_suffix", 
                      "Purpose" = "sensor_purpose", "Interval" = "interval_min", "Collection Date" = "collection_dtime_est"))
    })
    
  observe({toggleState(id = "deploy_sensor", condition = nchar(input$smp_id_deploy) > 0 &
                         nchar(input$well_name) > 0 & nchar(input$sensor_id) > 0 & nchar(input$sensor_purpose) > 0 &
                        nchar(input$interval) > 0 & length(input$deploy_date) > 0 )})
            
   
}

shinyApp(ui, server)