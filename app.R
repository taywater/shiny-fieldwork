library(shiny)
#library(pool)
library(odbc)
library(tidyverse)
library(shinythemes)
library(lubridate)
library(shinyjs)
library(DT)

options(DT.options = list(pageLength = 15))

# Setup for adding observation well ---------------------------------------

  #set database connection
  conn <- odbc::dbConnect(odbc(), dsn = "mars_testing", uid = Sys.getenv("shiny_uid"), pwd = Sys.getenv("shiny_pwd"))
  #conn <- odbc::dbConnect(odbc(), dsn = "marsDB", uid = "shiny_user", pwd = "bat-hog-verse-fun-crypt-noise")
  #query all SMP IDs
  smp_id <- odbc::dbGetQuery(conn, paste0("select distinct smp_id from smpid_facilityid_componentid")) %>% 
    dplyr::arrange(smp_id) %>% 
    dplyr::pull()
  
  #read in ow prefixes and names
  ow_prefixes_db <- dbGetQuery(conn, "select * from ow_prefixes") %>% rename("asset_type" = "ow_name")
  new_and_ex_wells <- ow_prefixes_db %>% dplyr::select(ow_prefix, asset_type) 
  new_wells <- ow_prefixes_db %>% dplyr::filter(componentless == 1) %>% dplyr::select(ow_prefix,asset_type)
  
  #disconnect from db on stop 
  #db connection may be replaced with POOL soon 
  onStop(function(){
    odbc::dbDisconnect(conn)
    })
# Setup for Sensor Inventory ----------------------------------------------

  #Sensor Model Number options
  hobo_options <- c("", "U20-001-01", "U20-001-04", "U20L-01", "U20L-04")

# Setup for Sensor Deployment ---------------------------------------------

  #Deployment purpose lookup table
  deployment_lookup <- dbGetQuery(conn, "select * from deployment_lookup_table")
  
# UI ----------------------------------------------------------------------
ui <- navbarPage("Fieldwork DB", theme = shinytheme("cerulean"), id = "inTabset", #create a navigation bar at top of page, called inTabset
    
    #create a tabPanel for each tab
    tabPanel("Collection Calendar", value = "calendar_tab", 
             #tags$style(HTML('table.dataTable tr.selected td, table.dataTable td.selected {background-color: pink !important;}')),
             
             titlePanel("Collection Calendar"), 
             sidebarPanel(
               selectInput("property_type", "Property Type", choices = c("All" = .5, "Public" = 1, "Private" = 0)),
               selectInput("interval_filter", "Interval", choices = c("All" = 10, "5" = 5, "15" = 15)),
               selectInput("capacity_used", "Capacity Used", choices = c("All", "Less than 80%", "80% or more")), 
               #radioButtons("sort_calendar", "Sort Collection Calendar By:", choices = c("Collection Date", "SMP ID"))
             ), 
             mainPanel(
               DTOutput("collection")
             )
    ),
    tabPanel(title = "Add OW", value = "add_ow",  
      titlePanel("Add Observation Well"),
      useShinyjs(), #this function needs to be called anywhere in the UI to use any other Shinyjs() functions
      sidebarPanel(
        selectInput("smp_id", "Select an SMP ID", choices = c("", smp_id), selected = NULL),
        selectInput("component_id", "Select a Component ID", choices = c("", "")),
        textInput("ow_suffix", "OW Suffix"), 
        disabled(textInput("facility_id", "Facility ID")),
        actionButton("add_ow", "Add New"), 
        actionButton("add_ow_deploy", "Deploy Sensor at this SMP"),
        actionButton("clear_ow", "Clear All Fields")
        ),
      mainPanel(
        h4("List of Observation Wells at this SMP"), 
        DTOutput("ow_table"), 
        )
      ),
    
  tabPanel(title = "Add Sensor", value = "add_sensor",
    titlePanel("Add Sensor to Inventory"), 
    
    sidebarPanel(
      textInput("serial_no", "Sensor Serial Number"), 
      selectInput("model_no", "Sensor Model Number", choices = hobo_options, selected = NULL), 
      dateInput("date_purchased", "Purchase Date", value = as.Date(NA)), 
      actionButton("add_sensor", "Add Sensor"), 
      actionButton("add_sensor_deploy", "Deploy this Sensor")
      ),
    
    mainPanel(
      DTOutput("sensor_table"),
      textOutput("testing")
    )
    
    ), 
  tabPanel("Deploy Sensor", value = "deploy_tab",
    titlePanel("Deploy Sensor"), 
    
    fluidRow(
      column(width = 4, 
        sidebarPanel(width = 12, 
          selectInput("smp_id_deploy", "Select an SMP ID", choices = c("", smp_id), selected = NULL), 
          selectInput("well_name", "Well Name", choices = ""), 
          selectInput("sensor_id", paste("Sensor ID"), choices = c("", ""), selected = NULL),
          selectInput("sensor_purpose", "Sensor Purpose", choices = c("", "BARO", "LEVEL"), selected = NULL),
          selectInput("interval", "Measurement Interval (min)", choices = c("", 5, 15), selected = NULL),
          dateInput("deploy_date", "Deployment Date", value = as.Date(NA)),
          dateInput("collect_date", "Collection Date", value = as.Date(NA)), 
          actionButton("deploy_sensor", "Deploy Sensor"), 
          conditionalPanel(width = 12, 
            condition = "input.collect_date",
            checkboxInput("redeploy", "Redeploy Sensor?"),
            h6("Redeploy sensor in the same well on this collection date")
          ),
          actionButton("clear_deploy_fields", "Clear all Fields") 
      )
      ),
      column(width = 8,
        h4("Active Deployments at this SMP"),
        DTOutput("current_deployment"), 
        h4("Previous Deployments at this SMP"),
        DTOutput("prev_deployment")
      )
    ),


    )
           
    
  )

# Server ------------------------------------------------------------------
server <- function(input, output, session){
  
 # Add Observation Well  ----
  #set component IDs based on SMP ID
  component_and_asset_query <- reactive(paste0(
    "select distinct component_id, asset_type from smpid_facilityid_componentid where smp_id = '", input$smp_id, "' 
        AND component_id IS NOT NULL"))
  component_and_asset <- reactive(odbc::dbGetQuery(conn, component_and_asset_query()))
  
  #set ow code based on asset comp
  #set "asset comp code" to be shown in component ID combo box. asset comp code is a concat of asset type, ow_prefix, and component id 
  asset_comp <- reactive(component_and_asset() %>% 
                           dplyr::left_join(new_and_ex_wells, by = "asset_type") %>% 
                           dplyr::bind_rows(new_wells) %>% 
                           mutate(asset_comp_code = paste(component_id, ow_prefix, asset_type, sep = " | ")))

  asset_combo <- reactive(asset_comp()$asset_comp_code)
  
  #update component ID box (to include the asset combo) based on the SMP chosen
  observe(updateSelectInput(session, "component_id", choices = c("", asset_combo()), selected = NULL))
  
  #use reactive values to read in table, and see which wells already exist at the SMP
  rv_ow <- reactiveValues()
  ow_table_query <- reactive(paste0("SELECT * FROM ow_testing WHERE smp_id = '", input$smp_id, "'"))
  rv_ow$ow_table <- reactive(odbc::dbGetQuery(conn, ow_table_query()))
  #get the existing ow_prefixes. these will be counted and used to suggest and OW Suffix
  rv_ow$existing_ow_prefixes <- reactive(gsub('\\d+', '', rv_ow$ow_table()$ow_suffix))
  
  #render ow table
  #allow for only one selection; rename columns
  #'t' means only show the table, no search bar, etc
  output$ow_table <- renderDT(
    rv_ow$ow_table(), 
    selection = 'single',
    style = 'bootstrap', 
    class = 'table-responsive, table-hover', 
    colnames = c('OW UID', 'SMP ID', 'OW Suffix', 'Facility ID'), 
    options = list(dom = 't')
  )
  
  rv_ow$asset_comp_code_click <- 0
  
  #add info from table to selectboxes, on click
  #need to reverse engineer the asset/ow/component combo
  observeEvent(input$ow_table_rows_selected,{ 
               #get facility id from table
               rv_ow$fac <- (rv_ow$ow_table()[input$ow_table_rows_selected, 4])
               #get component id
               comp_id_query <- paste0("select distinct component_id from smpid_facilityid_componentid where facility_id = '", rv_ow$fac, "' 
        AND component_id IS NOT NULL")
               comp_id_step <- odbc::dbGetQuery(conn, comp_id_query) %>% pull()
               #determine whether component id exists and is useful
               comp_id_click <- if(length(comp_id_step) > 0) comp_id_step else "NA"
               #get ow prefix, to sort by asset type
               ow_prefix_click <- gsub('\\d+', '', rv_ow$ow_table()[input$ow_table_rows_selected, 3])
               #get asset type - base on component id (if exists), then base on prefix if not. 
               #need to do both to check for OW (fittings/obs wells)
               asset_type_click <- if(nchar(comp_id_click) > 2){
                 dplyr::filter(asset_comp(), component_id == comp_id_click) %>% select(asset_type) %>% pull()
               }else{
                 new_and_ex_wells %>% dplyr::filter(ow_prefix == ow_prefix_click) %>% dplyr::select(asset_type) %>% pull()
               }
               #combine asset type, ow, and component id
               rv_ow$asset_comp_code_click = paste(comp_id_click, ow_prefix_click, asset_type_click,  sep = " | ")
               updateSelectInput(session, "component_id", selected = rv_ow$asset_comp_code_click)
               #updateSelectInput(session, "ow_suffix", selected = rv_ow$ow_table()[input$ow_table_rows_selected, 3])
    })
  
  #get the row/case that has the selected asset_comp_code
  select_combo_row <- reactive(asset_comp() %>% 
                                 dplyr::filter(asset_comp_code == input$component_id))
  
  #get component id from the chosen asset_comp_code, then use to get facility id
  select_component_id <- reactive(select_combo_row() %>% 
                                     dplyr::select(component_id) %>% 
                                     dplyr::pull())
   
  #get ow_prefix/suffix from chosen asset_comp_code, then use to get facility ID and suggest new ow suffix
  select_ow_prefix <- reactive(select_combo_row() %>% 
                                dplyr::select(ow_prefix) %>% 
                                dplyr::pull())
  
  #get facility ID. Either use SMP footprint (for a new well) or the facility ID of the existing component
  facility_id <- reactive(if(input$component_id != "" & length(select_ow_prefix() > 0 )){
    if(select_ow_prefix() %in% new_wells$ow_prefix) odbc::dbGetQuery(conn, paste0(
       "SELECT facility_id FROM smpid_facilityid_componentid WHERE component_id IS NULL AND smp_id = '", input$smp_id, "'"))[1,1] else 
      odbc::dbGetQuery(conn, paste0(
      "SELECT facility_id from smpid_facilityid_componentid WHERE component_id = '", select_component_id(), "'"))[1,1]
  }else{
    ""
  }
  )
  
  #udpate facility id shown based on above
  observe(updateSelectInput(session, "facility_id", selected = facility_id()))
  
  #count how many existing observation wells of the selected type already exist at the SMP
  rv_ow$well_count <- reactive(length(rv_ow$existing_ow_prefixes()[rv_ow$existing_ow_prefixes() == select_ow_prefix()]))
  
  #OW + (Count + 1) for suggested name of the new well 
  rv_ow$ow_suggested_pre <- reactive(paste0(select_ow_prefix(), (rv_ow$well_count() + 1))) 
  
  #only show suggested suffix if there is a selected component id
  rv_ow$ow_suggested <- reactive(if(input$component_id==""){
    NA
  }else if(length(input$ow_table_rows_selected) > 0 & rv_ow$asset_comp_code_click == select_combo_row()$asset_comp_code){
    rv_ow$ow_table()[input$ow_table_rows_selected, 3]
  }else{
    rv_ow$ow_suggested_pre()
  })
  
  observe(updateTextInput(session, "ow_suffix", value = rv_ow$ow_suggested()))
  
  rv_ow$toggle_suffix <- reactive(!(input$ow_suffix %in% rv_ow$ow_table()$ow_suffix) | length(input$ow_table_rows_selected) > 0)
  rv_ow$toggle_component_id <- reactive(!input$facility_id %in% rv_ow$ow_table()$facility_id | 
                                          length(input$ow_table_rows_selected) > 0 |
                                          is.na(select_component_id()))
  
  #toggle state (enable/disable) buttons based on whether smp id, component id, and ow suffix are selected (this is shinyjs)
  observe({toggleState(id = "add_ow", condition = nchar(input$smp_id) > 0 & nchar(input$component_id) > 0 & nchar(input$ow_suffix) >0 &
                         rv_ow$toggle_suffix() & rv_ow$toggle_component_id())})
  rv_ow$label <- reactive(if(length(input$ow_table_rows_selected) == 0) "Add New" else "Edit Selected")
  observe(updateActionButton(session, "add_ow", label = rv_ow$label()))
  #observe({toggleState(id = "delete_ow", condition = length(input$ow_table_rows_selected) > 0)})
  observe({toggleState(id = "add_ow_deploy", nchar(input$smp_id) > 0)})
  
  #Write to database when button is clicked
  #update if editing
  observeEvent(input$add_ow, {
    if(length(input$ow_table_rows_selected) == 0){
      odbc::dbGetQuery(conn, paste0(
      "INSERT INTO ow_testing (smp_id, ow_suffix, facility_id) 
  	      VALUES ('", input$smp_id, "','", input$ow_suffix, "','",  facility_id(), "')"
      ))
    }else{
      edit_ow_query <- paste0(
        "UPDATE ow_testing SET ow_suffix = '", input$ow_suffix, "', facility_id = '", facility_id(), "' 
        WHERE smp_id = '", input$smp_id, "'
        AND ow_suffix = '", rv_ow$ow_table()[input$ow_table_rows_selected, 3], "' 
        AND facility_id = '", rv_ow$ow_table()[input$ow_table_rows_selected, 4], "'")
      dbGetQuery(conn, edit_ow_query)
    }
    #update ow_table with new well
    rv_ow$ow_table <- reactive(odbc::dbGetQuery(conn, ow_table_query()))
    #upon click update the smp_id in the Deploy Sensor tab
    #it needs to switch to NULL and then back to the input$smp_id to make sure the change is registered
    updateSelectInput(session, "smp_id_deploy", selected = "")
    updateSelectInput(session, "smp_id_deploy", selected = input$smp_id)
    #reset ow_suffix to blank
    updateTextInput(session, "ow_suffix", value = NA)
    #reset component id to blank
    updateSelectInput(session, "component_id", selected = NA)
    #query collection table
    rv_collect$collect_table_db <- odbc::dbGetQuery(conn, collect_query)
    
  })
  
  
  #upon click of "Deploy Sensor at this SMP" update the smp_id in the Deploy Sensor tab, and switch tabs
  #it needs to switch to NULL and then back to the input$smp_id to make sure the change is registered
  observeEvent(input$add_ow_deploy, {
    updateSelectInput(session, "smp_id_deploy", selected = "")
    updateSelectInput(session, "smp_id_deploy", selected = input$smp_id)
    updateTabsetPanel(session, "inTabset", selected = "deploy_tab")
  })
  
  observeEvent(input$clear_ow, {
    reset("ow_suffix")
    reset("component_id")
    reset("smp_id")
  })
  
  
 # Sensor Inventory ----

  #start reactiveValues for this section
  rv_sensor <- reactiveValues()
  
  #Sensor Serial Number List
  hobo_list_query <-  "select inv.sensor_serial, inv.sensor_model, inv.date_purchased, ow.smp_id, ow.ow_suffix from inventory_sensors_testing inv
                          left join deployment_testing d on d.inventory_sensors_uid = inv.inventory_sensors_uid
                          left join ow_testing ow on ow.ow_uid = d.ow_uid"
  rv_sensor$hobo_list <- odbc::dbGetQuery(conn, hobo_list_query)

  #if input serial number is already in the list, then suggest the existing model number. if it isn't already there, show NULL
  model_no_select <- reactive(if(input$serial_no %in% rv_sensor$hobo_list$sensor_serial) dplyr::filter(rv_sensor$hobo_list, sensor_serial == input$serial_no) %>% dplyr::select(sensor_model) %>% dplyr::pull() else "")
  
  observe(updateSelectInput(session, "model_no", selected = model_no_select()))
  
  #if input serial number is already in the list, then suggest the date_purchased. if it isn't already there, show NULL
  date_purchased_select <- reactive(if(input$serial_no %in% rv_sensor$hobo_list$sensor_serial) dplyr::filter(rv_sensor$hobo_list, sensor_serial == input$serial_no) %>% dplyr::select(date_purchased) %>% dplyr::pull() else as.Date(NA))
  
  observe(updateDateInput(session, "date_purchased", value = date_purchased_select()))
  
  rv_sensor$date_purchased <- reactive(if(length(input$date_purchased) == 0) "NULL" else paste0("'", input$date_purchased, "'"))
  
  #enable/disable the "add sensor button" if all fields are not empty
  observe({toggleState(id = "add_sensor", condition = nchar(input$serial_no) > 0 & nchar(input$model_no) > 0)})
  observe({toggleState(id = "add_sensor_deploy", input$serial_no %in% rv_sensor$hobo_list$sensor_serial)})
  
  #change label from Add to Edit if the sensor already exists in db
  rv_sensor$label <- reactive(if(!(input$serial_no %in% rv_sensor$hobo_list$sensor_serial)) "Add Sensor" else "Edit Sensor")
  observe(updateActionButton(session, "add_sensor", label = rv_sensor$label()))
  
  #Write to database when button is clicked
  observeEvent(input$add_sensor, { #write new sensor info to db
    if(!(input$serial_no %in% rv_sensor$hobo_list$sensor_serial)){
      odbc::dbGetQuery(conn, paste0(
      "INSERT INTO inventory_sensors_testing (sensor_serial, sensor_model, date_purchased) 
	      VALUES ('", input$serial_no, "','", input$model_no, "',",  rv_sensor$date_purchased(), ")"))
      output$testing <- renderText({
        isolate(paste("Sensor", input$serial_no, "added."))
      })
    }else{ #edit sensor info
      odbc::dbGetQuery(conn, paste0("UPDATE inventory_sensors_testing SET sensor_model = '", input$model_no, 
                                    "', date_purchased = ", rv_sensor$date_purchased(), " WHERE sensor_serial = '", input$serial_no, "'"))
      output$testing <- renderText({
        isolate(paste("Sensor", input$serial_no, "edited."))
      })
    }
    reset("serial_no")
    reset("model_no")
    reset("date")
    #update sensor list following addition
    rv_sensor$hobo_list <- odbc::dbGetQuery(conn, hobo_list_query)
  })
  
  #switch tabs to "Deploy" and update Sensor ID to the current Sensor ID (if the add/edit button says edit sensor)
  observeEvent(input$add_sensor_deploy, {
    updateSelectInput(session, "sensor_id", selected = input$serial_no)
    updateTabsetPanel(session, "inTabset", selected = "deploy_tab")
  })
  
  rv_sensor$hobo_list_display <- reactive(rv_sensor$hobo_list %>% mutate("date_purchased" = lubridate::ymd("date_purchased")) %>% 
                                            rename("Serial Number" = "sensor_serial", "Model Number" = "sensor_model", 
                                                   "Date Purchased" = "date_purchased", "SMP ID" = "smp_id", "OW Suffix" = "ow_suffix"))
  
  output$sensor_table <- renderDT(
    rv_sensor$hobo_list_display(),
    selection = "single",
    style = 'bootstrap', 
    class = 'table-responsive, table-hover'
  )
  
  observeEvent(input$sensor_table_rows_selected, {
    updateTextInput(session, "serial_no", value = rv_sensor$hobo_list$sensor_serial[input$sensor_table_rows_selected])
  })
  
 # Deploy Sensor ----
  
  #update sensor id choices based on the hobo list
  observe(updateSelectInput(session, inputId = "sensor_id", choices = c("", rv_sensor$hobo_list$sensor_serial), selected = NULL))
  
  #start reactiveValues for this section/tab
  rv_deploy <- reactiveValues()
  ## well panel
    #query ow_suffixes based on smp_id
    rv_deploy$ow_suffixes <- reactive(odbc::dbGetQuery(conn, paste0(
      "select ow_suffix from ow_testing where smp_id = '", input$smp_id_deploy, "'")) %>% dplyr::pull())
    
    observe(updateSelectInput(session, "well_name", choices = c("", rv_deploy$ow_suffixes())))
    observe(updateDateInput(session, "collect_date", min = input$deploy_date))
    
  ## sensor panel
  
    rv_deploy$purpose <- reactive(deployment_lookup %>% dplyr::filter(type == input$sensor_purpose) %>% 
      select(sensor_purpose_lookup_uid) %>% pull())
    #rv_deploy$purpose <- reactive(if(input$sensor_purpose == "LEVEL") 1 else if(input$sensor_purpose == "BARO") 2 else NA)
    rv_deploy$inventory_sensors_uid <- reactive(odbc::dbGetQuery(conn, paste0(
      "SELECT inventory_sensors_uid FROM inventory_sensors_testing WHERE sensor_serial = '", input$sensor_id, "'"
    )))
    rv_deploy$collect_date <- reactive(if(length(input$collect_date) == 0) "NULL" else paste0("'", input$collect_date, "'"))
  
  ## show tables
    #query for active deployments
    active_table_query <- reactive(paste0(
      "SELECT * FROM active_deployments_testing
        WHERE smp_id = '", input$smp_id_deploy, "'"))
    
    #create table as a reactive value based on query
    rv_deploy$active_table_db <- reactive(odbc::dbGetQuery(conn, active_table_query()))
    #select columns to show in app, and rename
    rv_deploy$active_table <- reactive(rv_deploy$active_table_db() %>% 
                   mutate_at(c("deployment_dtime_est", "date_80percent", "date_100percent"), as.character) %>% 
                   dplyr::select(deployment_dtime_est, smp_id, ow_suffix, type, interval_min, date_80percent, date_100percent) %>% 
                   dplyr::rename("Deploy Date" = "deployment_dtime_est", "SMP ID" = "smp_id", 
                                 "OW" = "ow_suffix", "Purpose" = "type", "Interval (min)" = "interval_min", 
                                 "80% Full Date" = "date_80percent", "100% Full Date" = "date_100percent"))
    
    #when a row in active deployments table is clicked
    observeEvent(input$current_deployment_rows_selected, {
      #deselect from other table
      dataTableProxy('prev_deployment') %>% selectRows(NULL)
      
    })
    #render Datatable
    output$current_deployment <- renderDT(
      rv_deploy$active_table(),
                selection = "single",
                style = 'bootstrap', 
                class = 'table-responsive, table-condensed',
                options = list(dom = 'tp')
    )
    
    #query for previous deployments
    old_table_query <- reactive(paste0(
      "SELECT te.deployment_uid, te.deployment_dtime_est, v.smp_id, v.ow_suffix, te.sensor_purpose, te.interval_min, te.collection_dtime_est, inv.sensor_serial
       FROM deployment_testing te
        LEFT JOIN ow_testing v ON te.ow_uid = v.ow_uid
        LEFT JOIN inventory_sensors_testing inv on te.inventory_sensors_uid = inv.inventory_sensors_uid
        WHERE v.smp_id = '", input$smp_id_deploy, "' AND te.collection_dtime_est IS NOT NULL"))
    
    #create table as a reactive value based on query
    rv_deploy$old_table_db <- reactive(odbc::dbGetQuery(conn, old_table_query()))
    rv_deploy$old_table <- reactive(rv_deploy$old_table_db() %>% 
        mutate_at(c("deployment_dtime_est", "collection_dtime_est"), as.character) %>% 
          left_join(y = deployment_lookup, by = c("sensor_purpose" = "sensor_purpose_lookup_uid")) %>% 
        dplyr::select(deployment_dtime_est, collection_dtime_est, smp_id, ow_suffix, type, interval_min) %>% 
        dplyr::rename("Deploy Date" = "deployment_dtime_est", "SMP ID" = "smp_id", "OW" = "ow_suffix", 
                      "Purpose" = "type", "Interval (min)" = "interval_min", "Collection Date" = "collection_dtime_est"))
   
    #render datatable
    output$prev_deployment <- renderDT(
      rv_deploy$old_table(),
        selection = "single",
        style = 'bootstrap', 
        class = 'table-responsive, table-condensed', 
        options = list(dom = 'tp')
    )
    
    #shorten name of selected rows from active and prev deployments tables
    rv_deploy$active <- reactive(input$current_deployment_rows_selected) 
    rv_deploy$prev <- reactive(input$prev_deployment_rows_selected)
    
    #define inputs, based on whether previous or active deployments tables are selected
    #this was in two separate observeEvent calls, but changed to try to address the issue where collection date is sometimes blank when it shouldn't be
    #that was not resolved - I believe the issue is related to having two dateInputs update at the same time
    rv_deploy$well_name <- reactive(if(length(rv_deploy$active()) > 0) rv_deploy$active_table_db()$ow_suffix[rv_deploy$active()] else if(length(rv_deploy$prev()) > 0) rv_deploy$old_table_db()$ow_suffix[rv_deploy$prev()])
    rv_deploy$sensor_id <- reactive(if(length(rv_deploy$active()) > 0) rv_deploy$active_table_db()$sensor_serial[rv_deploy$active()] else if(length(rv_deploy$prev()) > 0) rv_deploy$old_table_db()$sensor_serial[rv_deploy$prev()])
    rv_deploy$sensor_purpose <- reactive(if(length(rv_deploy$active()) > 0) rv_deploy$active_table()$`Purpose`[rv_deploy$active()] else if(length(rv_deploy$prev()) > 0) rv_deploy$old_table()$`Purpose`[rv_deploy$prev()])
    rv_deploy$mea_int <- reactive(if(length(rv_deploy$active()) > 0) rv_deploy$active_table_db()$interval_min[rv_deploy$active()] else if(length(rv_deploy$prev()) > 0) rv_deploy$old_table()$interval_min[rv_deploy$prev()])
    rv_deploy$deploy_date <- reactive(if(length(rv_deploy$active()) > 0) rv_deploy$active_table_db()$deployment_dtime_est[rv_deploy$active()] else if(length(rv_deploy$prev()) > 0) rv_deploy$old_table_db()$deployment_dtime_est[rv_deploy$prev()])
    rv_deploy$collect <- reactive(if(length(rv_deploy$active()) > 0) NA else if(length(rv_deploy$prev()) > 0) rv_deploy$old_table_db()$collection_dtime_est[rv_deploy$prev()])
    
    observe(updateSelectInput(session, "well_name", selected = rv_deploy$well_name()))
    observe(updateSelectInput(session, "sensor_id", selected = rv_deploy$sensor_id()))
    observe(updateSelectInput(session, "sensor_purpose", selected = rv_deploy$sensor_purpose()))
    observe(updateSelectInput(session, "interval", selected = rv_deploy$mea_int()))
    observe(updateDateInput(session, "deploy_date", value = rv_deploy$deploy_date()))
    observe(updateDateInput(session, "collect_date", value = rv_deploy$collect()))
    
    #when a row in the previous deployments table is clicked
    observeEvent(input$prev_deployment_rows_selected, {
      #deselect from other table
      dataTableProxy('current_deployment') %>% selectRows(NULL)
    })
        
  #control for redeploy checkbox. had to add, because after checking it, then removing collect_date, it goes away, but is still TRUE
    rv_deploy$redeploy <- reactive(if(length(input$collect_date > 0) & input$redeploy == TRUE) TRUE else FALSE)
    
    #relabel deploy button if editing
    rv_deploy$label <- reactive(if(length(input$prev_deployment_rows_selected) == 0 & length(input$current_deployment_rows_selected) == 0) "Add New Deployment" else "Edit Selected Deployment")
    
    observe(updateActionButton(session, "deploy_sensor", label = rv_deploy$label()))
  
    #rv_deploy$update_deployment_uid <- reactive(if(length(input$current_deployment_rows_selected) > 0) rv_deploy$active_table_db()$deployment_uid[input$current_deployment_rows_selected] else if(length(input$prev_deployment_rows_selected) > 0) rv_deploy$old_table_db()$deployment_uid[input$current_deployment_rows_selected] else 0)
    
    rv_deploy$update_deployment_uid <- reactive(if(length(input$current_deployment_rows_selected) > 0){
      rv_deploy$active_table_db()$deployment_uid[input$current_deployment_rows_selected]
    }else if(length(input$prev_deployment_rows_selected) > 0){
      rv_deploy$old_table_db()$deployment_uid[input$prev_deployment_rows_selected]
    }else{
      0
    }
    )
    
  #write to database on click
  #1/7/2020 does not yet do anything different for editing,
  observeEvent(input$deploy_sensor, {
    if(length(input$prev_deployment_rows_selected) == 0 & length(input$current_deployment_rows_selected) == 0){
    #write deployment
    odbc::dbGetQuery(conn,
     paste0("INSERT INTO deployment_testing (deployment_dtime_est, ow_uid,
     inventory_sensors_uid, sensor_purpose, interval_min, collection_dtime_est)
        VALUES ('", input$deploy_date, "', get_ow_uid_testing('",input$smp_id_deploy,"', '", input$well_name, "'), '",
            rv_deploy$inventory_sensors_uid(), "','", rv_deploy$purpose(), "','",input$interval, "',", rv_deploy$collect_date(),")"))
    }else{
      odbc::dbGetQuery(conn, 
                       paste0("UPDATE deployment_testing SET deployment_dtime_est = '", input$deploy_date, "', 
                       ow_uid = get_ow_uid_testing('",input$smp_id_deploy,"', '", input$well_name, "'), 
                              inventory_sensors_uid = '",  rv_deploy$inventory_sensors_uid(), "', 
                              sensor_purpose = '", rv_deploy$purpose(), "', 
                              interval_min = '", input$interval, "',
                              collection_dtime_est = ", rv_deploy$collect_date(), " WHERE 
                              deployment_uid = '", rv_deploy$update_deployment_uid(), "'"))
    }
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
    
    #query collection table
    rv_collect$collect_table_db <- odbc::dbGetQuery(conn, collect_query)
    })
    
  #enable/disable deploy sensor button based on whether all inputs (except collect date) are not empty
  observe({toggleState(id = "deploy_sensor", condition = nchar(input$smp_id_deploy) > 0 &
                         nchar(input$well_name) > 0 & nchar(input$sensor_id) > 0 & nchar(input$sensor_purpose) > 0 &
                        nchar(input$interval) > 0 & length(input$deploy_date) > 0 &
                         (!(input$sensor_id %in% rv_collect$collect_table_db$sensor_serial) | length(input$collect_date) > 0 |
                            length(input$current_deployment_rows_selected) > 0 | length(input$prev_deployment_rows_selected) > 0))})
            
  #clear all fields
  observeEvent(input$clear_deploy_fields, {
    reset("sensor_id")
    reset("sensor_purpose")
    reset("interval")
    reset("deploy_date")
    reset("collect_date")
    reset("smp_id_deploy")
    reset("well_name")
  })
  

# Collection Calendar -----------------------------------------------------

  rv_collect <- reactiveValues()  
  
  #query the collection calendar and arrange by deployment_uid
  collect_query <- "select ac.*, own.public from active_deployments_testing ac left join ow_ownership_testing own on ac.ow_uid = own.ow_uid"
  rv_collect$collect_table_db<- odbc::dbGetQuery(conn, collect_query)
  #arrange and filtered the collection calendar
  rv_collect$collect_table_filter <- reactive(rv_collect$collect_table_db %>% 
    dplyr::arrange(deployment_uid) %>% 
       #force tz to assure that today() is being properly compared to dates, and trim to ymd for appearance in app
      #filter based on the less than 80% or more than 80% capacity
    mutate(deployment_dtime_est = lubridate::force_tz(deployment_dtime_est, "America/New_York") %>% lubridate::ymd(), 
           date_80percent = lubridate::force_tz(date_80percent, "America/New_York") %>% lubridate::ymd(),
           date_100percent = lubridate::force_tz(date_100percent, "America/New_York") %>% lubridate::ymd(), 
           filter_80 = case_when(input$capacity_used == "Less than 80%" & date_80percent > today() ~ 1, 
                                  input$capacity_used == "Less than 80%" & date_80percent < today() ~ 0, 
                                  input$capacity_used == "80% or more" & date_80percent < today() ~ 1, 
                                  input$capacity_used == "80% or more" & date_80percent > today() ~ 0, 
                                  TRUE ~ 1)) %>% 
      #use 1 or 0 for public or private, respectively, and 0.5 for both, with a tolerance of .51 
      #so if .5 is selected, 0 and 1 are picked up
    dplyr::filter(near(as.numeric(public), as.numeric(input$property_type), tol =.51) & 
                    #use 5 or 15 minute intervals, with 10 for both, with a tolerance of 5.1 
                    #so if 10 is selected, 5 and 15 are picked up
                    near(interval_min, as.numeric(input$interval_filter), tol = 5.1) &
                    filter_80 == 1))
  #select and rename columns to show in app
  rv_collect$collect_table <- reactive(rv_collect$collect_table_filter() %>% 
                                         dplyr::select(smp_id, ow_suffix, type, deployment_dtime_est,date_80percent,date_100percent)  %>% 
    rename("SMP ID" = "smp_id", "OW Suffix" = "ow_suffix", "Purpose" = "type", "Deploy Date" = "deployment_dtime_est", 
           "80% Full Date" = "date_80percent", "100% Full Date" = "date_100percent"))
  
  output$collection <- renderDT(
    DT::datatable(
      rv_collect$collect_table(), 
      selection = "single", 
      style = 'bootstrap', 
      class = 'table-responsive, table-hover', 
      options = list(scroller = TRUE, 
                     scrollX = TRUE, 
                     scrollY = 550, 
                     order = list(6, 'asc'))) %>%
    formatStyle(
      '80% Full Date',
      backgroundColor = styleInterval(lubridate::today(), c('yellow', 'transparent')), 
      color = 'black'
    ) %>%
    formatStyle(
      '100% Full Date',
      backgroundColor = styleInterval(lubridate::today(), c('red', 'transparent')),
      color = styleInterval(lubridate::today(), c('white', 'black'))
    ) 
  )
  
  #this is a two-step observeEvent
  #when a line in the calendar is clicked, go toggle and update "smp_id_deploy", and switch tabs
  observeEvent(input$collection_rows_selected, {
    updateSelectInput(session, "smp_id_deploy", selected = "")
    updateSelectInput(session, "smp_id_deploy", selected = rv_collect$collect_table_filter()$smp_id[input$collection_rows_selected])
    updateTabsetPanel(session, "inTabset", selected = "deploy_tab")
  })
  
  #when the active table is updated, AND it follows a click on the collection table, make the row in the selected active table match the collection table
  observeEvent(rv_deploy$active_table_db(), {
    if(length(input$collection_rows_selected) > 0){
      #print(paste("input row selected", input$collection_rows_selected))
      #print(paste("collect_deployment_uid", collect_table_db$deployment_uid[input$collection_rows_selected]))
      active_row <- which(rv_deploy$active_table_db()$deployment_uid == rv_collect$collect_table_filter()$deployment_uid[input$collection_rows_selected], arr.ind = TRUE)
      dataTableProxy('current_deployment') %>% selectRows(active_row)
    }
  })
  
}

shinyApp(ui, server)




















