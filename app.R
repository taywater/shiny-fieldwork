#6/24/2020 - Nick Manna, AKRF, Inc.
#This file, app.R, works with all sourced files to run the PWD GSI MARS Fieldwork App
#The app is run through app.R
#when naming input variables, ns("name") is needed
#often, I import a table as "x_table_db" - that's a copy of the table that is in the database, and 
#it's easier and better to reference columns directly from the db
#then i'll modify it as "x_table", and that's the table that I feed to dt or reactable to show in the app 
#these modifications could be changing dates to characters, to show better, rounding decimals, or changing boolean values to be Yes or No
#I added global variables to both UI and server in app.R because (and added function(req){}) to UI) because they need to be checked by each every time the app runs, not just when it is deployed. 

#load libraries
library(shiny)
library(pool)
library(odbc)
library(tidyverse)
library(shinythemes)
library(lubridate)
library(shinyjs)
library(DT)
library(reactable)

#set default page length for datatables
options(DT.options = list(pageLength = 15))

#set db connection
poolConn <- dbPool(odbc(), dsn = "mars_testing", uid = Sys.getenv("shiny_uid"), pwd = Sys.getenv("shiny_pwd"))

#disconnect from db on stop 
onStop(function(){
  poolClose(poolConn)
})

#js warning about leaving page
jscode <- 'window.onbeforeunload = function() { return "Please use the button on the webpage"; };'

######  
 #source scripts. each script contains a module, which includes UI and server code
  #source("setup.R")
  source("collection_calendar.R")
  source("add_ow.R")
  source("add_sensor.R")
  source("deploy.R")
  source("srt.R")
  source("porous_pavement.R")
  source("capture_efficiency.R")
  source("monitoring_stats.R")
  source("inlet_conveyance.R")
  source("special_investigations.R")
  source("documentation.R")

  #call all the UI functions
  ui <- function(req){
    
    #req variables -----
    #define global variables that will be required each time the UI runs
    #query all SMP IDs
    smp_id <- odbc::dbGetQuery(poolConn, paste0("select distinct smp_id from smpid_facilityid_componentid")) %>% 
      dplyr::arrange(smp_id) %>% 
      dplyr::pull()
    
    sys_id <- odbc::dbGetQuery(poolConn, paste0("select distinct system_id from smpid_facilityid_componentid")) %>% 
      dplyr::arrange(system_id) %>% 
      dplyr::pull()
    
    #query site names (non SMP)
    site_name_query <- "select * from fieldwork.site_name_lookup"
    site_names <- odbc::dbGetQuery(poolConn, site_name_query) %>% 
      dplyr::arrange(site_name) %>% 
      dplyr::pull()
    
    #Sensor Model Number options
    hobo_options <- c("", "U20-001-01", "U20-001-04", "U20L-01", "U20L-04")
    
    sensor_status_lookup <- dbGetQuery(poolConn, "select * from fieldwork.sensor_status_lookup")
    
    #Sensor Serial Number List
    hobo_list_query <-  "select inv.sensor_serial, inv.sensor_model, inv.date_purchased, ow.smp_id, ow.ow_suffix from fieldwork.inventory_sensors inv
                          left join fieldwork.deployment d on d.inventory_sensors_uid = inv.inventory_sensors_uid AND d.collection_dtime_est is NULL
                          left join fieldwork.ow ow on ow.ow_uid = d.ow_uid"
    hobo_list <- odbc::dbGetQuery(poolConn, hobo_list_query)
    sensor_serial <- hobo_list$sensor_serial
    
    #Deployment purpose lookup table
    deployment_lookup <- dbGetQuery(poolConn, "select * from fieldwork.deployment_lookup")
    
    long_term_lookup <- dbGetQuery(poolConn, "select * from fieldwork.long_term_lookup")
    
    research_lookup <- dbGetQuery(poolConn, "select * from fieldwork.research_lookup")
    
    #field test priority
    priority <- dbGetQuery(poolConn, "select * from fieldwork.field_test_priority_lookup")
    
    #srt_types & con phase
    srt_types <- dbGetQuery(poolConn, "select * from fieldwork.srt_type_lookup")
    con_phase <- dbGetQuery(poolConn, "select * from fieldwork.con_phase_lookup")
    
    #porous pavement surface types
    surface_type <- dbGetQuery(poolConn, "select * from fieldwork.surface_type_lookup")
    # 
    # #capture efficiency high flow types
    high_flow_type <- dbGetQuery(poolConn, "select * from fieldwork.est_high_flow_efficiency_lookup")
    
    #capture efficiency asset types 
    cet_asset_type <- dbGetQuery(poolConn, "select distinct asset_type from smpid_facilityid_componentid_inlets where component_id is not null order by asset_type")   
    
    #this function adds a little red star to indicate that a field is required. It uses HTML, hence "html_req"
    html_req <- function(label){
      HTML(paste(label, tags$span(style="color:red", tags$sup("*"))))
    }
    
    #this function adds a blue dagger to indicate that a field is required for future tests. It uses HTML
    future_req <- function(label){
      HTML(paste(label, tags$span(style="color:blue", tags$sup("†"))))
    }
    
    #monitoring stats
    current_fy <- lubridate::today() %m+% months(6) %>% year()
    start_fy <- 2012
    years <- start_fy:current_fy %>% sort(decreasing = TRUE)
    
    #project work numbers
    work_number <- dbGetQuery(poolConn, "select worknumber from greenit_projectbestdata") %>% pull()
    
    #special investigation types
    si_lookup <- dbGetQuery(poolConn, "select * from fieldwork.special_investigation_lookup")
    
    #request by lookup
    requested_by_lookup <- dbGetQuery(poolConn, "select * from fieldwork.requested_by_lookup")
    
    #actual UI----
    #use tagList so tags and shinyjs can be called without being inside of the navbarPage. When they're inside navbarpage, they create small invisible fake tabs that take up space and act weird when clicked on
    tagList(
      #call jscode to warn when leaving page
    tags$head(tags$script(jscode)),
    #must call useShinyjs() for shinyjs() functionality to work in app
    useShinyjs(),
    navbarPage("Fieldwork", theme = shinytheme("cerulean"), id = "inTabset",
               do.call(navbarMenu, list(title = "Deployments") %>%
                       append(collection_calendarUI("collection_calendar")) %>%
                       append(deployUI("deploy", smp_id = smp_id, sensor_serial = sensor_serial, site_names = site_names, html_req = html_req, long_term_lookup = long_term_lookup, deployment_lookup = deployment_lookup, research_lookup = research_lookup, priority = priority, future_req = future_req))),
                  add_owUI("add_ow", smp_id = smp_id, site_names = site_names, html_req = html_req),
                  add_sensorUI("add_sensor", hobo_options = hobo_options, html_req = html_req, sensor_status_lookup = sensor_status_lookup),
                SRTUI("srt", sys_id = sys_id, srt_types = srt_types, html_req = html_req, con_phase = con_phase, priority = priority, future_req = future_req),
                porous_pavementUI("porous_pavement", smp_id = smp_id, html_req = html_req, surface_type = surface_type, con_phase = con_phase, priority = priority, future_req = future_req),
                capture_efficiencyUI("capture_efficiency", sys_id = sys_id, high_flow_type = high_flow_type, html_req = html_req, con_phase = con_phase, priority = priority, future_req = future_req, cet_asset_type = cet_asset_type),
              inlet_conveyanceUI("inlet_conveyance", sys_id = sys_id, work_number = work_number, html_req = html_req, con_phase = con_phase, priority = priority, site_names = site_names, future_req = future_req),
               special_investigationsUI("special_investigations", sys_id = sys_id, work_number = work_number, html_req = html_req, con_phase = con_phase, priority = priority, site_names = site_names, si_lookup = si_lookup, requested_by_lookup = requested_by_lookup, future_req = future_req),
               m_statsUI("stats", current_fy = current_fy, years = years),
                documentationUI("documentation")
  )
    )
  }
  
  #call modules, referencing the UI names above. These are functions, so any data originating outside the function needs to be named as an argument, whether it is lookup data, or from another tab
  server <- function(input, output, session) {
    
    #req variables -----
    #define global variables that will be defined each time server runs
    #query all SMP IDs
    smp_id <- odbc::dbGetQuery(poolConn, paste0("select distinct smp_id from smpid_facilityid_componentid")) %>% 
      dplyr::arrange(smp_id) %>% 
      dplyr::pull()
    
    sys_id <- odbc::dbGetQuery(poolConn, paste0("select distinct system_id from smpid_facilityid_componentid")) %>% 
      dplyr::arrange(system_id) %>% 
      dplyr::pull()
    
    #query site names (non SMP)
    site_name_query <- "select * from fieldwork.site_name_lookup"
    site_names <- odbc::dbGetQuery(poolConn, site_name_query) %>% 
      dplyr::arrange(site_name) %>% 
      dplyr::pull()
    
    #Sensor Model Number options
    hobo_options <- c("", "U20-001-01", "U20-001-04", "U20L-01", "U20L-04")
    
    sensor_status_lookup <- dbGetQuery(poolConn, "select * from fieldwork.sensor_status_lookup")
    
    #Sensor Serial Number List
    hobo_list_query <-  "select inv.sensor_serial, inv.sensor_model, inv.date_purchased, ow.smp_id, ow.ow_suffix from fieldwork.inventory_sensors inv
                          left join fieldwork.deployment d on d.inventory_sensors_uid = inv.inventory_sensors_uid AND d.collection_dtime_est is NULL
                          left join fieldwork.ow ow on ow.ow_uid = d.ow_uid"
    hobo_list <- odbc::dbGetQuery(poolConn, hobo_list_query)
    sensor_serial <- hobo_list$sensor_serial
    
    #Deployment purpose lookup table
    deployment_lookup <- dbGetQuery(poolConn, "select * from fieldwork.deployment_lookup")
    
    long_term_lookup <- dbGetQuery(poolConn, "select * from fieldwork.long_term_lookup")
    
    research_lookup <- dbGetQuery(poolConn, "select * from fieldwork.research_lookup")
    
    #field test priority
    priority <- dbGetQuery(poolConn, "select * from fieldwork.field_test_priority_lookup")
    
    #srt_types & con phase
    srt_types <- dbGetQuery(poolConn, "select * from fieldwork.srt_type_lookup")
    con_phase <- dbGetQuery(poolConn, "select * from fieldwork.con_phase_lookup")
    
    #porous pavement surface types
    surface_type <- dbGetQuery(poolConn, "select * from fieldwork.surface_type_lookup")
    # 
    # #capture efficiency high flow types
    high_flow_type <- dbGetQuery(poolConn, "select * from fieldwork.est_high_flow_efficiency_lookup")
    
    #capture efficiency asset types 
    cet_asset_type <- dbGetQuery(poolConn, "select distinct asset_type from smpid_facilityid_componentid_inlets where component_id is not null order by asset_type")
    
    #this function adds a little red star to indicate that a field is required. It uses HTML, hence "html_req"
    html_req <- function(label){
      HTML(paste(label, tags$span(style="color:red", tags$sup("*"))))
    }
    
    #this function adds a blue dagger to indicate that a field is required for future tests. It uses HTML
    future_req <- function(label){
      HTML(paste(label, tags$span(style="color:blue", tags$sup("†"))))
    }
    
    #monitoring stats
    current_fy <- lubridate::today() %m+% months(6) %>% year()
    start_fy <- 2012
    years <- start_fy:current_fy %>% sort(decreasing = TRUE)
    
    
    #special investigation types
    si_lookup <- dbGetQuery(poolConn, "select * from fieldwork.special_investigation_lookup")
    
    #request by lookup
    requested_by_lookup <- dbGetQuery(poolConn, "select * from fieldwork.requested_by_lookup")
    
    #-------
   collection_cal <- callModule(collection_calendar, "collection_calendar", parent_session = session, ow = ow, deploy = deploy, poolConn = poolConn)
   ow <- callModule(add_ow, "add_ow", parent_session = session, smp_id = smp_id, poolConn = poolConn)
   sensor <- callModule(add_sensor, "add_sensor", parent_session = session, poolConn = poolConn, sensor_status_lookup = sensor_status_lookup)
   deploy <- callModule(deploy, "deploy", parent_session = session, ow = ow, collect = collection_cal, sensor = sensor, poolConn = poolConn, deployment_lookup = deployment_lookup)
  callModule(SRT, "srt", parent_session = session, poolConn = poolConn, srt_types = srt_types, con_phase = con_phase)
  callModule(porous_pavement, "porous_pavement", parent_session = session, surface_type = surface_type, poolConn = poolConn, con_phase = con_phase)
  callModule(capture_efficiency, "capture_efficiency", parent_session = session, poolConn = poolConn, high_flow_type = high_flow_type, con_phase = con_phase, cet_asset_type = cet_asset_type)
   callModule(inlet_conveyance, "inlet_conveyance", parent_session = session, poolConn = poolConn, con_phase = con_phase)
   callModule(special_investigations, "special_investigations", parent_session = session, poolConn = poolConn, con_phase = con_phase, si_lookup = si_lookup, requested_by_lookup = requested_by_lookup)
   callModule(m_stats, "stats", parent_session = session, current_fy = current_fy, poolConn = poolConn)
  }
  
  shinyApp(ui, server)