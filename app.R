#6/24/2020 - Nick Manna, AKRF, Inc.
#This file, app.R, works with all sourced files to run the PWD GSI MARS Fieldwork App
#The app is run through app.R
#when naming input variables, ns("name") is needed
#often, I import a table as "x_table_db" - that's a copy of the table that is in the database, and 
#it's easier and better to reference columns directly from the db
#then i'll modify it as "x_table", and that's the table that I feed to dt or reactable to show in the app 
#these modifications could be changing dates to characters, to show better, rounding decimals, or changing boolean values to be Yes or No
#I added global variables to both UI and server (and added function(req){}) to UI)  in app.R because they need to be checked by each function every time the app runs, not just when it is deployed. 
#passing the parent session gives the module the ability to look to the parent session and update tabs to other modules

#11/13/2020 - Nick Manna, AKRF, Inc. 
#I am going to try to create a uniform approach to each module so things are in a good order and have standard comments/headers

#1/26/21 notes from talking to taylor  
#look out for deprecated functions

#number sections and treat as level of indentation
#like section 0.0 for libraries
#make visually more distinct betwn variables and UI
#this might be helpful https://github.com/r-lib/styler

#data glossary - maybe part of documentation or a supplemental document or vignette

#3/2/2021
#updating SMP ID inputs to be server-side selectizeInput instead of selectInputs. relevant links below: 
  # general use
  # https://shiny.rstudio.com/articles/selectize.html
  # placeholder (for initializing with no smp_id or system_id selected)
  # https://shiny.rstudio.com/gallery/selectize-examples.html
  # using  selected = character(0) instead of selected = NULL 
  # https://github.com/rstudio/shiny/issues/1182#issuecomment-238661390
  # need to do the full updateSelectizeInput AGAIN when updating vectors with length > 1000
  # for example, updateSelectizeInput(session, "smp_id", choices = smp_id, selected = "250-1-1", server = TRUE)
  # if just resetting it, then, updateSelectizeInput(session, "smp_id", selected = character(0)) is fine, because it pulls from the first 1000
  # i tried increasing maxOptions but that makes searching slow, which is bad

#3/3/2021
# ~ is needed for dplyr::mutate(across, ~ case_when). not needed for other mutates across
# https://stackoverflow.com/questions/64189561/using-case-when-with-dplyr-across

# shinyjs(delay) is used after updateSelectizeInputs to ensure the next step happens at the right time

# 

# SET UP
#0.0: load libraries --------------
  #shiny
  library(shiny)
  #pool for database connections
  library(pool)
  #odbc for database connections
  library(odbc)
  #tidyverse for data manipulations
  library(tidyverse)
  #shinythemes for colors
  library(shinythemes)
  #lubridate to work with dates
  library(lubridate)
  #shinyjs() to use easy java script functions
  library(shinyjs)
  #DT for datatables
  library(DT)
  #reactable for reactable tables
  library(reactable)

#0.1: database connection and global options --------

  #set default page length for datatables
  options(DT.options = list(pageLength = 15))
  
  #set db connection
  #using a pool connection so separate connnections are unified
  #gets environmental variables saved in local or pwdrstudio environment
  poolConn <- dbPool(odbc(), dsn = "mars_testing", uid = Sys.getenv("shiny_uid"), pwd = Sys.getenv("shiny_pwd"))
  
  #disconnect from db on stop 
  onStop(function(){
    poolClose(poolConn)
  })
  
  #js warning about leaving page
  jscode <- 'window.onbeforeunload = function() { return "Please use the button on the webpage"; };'
  
#0.2 source scripts.  ----
  #each script contains a module, which includes UI and server code
  source("collection_calendar.R")
  source("add_ow.R")
  source("add_sensor.R")
  source("deploy.R")
  source("srt.R")
  source("porous_pavement.R")
  source("capture_efficiency.R")
  source("inlet_conveyance.R")
  source("special_investigations.R")
  source("monitoring_stats.R")
  source("history.R")
  source("documentation.R")

  
#1: UI FUNCTION -----
  #initialize variables for UI and call all UI functions
  #call all the UI functions
  
  ui <- function(req){
    
    #1.1: load required variables -----
      #define global variables that will be required each time the UI runs
      
      #query site names (non SMP)
      site_name_query <- "select * from fieldwork.site_name_lookup"
      site_names <- odbc::dbGetQuery(poolConn, site_name_query) %>% 
        dplyr::arrange(site_name) %>% 
        dplyr::pull()
      
      #Sensor Model Number options
      hobo_options <- c("", "U20-001-01", "U20-001-04", "U20L-01", "U20L-04")
      
      sensor_status_lookup <- dbGetQuery(poolConn, "select * from fieldwork.sensor_status_lookup order by sensor_status_lookup_uid")
      
      sensor_issue_lookup <- dbGetQuery(poolConn, "select * from fieldwork.sensor_issue_lookup order by sensor_issue_lookup_uid")
      
      #Sensor Serial Number List
      hobo_list_query <-  "select inv.sensor_serial, inv.sensor_model, inv.date_purchased, 
      ow.smp_id, ow.ow_suffix from fieldwork.inventory_sensors inv
                          left join fieldwork.deployment d on d.inventory_sensors_uid = inv.inventory_sensors_uid AND d.collection_dtime_est is NULL
                            left join fieldwork.ow ow on ow.ow_uid = d.ow_uid"
      hobo_list <- odbc::dbGetQuery(poolConn, hobo_list_query)
      sensor_serial <- hobo_list$sensor_serial
      
      #Deployment purpose lookup table
      deployment_lookup <- dbGetQuery(poolConn, "select * from fieldwork.deployment_lookup")
      
      #long term lookup types
      long_term_lookup <- dbGetQuery(poolConn, "select * from fieldwork.long_term_lookup")
      
      #research types
      research_lookup <- dbGetQuery(poolConn, "select * from fieldwork.research_lookup")
      
      #field test priority
      priority <- dbGetQuery(poolConn, "select * from fieldwork.field_test_priority_lookup")
      
      #srt_types & construction phase types
      srt_types <- dbGetQuery(poolConn, "select * from fieldwork.srt_type_lookup")
      con_phase <- dbGetQuery(poolConn, "select * from fieldwork.con_phase_lookup")
      
      #porous pavement surface types
      surface_type <- dbGetQuery(poolConn, "select * from fieldwork.surface_type_lookup")
      # 
      # #capture efficiency high flow types
      high_flow_type <- dbGetQuery(poolConn, "select * from fieldwork.est_high_flow_efficiency_lookup")
      
      #capture efficiency asset types 
      cet_asset_type <- dbGetQuery(poolConn, "select distinct asset_type from smpid_facilityid_componentid_inlets_limited where component_id is not null order by asset_type")   
      
      #this function adds a little red star to indicate that a field is required. It uses HTML, hence "html_req"
      html_req <- function(label){
        HTML(paste(label, tags$span(style="color:red", tags$sup("*"))))
      }
      
      #this function adds a blue dagger to indicate that a field is required for future tests. It uses HTML. it is slightly Christian
      future_req <- function(label){
        HTML(paste(label, tags$span(style="color:blue", tags$sup("†"))))
      }
      
      #monitoring stats
      #get the current fiscal year and list of all years from start of data (2012) to now
      current_fy <- lubridate::today() %m+% months(6) %>% year()
      start_fy <- 2012
      years <- start_fy:current_fy %>% sort(decreasing = TRUE)
      
      #project work numbers
      work_number <- dbGetQuery(poolConn, "select distinct worknumber from greenit_projectbestdata") %>% pull()
      
      #special investigation types
      si_lookup <- dbGetQuery(poolConn, "select * from fieldwork.special_investigation_lookup")
      
      #"requested by" lookup for special investigations
      requested_by_lookup <- dbGetQuery(poolConn, "select * from fieldwork.requested_by_lookup")
    
    # 1.2: actual UI------------------------
    
      #use tagList so tags and shinyjs can be called without being inside of the navbarPage. When they're inside navbarpage, they create small invisible fake tabs that take up space and act weird when clicked on
      tagList(
        #call jscode to warn when leaving page
        tags$head(tags$script(jscode)),
        #must call useShinyjs() for shinyjs() functionality to work in app
        useShinyjs(),
        navbarPage("Fieldwork", theme = shinytheme("cerulean"), id = "inTabset",
                   #do.call is needed to use a list with appended UI functions with navbarMenu
                   #this is so a navbarMenu (dropdown) can consist of tabs from different modules
                   #this navbarMenu has both tabs from collection calendar (cc & future deployments) and "Deploy Sensor"
                    do.call(navbarMenu, list(title = "Deployments") %>%
                         append(collection_calendarUI("collection_calendar")) %>%
                         append(deployUI("deploy", sensor_serial = sensor_serial, site_names = site_names,
                                         html_req = html_req, long_term_lookup = long_term_lookup, deployment_lookup = deployment_lookup,
                                         research_lookup = research_lookup, priority = priority, future_req = future_req))),
                   #Add/Edit Location
                    add_owUI("add_ow", site_names = site_names, html_req = html_req),
                   #Add/Edit Sensor
                    add_sensorUI("add_sensor", hobo_options = hobo_options, html_req = html_req,
                                 sensor_status_lookup = sensor_status_lookup,
                                 sensor_issue_lookup = sensor_issue_lookup),
                   #SRT (Add/Edit SRT, View SRTs, View Future SRTs)
                    SRTUI("srt", srt_types = srt_types, html_req = html_req,
                          con_phase = con_phase, priority = priority, future_req = future_req),
                   #Porous Pavement (Add/Edit Porous Pavement Test, View Porous Pavement Tests, View Future Porous Pavement Tests)
                    porous_pavementUI("porous_pavement", html_req = html_req,
                                    surface_type = surface_type, con_phase = con_phase, priority = priority, future_req = future_req),
                   #Capture Efficiency (Add/Edit Capture Efficiency Test, View Capture Efficiency Tests, View Future Capture Efficiency Tests)
                    capture_efficiencyUI("capture_efficiency", high_flow_type = high_flow_type,
                                       html_req = html_req, con_phase = con_phase, priority = priority,
                                       future_req = future_req, cet_asset_type = cet_asset_type),
                   #Inlet Conveyance (Add/Edit Inlet Conveyance Test, View Inlet Conveyance Tests, View Future Inlet Conveyance Tests)
                    inlet_conveyanceUI("inlet_conveyance", work_number = work_number, html_req = html_req,
                                       con_phase = con_phase, priority = priority, site_names = site_names, future_req = future_req),
                   #Special Investigations (Add/Edit Special Investigations, View Special Investigations, View Future Special Investigations)
                    special_investigationsUI("special_investigations", work_number = work_number, html_req = html_req,
                                             con_phase = con_phase, priority = priority, site_names = site_names, si_lookup = si_lookup,
                                             requested_by_lookup = requested_by_lookup, future_req = future_req),
                   #Stats
                   m_statsUI("stats", current_fy = current_fy, years = years),
                   #Monitoring History 
                   historyUI("history"),
                   #Documentation
                 documentationUI("documentation")
      )
    )
  }

# 2: server function ----
  #call modules, referencing the UI names above. These are functions, so any data originating outside the function needs to be named as an argument, whether it is lookup data, or from another tab. Modules need to be assigned to variables so they can be used in other module functions. 
  server <- function(input, output, session) {
    
    # 2.1: required variables -----
    #define global variables that will be defined each time server runs
    #query all SMP IDs
    smp_id <- odbc::dbGetQuery(poolConn, paste0("select distinct smp_id from smpid_facilityid_componentid")) %>% 
      dplyr::arrange(smp_id) %>% 
      dplyr::pull()
    
    pp_smp_id <- odbc::dbGetQuery(poolConn, paste0("select distinct smp_id from smpid_facilityid_componentid where asset_type = 'Permeable Pavement'")) %>% 
      dplyr::arrange(smp_id) %>% 
      dplyr::pull()
    
    sys_id <- odbc::dbGetQuery(poolConn, paste0("select distinct system_id from smpid_facilityid_componentid")) %>% 
      dplyr::arrange(system_id) %>% 
      dplyr::pull()
    
    #Sensor Model Number options
    sensor_status_lookup <- dbGetQuery(poolConn, "select * from fieldwork.sensor_status_lookup order by sensor_status_lookup_uid")
    
    sensor_issue_lookup <- dbGetQuery(poolConn, "select * from fieldwork.sensor_issue_lookup order by sensor_issue_lookup_uid")
    
    #Deployment purpose lookup table
    deployment_lookup <- dbGetQuery(poolConn, "select * from fieldwork.deployment_lookup")
    
    #srt_types & con phase
    srt_types <- dbGetQuery(poolConn, "select * from fieldwork.srt_type_lookup")
    con_phase <- dbGetQuery(poolConn, "select * from fieldwork.con_phase_lookup")
    
    #porous pavement surface types
    surface_type <- dbGetQuery(poolConn, "select * from fieldwork.surface_type_lookup")
    # 
    # #capture efficiency high flow types
    high_flow_type <- dbGetQuery(poolConn, "select * from fieldwork.est_high_flow_efficiency_lookup")
    
    #capture efficiency asset types 
    cet_asset_type <- dbGetQuery(poolConn, "select distinct asset_type from smpid_facilityid_componentid_inlets_limited where component_id is not null order by asset_type")
    
    #monitoring stats
    current_fy <- lubridate::today() %m+% months(6) %>% year()
    
    #special investigation types
    si_lookup <- dbGetQuery(poolConn, "select * from fieldwork.special_investigation_lookup")
    
    #request by lookup
    requested_by_lookup <- dbGetQuery(poolConn, "select * from fieldwork.requested_by_lookup")
    
    #refresh starter 
    refresh_location <- 0
    
    # 2.2: Server Module functions ---------------------------
    # Collection Calendar
    collection_cal <- collection_calendarServer("collection_calendar", parent_session = session,
                                 ow = ow, deploy = deploy, poolConn = poolConn)
    #Add Edit/Location
    ow <- add_owServer("add_ow", parent_session = session, smp_id = smp_id, poolConn = poolConn, deploy = deploy)
    #Add Edit/Sensor
    sensor <- add_sensorServer("add_sensor", parent_session = session, poolConn = poolConn,
                         sensor_status_lookup = sensor_status_lookup, deploy = deploy,
                         sensor_issue_lookup = sensor_issue_lookup)
    #Deploy Sensor
    deploy <- deployServer("deploy", parent_session = session, ow = ow, collect = collection_cal,
                         sensor = sensor, poolConn = poolConn, deployment_lookup = deployment_lookup,
                         srt = srt, si = special_investigations, cwl_history = cwl_history, smp_id = smp_id)
    #SRT
    srt <- SRTServer("srt", parent_session = session, poolConn = poolConn, 
                     srt_types = srt_types, con_phase = con_phase, sys_id = sys_id)
    #Porous Pavement
    porous_pavement <- porous_pavementServer("porous_pavement", parent_session = session, surface_type = surface_type,
                                  poolConn = poolConn, con_phase = con_phase, smp_id = pp_smp_id)
    #Capture Efficiency
    capture_efficiency <- capture_efficiencyServer("capture_efficiency", parent_session = session,
                                     poolConn = poolConn, high_flow_type = high_flow_type, con_phase = con_phase,
                                     cet_asset_type = cet_asset_type, deploy = deploy, sys_id = sys_id)
    #Inlet Conveyance
    inlet_conveyance <- inlet_conveyanceServer("inlet_conveyance", parent_session = session, poolConn = poolConn, con_phase = con_phase, sys_id =sys_id)
    #Special Investigations
    special_investigations<- special_investigationsServer("special_investigations", parent_session = session,
                                        poolConn = poolConn, con_phase = con_phase, si_lookup = si_lookup,
                                        requested_by_lookup = requested_by_lookup, sys_id = sys_id)
    #Stats
    stats <- m_statsServer("stats", parent_session = session, current_fy = current_fy, poolConn = poolConn)
    #History
    cwl_history <- cwl_historyServer("history", parent_session = session, poolConn = poolConn, deploy = deploy)
    
  }

#Run this function to run the app!
shinyApp(ui, server)