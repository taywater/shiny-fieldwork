#Add Observation Well tab
#This page is for the user to add an observation well to fieldwork.ow, by finding a component, and adding an observation well suffix
#User can also add well measurements

add_owUI <- function(id, label = "add_ow", smp_id, site_names, html_req){
  ns <- NS(id)
  tabPanel(title = "Add Location", value = "add_ow",  
           titlePanel("Add Monitoring Location"),
           fluidRow(
             column(width = 4, 
             #put 
             sidebarPanel(width = 12,
               selectInput(ns("at_smp"), html_req("At SMP?"), choices = c("", "Yes" = 1, "No" = 2), selected = NULL),
               #conditional specific to smp
             conditionalPanel(condition = "input.at_smp == 1", 
                              ns = ns, 
                             selectInput(ns("smp_id"), html_req("SMP ID"), choices = c("", smp_id), selected = NULL)),
             #conditional specific to site
             conditionalPanel(condition = "input.at_smp == 2", 
                              ns = ns, 
                              checkboxInput(ns("need_new_site"), "Add a New Site?"),
                              conditionalPanel(condition = "input.need_new_site", 
                                               ns = ns, 
                                               textInput(ns("new_site_name"), "New Site Name"), 
                                               actionButton(ns("add_site_name"), "Add New Site Name")),
                              selectInput(ns("site_name"), html_req("Site Name"), choices = c("", site_names), selected = NULL)), 
                              #textInput(ns("location"), html_req("Location")), 
                              #actionButton(ns("add_non_smp_ow"), "Add New")),
             #conditional for smp or site
             conditionalPanel(condition = "input.at_smp == 1 || input.at_smp == 2", 
                              ns = ns, 
                              fluidRow(
                                column(8, selectInput(ns("component_id"), html_req("Component ID"), choices = c("", ""))),
                                column(4, textInput(ns("ow_suffix"), html_req("Location"))))), 
             #conditional specific to smp 
             conditionalPanel(condition = "input.at_smp == 1", 
                              ns = ns, 
                              disabled(textInput(ns("facility_id"), html_req("Facility ID"))),
                              actionButton(ns("add_ow"), "Add New")),
             #conditional specific to site
             conditionalPanel(condition = "input.at_smp == 2", 
                              ns = ns, 
                              actionButton(ns("add_non_smp_ow"), "Add New"))
             ),
             #conditional for neither smp or site, but for moving a site to an smp
             conditionalPanel(condition = "input.at_smp != 1 && input.at_smp != 2", 
                                           ns = ns, 
                              sidebarPanel(
                                width = 12, 
                                checkboxInput(ns("convert"), "Move locations from a Site to an SMP?"), 
                                conditionalPanel(condition = "input.convert == 1",
                                                 ns = ns,
                                                 selectInput(ns("old_site_name"), html_req("Site Name"), choices = c("", site_names), selected = NULL),
                                                 selectInput(ns("new_smp_id"), html_req("SMP ID"), choices = c("", smp_id), selected = NULL),
                                                 actionButton(ns("convert_wells"), "Move Locations from Site to SMP")
                                )
                                            )),
                          
             #break well measurements into a lower sidebar
             sidebarPanel(width = 12,  #----
                numericInput(ns("well_depth"), html_req("Well Depth (ft)"), value = NULL),
               fluidRow(
                 column(6, selectInput(ns("sensor_one_in"), html_req("Sensor Installed 1\" off Bottom?"), choices = c("", "Yes" = "1", "No" = "0"), selected = NULL)),
                 column(6, selectInput(ns("weir"), html_req("Is there a Weir?"), choices = c("", "Yes" = "1", "No" = "0"), selected = NULL))),
               #show panel if the sensor is NOT installed 1" off bottom
               conditionalPanel("input.sensor_one_in == \"0\"", 
                                ns = ns, 
                                fluidRow(
                                  column(6, numericInput(ns("cth"), "Cap-to-Hook (ft)", value = NULL)), 
                                  column(6, numericInput(ns("hts"), "Hook-to-Sensor (ft)", value = NULL))
                                )),
               #disable this field, auto calculate it later
               disabled(numericInput(ns("install_height"), "Installation Height (ft)", value = NULL)),
               #show panel if there is a weir
               conditionalPanel("input.weir == \"1\"", 
                                ns = ns, 
                                fluidRow(
                                  column(6, numericInput(ns("ctw"), "Cap-to-Weir (ft)", value = NULL)), 
                                  column(6, numericInput(ns("cto"), "Cap-to-Orifice (ft)", value = NULL))),
                                fluidRow(
                                  #disable these fields, auto calculate later
                                  column(4, disabled(numericInput(ns("wts"), "Weir-to-Sensor (ft)", value = NULL))), 
                                  column(4, disabled(numericInput(ns("wto"), "Weir-to-Orifice (ft)", value = NULL))), 
                                  column(4, disabled(numericInput(ns("ots"), "Orifice-to-Sensor (ft)", value = NULL))))
                                ),
               fluidRow(
                 column(6, dateInput(ns("start_date"), "Initial Deployment Date", value = as.Date(NA))), 
                 column(6, dateInput(ns("end_date"), "Sensor Removal Date", value = as.Date(NA)))
               ),
               actionButton(ns("add_well_meas"), "Add Well Measurements"),
               actionButton(ns("add_ow_deploy"), "Deploy Sensor at this SMP"),
               actionButton(ns("clear_ow"), "Clear All Fields")
             )),
             column(width = 8, 
                    #show the table if an smp_id is selected
               conditionalPanel(condition = "input.smp_id || input.site_name",
                 ns = ns, 
                h4(textOutput((ns("header")))),
               DTOutput(ns("ow_table"))), 
           ))
  )
  
}

add_ow <- function(input, output, session, parent_session, smp_id, poolConn) {
  
  #define ns to use in modals
  ns <- session$ns
  
  #first part - SMP #####
  #read in ow prefixes and names
  ow_prefixes_db <- dbGetQuery(poolConn, "select * from fieldwork.ow_prefixes") %>% rename("asset_type" = "ow_name")
  new_and_ex_wells <- ow_prefixes_db %>% dplyr::select(ow_prefix, asset_type) 
  new_wells <- ow_prefixes_db %>% dplyr::filter(componentless == 1) %>% dplyr::select(ow_prefix,asset_type)
  
  #Add Observation Well  --
  #set component IDs based on SMP ID
  component_and_asset_query <- reactive(paste0(
    "select distinct component_id, asset_type from smpid_facilityid_componentid where smp_id = '", input$smp_id, "' 
        AND component_id IS NOT NULL"))
  component_and_asset <- reactive(odbc::dbGetQuery(poolConn, component_and_asset_query()))
  
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
  rv <- reactiveValues()
  ow_view_query <- reactive(paste0("SELECT * FROM fieldwork.ow_plus_measurements WHERE smp_id = '", input$smp_id, "' OR site_name = '", input$site_name, "'"))
  rv$ow_view_db <- reactive(odbc::dbGetQuery(poolConn, ow_view_query()))
  rv$ow_table <- reactive(if(nchar(input$smp_id) > 0){
    rv$ow_view_db() %>% 
          dplyr::select("smp_id", "ow_suffix", "installation_height_ft", "deployment_depth_ft", "start_dtime_est", "end_dtime_est") %>% 
          mutate_at(c("start_dtime_est", "end_dtime_est"), as.character) %>% 
          dplyr::rename("SMP ID" = "smp_id", "Location" = "ow_suffix", "Installation Height (ft)" = "installation_height_ft", 
                        "Deployment Depth (ft)" = "deployment_depth_ft", "Start Date" = "start_dtime_est", "End Date" = "end_dtime_est")
  }else{
    rv$ow_view_db() %>% 
      dplyr::select("site_name", "ow_suffix", "installation_height_ft", "deployment_depth_ft", "start_dtime_est", "end_dtime_est") %>% 
      mutate_at(c("start_dtime_est", "end_dtime_est"), as.character) %>% 
      dplyr::rename("Site Name" = "site_name", "Location" = "ow_suffix", "Installation Height (ft)" = "installation_height_ft", 
                    "Deployment Depth (ft)" = "deployment_depth_ft", "Start Date" = "start_dtime_est", "End Date" = "end_dtime_est")}
  )
  #get the existing ow_prefixes. these will be counted and used to suggest and OW Suffix
  rv$existing_ow_prefixes <- reactive(gsub('\\d+', '', rv$ow_view_db()$ow_suffix))
  
  #set a counter to be incremented, and send it to collection calendar to refresh it whenever a well is edited
  rv$counter <- 0 
  
  #Get the Project name, combine it with SMP ID, and create a reactive header
  rv$smp_and_name_step <- reactive(odbc::dbGetQuery(poolConn, paste0("select smp_id, project_name from project_names where smp_id = '", input$smp_id, "'")))
  
  rv$smp_and_name <- reactive(paste(rv$smp_and_name_step()$smp_id[1], rv$smp_and_name_step()$project_name[1]))
  
  rv$header <- reactive(if(nchar(input$smp_id) > 0){
    paste("Observation Wells at", rv$smp_and_name())
  }else{
    paste("Observation Wells at", input$site_name)
  })

  
  output$header <- renderText(
    rv$header()
  )
  
  #render ow table
  #allow for only one selection; rename columns
  #'t' means only show the table, no search bar, etc
  output$ow_table <- renderDT(
    rv$ow_table(), 
    selection = 'single',
    style = 'bootstrap', 
    class = 'table-responsive, table-hover', 
    options = list(dom = 't')
  )
  
  rv$asset_comp_code_click <- 0
  
  #add info from table to selectboxes, on click
  #need to reverse engineer the asset/ow/component combo
  observeEvent(input$ow_table_rows_selected,{ 
    
    if(input$at_smp == 1){
      #get facility id from table
      rv$fac <- rv$ow_view_db()[input$ow_table_rows_selected, 5]
      #get component id
      comp_id_query <- paste0("select distinct component_id from smpid_facilityid_componentid where facility_id = '", rv$fac, "' 
          AND component_id IS NOT NULL")
      comp_id_step <- odbc::dbGetQuery(poolConn, comp_id_query) %>% pull()
      #determine whether component id exists and is useful
      comp_id_click <- if(length(comp_id_step) > 0) comp_id_step else "NA"
      #get ow prefix, to sort by asset type
      ow_prefix_click <- gsub('\\d+', '', rv$ow_view_db()[input$ow_table_rows_selected, 4])
      #get asset type - base on component id (if exists), then base on prefix if not. 
      #need to do both to check for OW (fittings/obs wells)
      asset_type_click <- if(nchar(comp_id_click) > 2){
        dplyr::filter(asset_comp(), component_id == comp_id_click) %>% select(asset_type) %>% pull()
      }else{
        new_and_ex_wells %>% dplyr::filter(ow_prefix == ow_prefix_click) %>% dplyr::select(asset_type) %>% pull()
      }
      #combine asset type, ow, and component id
      rv$asset_comp_code_click = paste(comp_id_click, ow_prefix_click, asset_type_click,  sep = " | ")
      updateSelectInput(session, "component_id", selected = rv$asset_comp_code_click)
    }else{
      ow_prefix_click <- gsub('\\d+', '', rv$ow_view_db()[input$ow_table_rows_selected, 4])
      asset_type_click <- new_and_ex_wells %>%  dplyr::filter(ow_prefix == ow_prefix_click) %>% dplyr::select(asset_type) %>% pull()
      
      rv$asset_comp_code_click = paste("NA", ow_prefix_click, asset_type_click, sep = " | ")
      updateSelectInput(session, "component_id", selected = rv$asset_comp_code_click)
    }
    
    #update well depth based on selected row
    rv$well_depth_edit <- rv$ow_view_db()[input$ow_table_rows_selected, 8]
    updateNumericInput(session, "well_depth", value= rv$well_depth_edit)
    
    #update sensor 
    rv$sensor_one_inch_edit <- rv$ow_view_db()[input$ow_table_rows_selected, 11]
    updateSelectInput(session, "sensor_one_in", selected = rv$sensor_one_inch_edit)
    
    #update weir 
    rv$weir_edit <- rv$ow_view_db()[input$ow_table_rows_selected, 12]
    updateSelectInput(session, "weir", selected = rv$weir_edit)
    
    #get measurements from db table
    rv$cth_edit <- rv$ow_view_db()[input$ow_table_rows_selected, 13]
    rv$hts_edit <- rv$ow_view_db()[input$ow_table_rows_selected, 14]
    rv$ctw_edit <- rv$ow_view_db()[input$ow_table_rows_selected, 17]
    rv$cto_edit <- rv$ow_view_db()[input$ow_table_rows_selected, 18]
    rv$wto_edit <- rv$ow_view_db()[input$ow_table_rows_selected, 19]

    #udpate inputs in app with values from table
    updateNumericInput(session, "cth", value = rv$cth_edit)
    updateNumericInput(session, "hts", value = rv$hts_edit)
    updateNumericInput(session, "ctw", value = rv$ctw_edit)
    updateNumericInput(session, "cto", value = rv$cto_edit)
    updateNumericInput(session, "wto", value = rv$wto_edit)
    
    #get dates from table
    rv$start_date_edit <- rv$ow_view_db()[input$ow_table_rows_selected, 9]
    rv$end_date_edit <- rv$ow_view_db()[input$ow_table_rows_selected, 10]
    
    #update inputs in app with values from table
    updateDateInput(session, "start_date", value = rv$start_date_edit)
    updateDateInput(session, "end_date", value = rv$end_date_edit)
    
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
    if(select_ow_prefix() %in% new_wells$ow_prefix) odbc::dbGetQuery(poolConn, paste0(
      "SELECT facility_id FROM smpid_facilityid_componentid WHERE component_id IS NULL AND smp_id = '", input$smp_id, "'"))[1,1] else 
        odbc::dbGetQuery(poolConn, paste0(
          "SELECT facility_id from smpid_facilityid_componentid WHERE component_id = '", select_component_id(), "'"))[1,1]
  }else{
    ""
  }
  )
  
  #udpate facility id shown based on above
  observe(updateTextInput(session, "facility_id", value = facility_id()))
  
  #count how many existing observation wells of the selected type already exist at the SMP
  rv$well_count <- reactive(length(rv$existing_ow_prefixes()[rv$existing_ow_prefixes() == select_ow_prefix()]))
  
  #OW + (Count + 1) for suggested name of the new well 
  rv$ow_suggested_pre <- reactive(paste0(select_ow_prefix(), (rv$well_count() + 1))) 
  
  #only show suggested suffix if there is a selected component id
  rv$ow_suggested <- reactive(if(nchar(input$component_id) == 0){
    NA
  }else if(length(input$ow_table_rows_selected) > 0 & rv$asset_comp_code_click == select_combo_row()$asset_comp_code){
    rv$ow_view_db()[input$ow_table_rows_selected, 4]
  }else{
    rv$ow_suggested_pre()
  })
  
  #only update ow_suffix when component ID changes, or when a table row is selected or deselected. 
  #udpateTextInput was previously in an observe(), but if OW Suffix was populated, changing SMP ID would cause the app to crash 
  #this was likely because there was a blank instance, which led to ow_suffix trying to populate off of an item of length zero
  #the "== 0" was added so that the observeEvents also trigger when a row is deselected
  
  observeEvent(input$component_id, {
    updateTextInput(session, "ow_suffix", value = rv$ow_suggested())
  })
  
  observeEvent(input$ow_table_rows_selected == 0, {
    updateTextInput(session, "ow_suffix", value = rv$ow_suggested())
  })
  
  rv$toggle_suffix <- reactive(!(input$ow_suffix %in% rv$ow_view_db()$ow_suffix) | length(input$ow_table_rows_selected) > 0)
  
  #toggle state (enable/disable) buttons based on whether smp id, component id, and ow suffix are selected (this is shinyjs)
  observe({toggleState(id = "add_ow", condition = nchar(input$smp_id) > 0 & nchar(input$component_id) > 0 & nchar(input$ow_suffix) >0 &
                         rv$toggle_suffix())})
  
  #Add ow
  rv$add_ow_label <- reactive(if(length(input$ow_table_rows_selected) == 0) "Add New Location" else "Edit Selected Location")
  observe(updateActionButton(session, "add_ow", label = rv$add_ow_label()))
  
  observe({toggleState(id = "add_ow_deploy", nchar(input$smp_id) > 0)})
  
  rv$ow_suffix <- reactive(gsub('\'', '\'\'', input$ow_suffix))
  
  #Write to database (fieldwork.ow) when "add_ow" button is clicked
  #update if editing
  observeEvent(input$add_ow, {
    if(length(input$ow_table_rows_selected) == 0){
      odbc::dbGetQuery(poolConn, paste0(
        "INSERT INTO fieldwork.ow_all (smp_id, ow_suffix, facility_id) 
  	      VALUES ('", input$smp_id, "','", rv$ow_suffix(), "','",  facility_id(), "')"
      ))
    }else{
      edit_ow_query <- paste0(
        "UPDATE fieldwork.ow_all SET ow_suffix = '", rv$ow_suffix(), "', facility_id = '", facility_id(), "' 
        WHERE ow_uid = '", rv$ow_view_db()[input$ow_table_rows_selected, 1], "'")
      dbGetQuery(poolConn, edit_ow_query)
    }
    #update ow_table with new well
    rv$ow_view_db <- reactive(odbc::dbGetQuery(poolConn, ow_view_query()))
    #upon click update the smp_id in the Deploy Sensor tab
    #it needs to switch to NULL and then back to the input$smp_id to make sure the change is registered
    #updateSelectInput(session, "smp_id_deploy", selected = "")
    #updateSelectInput(session, "smp_id_deploy", selected = input$smp_id)
    #reset ow_suffix to blank
    reset("ow_suffix")
    #reset component id to blank
    reset("component_id")
    #query collection table
    rv$counter <- rv$counter + 1
    #rv_collect$collect_table_db <- odbc::dbGetQuery(poolConn, collect_query)
  })
  
  #first part - non SMP----
  site_name_query <- "select * from fieldwork.site_name_lookup"
  rv$site_name_db <- reactive(odbc::dbGetQuery(poolConn, site_name_query))
  rv$site_names <- reactive(rv$site_name_db() %>% dplyr::select("site_name") %>% pull())
  
  rv$site_name_lookup_uid <- reactive(odbc::dbGetQuery(poolConn, paste0("select site_name_lookup_uid from fieldwork.site_name_lookup where site_name = '", input$site_name, "'")) %>% pull())

  observe(updateSelectInput(session, "site_name", choices = c("", rv$site_names())))

  rv$new_site_name <- reactive(gsub('\'', '\'\'', input$new_site_name))
  
  #rv$location <- reactive(gsub('\'', '\'\'', input$location))
  
  observe(toggleState(id = "add_site_name", condition = nchar(input$new_site_name) > 0))
  
  #rv$toggle_location <- reactive(!(input$location %in% rv$ow_view_db()$ow_suffix) | length(input$ow_table_rows_selected) > 0)
  
  #toggle state based on whether site name and location are entered
  observe({toggleState(id = "add_non_smp_ow", condition = nchar(input$site_name) > 0 & nchar(input$ow_suffix) > 0 &
                         rv$toggle_suffix())})
  
  #Add ow
  rv$add_location_label <- reactive(if(length(input$ow_table_rows_selected) == 0) "Add New Location" else "Edit Selected Location")
  observe(updateActionButton(session, "add_non_smp_ow", label = rv$add_location_label()))
  
  
  observeEvent(input$add_site_name, {
    add_site_name_query <- paste0("INSERT INTO fieldwork.site_name_lookup (site_name) VALUES ('", rv$new_site_name(), "')")
    odbc::dbGetQuery(poolConn, add_site_name_query)
    rv$site_name_db <- reactive(odbc::dbGetQuery(poolConn, site_name_query))
    
    updateTextInput(session, "new_site_name", value = "")
    
  })
  
  observeEvent(input$add_non_smp_ow, {
    if(length(input$ow_table_rows_selected) == 0){
      odbc::dbGetQuery(poolConn, paste0(
        "INSERT INTO fieldwork.ow_all (site_name_lookup_uid, ow_suffix)
        VALUES ('", rv$site_name_lookup_uid(), "','", rv$ow_suffix(), "')"
      ))
    }else{
      odbc::dbGetQuery(poolConn, paste0(
        "UPDATE fieldwork.ow_all SET ow_suffix = '", rv$ow_suffix(), "' WHERE ow_uid = '", rv$ow_view_db()[input$ow_table_rows_selected, 1], "'"
      ))
    }
    #update ow_table with new well
    rv$ow_view_db <- reactive(odbc::dbGetQuery(poolConn, ow_view_query()))
    reset("need_new_site")
    reset("location")
  })
  
  ##### second part --------
  
  observe(updateSelectInput(session, "old_site_name", choices = c("", rv$site_names())))
  
  observe(toggleState(id = "convert_wells", condition = nchar(input$old_site_name) > 1 & nchar(input$new_smp_id)))
  
  rv$old_site_name_lookup_uid <- reactive(odbc::dbGetQuery(poolConn, paste0("select site_name_lookup_uid from fieldwork.site_name_lookup where site_name = '", input$old_site_name, "'")) %>% pull())
  
  observeEvent(input$convert_wells, {
    
    rv$new_fac_id <- odbc::dbGetQuery(poolConn, paste0("SELECT facility_id FROM smpid_facilityid_componentid WHERE component_id IS NULL AND smp_id = '", input$new_smp_id, "'"))[1,1]
    
    convert_query <- paste0("UPDATE fieldwork.ow_all SET smp_id = '", input$new_smp_id, "', facility_id = '", rv$new_fac_id, "', site_name_lookup_uid = NULL WHERE site_name_lookup_uid = '", rv$old_site_name_lookup_uid(), "'; delete from fieldwork.site_name_lookup where site_name_lookup_uid = '", rv$old_site_name_lookup_uid(), "'")
    
    odbc::dbGetQuery(poolConn, convert_query)
    
    rv$site_name_db <- reactive(odbc::dbGetQuery(poolConn, site_name_query))
    
  })
  
  ###### third part -------
  #update installation height to show in UI
  rv$install_height <- reactive(if(input$sensor_one_in == 1){
    1/12
  }else{
    input$well_depth - (input$cth+input$hts)
  })
  observe(updateNumericInput(session, "install_height", value = rv$install_height()))
  
  
  #update weir to sensor values depending on sensor height
  rv$wts <- reactive(if(input$sensor_one_in == "1"){
    input$well_depth_ft - 1/12 - input$ctw
  }else{
    input$cth + input$hts - input$ctw
  })
  
  #update weir to orifice
  rv$wto <- reactive(input$cto-input$ctw)
  
  #udpate orifice to sensor
  rv$ots <- reactive(if(input$sensor_one_in == "1"){
    input$well_depth_ft - 1/12 - input$cto
  }else{
    input$cth+input$hts - input$cto
  })
  
  #toggle state for "add/edit well measurement"
  observe(toggleState("add_well_meas", condition = nchar(input$weir) > 0 & (nchar(input$smp_id) > 0 | nchar(input$site_name) > 0) & (nchar(input$component_id) > 0 | input$at_smp == 2) & (nchar(input$ow_suffix) > 0) &
            rv$toggle_suffix() & nchar(input$sensor_one_in) > 0))
  
  observe(updateNumericInput(session, "wts", value = rv$wts()))
  observe(updateNumericInput(session, "wto", value = rv$wto()))
  observe(updateNumericInput(session, "ots", value = rv$ots()))
  
  #set to NULL if blank 
  rv$start_date <- reactive(if(length(input$start_date) == 0) "NULL" else paste0("'", input$start_date, "'"))
  rv$end_date <- reactive(if(length(input$end_date) == 0) "NULL" else paste0("'", input$end_date, "'"))
  rv$cth <- reactive(if(is.na(input$cth)) "NULL" else paste0("'", input$cth, "'"))
  rv$hts <- reactive(if(is.na(input$hts)) "NULL" else paste0("'", input$hts, "'"))
  rv$ctw <- reactive(if(is.na(input$ctw)) "NULL" else paste0("'", input$ctw, "'"))
  rv$cto <- reactive(if(is.na(input$cto)) "NULL" else paste0("'", input$cto, "'"))
  #rv$weir <- reactive(if(is.na(input$weir)) "NULL" else paste0("'", input$weir, "'"))
  #query well measurements at each oW from db
  #if there aren't any, and a row is selected, Add the measurement
  #if there are, and the row is selected, edit measurement
  
  rv$well_measurements_at_ow_uid <- reactive(
    if(nchar(input$smp_id) > 0 & nchar(rv$ow_suffix()) > 0){
      odbc::dbGetQuery(poolConn, paste0("select well_measurements_uid from fieldwork.ow_plus_measurements 
                                                   where smp_id = '", input$smp_id, "' and ow_suffix = '", rv$ow_suffix(), "' and
                                      well_measurements_uid is not null")) %>% pull()
    }else if(nchar(input$site_name) > 0 & nchar(rv$ow_suffix()) > 0){
      odbc::dbGetQuery(poolConn, paste0("select well_measurements_uid from fieldwork.ow_plus_measurements 
                                                   where site_name = '", input$site_name, "' and ow_suffix = '", rv$ow_suffix(), "' and
                                      well_measurements_uid is not null")) %>%  pull()
    }
  )
  
  #add well measurement
  rv$add_meas_label <- reactive(if(length(input$ow_table_rows_selected) == 0 | length(rv$well_measurements_at_ow_uid()) == 0) "Add Well Measurements" else "Edit Selected Well Measurements")
  observe(updateActionButton(session, "add_well_meas", label = rv$add_meas_label()))
  
  observe(updateDateInput(session, "end_date", min = input$start_date))
  
  #write to table
  observeEvent(input$add_well_meas, {
    if(length(input$ow_table_rows_selected) == 0 | length(rv$well_measurements_at_ow_uid()) == 0){
      if(input$at_smp == 1){
      odbc::dbGetQuery(poolConn, paste0(
        "INSERT INTO fieldwork.well_measurements (ow_uid, well_depth_ft, start_dtime_est, end_dtime_est, 
                                                  sensor_one_inch_off_bottom, cap_to_hook_ft, hook_to_sensor_ft, 
                                                  cap_to_weir_ft, cap_to_orifice_ft, weir)
        VALUES(fieldwork.get_ow_uid('", input$smp_id, "', '", rv$ow_suffix(), "', NULL), '", input$well_depth, "', ", rv$start_date(), ", ", rv$end_date(), ", 
        '", input$sensor_one_in, "', ", rv$cth(), ", ", rv$hts(), ", ", rv$ctw(), ", ", rv$cto(), ", '", input$weir, "')"
      ))
      }else{
        odbc::dbGetQuery(poolConn, paste0(
          "INSERT INTO fieldwork.well_measurements (ow_uid, well_depth_ft, start_dtime_est, end_dtime_est, 
                                                  sensor_one_inch_off_bottom, cap_to_hook_ft, hook_to_sensor_ft, 
                                                  cap_to_weir_ft, cap_to_orifice_ft, weir)
        VALUES(fieldwork.get_ow_uid(NULL, '", rv$ow_suffix(), "','", rv$site_name_lookup_uid(), "'), '", input$well_depth, "', ", rv$start_date(), ", ", rv$end_date(), ", 
        '", input$sensor_one_in, "', ", rv$cth(), ", ", rv$hts(), ", ", rv$ctw(), ", ", rv$cto(), ", '", input$weir, "')")
        )
      }
    }else{
      odbc::dbGetQuery(poolConn, paste0(
        "UPDATE fieldwork.well_measurements SET well_depth_ft = '", input$well_depth, "', 
        start_dtime_est = ", rv$start_date(), ", 
        end_dtime_est = ", rv$end_date(), ", 
        sensor_one_inch_off_bottom = '", input$sensor_one_in, "', 
        cap_to_hook_ft = ", rv$cth(), ", 
        hook_to_sensor_ft = ", rv$hts(), ", 
        cap_to_weir_ft = ", rv$ctw(), ", 
        cap_to_orifice_ft = ", rv$cto(), ", 
        weir = '", input$weir, "'
        WHERE ow_uid = '", rv$ow_view_db()[input$ow_table_rows_selected, 1], "'"))
    }
    #update table with edit
    rv$ow_view_db <- reactive(odbc::dbGetQuery(poolConn, ow_view_query()))
    reset("well_depth")
    reset("sensor_one_in")
    reset("weir")
    reset("cth")
    reset("hts")
    reset("ctw")
    reset("cto")
    reset("wts")
    reset("wto")
    reset("ots")
    reset("start_date")
    reset("end_date")
  })
  
  rv$refresh_deploy <- 0 
  
  #upon click of "Deploy Sensor at this SMP" update the smp_id in the Deploy Sensor tab, and switch tabs
  #it needs to switch to NULL and then back to the input$smp_id to make sure the change is registered
  observeEvent(input$add_ow_deploy, {
    #updateSelectInput(session, "smp_id_deploy", selected = "")
    #updateSelectInput(session, "smp_id_deploy", selected = input$smp_id)
    rv$refresh_deploy <- rv$refresh_deploy + 1
    updateTabsetPanel(session = parent_session, "inTabset", selected = "deploy_tab")
  })
  
  
  #clear all fields when switch from SMP to non SMP
  observeEvent(input$at_smp, {
    reset("ow_suffix")
    reset("component_id")
    reset("smp_id")
    reset("site_name")
    reset("add_site_name")
    reset("need_new_site")
    reset("new_site_name")
    reset("location")
    reset("well_depth")
    reset("sensor_one_in")
    reset("weir")
    reset("cth")
    reset("hts")
    reset("ctw")
    reset("cto")
    reset("wts")
    reset("wto")
    reset("ots")
    reset("start_date")
    reset("end_date")
  })
  
  #clear all fields
  #bring up dialogue box to confirm
  observeEvent(input$clear_ow, {
    showModal(modalDialog(title = "Clear All Fields", 
                          "Are you sure you want to clear all fields on this tab?", 
                          modalButton("No"), 
                          actionButton(ns("confirm_clear"), "Yes")))
  })
  
  observeEvent(input$confirm_clear, {
    reset("at_smp")
    reset("ow_suffix")
    reset("component_id")
    reset("smp_id")
    reset("site_name")
    reset("add_site_name")
    reset("need_new_site")
    reset("new_site_name")
    reset("location")
    reset("well_depth")
    reset("sensor_one_in")
    reset("weir")
    reset("cth")
    reset("hts")
    reset("ctw")
    reset("cto")
    reset("wts")
    reset("wto")
    reset("ots")
    reset("start_date")
    reset("end_date")
    removeModal()
  })
  
  #clear well measurements when smp_id changes
  
  observeEvent(input$smp_id, {
    reset("well_depth")
    reset("sensor_one_in")
    reset("weir")
    reset("cth")
    reset("hts")
    reset("ctw")
    reset("cto")
    reset("wts")
    reset("wto")
    reset("ots")
    reset("start_date")
    reset("end_date")
  })
  
  #return these to be sent as arguments to other tabs to tell them to refresh
  return(
    list(
      refresh_collect = reactive(rv$counter), 
      refresh_deploy = reactive(rv$refresh_deploy),
      site_name_db = reactive(rv$site_name_db()),
      smp_id = reactive(input$smp_id)
    )
  )
}