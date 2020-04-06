add_owUI <- function(id, label = "add_ow"){
  ns <- NS(id)
  tabPanel(title = "Add OW", value = "add_ow",  
           titlePanel("Add Observation Well"),
           sidebarPanel(
             selectInput(ns("smp_id"), html_req("SMP ID"), choices = c("", smp_id), selected = NULL),
             selectInput(ns("component_id"), html_req("Component ID"), choices = c("", "")),
             textInput(ns("ow_suffix"), html_req("OW Suffix")), 
             disabled(textInput(ns("facility_id"), html_req("Facility ID"))),
             actionButton(ns("add_ow"), "Add New"), 
             actionButton(ns("add_ow_deploy"), "Deploy Sensor at this SMP"),
             actionButton(ns("clear_ow"), "Clear All Fields")
           ),
           mainPanel(
             h4("Observation Wells at this SMP"), 
             DTOutput(ns("ow_table"))
           )
  )
}

add_ow <- function(input, output, session, parent_session) {
  
  #define ns to use in modals
  ns <- session$ns
  
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
  ow_table_query <- reactive(paste0("SELECT * FROM fieldwork.ow WHERE smp_id = '", input$smp_id, "'"))
  rv$ow_table <- reactive(odbc::dbGetQuery(poolConn, ow_table_query()))
  #get the existing ow_prefixes. these will be counted and used to suggest and OW Suffix
  rv$existing_ow_prefixes <- reactive(gsub('\\d+', '', rv$ow_table()$ow_suffix))
  
  rv$counter <- 0 
  
  #render ow table
  #allow for only one selection; rename columns
  #'t' means only show the table, no search bar, etc
  output$ow_table <- renderDT(
    rv$ow_table(), 
    selection = 'single',
    style = 'bootstrap', 
    class = 'table-responsive, table-hover', 
    colnames = c('OW UID', 'SMP ID', 'OW Suffix', 'Facility ID'), 
    options = list(dom = 't')
  )
  
  rv$asset_comp_code_click <- 0
  
  #add info from table to selectboxes, on click
  #need to reverse engineer the asset/ow/component combo
  observeEvent(input$ow_table_rows_selected,{ 
    
    #get facility id from table
    rv$fac <- (rv$ow_table()[input$ow_table_rows_selected, 4])
    #get component id
    comp_id_query <- paste0("select distinct component_id from smpid_facilityid_componentid where facility_id = '", rv$fac, "' 
        AND component_id IS NOT NULL")
    comp_id_step <- odbc::dbGetQuery(poolConn, comp_id_query) %>% pull()
    #determine whether component id exists and is useful
    comp_id_click <- if(length(comp_id_step) > 0) comp_id_step else "NA"
    #get ow prefix, to sort by asset type
    ow_prefix_click <- gsub('\\d+', '', rv$ow_table()[input$ow_table_rows_selected, 3])
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
    #updateSelectInput(session, "ow_suffix", selected = rv$ow_table()[input$ow_table_rows_selected, 3])
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
  observe(updateSelectInput(session, "facility_id", selected = facility_id()))
  
  #count how many existing observation wells of the selected type already exist at the SMP
  rv$well_count <- reactive(length(rv$existing_ow_prefixes()[rv$existing_ow_prefixes() == select_ow_prefix()]))
  
  #OW + (Count + 1) for suggested name of the new well 
  rv$ow_suggested_pre <- reactive(paste0(select_ow_prefix(), (rv$well_count() + 1))) 
  
  #only show suggested suffix if there is a selected component id
  rv$ow_suggested <- reactive(if(nchar(input$component_id) == 0){
    NA
  }else if(length(input$ow_table_rows_selected) > 0 & rv$asset_comp_code_click == select_combo_row()$asset_comp_code){
    rv$ow_table()[input$ow_table_rows_selected, 3]
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
  
  rv$toggle_suffix <- reactive(!(input$ow_suffix %in% rv$ow_table()$ow_suffix) | length(input$ow_table_rows_selected) > 0)
  
  #toggle state (enable/disable) buttons based on whether smp id, component id, and ow suffix are selected (this is shinyjs)
  observe({toggleState(id = "add_ow", condition = nchar(input$smp_id) > 0 & nchar(input$component_id) > 0 & nchar(input$ow_suffix) >0 &
                         rv$toggle_suffix())})#rv$toggle_component_id())})
  rv$label <- reactive(if(length(input$ow_table_rows_selected) == 0) "Add New" else "Edit Selected")
  observe(updateActionButton(session, "add_ow", label = rv$label()))
  observe({toggleState(id = "add_ow_deploy", nchar(input$smp_id) > 0)})
  
  rv$ow_suffix <- reactive(gsub('\'', '\'\'', input$ow_suffix))
  
  
  #Write to database when button is clicked
  #update if editing
  observeEvent(input$add_ow, {
    if(length(input$ow_table_rows_selected) == 0){
      odbc::dbGetQuery(poolConn, paste0(
        "INSERT INTO fieldwork.ow (smp_id, ow_suffix, facility_id) 
  	      VALUES ('", input$smp_id, "','", rv$ow_suffix(), "','",  facility_id(), "')"
      ))
    }else{
      edit_ow_query <- paste0(
        "UPDATE fieldwork.ow SET ow_suffix = '", rv$ow_suffix(), "', facility_id = '", facility_id(), "' 
        WHERE ow_uid = '", rv$ow_table()[input$ow_table_rows_selected, 1], "'")
      dbGetQuery(poolConn, edit_ow_query)
    }
    #update ow_table with new well
    rv$ow_table <- reactive(odbc::dbGetQuery(poolConn, ow_table_query()))
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
  
  rv$refresh_deploy <- 0 
  
  #upon click of "Deploy Sensor at this SMP" update the smp_id in the Deploy Sensor tab, and switch tabs
  #it needs to switch to NULL and then back to the input$smp_id to make sure the change is registered
  observeEvent(input$add_ow_deploy, {
    #updateSelectInput(session, "smp_id_deploy", selected = "")
    #updateSelectInput(session, "smp_id_deploy", selected = input$smp_id)
    rv$refresh_deploy <- rv$refresh_deploy + 1
    updateTabsetPanel(session = parent_session, "inTabset", selected = "deploy_tab")
  })
  
  observeEvent(input$clear_ow, {
    reset("ow_suffix")
    reset("component_id")
    reset("smp_id")
  })
  
  return(
    list(
      refresh_collect = reactive(rv$counter), 
      refresh_deploy = reactive(rv$refresh_deploy),
      smp_id = reactive(input$smp_id)
    )
  )
}