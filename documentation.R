#Documentation tab
#Two columns 
#one with description of each page
#one with each update

documentationUI <- function(id, label = "documentation"){
  tabPanel("Documentation", value = "readme_tab", 
           titlePanel("MARS Fieldwork Database v1.0"), 
           column(width = 5,
                  h2("User Guide"),
                  
                  h3("Deployments"),
                  h4("Collection Calendar"),
                  h5("The Collection Calendar tab presents a table of all active deployments. The sidebar has filters for property type, interval, and capacity. The table is searchable, and the \"80% Full Date\" column is highlighted yellow if that date has passed. The \"100% Full Date\" column is highlighted red if that date has passed. Click a row to go to the \"Deploy Sensor\" tab, where the same deployment will be selected and available to edit."),
                  h4("Deploy Sensor"), 
                  h5("Add a new sensor deployment by filling out SMP ID (or Site Name), location, sensor serial number, purpose (baro or level), interval (5 or 15 mintues), and deployment date. Collection date is optional. Only SMP ID (or Site Name) and location are required for future deployments. Deployments with dataloggers do not have a sensor serial number. New deployments can not be added for sensors that are already deployed elsewhere. Edit existing deployments by clicking on either the active or previous deployments table. Once a collection date is added, a checkbox is presented with the option to redeploy the sensor. If selected, then upon deployment, two entries to the database will be made: one for the collected deployment (which could be an edit of a previous active deployment), one for the new active deployment. The tables will instantly update with new deployments."), 
                  h4("Future Deployments"),
                  h5("The Future Deployments tab presents a table of all future, or planned, deployments. Click a row to go to the Deploy Sensor tab, where the same deployment should be selected and available to edit."),
                  
                  h3("Add Location"), 
                  h5("Choose whether to add a location at an SMP or non-SMP site (\"Site\")."),
                  h5("If SMP: Select an SMP ID to give options for Component ID. A suggested Location (ie, OW1) is autofilled, which can be overwritten if needed. Facility ID is autofilled based on the selected Component ID. If the selected feature does not have a component ID, the facility ID of the SMP will be used. Once all field are in the top sidebar are complete, and the new location does not already exist, add the location. Click a row on the table to enable editing of location. Use the lower sidebar to add or edit well measurements for the location noted in the top sidebar."), 
                  h5("If Site: Select a Site in the dropdown. If the Site you need isn't there, you can add it by checking off \"Add New Site?\" and typing a new name. Once you click, it will appear in the dropdown. Select a component ID and location. If \"At SMP\" is blank, you can check a box if you want to move monitoring locations from a site to an SMP. This is useful if a monitored pre-construction site was given an SMP ID. The locations will be assigned to that SMP ID, and the site name will removed."), 
                  
                  h3("Add Sensor"), 
                  h5("Add a new sensor to the inventory by entering sensor serial number, model number, and sensor status. Purchase date is optional. Edit existing sensors by entering the sensor serial number, or clicking on the desired table row. The table can be sorted and searched. Click \"Deploy this Sensor\" to go to the \"Deploy Sensor\" tab with the sensor selected."),
                  
                  h3("Simulated Runoff Tests"), 
                  h4("Add/Edit SRT"),
                  h5("Add or edit an SRT record. System ID, Test Date, and SRT Type are required fields. Edit by selecting an System ID from the drop-down, and then clicking on one of the table rows to the right. All previously recorded fields (including those not shown in the table) will autofill. Future SRTs only require an System ID and no date."), 
                  h4("View SRTs"),
                  h5("View all recorded SRTs. Records per page can be adjusted and records can be sorted by clicking on the column header. Clicking the triangle on the left will show the SRT summary. Similar to the Collection Calendar, clicking on a row goes to the \"Add/Edit SRT\" tab where the same record will be selected and ready to edit."), 
                  h4("View Future SRTs"), 
                  h5("View all future SRTs. This works the same as the View SRTs tab."),
                  
                  h3("Porous Pavement Tests"),
                  h4("Add/Edit Porous Pavement Tests"),
                  h5("Add or edit a Porous Pavement Test record. SMP ID, Test Date, and Surface Type are required fields. Edit by selecting an SMP ID from the drop-down, and then clicking on one of the table rows to the right. All previously recorded fields (including those not shown in the table) will autofill. Only an SMP ID is required to add future tests."),
                  h4("View Porous Pavement Tests"),
                  h5("View all recorded Porous Pavement Tests. Table can be searched, sorted, and adjusted. Clicking on a row goes to the \"Add/Edit Porous Pavement Test\" tab where the same record will be selected and ready to edit."),
                  h4("View Future Porous Pavement Test"), 
                  h5("View all future Porous Pavement Tests. This works the same as the View Porous Pavement Tests tab."),
                  
                  h3("Capture Efficiency Tests"),
                  h4("Add/Edit Capture Efficiency Test"),
                  h5("Add or edit a Capture Efficiency Test record. System ID, Component ID (or Custom Component ID), and Test Date are required fields. Edit by selecting SMP from the drop-down, and then clicking on one of the table rows. All previously recorded fields will autofill. System ID and Component ID are required to add future tests."),
                  h4("View Capture Efficiency Tests"),
                  h5("View all recorded Capture Efficiency Tests. Table can be searched, sorted, and adjusted. Clicking on a row goes to the \"Add/Edit Capture Efficiency Test\" tab where the same record will be selected and ready to edit."),
                  h4("View Future Capture Efficiency Test"), 
                  h5("View all future Capture Efficiency Tests. This works the same as the View Capture Efficiency Tests tab."),
                  
                  h3("Inlet Conveyance Tests"),
                  h4("Add/Edit Inlet Conveyance Test"),
                  h5("Add or edit a Inlet Conveyance Test record. System ID (or Work Number, or Site Name), Component ID (or Custom Component ID), and Test Date are required fields. Edit by selecting System ID from the drop-down, and then clicking on one of the table rows. All previously recorded fields will autofill. System ID (or Work Number or Site Name) and Component ID are required to add future tests."),
                  h4("View Inlet Conveyance Tests"),
                  h5("View all recorded Inlet Conveyance Tests. Table can be searched, sorted, and adjusted. Clicking on a row goes to the \"Add/Edit Inlet Conveyance Test\" tab where the same record will be selected and ready to edit."),
                  h4("View Future Inlet Conveyance Test"), 
                  h5("View all future Inlet Conveyance Tests. This works the same as the View Inlet Conveyance Tests tab."),
                  
                  h3("Special Investigations"),
                  h4("Add/Edit Special Investigation"),
                  h5("Add or edit a Special Investigation record. System ID (or Work Number, or Site Name), Special Investigation Type, Requested By, and Test Date are required fields. Edit by selecting System Id from the drop-down, and then clicking on one of the table rows. All previously recorded fields will autofill. System ID (or Work Number or Site Name)  are required to add future tests."),
                  h4("View Special Investigations"),
                  h5("View all recorded Special Investigations. Table can be searched, sorted, and adjusted. Clicking on a row goes to the \"Add/Edit Special Investigation\" tab where the same record will be selected and ready to edit."),
                  h4("View Future Special Investigations"), 
                  h5("View all future Special Investigations. This works the same as the View Special Investigations tab."),
                  
                  h3("Monitoring Stats"), 
                  h5("Analyze records for a selected date range, or look through all records to date. Select whether to look for post-construction or construction records. Check box for capture efficiency statistics parameters Once a table is genarated, it shows in the main panel. The \"Download\" button downloads the table shown. When parameters are changed, the table must be generated again before downloading.")
           ), 
           column(width = 5, offset = 1,
                  h2("Current Status"), 
                  h3("v1.0"),
                  h5("All features of the previously used Current Monitoring Sites and Field Testing Master Record spreadsheets have been integrated. MARS Field staff shall use this app to enter all fieldwork data into the MARS Fieldwork database. Since v0.6, inlet conveyance tests and special investigations have been added, with some adjustments to user experience, and smaller back end adjustments"),
                  
                  h3("v0.6"), 
                  h5("An introduction to working with sites without an SMP ID. They will be referred to as \"Sites\", while locations with an SMP ID are \"SMPs\" or \"Systems\". You can add monitoring locations and deployments (future, active, and previous) at sites, and track them in the collection calendar like SMPs."),
                  h3("v0.5.2"), 
                  h5("Future SRTs, Porous Pavement Tests, and Capture Efficiency Tests can be added and edited."),
                  h3("v0.5.1"), 
                  h5("Added future deployments; they can be added by just entering an SMP ID and OW Suffix in the Deploy tab. They can be edited by clicking the row, or by clicking the row in the Future Deployments tab."),
                  h3("v0.5"), 
                  h5("Added the ability to view, add, and edit well measurements on the \"Add OW\" tab. Users should enter their deployment and collection info here and in the \"Current Monitoring Sites\" spreadsheet and give feedback to Nick; once updates are made, CMS can be partially phased out."),
                  h3("v0.4.1"),
                  h5("Added new deployment attributes and updated collection calendar; added reactive headers."),
                  h3("v0.4"),
                  h5("Added Monitoring Stats and made updates to Deploy, Collection Calendar, and Capture Efficiency."),
                  h3("v0.3.2"),
                  h5("Added Capture Efficiency tab."),
                  h3("v0.3.1"),
                  h5("Added Porous Pavement tab for Porous Pavement Tests, which includes the \"Add/Edit Porous Pavement Tests\" page and \"View Porous Pavement Tests\" page."),
                  h3("v0.3"), 
                  h5("Added SRT tab for Simulated Runoff Tests, which includes the \"Add/Edit SRT\" page and \"View All SRTs\" page."),
                  h3("v0.2.1"),
                  h5("Updated the \"Deploy Sensor\" tab to help prevent accidental double deployment of the same sensor, and to clear inputs (other than SMP ID) and deselect rows following an add or edit."),
                  h3("v0.2"), 
                  h5("Some user feedback has been incorporated, including adding dialogue boxes to confirm actions on the \"Deploy Sensor\" tab. The public/private filter on the \"Collection Calendar\" tab has been updated to work properly."),
                  h3("v0.1"), 
                  h5("This is the user testing phase. Changes will be made based on feedback, then v1.0 will be issued. Current tabs include:" , style = "margin-left: 10px"), 
                  HTML("<h5><ul>
                 <li>Collection Calendar</li>
                 <li>Add Location</li>
                 <li>Add Sensor</li>
                 <li>Deploy Sensor</li>
                 <li>Documentation</li>
                 </ul></h5>"), 
                  h3("Potential Updates"), 
                  h5("View Nick Manna's Asana (https://app.asana.com/0/1168242610391697/list)")
           )) 
}