documentationUI <- function(id, label = "documentation"){
  tabPanel("Documentation", value = "readme_tab", 
           titlePanel("MARS Fieldwork Database v0.3.2"), 
           column(width = 5,
                  h2("User Guide"),
                  h3("Collection Calendar"),
                  h5("The Collection Calendar tab presents a table of all active deployments. The sidebar has filters for property type, interval, and capacity. The table is searchable, and the \"80% Full Date\" column is highlighted yellow if that date has passed. The \"100% Full Date\" column is highlighted red if that date has passed. Click a row to go to the \"Deploy Sensor\" tab, where the same deployment will be selected and available to edit.", style = "margin-left: 20px"), 
                  h3("Add Observation Well"), 
                  h5("Select an SMP ID to give options for Component ID. A suggested OW Suffix (ie, OW1) is autofilled, which can be overwritten if needed. Facility ID is autofilled based on the selected Component ID. If the selected feature does not have a component ID, the facility ID of the SMP will be used. Once all field are complete, and the new observation does not already exist, add the observation well. Click a row on the table to enable editing of an observation well." , style = "margin-left: 20px"), 
                  h3("Add Sensor"), 
                  h5("Add a new sensor to the inventory by entering sensor serial number and model number. Purchase date is optional. Edit existing sensors by entering the sensor serial number, or clicking on the desired table row. The table can be sorted and searched. Click \"Deploy this Sensor\" to go to the \"Deploy Sensor\" tab with the sensor selected.", style = "margin-left: 20px"),
                  h3("Deploy Sensor"), 
                  h5("Add a new sensor deployment by filling out SMP ID, well name, sensor serial number, purpose (baro or level), interval (5 or 15 mintues), and deployment date. Collection date is optional. New deployments can not be added for sensors that are already deployed elsewhere. Edit existing deployments by clicking on either the active or previous deployments table. Once a collection date is added, a checkbox is presented with the option to redeploy the sensor. If selected, then upon deployment, two entries to the database will be made: one for the collected deployment (which could be an edit of a previous active deployment), one for the new active deployment. The tables will instantly update with new deployments.", style = "margin-left: 20px"), 
                  h3("Simulated Runoff Tests"), 
                  h4("Add/Edit SRT"),
                  h5("Add or edit an SRT record. System ID, Test Date, and SRT Type are required fields. Edit by selecting an System ID from the drop-down, and then clicking on one of the table rows to the right. All previously recorded fields (including those not shown in the table) will autofill."), 
                  h4("View SRTs"),
                  h5("View all recorded SRTs. Records per page can be adjusted and records can be sorted by clicking on the column header. Clicking the triangle on the left will show the SRT summary. Similar to the Collection Calendar, clicking on a row goes to the \"Add/Edit SRT\" tab where the same record will be selected and ready to edit."), 
                  h3("Porous Pavement Tests"),
                  h4("Add/Edit Porous Pavement Tests"),
                  h5("Add or edit a Porous Pavement Test record. SMP ID, Test Date, and Surface Type are required fields. Edit by selecting an SMP ID from the drop-down, and then clicking on one of the table rows to the right. All previously recorded fields (including those not shown in the table) will autofill."),
                  h4("View Porous Pavement Tests"),
                  h5("View all recorded Porous Pavement Tests. Table can be searched, sorted, and adjusted. Clicking on a row goes to the \"Add/Edit Porous Pavement Test\" tab where the same record will be selected and ready to edit."),
                  h3("Capture Efficiency Tests"),
                  h4("Add/Edit Capture Efficiency Test"),
                  h5("Add or edit a Capture Efficiency Test record. System ID, Component ID, and Test Date are required fields. Edit by selecting SMP from the drop-down, and then clicking on one of the table rows. All previously recorded fields will autofill."),
                  h4("View Capture Efficiency Tests"),
                  h5("View all recorded Capture Efficiency Tests. Table can be searched, sorted, and adjusted. Clicking on a row goes to the \"Add/Edit Capture Efficiency Test\" tab where the same record will be selected and ready to edit.")
           ), 
           column(width = 5, offset = 1,
                  h2("Current Status"), 
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
                 <li>Add Observation Well</li>
                 <li>Add Sensor</li>
                 <li>Deploy Sensor</li>
                 <li>Documentation</li>
                 </ul></h5>"), 
                  h3("Potential Updates"), 
                  h5("Add validation to catch and present backend SQL errors. Add database tables and user tabs for SRTs and dye tests. On the backend, modularize Shiny scripts.")
           )) 
}