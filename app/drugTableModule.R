source("patientFilterModules.R")

drugTableUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    # On the sidebar, show controls for filtering data in the table
    sidebarPanel(
      histologyFilterUI(ns("dt_hist_filter")),
      hr(),
      completePatientFilterUI(ns("dt_pt_filter"), label = "Additional Filters")
    ),
    
    # Show the table in the main panel
    mainPanel(br(), # Provides negative space for visual purposes,
              dataTableOutput(ns("drug_table")) %>% withSpinner())
  )
}
# 


drugTable <- function(input, output, session, patient_data, mutation_data, cna_data, treatment_data){
  histology <- callModule(histologyFilter, "dt_hist_filter", choices = patient_data$tumor_histology)
  pts <- callModule(completePatientFilter,
                    "dt_pt_filter",
                    patient_data = patient_data,
                    mutation_data = mutation_data,
                    cna_data = cna_data, treatment_data = NULL)
  
  output$drug_table <- renderDataTable({
    datatable(
      patient_data %>%
        filter(tumor_histology %in% histology()) %>% 
        filter(person_id %in% pts()) %>%
        inner_join(treatment_data) %>%
        group_by(drug_name) %>%
        rename("Drug Name" = drug_name) %>% 
        summarise(`Number of Patients` = n(),
                  `Num Patients still on Therapy` = sum(discontinued == "No", na.rm = TRUE),
                  `Median OS (Days)` = median(time_to_os_event),
                  `Average OS (Days)` = mean(time_to_os_event),
                  `OS Std Dev` = sd(time_to_os_event),
                  `Median PFS (Days)` = median(days_on_drug, na.rm = TRUE),
                  `Average PFS (Days)` = mean(days_on_drug, na.rm = TRUE),
                  `PFS Std Dev` = sd(days_on_drug, na.rm = TRUE)) %>%
        arrange(-`Number of Patients`, -`Average PFS (Days)`)) %>%
      formatRound(c(4:9), 2)
  })
}