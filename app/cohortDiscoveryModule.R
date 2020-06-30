library(shiny)
source("patientFilterModules.R")


cohortDiscoveryUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      # Put a group_by select here, which will determine how to group the table
      selectModuleUI(ns("cohorts_group_by_col")),
      selectModuleUI(ns("cohorts_mutation_gene")),
      selectModuleUI(ns("cohorts_treatment")),
      hr(),
      histologyFilterUI(ns("cohorts_hist_filter")),
      
      hr(),
      completePatientFilterUI(ns("cohort_pt_filter"), label = "Patient Filters")
    ),
    
    mainPanel(br(),
              dataTableOutput(ns("cohort_table")) %>% withSpinner())
  )
}
# 


cohortDiscovery <- function(input, output, session, patient_data, mutation_data, cna_data, treatment_data){
  group_by_col <- callModule(selectModule,
                             "cohorts_group_by_col",
                             choices = c("Histology" = "tumor_histology",
                                         "Gene Altered"="name_gene",
                                         "Gene Alteration"="name_alteration",
                                         "Stage of Disease"="cancer_stage_new"),
                             label="Group By",
                             selected = "tumor_histology",
                             multiple = T)
  
  histology <- callModule(histologyFilter, "cohorts_hist_filter", choices = patient_data$tumor_histology)
  pts <- callModule(completePatientFilter,
                         "cohort_pt_filter",
                         patient_data = patient_data,
                         cna_data = cna_data)

  gene <- callModule(selectModule,
                             "cohorts_mutation_gene",
                             choices = mutation_data$name_gene,
                             label="Altered Gene",
                             multiple = T)
  
  treatment <- callModule(selectModule,
                     "cohorts_treatment",
                     choices = treatment_data$drug_name,
                     label="Treatment Received",
                     multiple = T)
  
  output$cohort_table <- renderDataTable({
    datatable(
      patient_data %>%
        filter(tumor_histology %in% histology()) %>% 
        filter(person_id %in% pts()) %>%
        {if(!is.null(gene())) {
          inner_join(., mutation_data) %>% 
            filter(., name_gene %in% gene())
        } else if (any(c("name_gene", "name_alteration") %in% group_by_col())) {
          inner_join(., mutation_data)
        } else {
          .
        }
          } %>% 
        {if(!is.null(treatment())) {
          inner_join(., treatment_data) %>% 
            filter(., drug_name %in% treatment())
        } else .
        } %>% 
        group_by_(.dots = {if (is.null(group_by_col())) {NA} else {group_by_col()}}) %>%
        summarise(count = n())
    )
  })
  
  # output$frequentlyMutatedGenesPlot <- renderPlotly({
  #   gene_by_histology <- patientGroup1() %>%
  #     inner_join(selectedMutations()) %>%
  #     distinct(person_id, name_gene, .keep_all = TRUE) %>% # Some patients may have two or more mutations in the same gene
  #     select(person_id, tumor_histology, name_gene)
  #   
  #   # Get the top 10 mutated genes (by count) regardless of histology
  #   topMutatedGenes <- (selectedMutations() %>%
  #                         distinct(person_id, name_gene, .keep_all = TRUE) %>%
  #                         count(name_gene) %>%
  #                         arrange(-n) %>%
  #                         head(10)) # We don't use top_n because we care more about visuals than having ties
  #   
  #   
  #   
  #   (ggplot() +
  #       geom_bar(data = selectedMutations() %>%
  #                  inner_join(gene_by_histology) %>%
  #                  distinct(person_id, name_gene, .keep_all = TRUE) %>%
  #                  inner_join(topMutatedGenes), aes(x = reorder(name_gene, -n), fill = tumor_histology)) +
  #       geom_text(data=topMutatedGenes, aes(x = name_gene, y = n, label=n),vjust= -1) +
  #       labs(title = "Most frequently altered genes for this subset", fill = "Histology") +
  #       xlab("Gene name") +
  #       ylab("Number of patients with an alteration")) %>%
  #     ggplotly()
  # })
  # 
  # output$distPlot <- renderPlotly({
  #   return((ggplot(patientGroup1(),
  #                  aes(x = cancer_stage_new, fill = tumor_histology)) +
  #             geom_bar() +
  #             labs(title = "Patients in each stage", fill = "Histology") +
  #             xlab("Stage") +
  #             ylab("Number of patients")) %>% 
  #            ggplotly())
  # })
}