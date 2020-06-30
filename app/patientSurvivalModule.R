source("patientFilterModules.R")

patientSurvivalUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
        # TODO: PFS needs a new name but I can't remember what
        radioButtons(inputId = ns("pfs_or_os"),
                     label =  "Survival Metric",
                     choiceNames = c("Overall Survival","Time on Treatment"),
                     choiceValues = c("OS", "PFS"),
                     selected = "OS"),
        #, Checkbox show group 2
        checkboxInput(inputId = ns("showGroup2"),
                      label = "Show Group 2?",
                      value = F),
        # TODO: put a "Show Patients in both groups?" button that disables when the showgroup2 is unchecked
        hr(),
        completePatientFilterUI(ns("pt_1_filter"), label = "Patient Group 1"),
        selectModuleUI(ns("treatments_pt_1")),
        hr(),
        uiOutput(ns("pt_2_filter_ui"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(plotOutput(ns("survplot"), height = "700px") %>% withSpinner(),
              hr(),
              histologyFilterUI(ns("hist_filter")))
  )
}

patientSurvival <- function(input, output, session, patient_data, mutation_data, cna_data, treatment_data){
  output$pt_2_filter_ui <- renderUI({
    ns <- session$ns
    if (input$showGroup2) {
      return(tagList(
        completePatientFilterUI(ns("pt_2_filter"),  label = "Patient Group 2"),
             selectModuleUI(ns("treatments_pt_2")))
      )
    } else {
      return()
    }
  })
  
  histology <- callModule(histologyFilter, "hist_filter", choices = patient_data$tumor_histology)
  ptgroup1 <- callModule(completePatientFilter,
                         "pt_1_filter",
                         patient_data = patient_data,
                         mutation_data = mutation_data,
                         cna_data = cna_data)
  pt1Treatments <- callModule(selectModule,
                             "treatments_pt_1",
                             choices = treatment_data$drug_name,
                             label="Treatment Received",
                             multiple = T)
  

  ptgroup2checker <- reactive({
    if (input$showGroup2) {
      ptgroup2 <<- callModule(completePatientFilter,
                             "pt_2_filter",
                             patient_data = patient_data,
                             mutation_data = mutation_data,
                             cna_data = cna_data)
      pt2Treatments <<- callModule(selectModule,
                                  "treatments_pt_2",
                                  choices = treatment_data$drug_name,
                                  label="Treatment Received",
                                  multiple = T)
    } else {
      ptgroup2 <<- function(){NULL}
      pt2Treatments <<- function(){NULL}
    }
  })
  
  output$survplot <- renderPlot({
    ptgroup2checker()
    if (input$showGroup2) {
      group1 <- patient_data %>%
        filter(tumor_histology %in% histology()) %>%
        filter(person_id %in% ptgroup1() & !(person_id %in% ptgroup2()))
      group2 <- patient_data %>%
        filter(tumor_histology %in% histology()) %>%
        filter(person_id %in% ptgroup2() & !(person_id %in% ptgroup1()))
      overlap <- patient_data %>%
        filter(tumor_histology %in% histology()) %>%
        filter(person_id %in% ptgroup1() & person_id %in% ptgroup2())
    } else {
      group1 <-patient_data %>%
        filter(tumor_histology %in% histology()) %>%
        filter(person_id %in% ptgroup1())
      group2 <- data.frame()
      overlap <- data.frame()
    }
    allGroups <- dplyr::bind_rows(list("Group 1"=group1, "Group 2"=group2, "Pts matching\nboth groups"=overlap), .id = 'source')
    if (nrow(allGroups) == 0) {
      return(NULL)
    }

    if (input$pfs_or_os == "PFS") {
      if (is_empty(input$`treatments_pt_1-selected`)) {
        stop("Select one or more treatments to plot time on treatment.")
      }
      allGroups <- allGroups %>%
        inner_join(treatment_data) %>%
        filter(drug_name %in% c(pt1Treatments(), pt2Treatments()))
      allGroups$surv.event <- allGroups$discontinued == "Yes"
      allGroups$surv.time <- allGroups$days_on_drug
    } else if (input$pfs_or_os == "OS") {
      allGroups$surv.event <- allGroups$death_event
      allGroups$surv.time <- allGroups$time_to_os_event
    }

    fit <- surv_fit(Surv(allGroups$surv.time, allGroups$surv.event) ~ source, data = allGroups)
  
    pval <- FALSE
    if (length(unique(allGroups$source)) > 1) {
      pval <- TRUE
    }

    canPrintConfInt <- all((allGroups %>% group_by(source) %>% summarise(count = n()))$count > 1)

    ggsurvplot(fit,
               data = allGroups %>% filter(tumor_histology %in% histology()),
               conf.int = canPrintConfInt,
               title= ifelse(input$pfs_or_os == "PFS", "Time on Treatment", "Overall survival"),
               legend.labs=unique(allGroups$source),
               pval = pval,
               pval.coord = c(0, 0.03),
               risk.table = TRUE
    )
  }
  )
  }
