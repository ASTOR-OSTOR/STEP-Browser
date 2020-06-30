library(shiny)
library(DT)
library(dplyr)
library(readr)
library(rlang)
library(ggplot2)
library(plotly)
library(survival)
library(survminer)
library(httr)
library(rjson)
source("drugTableModule.R")
source("patientSurvivalModule.R")
source("cohortDiscoveryModule.R")

patients <- read_csv("../data/deidentified_patients.csv")
mutations <- read_csv("../data/deidentified_mutations.csv")
cnas <- read_csv("../data/deidentified_copynumbers.csv") %>% 
    filter(!is.na(copy_number))
treatments <- read_csv("../data/deidentified_treatments.csv")

shinyServer(function(input, output, session){
    
   # output$drug_table <- callModule(drugTable, "drug_table", patient_data = patients)
    callModule(drugTable, "drugtable", patient_data = patients, mutation_data = mutations, cna_data = cnas, treatment_data = treatments)
    callModule(patientSurvival, "surv", patient_data = patients, mutation_data = mutations, cna_data = cnas, treatment_data = treatments)
    callModule(cohortDiscovery, "cohorts", patient_data = patients, mutation_data = mutations, cna_data = cnas, treatment_data = treatments)
    
    # output$testDt <- reactive({
    #     datatable(observe(chosen_patients))
    # })
    
    ###
    # TODO: Move these to data cleaning pipeline
    patients$time_to_os_event <- ifelse(patients$following_patient == "Deceased",
                                        patients$dod - patients$date_diagnosis,
                                        patients$date_lost_followup - patients$date_diagnosis)
    patients$death_event <- patients$following_patient == "Deceased"
    ###
    
    output$resetButton <- renderUI({
        actionButton("trigger_reset", "Reset Fields")
    })
    
    observeEvent(input$trigger_reset, {
        updateTextInput(session, "histology_search", value = "")
        updateTextInput(session, "drug_group_1", value = "")
        updateTextInput(session, "drug_group_2", value = "")
        updateSliderInput(session, "age_group_1", value = c(min(patients$age),  max(patients$age)))
        updateSliderInput(session, "age_group_2", value = c(min(patients$age),  max(patients$age)))
        updateTextInput(session, "genes_inc_group_1", value = "")
        updateTextInput(session, "genes_inc_group_2", value = "")
        updateSliderInput(session, "amp_amount_group_1", value = c(1, max(cnas$ratio)))
        updateSliderInput(session, "amp_amount_group_2", value = c(1, max(cnas$ratio)))
        updateSliderInput(session, "loss_amount_group_1", value = c(0, 1))
        updateSliderInput(session, "loss_amount_group_2", value = c(0, 1))
        updateTextInput(session, "cna_amp_group_1", value = "")
        updateTextInput(session, "cna_amp_group_2", value = "")
        updateTextInput(session, "cna_loss_group_1", value = "")
        updateTextInput(session, "cna_loss_group_2", value = "")
    })
    
    observeEvent(input$trigger_demo_ddlps, {
            updateTextInput(session, "histology_search", value = "Liposarcoma, dedifferentiated")
    })
    
    output$include_vus_ui <- renderUI({
        checkboxInput(inputId = "includeVUS",
                      label = "Include Variants of Unknown Significance?",
                      value = FALSE)
    })


    

    


})