library(shiny)
library(shinyWidgets)

multiSelectWithRegexUI <- function(id) {
  ns <- NS(id)
  tagList(textInput(ns("search"),
                    label = "Cancer histology (text search):",
                    value = "",
                    placeholder = "Enter a cancer histology type"),
          uiOutput(ns("selected_ui")))
}

multiSelectWithRegex <- function(input, output, session, choices){
  output$selected_ui <- renderUI({
    ns <- session$ns
    selectInput(inputId = ns("selected"),
                label = "Cancer histology:",
                choices = sort(choices),
                selected = sort(choices[grepl(tolower(input$search), tolower(choices))]),
                multiple = TRUE,
                width="100%")
  })
  return(reactive({
    input$selected
  }))
}

histologyFilterUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("selected_ui"))
}

histologyFilter <- function(input, output, session, choices){
  output$selected_ui <- renderUI({
    ns <- session$ns
    pickerInput(inputId = ns("selected"),
                label = "Cancer histology:",
                choices = sort(unique(choices)), 
                selected = c(),
                multiple = TRUE,
                width = "100%",
                options = list(`actions-box` = TRUE))
  })
  return(reactive({
    input$selected
  }))
}

ageFilterUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("age_slider_ui"))
}

ageFilter <- function(input, output, session, ages){
  output$age_slider_ui <- renderUI({
    ns <- session$ns
    sliderInput(inputId = ns("ages"),
                label = "Age range:",
                min = min(ages),
                max = max(ages), 
                value = c(min(ages), max(ages)))
    })
  return(reactive({input$ages}))
}

selectModuleUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("select_ui"))
}

selectModule <- function(input, output, session, choices, label = "", multiple = F, selected = NULL) {
  output$select_ui <- renderUI({
    ns <- session$ns
    selectInput(inputId = ns("selected"),
                label = label,
                multiple = multiple,
                selected = selected,
                choices = sort(choices))
  })
  return(reactive({
    input$selected
  }))
}

optionalSelectUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("optional_select_ui"))
}

# Module for creating a select input, but if the list of choices is NULL, the UI is not rendered.
optionalSelectModule <- function(input, output, session, choices, label = "", multiple = F) {
  output$optional_select_ui <- renderUI({
    ns <- session$ns
    if (is.null(choices)) {
      return()
    } else {
      selectInput(inputId = ns("selected"),
                  label = label,
                  multiple = multiple,
                  choices = sort(choices))
    }
  })
  return(reactive({
    input$selected
  }))
}

optionalSelectWithRangeUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("optional_select_range_ui"))
}

# Module for creating a select input, but if the list of choices is NULL, the UI is not rendered.
optionalSelectWithRangeModule <- function(input, output, session, choices, ranges, label_select = "", label_range = "", multiple = F) {
  output$optional_select_range_ui <- renderUI({
    ns <- session$ns
    if (is.null(choices)) {
      return()
    } else {
      tagList(
      selectInput(inputId = ns("selected"),
                  label = label_select,
                  multiple = multiple,
                  choices = sort(choices)),
      sliderInput(inputId = ns("range"),
                  label = label_range,
                  min = min(ranges),
                  max = max(ranges),
                  value = c(min(ranges), max(ranges)))
      )
    }
  })
  return(reactive({
    list(selected = input$selected, range = input$range)
  }))
}

completePatientFilterUI <- function(id, label = "Patient Group") {
  ns <- NS(id)
  tagList(
    h4(label),
    ageFilterUI(ns("pts_age_filter")),
    optionalSelectUI(ns("pts_mutation_filter")),
    optionalSelectWithRangeUI(ns("pts_copy_number_amp_filter")),
    optionalSelectWithRangeUI(ns("pts_copy_number_loss_filter")),
    optionalSelectUI(ns("pts_treatment_filter"))
  )
}

completePatientFilter <- function (input, output, session, patient_data, mutation_data = NULL, cna_data = NULL, treatment_data = NULL) {
  ageFilter <- callModule(ageFilter, "pts_age_filter", ages = patient_data$age)
  mutations <- callModule(optionalSelectModule, "pts_mutation_filter", choices = mutation_data$name_gene, label="Mutations", multiple = T)
  # copyNumber <- callModule(optionalSelectModule, "pts_copy_number_filter", choices = cna_data$gene, label="Copy Number", multiple = T)
  copyNumberAmp <- callModule(optionalSelectWithRangeModule,
                           "pts_copy_number_amp_filter",
                           choices = (cna_data %>% filter(copy_number > 1))$gene,
                           ranges = c(1, (cna_data %>% filter(copy_number > 1))$copy_number),
                           label_select="CNV: Amplified Gene",
                           label_range="Amplified amount",
                           multiple = T)
  copyNumberLoss <- callModule(optionalSelectWithRangeModule,
                           "pts_copy_number_loss_filter",
                           choices = (cna_data %>% filter(copy_number < 1))$gene,
                           ranges = c(0, 1),
                           label_select="CNV: Lost Gene",
                           label_range="Loss amount",
                           multiple = T)
  treatment <- callModule(optionalSelectModule,
                          "pts_treatment_filter",
                          choices = treatment_data$drug_name,
                          label="Treatments",
                          multiple = T)

  return(
    reactive({
      (patient_data %>%
        filter(age >= ageFilter()[[1]] & age <= ageFilter()[[2]]) %>% 
        {if (is_empty(mutations())) filter(.) else filter(., person_id %in% (mutation_data %>% filter(name_gene %in% mutations()))$person_id)} %>% 
        {
          if (is_empty(copyNumberAmp()$selected)) {
            filter(.)
          } else {
            filter(., person_id %in% (cna_data %>%
                                        filter(gene %in% copyNumberAmp()$selected &
                                                 copy_number >= copyNumberAmp()$range[[1]] &
                                                 copy_number <= copyNumberAmp()$range[[2]]))$person_id)
            }
          } %>% 
        {
          if (is_empty(copyNumberLoss()$selected)) {
            filter(.)
          } else {
            filter(., person_id %in% (cna_data %>%
                                        filter(gene %in% copyNumberLoss()$selected &
                                                 copy_number >= copyNumberLoss()$range[[1]] &
                                                 copy_number <= copyNumberLoss()$range[[2]]))$person_id)
          }
        })$person_id
    })
  )
}

