library(shiny)
library(plotly)
library(DT)
library(shinycssloaders)
source("drugTableModule.R")
source("patientSurvivalModule.R")
source("cohortDiscoveryModule.R")

shinyUI(fluidPage(
  titlePanel("STEP: Sarcoma Treatment Efficacy Portal"),
  tabsetPanel(
    type = "tabs",
    tabPanel(
      "Cohort Discovery",
      cohortDiscoveryUI("cohorts")
    ),
    tabPanel(
      "Survival Explorer",
      patientSurvivalUI("surv")
    ),
    tabPanel(
      "Treatment Explorer",
      drugTableUI("drugtable")
    )
    # tabPanel("Models")
  )
))