library(shiny)
library(shinydashboard)
library(tidyverse)
library(lazyeval)

# read_csv("~/shiny/DNAsimpleShiny/shinydata.csv") -> my.data

#run script for filters and renaming of strings for easier plotting
source("functions.R")


ui <- dashboardPage(
  dashboardHeader(title = "DNAsimple Dashboard",
                  titleWidth = 250),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Database Summary", tabName = "summary", icon = icon("female")),
      menuItem("Explore Conditions", tabName = "conditions", icon = icon("th"),
               menuSubItem("Single Condition", tabName = "singlecond", icon = icon("medkit")),
               menuSubItem("Grouped Conditions", tabName = "multcond", icon = icon("sitemap")
               )
      ),
      menuItem("Map Conditions", tabName = "usmap", icon = icon("location-arrow")),
      menuItem("Statistical Analysis", tabName = "stats", icon = icon("connectdevelop")
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "summary",
              fluidRow(
                valueBoxOutput("totalusers"),
                infoBoxOutput("maleusers"),
                infoBoxOutput("femaleusers")
              ),
              fluidRow(
                box(title = "Select Race", status = "primary", solidHeader = T,
                    selectInput(inputId = 'racecomp',
                                label = 'Select:',
                                choices = unique(my.data$race),
                                selected = "ASIAN")),
                box(title = "Summary", status = "primary", solidHeader = T,
                    plotOutput("summaryplot"))
              )
      ),
      tabItem(tabName = "singlecond",
              fluidRow(
                valueBoxOutput("singlecondtotal"),
                infoBoxOutput("singlecondmale"),
                infoBoxOutput("singlecondfemale")
              ),
              fluidRow(
                box(
                  selectInput(inputId = 'sel_singlecond',
                              label = 'Select Condition',
                              choices = unique(my.data$name),
                              selected = "Diabetes (Type II)")),
                box(title = "Single Condition", status = "primary", solidHeader = T,
                    plotOutput("plot1")
                )
              )
      ),
      tabItem(tabName = "multcond",
              fluidRow(
                valueBoxOutput("mult_totalusers"),
                infoBoxOutput("mult_maleusers"),
                infoBoxOutput("mult_femaleusers")
              ),
              fluidRow(
                box(
                  selectInput(inputId = 'sel_multcond',
                              label = 'Explore groups of conditions:',
                              #call for switch() function switches these to datasets
                              choices = c("Cancer", "Psychological Disorders", "GI Disorders"),
                              selected = 'psych.test')
                ),
                box(title = "Multiple Condition Breakdown", status = "primary", solidHeader = T,
                    plotOutput("groupbreakdown"))
              )
      ),
      tabItem(tabName = "usmap",
              fluidRow(
                box(title = "title1"),
                box(title = "title2"),
                box(title = "title3")
              ),
              fluidRow(
                box(title = "title1"),
                box(title = "titletest", width = NULL)
              )
      ),
      tabItem(tabName = "stats",
              fluidRow(
                box(title = "infoboxtitle1", width = 8, height = 650,
                    verbatimTextOutput('CMHoutput')
                ),
                box(title = "infoboxtitle2", width = 4),
                fluidRow(
                  box(title = "View data object format", width = 4, height = 500),
                  infoBoxOutput("CMHtestdisplay")
                )
              )
      )
    )
  )
)