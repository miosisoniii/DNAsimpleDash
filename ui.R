
#read_csv("~/shiny/DNAsimpleShiny/shinydata.csv") -> my.data
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
                valueBoxOutput("totalusers", width = 4),
                infoBoxOutput("maleusers"),
                infoBoxOutput("femaleusers")
              ),
              fluidRow(
                box(title = "User Composition", status = "primary", solidHeader = T,
                    plotlyOutput("piechart")
                    ),
                box(title = "Summary", status = "primary", solidHeader = T,
                    plotOutput("summaryplot"),
                    selectInput(inputId = 'racecomp',
                                label = 'Select race:',
                                choices = unique(my.data$race),
                                selected = "ASIAN")
                )
              ),
              fluidRow(
                box(title = "User Ethnicities", status = "success", solidHeader = T,
                    plotOutput("ethnicities_raceplot")
                    ),
                box(title = "User Ethnicity Totals", color = "green", solidHeader = T,
                    plotOutput("totalethnicitycounts")
                    )
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
                              selected = "Diabetes (Type II)"),
                  
                  selectInput(inputId = 'sel_var',
                              label = 'Select Variable to View',
                              choices = c("Diagnosed by Physician",
                                          "Takes Medication",
                                          "Is Self Afflicted"),
                              selected = "Diagnosed by Physician"),
                  
                  selectInput(inputId = 'sel_demographic.test',
                              label = 'Select Demographic to View',
                              choices = c("Gender", 
                                          "Race", 
                                          "Ethnicity", 
                                          "Age Group"),
                              selected = "Gender"),
                  
                  checkboxInput(inputId = "checkbox.white",
                                label = "Include White_European?",
                                value = FALSE)
                  ),
                box(title = "Single Condition", status = "primary", solidHeader = T,
                    plotOutput("plot1")
                )
              ),
              fluidRow(
                box(title = "Select Variable Plot", width = 6,
                    plotOutput("selectvariableplot")
                    ),
                box(title = "Single Condition Plot2", width = 4,
                    plotOutput("diagnosed.phys")
                    ),
                box(title = "Single Condition Plot3", width = 2,
                    plotOutput("diagnosed.phys.count")
                    )
                # box(title = "Single Condition Plot4", width = 3,
                #     plotOutput("singlecond.takesmed")
                #     ),
                # box(title = "Single Condition Plot5", width = 3,
                #     plotOutput("singlecond.runsfamily"))
              ),
              fluidRow(
                box(title = "Another plot testing", width = NULL,
                    plotOutput("diagnosed.test.ethnicity"))
              ),
              fluidRow(
                box(title = "Single Condition: Self Afflicted",
                    plotOutput("singlecond.selfafflicted")
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
                box(title = "Grouped Conditions", status = "success", solidHeader = T,
                  radioButtons(inputId = 'sel_multcond',
                              label = 'Explore groups of conditions:',
                              #call for switch() function switches these to datasets
                              choices = c("Cancer", "Psychological Disorders", "GI Disorders"),
                              selected = 'Cancer')
                ),
                box(title = "Multiple Condition Breakdown", status = "success", solidHeader = F,
                    plotOutput("groupbreakdown")
                )
              ),
              fluidRow(
                box(title = "Select Conditions to Explore", status = "success", solidHeader = T,
                    checkboxGroupInput(inputId = "groupedcondcheckbox",
                                       label = "Select Condition",
                                       choices = "")
                ),
                box(title = "Selected Condition(s)", status = "success", solidHeader = F,
                    plotOutput("groupedselectedconditionplot"))
              )
      ),
      tabItem(tabName = "usmap",
              fluidRow(
                box(title = "Condition Select", status = "primary", solidHeader = T,
                    selectInput(inputId = 'sel_uscond',
                                label = 'Select Condition',
                                choices = unique(my.data$name),
                                selected = "Diabetes (Type I)")),
                valueBoxOutput("usmap_total")
              ),
              fluidRow(
                infoBoxOutput("usmap_hascond"),
                infoBoxOutput("usmap_nocond")
                ),
              
              fluidRow(
                box(title = "United States Condition Prevalence", 
                    status = "primary", solidHeader = T, width = NULL,
                    plotOutput("maptotal"))
                ),
              box(title = "US Condition Ratio?", status = "warning", width = NULL,
                  plotOutput("mapratio")
              )
      ),
      
      tabItem(tabName = "stats",
              fluidRow(
                box(
                  selectInput(inputId = 'stat_singlecond',
                              label = 'Select Condition',
                              choices = unique(my.data$name),
                              selected = "Diabetes (Type II)")),
                infoBoxOutput("CMHtestdisplay")
              ),
              fluidRow(
                box(title = "Select Statistical Test", status = "warning", solidHeader = TRUE)
              ),
              fluidRow(
                box(title = "Condition Matrix", width =  9,
                    verbatimTextOutput("CMHtestformat"))
              ),
              fluidRow(
                box(title = "Cochran-Mantel-Haenszel Test\nfor Repeated Tests of Independence", width = 8,
                    verbatimTextOutput("CMHtestoutput")
                )
              ),
              fluidRow(
                box(title = "Data", width = NULL,
                    verbatimTextOutput("CMHplot.table"),
                    plotOutput("CMHplot")
                )
              )
      )
    )
  )
)
