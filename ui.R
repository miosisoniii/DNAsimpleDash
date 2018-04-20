
#read_csv("~/shiny/DNAsimpleShiny/shinydata.csv") -> my.data
source("functions.R")

ui <- dashboardPage(
  dashboardHeader(title = "DNAsimpleDash"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Welcome", tabName = "welcome", icon = icon("graduation-cap")),
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
      tabItem(tabName = "welcome",
              fluidRow(
                box(width = 12, title = "", status = "primary",
                    column(6,
                           h1("Welcome to DNAsimpleDash!"),
                           p("With this app you can explore DNAsimple's patient database obtained on December 23rd, 2017.
                    The company has gained significant popularity since its inception in 2016. With a rapidly growing donor database,
                    a faster way to visualize their data was needed to streamline client-facing activities.")
                    ),
                    column(6,
                           img(src = "dnasimple_logo.png", 
                               style = "width:100%; max-width: 600px")
                           )
                )
              ),
              fluidRow(
                box(title = "About DNAsimple", status = "primary", width = 12, solidHeader = T,
                    column(6,
                           img(src = "investigator_process.png", style = "width: 100%; max-width: 500px;")
                    ),
                    column(6,
                           img(src = "sharktank.jpg", style = "width: 100%; max-width: 500px;"),
                           p("CEO of DNAsimple, Olivier Noel PhD presents to SharkTank on CBS.\n
                             Image source: allsharktankproducts.com")
                    )
                )
              ),
              fluidRow(
                box(title = "Donor Signup Process", status = "primary", width = 12, solidHeader = T,
                    img(src = "donor_process.png", style = "width: 100%; max-width: 1000px;")
                )
              ),
              fluidRow(
                box(title = "About the Author", status = "danger", width = 8, solidHeader = T,
                    column(8,
                           h2("Artemio Sison III"),
                           h3("Education"),
                           p("BS Biotechnology, Endicott College '16"),
                           p("PSM Bioinformatics, Temple University `18"),
                           h3("Experience"),
                           p("Manufacturing Asssociate I, WuXi Advanced Therapies")
                    ),
                    column(4,
                           img(src = "brochurephoto_sison.JPG", 
                               style = "width: 100%; max-width: 500px;")
                    )
                ),
                box(title = "Affiliations", status = "danger", width = 4, solidHeader = T,
                    img(src = "tu_cst.png", style = "width: 100%; max-width: 600px;")
                )
              )
      ),
      tabItem(tabName = "summary",
              fluidRow(
                valueBoxOutput("totalusers", width = 4),
                infoBoxOutput("maleusers"),
                infoBoxOutput("femaleusers")
              ),
              fluidRow(
                box(title = "Database Composition", status = "primary", solidHeader = T,
                    plotlyOutput("piechart")
                    ),
                box(title = "Donor Race Composition", status = "primary", solidHeader = T,
                    selectInput(inputId = 'racecomp',
                                label = 'Select race:',
                                choices = unique(my.data$race),
                                selected = "ASIAN"),
                    plotOutput("totalethnicitycounts")
                )
              ),
              fluidRow(
                box(title = "Ethnicity Age Distribution", status = "primary", solidHeader = T, width = 8,
                    plotOutput("ethnicities_raceplot")
                ),
                box(title = "Age Distribution Across Gender", status = "primary", solidHeader = T, width = 4,
                    plotOutput("summaryplot")
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
                box(title = "Explore", status = "info",
                  selectizeInput(inputId = 'sel_singlecond',
                                 label = 'Select Condition',
                                 choices = unique(my.data$name),
                                 selected = "Diabetes (Type II)",
                                 options = list(maxOptions = 1300)
                                 ),
                  
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
                              selected = "Race"),
                  sliderInput("singlecondslider", "Select age range:",
                              min = 5, max = 100, step = 5,
                              value = c(5, 100)),
                  
                  checkboxInput(inputId = "checkbox.recentdiagnosed",
                                label = "Recently Diagnosed (Years with Condition <= 1)?",
                                value = FALSE),
                  # sliderInput("yearpresentslider", "Select number of years with condition:",
                  #             min = 0, max = 50,
                  #             value = c(0, 50)),
                  
                  checkboxInput(inputId = "checkbox.white",
                                label = "Include White_European?",
                                value = FALSE)
                  ),
                box(title = "Composition of Selected Condition", status = "info", solidHeader = T,
                    plotOutput("plot1")
                )
              ),
              fluidRow(
                box(title = "Explore Selected Condition", status = "info", solidHeader = T, width = NULL,
                    plotOutput("selectvariableplot")
                    )
              ),
              fluidRow(
                box(title = "Is Self Afflicted", status = "info", width = 4,
                    plotOutput("self_afflicted.count")
                    ),
                box(title = "Diagnosed by Physician", status = "info", width = 4,
                    plotOutput("diagnosed_phys.count")
                    ),
                box(title = "Takes Medication", status = "info", width = 4,
                    plotOutput("takes_med.count")
                )
              ),
              fluidRow(
                box(title = "Composition: Ethnicity", status = "info", width = NULL,
                    plotOutput("diagnosed.test.ethnicity"))
              )
      ),
      tabItem(tabName = "multcond",
              fluidRow(
                valueBoxOutput("mult_totalusers"),
                infoBoxOutput("mult_maleusers"),
                infoBoxOutput("mult_femaleusers")
              ),
              fluidRow(
                box(title = "Explore Condition Groups", status = "success", solidHeader = T,
                  radioButtons(inputId = 'sel_multcond',
                              label = 'Select Grouped Conditions:',
                              #call for switch() function switches these to datasets
                              choices = c("Cancer", "Psychological Disorders", "GI Disorders"),
                              selected = 'Cancer'),
                  sliderInput("ageslider", "Select age range:",
                              min = 5, max = 100, step = 5,
                              value = c(5, 100))
                ),
                box(title = "Condition Breakdown", status = "success", solidHeader = F,
                    plotOutput("groupbreakdown")
                )
              ),
              fluidRow(
                infoBoxOutput("grouped_topethnicity"),
                infoBoxOutput("group_male.age"),
                infoBoxOutput("group_female.age")
              ),
              fluidRow(
                box(title = "Select Conditions to Explore", status = "success", solidHeader = T,
                    checkboxGroupInput(inputId = "groupedcondcheckbox",
                                       label = "Select Condition(s):",
                                       choices = "")
                ),
                box(title = "Diagnosed by Physician Counts", status = "success", solidHeader = F,
                    plotOutput("groupedselectedconditionplot"))
              )
      ),
      tabItem(tabName = "usmap",
              fluidRow(
                box(title = "Map Conditions", status = "primary", solidHeader = T,
                    selectInput(inputId = 'sel_uscond',
                                label = 'Select Condition:',
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
              box(title = "United States Condition Ratio", status = "warning", width = NULL,
                  plotOutput("mapratio")
              )
      ),
      
      tabItem(tabName = "stats",
              fluidRow(
                box(title = "Condition Input", status = "warning", solidHeader = TRUE,
                    selectizeInput(inputId = 'stat_singlecond',
                                   label = 'Select Condition:',
                                   choices = unique(my.data$name),
                                   selected = "Diabetes (Type II)",
                                   options = list(maxOptions = 1300)
                    )
                ),
                infoBoxOutput("CMHtestdisplay")
              ),
              fluidRow(
                box(title = "Condition Matrix", status = "warning", width = NULL,
                    verbatimTextOutput("CMHtestformat")
                ),
                box(title = "Cochran-Mantel-Haenszel Test\nfor Repeated Tests of Independence", 
                    status = "warning", solidHeader = TRUE, width = NULL,
                    verbatimTextOutput("CMHtestoutput")
                )
              )
      )
    )
  )
)
