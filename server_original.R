#run script for filters and renaming of strings for easier plotting



server <- function(input, output){
  
  # Tab1 - Database Summary Tab
  ##################################################################
  #Total Database - reactive, render*
  #summary data for race  
  summarydata <- reactive({
    my.data %>% filter(race == input$racecomp) %>%
      filter(gender != "OTHER") %>% na.omit()
  })
  
  #plot summary plot, age?
  output$summaryplot <- renderPlot({
    summarydata() %>% ggplot(aes(x = gender, y = age)) +
      geom_boxplot()
  })
  
  #total user tally for race
  totalusertally <- reactive({
    my.data %>% filter(race == input$racecomp) %>%
      filter(gender != "OTHER") %>% na.omit() %>%
      distinct(user_id) %>% tally()
  })
  
  output$totalusers <- renderValueBox(
    valueBox("Total Users", totalusertally(), icon = icon("users"))
  )
  
  #female users tally
  femaleusertally <- reactive({
    my.data %>% filter(race == input$racecomp) %>%
      filter(gender != "OTHER") %>% 
      filter(gender == "FEMALE") %>%
      distinct(user_id) %>% na.omit() %>% tally()
  })
  output$femaleusers <- renderValueBox(
    infoBox("Female Users", femaleusertally(), icon = icon("female"))
  )
  
  #male user tally
  maleusertally <- reactive({
    my.data %>% filter(race == input$racecomp) %>%
      filter(gender != "OTHER") %>% 
      filter(gender == "MALE") %>% 
      distinct(user_id) %>% na.omit() %>% tally()
  })
  output$maleusers <- renderInfoBox(
    infoBox("Male Users", maleusertally(), icon = icon("male"))
  )
  
  
  
  
  #
  ##################################################################
  # Tab2 - Single Condition Tab
  ##################################################################
  #for single condition plot
  singlecond.data <- reactive({
    my.data %>% filter(name == input$sel_singlecond) %>%
      filter(gender != "OTHER")
  })
  
  #first plot, gender/age
  output$plot1 <- renderPlot({
    singlecond.data() %>%
      ggplot(aes(x = gender, fill = race, y = age)) +
      ggtitle("Age/Gender Breakdown") +
      xlab("Gender") +
      ylab("Age") +
      geom_boxplot()
  })
  
  
  #singlecond info/valueboxes
  #singlecond reactive infobox
  singlecondtotaltally <- reactive({
    singlecond.data() %>% filter(gender != "OTHER") %>% distinct(user_id) %>% tally()
  })
  
  #single cond render infobox -> reactive
  output$singlecondtotal <- renderValueBox(
    valueBox("Total Users", singlecondtotaltally(), icon = icon("users")
    )
  )
  singlecondmaletally <- reactive({
    singlecond.data() %>% filter(gender != "OTHER") %>% filter(gender == "MALE") %>% tally()
  })
  
  output$singlecondmale <- renderInfoBox(
    infoBox("Male Users", singlecondmaletally(), icon = icon("male")
    )
  )
  
  singlecondfemaletally <- reactive({
    singlecond.data() %>% filter(gender != "OTHER") %>% filter(gender == "FEMALE") %>% tally()
  })
  
  output$singlecondfemale <- renderInfoBox(
    infoBox("Female Users", singlecondfemaletally(), icon = icon("female")
    )
  )
  
  
  
  #
  ##################################################################
  # Tab3 - Grouped Conditions Tab
  ##################################################################
  
  #for plotting grouping breakdown
  output$groupbreakdown <- renderPlot({
    datasetsel() %>% ggplot(aes(x = name, fill = name)) +
      geom_bar(color = "black") +
      coord_flip() +
      theme(legend.position = "none") +
      scale_x_discrete(labels = name_adjust)
  })
  
  
  datasetsel <- reactive({
    switch(input$sel_multcond,
           "Cancer" =  cancer.data,
           "GI Disorders" = digest_cond,
           "Psychological Disorders" = psych.data)
  })
  
  #grouped value/infoboxes
  #grouped total user tally for race
  multtotalusertally <- reactive({
    datasetsel() %>%
      filter(gender != "OTHER") %>%
      distinct(user_id) %>% tally()
  })
  
  #grouped total database valuebox
  output$mult_totalusers <- renderValueBox(
    valueBox("Total Users", multtotalusertally(), icon = icon("users"))
  )
  
  #grouped female users tally
  multfemaleusertally <- reactive({
    datasetsel() %>%
      filter(gender != "OTHER") %>% 
      filter(gender == "FEMALE") %>%
      distinct(user_id) %>% tally()
  })
  
  #grouped female infobox
  output$mult_femaleusers <- renderValueBox(
    infoBox("Female Users", multfemaleusertally(), icon = icon("female"))
  )
  
  #grouped male user tally
  multmaleusertally <- reactive({
    datasetsel() %>%
      filter(gender != "OTHER") %>% 
      filter(gender == "MALE") %>%
      distinct(user_id) %>% tally()
  })
  #grouped male infobox
  output$mult_maleusers <- renderInfoBox(
    infoBox("Male Users", multmaleusertally(), icon = icon("male"))
  )
  #
  ##################################################################
  # Assorted Plots (for single/grouped conditions)
  ##################################################################
  #second plot, diagnosed_by_physician
  output$plot2 <- renderPlot({
    my.data %>% filter(name == input$condition) %>%
      ggplot(aes(x = race, fill = diagnosed_by_physician)) +
      ggtitle("Diagnosed by Physician") +
      xlab("Race") +
      ylab("Users") +
      geom_bar(position = "fill")
  })
  #third plot, takes_medication
  output$plot3 <- renderPlot({
    my.data %>% filter(name == input$condition) %>%
      ggplot(aes(x = race, fill = takes_medication)) +
      ggtitle("Takes Medication") +
      xlab("Race") +
      ylab("Users") +
      geom_bar(position = "fill")
  })
  #fourth plot, runs_in_family
  output$plot4 <- renderPlot({
    my.data %>% filter(name == input$condition) %>%
      ggplot(aes(x = race, fill = runs_in_family)) +
      ggtitle("Condition Runs in Family") +
      xlab("Race") +
      ylab("Users") +
      geom_bar(position = "fill")
  })
  #fifth plot, is_self_afflicted
  output$plot5 <- renderPlot({
    my.data %>% filter(name == input$condition) %>%
      ggplot(aes(x = race, fill = is_self_afflicted)) +
      ggtitle("Users Self Afflicted") +
      xlab("Race") +
      ylab("Users") +
      geom_bar(position = "fill")
  })
  #
  ##################################################################
  # Tab4 - United States Tab
  ##################################################################
  #
  ##################################################################
  # Tab5 - Statistics Tab
  ##################################################################
  
  #display which condition you are testing for with infoBox
  #reactive that feeds into CMH subset reactive
  selectedsinglecond <- reactive({
    print(input$stat_singlecond)
  })
  
  #output for infobox displaying reactive selected input for SINGLE condition
  output$CMHtestdisplay <- renderInfoBox(
    infoBox("Condition: ", selectedsinglecond(), icon = icon("medkit")
    )
  )
  
  
  #add select condition table for stats tab
  stat_singlecond.data <- reactive({
    my.data %>% filter(name == input$stat_singlecond) %>%
      filter(gender != "OTHER")
  })
  
  
  
  
  
  ###show subset as reactive to coerce into correct table format -> display flat table
  CMHtest.table <- reactive({
    my.data %>%
      mutate(has_condition =
               if_else(name == input$stat_singlecond,
                       "TRUE", "FALSE")) %>% 
      distinct(user_id, .keep_all = TRUE) %>%
      filter(gender != "OTHER") %>%
      group_by(gender, race, diagnosed_by_physician,
               takes_medication,
               has_condition) %>%
      tally() -> CMHdata.table
    
    CMHdata.table$race[CMHdata.table$race == "MIDDLE\nEASTERN"] <- "MIDDLE.E"
    flatTable <- xtabs(n ~ gender + diagnosed_by_physician + race, data = CMHdata.table)
    #display flattened table
    ftable(flatTable)
  })
  
  #print CMHtest
  #bind original data to filtered data from selected condition in first reactive?
  CMHtest.output <- reactive({
    my.data %>%
      mutate(has_condition =
               if_else(name == input$stat_singlecond,
                       "TRUE", "FALSE")) %>% 
      distinct(user_id, .keep_all = TRUE) %>%
      filter(gender != "OTHER") %>%
      #Select which variable to stratify here
      group_by(race, gender, diagnosed_by_physician,
               takes_medication,
               has_condition) %>%
      tally() -> singlecondCMH
    
    #remove space!
    singlecondCMH$race[singlecondCMH$race == "MIDDLE\nEASTERN"] <- "MIDDLE.E"
    
    #Select which variable to stratify here
    xtabCMH.table <- xtabs(n ~ gender + #can use gender here too!
                             diagnosed_by_physician + #select other variables here
                             race, data = singlecondCMH)
    mantelhaen.test(xtabCMH.table)
    
  })
  #print to normal output not table
  output$CMHtestoutput <- renderPrint({
    CMHtest.output()
  })
  
  #print output to datatable
  output$CMHtestformat <- renderPrint({
    CMHtest.table()
  })
}

