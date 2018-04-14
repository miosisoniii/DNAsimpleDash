#run script for filters and renaming of strings for easier plotting
source("functions.R")


server <- function(input, output, session){
  
  # Tab1 - Database Summary Tab
  ##################################################################
  #Total Database - reactive, render*
  #summary data for race  
  summarydata <- reactive({
    my.data %>% 
      filter(race == input$racecomp) %>%
      filter(gender != "OTHER") %>%
      distinct(user_id, .keep_all = TRUE)
  })
  
  #plot summary plot, age?
  output$summaryplot <- renderPlot({
    summarydata() %>% ggplot(aes(x = gender, y = age)) +
      geom_boxplot() +
      xlab("Age") +
      ylab("Gender")
  })

  #filter for all data
  piedata <- reactive({
    my.data %>%
      distinct(user_id, .keep_all = TRUE) %>%
      group_by(race) %>%
      tally() %>%
      na.omit()
  })
  #render plotly pie chart
  output$piechart <- renderPlotly(
    plot_ly(data = piedata(), 
            labels = ~race, 
            values = ~n, 
            type = 'pie',
            textposition = 'outside',
            textinfo = 'label+percent') %>%
      layout(
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  )

  

  #total user tally for race
  totalusertally <- reactive({
    my.data %>% filter(race == input$racecomp) %>%
      filter(gender != "OTHER") %>%
      distinct(user_id) %>% tally()
  })
  
  output$totalusers <- renderValueBox(
    valueBox(totalusertally(), 
             paste0("Total Users, Race: ", input$racecomp), 
             icon = icon("users"))
  )
  
  #female users tally
  femaleusertally <- reactive({
    summarydata() %>%
      filter(gender == "FEMALE") %>%
      distinct(user_id) %>% tally()
  })
  output$femaleusers <- renderValueBox(
    infoBox("Female Users", femaleusertally(), icon = icon("female"))
  )
  
  #male user tally
  maleusertally <- reactive({
    summarydata() %>% 
      filter(gender == "MALE") %>% 
      distinct(user_id) %>% tally()
  })
  output$maleusers <- renderInfoBox(
    infoBox("Male Users", maleusertally(), icon = icon("male"))
  )
  
  
  # top ethnicities facet FOR EACH RACE
  selrace.data <- reactive({
    summarydata() %>% 
      group_by(ethnicity) %>%
      tally() %>%
      arrange(. , desc(n)) -> ethnicity.count
    
    top_n(ethnicity.count, 5, n) -> topethnicities
    #join only top 5 ethnicities
    inner_join(summarydata(), topethnicities, by = "ethnicity")
  })
  
  #plot top ethnicities age distribution
  output$ethnicities_raceplot <- renderPlot({
    summarydata() %>%
    ggplot(aes(x = age)) +
      facet_grid(gender~ethnicity,
                 scales = "free_y",
                 labeller = as_labeller(name_adjust)) +
      theme_classic() +
      xlab("Age") +
      ylab("Count") +
      geom_histogram(bins = 20)
      #ggtitle(paste0("Age Distribution of DNAsimple Donor Ethnicities for ", input$racecomp))
  })
  
  #plot totals for each race
  output$totalethnicitycounts <- renderPlot({
    summarydata() %>%
      ggplot(aes(x = ethnicity)) +
      scale_x_discrete(labels = name_adjust) +
      geom_bar(stat = "count") +
      geom_text(stat ='count', aes(label=..count..), vjust= -0.5) +
      ylab("Users") +
      xlab("Ethnicity")
  })
  
  
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
      geom_boxplot() +
      labs(fill = "Race")
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
  
  # Single Cond Plots
  # is self afflicted, count
  output$self_afflicted.count <- renderPlot({
    singlecond.data() %>% 
      filter(!is.na(is_self_afflicted)) %>%
      ggplot(aes(x = is_self_afflicted)) +
      xlab("Self Afflicted") +
      ylab("Users") +
      geom_bar(stat = "count") +
      geom_text(stat='count', aes(label=..count..), vjust = -0.5,
                position = position_dodge(0.9), size=3.5)
  })
  
  #takes medication, count
  output$takes_med.count <- renderPlot({
    singlecond.data() %>%
      ggplot(aes(x = takes_medication)) +
      xlab("Takes Medication") +
      ylab("Users") +
      geom_bar(stat = "count") +
      geom_text(stat='count', aes(label=..count..), vjust = -0.5,
                position = position_dodge(0.9), size=3.5)
      
  })
  
  # diagnosed by physician, count
  output$diagnosed_phys.count <- renderPlot({
    singlecond.data() %>%
      ggplot(aes(x = diagnosed_by_physician)) +
      xlab("Diagnosed") +
      ylab("Users") +
      geom_bar(stat = "count") +
      geom_text(stat='count', aes(label=..count..), vjust = -0.5,
                position = position_dodge(0.9), size=3.5)
  })
  ###################################################
  #REORDER CHECKBOX!
  
  # diagnosed by physician counts across gender, count
  output$diagnosed.test.ethnicity <- renderPlot({
    my.data %>% filter(name == input$sel_singlecond) %>%
      filter(ethnicity != "WHITE_EUROPEAN") %>%
      ggplot(aes(x = reorder(ethnicity, -table(ethnicity)[ethnicity]), 
                 fill = diagnosed_by_physician)) +
      ggtitle("Users Diagnosed by Physician") +
      xlab("Race") +
      ylab("Users") +
      scale_x_discrete(labels = name_adjust) +
      geom_bar(position = "dodge") +
      theme(legend.position = c(0.8, 0.8)) +
      labs(fill = "Diagnosed by Physician") +
      geom_text(stat='count', aes(label=..count..), vjust = -0.5,
                position = position_dodge(0.9), size=3.5) +
      scale_fill_brewer(palette="Paired")
  })
  

  ##################################################################
  # Tab2 - Single Condition Tab, Select Variable
  ##################################################################
  
  selectvariable <- reactive({
    switch(input$sel_var,
           "Diagnosed by Physician" =  "diagnosed_by_physician",
           "Takes Medication" = "takes_medication",
           "Is Self Afflicted" = "is_self_afflicted")
  })
  
  selectdemo <- reactive({
    switch(input$sel_demographic.test,
           "Gender" =  "gender",
           "Race" = "race",
           "Ethnicity" = "ethnicity",
           "Age Group" = "age.group")
  })
  
  output$selectvariableplot <- renderPlot({
    if(input$checkbox.white != FALSE) {
      singlecond.data() %>%
        ggplot(aes_string(x = selectdemo(),
                          fill = selectvariable()), na.rm = TRUE) +
        ggtitle(paste0(input$sel_var, 
                       " for ",
                       input$sel_singlecond, 
                       ", by ", 
                       input$sel_demographic.test)) +
        scale_x_discrete(labels = name_adjust) +
        geom_bar(stat = "count", position = "dodge") +
        theme(legend.position = c(0.8, 0.8)) +
        labs(fill = input$sel_var) +
        ylab("Users") +
        xlab(input$sel_demographic.test) +
        geom_text(stat='count', aes(label=..count..), vjust = -0.5,
                  position = position_dodge(0.9), size=3.5) +
        scale_fill_brewer(palette = "Paired")
    }
    else{
      singlecond.data() %>%
        filter(ethnicity != "WHITE_EUROPEAN") %>%
        ggplot(aes_string(x = selectdemo(),
                          fill = selectvariable()), na.rm = TRUE) +
        geom_bar(stat = "count", position = "dodge") +
        scale_x_discrete(labels = name_adjust) +
        ggtitle(paste0(input$sel_var, 
                       " for ", 
                       input$sel_singlecond, 
                       ", by ", 
                       input$sel_demographic.test)) +
        theme(legend.position = c(0.8, 0.8)) +
        labs(fill = input$sel_var) +
        ylab("Users") +
        xlab(input$sel_demographic.test) +
        geom_text(stat='count', aes(label=..count..), vjust= -0.5, 
                  position = position_dodge(0.9), size=3.5) +
        scale_fill_brewer(palette = "Paired")
      
      
    }
  })
  
  
  ##################################################################
  # Tab3 - Grouped Conditions Tab
  ##################################################################
  
  #for plotting grouping breakdown
  output$groupbreakdown <- renderPlot({
    datasetsel() %>% 
      ggplot(aes(x = reorder(name, -table(name)[name]), 
                 fill = reorder(name, -table(name)[name]))) +
      geom_bar(color = "black") +
      coord_flip() +
      theme(legend.position = "none") +
      scale_x_discrete(labels = name_adjust) +
      geom_text(stat='count', aes(label=..count..), hjust= -0.2) +
      ylab("Conditions") +
      xlab("Users")
    
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
    valueBox("Total Users", multtotalusertally(), color = "green", icon = icon("users"))
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
    infoBox("Female Users", multfemaleusertally(), color = "green", icon = icon("female"))
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
    infoBox("Male Users", multmaleusertally(), color = "green", icon = icon("male"))
  )
  
  # check box select condition from group
  #reactive for condition names
  conditionnames <- reactive({
    unique(datasetsel()$name)
  })
  
  #display checkboxes for selected group
  observeEvent(datasetsel(), {
    updateCheckboxGroupInput(session, 
                        "groupedcondcheckbox",
                        choices = conditionnames(),
                        selected = conditionnames()[1])
  })
  
  #choose by UPDATED checkbox input
  groupedselectedcond <- reactive({
    datasetsel() %>% filter(name %in% input$groupedcondcheckbox)
  })
  
  #plot for only selected conditions
  output$groupedselectedconditionplot <- renderPlot(
      ggplot(data =  groupedselectedcond(),
             aes(x = diagnosed_by_physician)) +
        geom_bar(stat = "count") +
        ylab("Users") +
        xlab("Diagnosed by Physician") +
        geom_text(stat='count', aes(label=..count..), vjust = -0.5,
                  position = position_dodge(0.9), size=3.5)  
  )
    

  #
  ##################################################################
  # Tab4 - United States Plot Tab
  ##################################################################
  
  # US plot reactive
  usmap.data <- reactive({
    
    #set map data to abbreviated name
    states <- map_data("state")
    state.abbr.name <- tibble("state.abbr" = state.abb, "state.fullname" = state.name)
    #head(state.abbr.name)
    
    #change column name to state.abbr to join with state.abbr.name
    us_selcond.data() %>%
      rename(state.abbr = state) %>% 
      inner_join(state.abbr.name) -> db_state.abbr
    
    db_state.abbr %>%
      group_by(state.fullname, hascond.us) %>%
      tally() %>%
      spread(hascond.us, n) %>%
      rename(noncond = "FALSE", has.condition = "TRUE") -> cond.by.state
    
    cond.by.state %>%
      #undo grouping for tallying to rename region
      ungroup() %>%
      #rename columns using function as newname = oldname
      rename(region = state.fullname) %>%
      #make lowercase
      mutate(region = tolower(region)) %>%
      left_join(states)
  })
  
  # render output to map
  output$maptotal <- renderPlot({
    ggplot(usmap.data()) +
    geom_polygon(aes(x = long,
                     y = lat,
                     fill = has.condition,
                     group = group)) -> uscondmap
  uscondmap +
    ggtitle("DNAsimple Donor Condition Demographics") +
    coord_fixed(1.5)
  })
  
  output$mapratio <- renderPlot({
    usmap.data() %>%
      group_by(region) %>%
      mutate(condition.ratio =
               has.condition / (noncond + has.condition)) -> ratiomap
    
      ggplot(ratiomap) +
      geom_polygon(aes(x = long,
                       y = lat,
                       fill = condition.ratio,
                       group = group)) +
      scale_fill_gradient(name = "Condition Ratio",
                          low = "yellow", high = "red") +
      coord_fixed(1.5) +
      ggtitle("DNAsimple User with Condition Ratio")
  })
  
  
  
  # USplot reactive for selected condition TRUE FALSE for having condition
  us_selcond.data <- reactive({
    my.data %>%
      mutate(hascond.us = 
               if_else(name == input$sel_uscond,
                       "TRUE", "FALSE")) %>%
      distinct(user_id, .keep_all = TRUE) %>%
      filter(gender != "OTHER")
  })
  
  # USplot reactive for counting total users
  us_totaltally <- reactive({
    us_selcond.data() %>% tally()
  })
  
  # USplot single cond render valuebox for TOTAL users
  output$usmap_total <- renderValueBox(
    valueBox("Total Users", us_totaltally(), icon = icon("users")
    )
  )
  
  # USplot users with condition reactive
  us_hascondtotaltally <- reactive({
    us_selcond.data() %>% filter(hascond.us == "TRUE") %>% tally()
  })
  # USplot render infobox for users WITH conditon
  output$usmap_hascond <- renderInfoBox(
    infoBox("Users with Condition", us_hascondtotaltally(), icon = icon("thumbs-up")
    )
  )
  
  # USplot users NO condition reactive
  us_nocondtotaltally <- reactive({
    us_selcond.data() %>% filter(hascond.us == "FALSE") %>% tally()
  })
  # USplot render infobox for users NO condition
  output$usmap_nocond <- renderInfoBox(
    infoBox("Users without Condition", us_nocondtotaltally(), icon = icon("thumbs-down")
    )
  )
  
  
  
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
    infoBox("Condition: ", selectedsinglecond(), color = "yellow", icon = icon("medkit")
    )
  )
  

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
      tally() %>% 
      mutate(n = ceiling(n/2)) ######## SJS
  })
  
  #print cross-tabulated data
  output$CMHtestformat <- renderPrint({
    CMHtest.table() -> CMHdata.table
    CMHdata.table$race[CMHdata.table$race == "MIDDLE\nEASTERN"] <- "MIDDLE.E"
    Table <- xtabs(n ~ gender + has_condition + race, data = CMHdata.table)
    ftable(Table)
  })
  
  
  #print CMH test output
  output$CMHtestoutput <- renderPrint({
    CMHtest <- xtabs(n ~ gender + has_condition + race, data = CMHtest.table())
    mantelhaen.test(CMHtest)
    # IF Woolf Test is SIGNIFICANT, then CMH test is not needed.
    #woolf_test(CMHtest)
  })

  
  
  #create format for error bars
  # CMHplot.data <- reactive({
  #   #to be used if able to get proportion
  #   
  #   # fun.low = function(x){
  #   #   binom.test(x["Count"], x["Total"],
  #   #              0.5)$conf.int[1]
  #   # }
  #   # fun.up = function(x){
  #   #   binom.test(x["COUNT"], x["Total"],
  #   #              0.5)$conf.int[2]
  #   # }
  #   # 
  #   # 
  #   # Data = mutate(CMHtest.table(),
  #   #               low.ci = apply(CMHtest.table()[c("Count", "Total")], 1, fun.low),
  #   #               upper.ci = apply(CMHtest.table()[c("Count", "Total")], 1, fun.up))
  #   # Data
  #   
  #   #to be used if unable to get proportion?
  #   # CMHtest.table() -> CMHdata.table
  #   # CMHdata.table$race[CMHdata.table$race == "MIDDLE\nEASTERN"] <- "MIDDLE.E"
  #   # Table <- xtabs(n ~ gender + race, data = CMHdata.table)
  #   # as.data.frame(Table) -> df
  #   # df %>% 
  #   #   rename(n = Freq) %>%
  #   # mutate(percent = n/sum(n),
  #   #          error = sqrt((percent * (1-percent))/n))
  #   library(data.table)
  #   CMHtest.table() -> CMHdata.table
  #   CMHdata.table$race[CMHdata.table$race == "MIDDLE\nEASTERN"] <- "MIDDLE.E"
  #   Table <- xtabs(n ~ gender + has_condition + race, data = CMHdata.table)
  #   as.data.frame(Table) -> df
  #   setDT(df)[,prop:=Freq/sum(Freq),by=race] -> df2
  #   
  #   df2 %>% mutate(Total = Freq/prop)
    
    # fun.low = function(x){
    #   binom.test(x["Freq"], x["Total"],
    #              0.5)$conf.int[1]
    # }
    # fun.up = function(x){
    #   binom.test(x["COUNT"], x["Total"],
    #              0.5)$conf.int[2]
    # }
    # 
    # 
    # Data = mutate(CMHtest.table(),
    #               low.ci = apply(CMHtest.table()[c("Count", "Total")], 1, fun.low),
    #               upper.ci = apply(CMHtest.table()[c("Count", "Total")], 1, fun.up))
    #extract Confidence interval from CMH test
    # CMHtest <- xtabs(n ~ gender + has_condition + race, data = CMHtest.table())
    # mantelhaen.test(CMHtest) -> ct
    # #str(ct)
    # 
    # ct$conf.int[1:2] -> ct1
    # ct1
    
  #})
  
  
  # #print CMH CMHdata table
  # output$CMHplot.table <- renderPrint({
  #   CMHplot.data()
  # })
  # 
  # 
  # #print CMH test plot
  # output$CMHplot <- renderPlot({
  #   ggplot(CMHplot.data(), aes(race, percent, fill = gender)) +
  #     geom_col(position = "dodge") +
  #     geom_errorbar(aes(ymin = percent - error,
  #                       ymax = percent + error),
  #                   position = position_dodge(0.9), width = 0.2)
  # })
  
}


