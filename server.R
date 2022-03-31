
function(input, output, session) {
  
  #trial on summary stats table
  output$summarytable <- renderTable(descriptive.table)
  
  output$hostcountry <- renderTable(host.country.table(input$hostc))
  
  output$countrysummarychart <- renderPlot({
    country.chart(input$desstat)})
  
  output$hostcity <- renderTable(host.city.summary.table(input$hostcity))
  
  output$hostcity3 <- renderTable(host.city.summary.table(input$hostcity3))
  
  
  
  output$unishostcity <- renderDataTable(dt.host.organisation.summary.city.input(input$hostcityinput))
  
  #trial on summary stats table
  
  
  ###page 1.3
  observeEvent(input$dreamcountry,{
    updatePickerInput(session, inputId = "hostcity", choices = create_host_cities_per_country(input$dreamcountry))
  })
  
  observeEvent(input$dreamcountry3,{
    updatePickerInput(session, inputId = "hostcity3", choices = create_host_cities_per_country(input$dreamcountry3))
  })
  
  
  
  
  ###
  
  
  # Page 2-1
  output$universities.countries <- renderVisNetwork({
    create_subgraph(input$home_country,input$host_country, input$min_students, input$level2.1)
  })
  
  output$universities.countries.average.metrics <- renderTable({
    create_subgraph_average_metrics(input$home_country,input$host_country, input$min_students, input$level2.1)
  })
  output$universities.countries.detailed.metrics <- renderPlot({
    create_subgraph_detailed_metrics(input$home_country,input$host_country, input$min_students, input$level2.1)
  })
  
  
  # Page 2-2
  output$university <- renderVisNetwork({
    create_subgraph_university(input$university2.2)
  })
  
  output$university.summary <- renderDataTable({
    create_subgraph_university_metrics(input$university2.2)
  })  
  
  observeEvent(input$country_ue,{
    updatePickerInput(session, inputId = "city_ue", choices = create_cities_per_country(input$country_ue))
  })
  
  observeEvent(input$city_ue,{
    updatePickerInput(session, inputId = "university2.2", choices = create_univ_per_city(input$city_ue))
  })
  
  # 3.2 Grant Prediction
  output$grant <- renderTable({
    predict_grant(input$home_gp, input$host_gp, input$length)
  })
  
  
}
