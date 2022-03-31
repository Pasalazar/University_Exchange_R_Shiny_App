
function(input, output, session) {
  
  #trial on summary stats table
  output$summarytable <- renderTable(dt.descriptive.table)
  
  output$hostcountry <- renderTable(host.country.table(input$country.host))
  
  output$countrysummarychart <- renderPlot({
    country.chart(input$desstat)})
  
  
  
  ## 1.3 new output 
  
  output$twohostcities <- renderTable(host.city.summary.table.two.cities(input$hostcityone, input$hostcitytwo))
  
  
  
  output$unishostcity <- renderDataTable(dt.host.organisation.summary.city.input(input$hostcityinput))
  
  
  ### Page 1.3 New
  observeEvent(input$dreamcountryone,{
    updatePickerInput(session, inputId = "hostcityone", choices = create.host.cities.per.country(input$dreamcountryone))
  })
  
  observeEvent(input$dreamcountrytwo,{
    updatePickerInput(session, inputId = "hostcitytwo", choices = create.host.cities.per.country(input$dreamcountrytwo))
  })
  
  
  output$dynamictext <- renderText(descriptive.text(input$desstat))
  
  
  ###
  
  #Page 2.1
  
  output$networkdesc <- renderTable(dt.network.descriptives)
  
  output$networkdegree <- renderPlot(ggplot.degree.network)
  
  output$network.degree.zoomed <- renderPlot(ggplot.degree.network.zoomed)
  
  output$unitable <- renderTable(dt.uni.network.descriptives)
  
  output$unidegree <- renderPlot(ggplot.degree.universities)
  
  output$uni.degree.zoomed <- renderPlot(ggplot.degree.universities.zoomed)
  
  #Page 2-2
  output$universities.countries <- renderVisNetwork({
    create.subgraph(input$home_country,input$host_country, input$min_students, input$level2.1)
  })
  
  output$universities.countries.average.metrics <- renderTable({
    create.subgraph.average.metrics(input$home_country,input$host_country, input$min_students, input$level2.1)
  })
  output$universities.countries.detailed.metrics <- renderPlot({
    create.subgraph.detailed.metrics(input$home_country,input$host_country, input$min_students, input$level2.1)
  })
  
  #Page 2-3
  output$university <- renderVisNetwork({
    create.subgraph.university(input$university2.2)
  })
  
  output$university.summary <- renderDataTable({
    create.subgraph.university.metrics(input$university2.2)
  })  
  
  
  observeEvent(input$country_ue,{
    updatePickerInput(session, inputId = "city_ue", choices = create.cities.per.country(input$country_ue))
  })
  
  observeEvent(input$city_ue,{
    updatePickerInput(session, inputId = "university2.2", choices = create.univ.per.city(input$city_ue))
  })
  
  # Advanced Analysis
  
  observeEvent(input$lpcountry,{
    updatePickerInput(session, inputId = "lpcity", choices = create.cities.per.country(input$lpcountry))
  })
  
  observeEvent(input$lpcity,{
    updatePickerInput(session, inputId = "checkuni", choices = create.univ.per.city.lp(input$lpcity))
  })
  dataa <- eventReactive(input$go, find.potential.partner(input$checkuni, input$topx))
  output$topp <- renderTable(dataa())
  
  # 3.2 Grant Prediction
  output$grant <- renderTable({
    predict.grant(input$home_gp, input$host_gp, input$length)
  })
}



