
ui <- fluidPage(theme = shinytheme("readable"),
                
                # Navbar structure for UI
                navbarPage(title = "Shiny Exchange", collapsible=TRUE,
                           tabPanel("Home", icon = icon("home"),
                                    fluidRow(h1("Welcome to Shiny Exchange"), align = "center")
                                    ,
                                    fluidRow(
                                      column(8, offset =2, align = "center",
                                             tags$p("Welcome to Shiny Exchange, the app where you can find your dream exchange destination in a few clicks!
Always dreamed about learning Italian? Or maybe you are more into meeting as many different cultures as possible? Do you need to find a place where financial help is available for exchange students? If your answer is yes to any of these questions, then you are at the right place!
"))),
                                    fluidRow(
                                      column(8, offset =2, align = "center", tags$p("On the Descriptive Statistics page, you can find information on all possible exchange locations. First, you can see a summary of the full dataset, the number of variables and some basic statistics. Then on the next pages, you only have to choose the country or city you are interested in, and you will see the basic characteristics of exchange students in that location, such as the number of male/female, bachelor/master students, their average age and the average amount of grant they have received per month. Finally, you can also see and compare all the universities in a certain city based on the characteristics of exchange students in that certain location."
                                      ))
                                    ),
                                    fluidRow(
                                      column(8, offset =2, align = "center", tags$p("The Exploratory Analysis page shows the connections between partner universities in the Erasmus+ program. You can see on an interactive plot which universities have sent exchange students to each other before. You can also check with which universities your home university has the strongest connections with."
                                      ))
                                    ),
                                    fluidRow(
                                      column(8, offset =2, align = "center", tags$p("Finally, we also offer some interesting insights on the Advanced Analytics page. Under Link Prediction, you can see the most likely new partners of your own university based on common neighbors analysis. This provides a chance for universities who are looking to expand their network of partner organisations to see which partnerships they should pursue, as well as for students planning their exchange in the following years. Under the Grant Prediction tab, you will be able to calculate the expected grant for your exchange based on your home and host country and the length of your study period."
                                      ))),
                                    fluidRow(
                                      column(8, offset =2, align = "center", tags$p(tags$b("Have fun!")))
                                    ),
                                    
                                    #  column(3, offset = 9,  
                                    #         tags$img(height = 250,
                                    #                 width = 300,
                                    #                src = "background.jpeg"))
                                    
                                    tags$hr(),
                                    fluidRow(h3("Meet the team"), align = "center"),
                                    tags$br(),
                                    fluidRow(
                                      column(3, align = "center",
                                             tags$img(height = 250, 
                                                      width = 250, 
                                                      src = "adrienn-modified.png")),
                                      column(3, align = "center",
                                             tags$img(height = 250, 
                                                      width = 250, 
                                                      src = "jonah-modified.png")),
                                      column(3, align = "center",
                                             tags$img(height = 250, 
                                                      width = 250, 
                                                      src = "pablo-modified.png")),
                                      column(3, align = "center",
                                             tags$img(height = 250, 
                                                      width = 250, 
                                                      src = "jelmer-modified.png"))
                                    ),
                                    tags$br(),
                                    fluidRow(
                                      column(3, align = "center",
                                             (tags$a(href = "https://www.linkedin.com/in/adrienn-t%C3%B3th-bb78b1133/", "Adrienn Tóth"))),
                                      column(3, align = "center",
                                             tags$a(href = "https://www.linkedin.com/in/jonahchanwh/", "Jonah Chan")),
                                      column(3, align = "center",
                                             tags$a(href = "https://www.linkedin.com/in/pablo-salazar-pastene/", "Pablo Salazar")),
                                      column(3, align = "center",
                                             tags$a(href = "https://www.linkedin.com/in/jelmer-van-slooten/", "Jelmer van Slooten"),
                                      )
                                      
                                    ),
                                    tags$br(),
                                    fluidRow(
                                      column(12, align = "center",
                                             tags$img(height = 300,
                                                      width = 700,
                                                      src = "background-removebg.png"))),
                                    
                           ),
                           
                           ##DESCRIPTIVE STATISTICS
                           
                           #Descriptive statistics - Basic descriptives
                           navbarMenu("Descriptive Statistics", icon = icon("table"),
                                      tabPanel("Basic Descriptives", 
                                               fluidRow(column(8, offset =2, align = "center", tags$h2("Basic Descriptives"))),
                                               fluidRow(
                                                 column(8, offset =2, align = "center", "This page shows you the basic characteristics of the dataset we used to create the app.
In the below table, you can see how many variables are available in the dataset and explore some basic descriptive statistics.
")), 
                                               tags$br(),
                                               fluidRow(
                                                 column(8, offset =2, align = "center",
                                                        tableOutput("summarytable"), #trial on summary stats
                                                 )),
                                               tags$br(),
                                               fluidRow(column(8, offset = 2, align = "center", "The data used for the app was retrieved from ", tags$a(href = "https://data.europa.eu/data/datasets/erasmus-mobility-statistics-2011-12?locale=en", "The European Union Open Data Portal."), " The data contains the Erasmus mobility statistics of the school year 2011-2012.")),
                                               fluidRow(
                                                 column(12, align = "center",
                                                        tags$img(height = 300,
                                                                 width = 700,
                                                                 src = "background-removebg.png"))),
                                      ),
                                      
                                      #Descriptive statistics - Preferred country
                                      tabPanel("Preferred Country", 
                                               fluidRow(tags$h2("Preferred Country"), align = "center"),
                                               
                                               fluidRow(
                                                 column(8, offset =2, align = "center", "This page is for getting to know your potential host country. After choosing a country in the dropdown menu,
you will see what kind of students usually choose that country for their exchange in both a table and on graphs.")), 
                                               tags$br(),
                                               wellPanel(align = "center",
                                                         selectInput(inputId = "country.host",
                                                                     label = "Select your dream host country", choices = dt.host.countries.sorted,
                                                         )
                                               ),
                                               fluidRow(
                                                 tableOutput("hostcountry"),
                                               ),
                                               tags$hr(),
                                               tags$br(),
                                               fluidRow(column(8, offset = 2, align = "center", "In the graph below you can select one of the variables that can also be seen in the table above. By selecting one of the variables, the plot will show the results for all the countries in the dataset. In this way, you can easily compare countries on a specific variable.")),
                                               tags$br(),
                                               fluidRow(align = "center",
                                                        selectInput(inputId = "desstat", label = "Choose one of the variables", choices = v.choices),
                                                        
                                               ),
                                               fluidRow(plotOutput("countrysummarychart")),
                                               tags$br(),
                                               fluidRow(column(8, offset =2, align = "center", textOutput("dynamictext"))),
                                               tags$br(),
                                               fluidRow(
                                                 column(12, align = "center",
                                                        tags$img(height = 300,
                                                                 width = 700,
                                                                 src = "background-removebg.png")))),
                                      
                                      #Descriptive statistics - Preferred city
                                      tabPanel("Preferred City",
                                               fluidRow(align = "center", h3("Preferred City")),
                                               fluidRow(column(8, offset =2, align = "center",
                                                               tags$p("This page is for getting to know your potential host city. After choosing a city in the dropdown menu, you will see what kind of students usually choose that city for their exchange in both a table and on graphs.")
                                                               
                                               )),
                                               wellPanel(fluidRow(column(6, pickerInput(inputId = "dreamcountryone", label = "Choose a dream country", choices = v.host.countries, options = list(title = "This is a placeholder")),
                                                                         pickerInput(inputId = "hostcityone", label = "Choose a dream city", choices = create.host.cities.per.country("Belgium"), options = list(title = "This is a placeholder"))),
                                                                  column(6, pickerInput(inputId = "dreamcountrytwo", label = "Choose another dream country", choices = v.host.countries, options = list(title = "This is a placeholder"
                                                                  )),
                                                                  pickerInput(inputId = "hostcitytwo", label = "Choose another dream city", choices = create.host.cities.per.country("Belgium"), options = list(title = "This is a placeholder"))))),
                                               
                                               
                                               fluidRow(tableOutput("twohostcities")),
                                               
                                               
                                               
                                               tags$br(),
                                               fluidRow(
                                                 column(12, align = "center",
                                                        tags$img(height = 300,
                                                                 width = 700,
                                                                 src = "background-removebg.png"))),
                                      ),
                                      
                                      #Descriptive statistics - Universities in city
                                      tabPanel("Universities in City",
                                               fluidRow(align = "center", h3("Universities in city")),
                                               fluidRow(column(8, offset =2, align = "center",
                                                               tags$p("In the table below, you can compare the universities located in your dream city. You can see how many and what kind of students typically choose each university as their exchange location, and also find information about available grants.")
                                               )),
                                               wellPanel(pickerInput(inputId = "hostcityinput", label = "Compare Universities in your dream city", choices = v.host.cities,  options = list(
                                                 title = "This is a placeholder"), selected = "London"
                                               )),
                                               
                                               dataTableOutput("unishostcity"),
                                               tags$br(),
                                               fluidRow(
                                                 column(12, align = "center",
                                                        tags$img(height = 300,
                                                                 width = 700,
                                                                 src = "background-removebg.png"))),
                                      )),
                           
                           
                           ##EXPLORATORY ANALYSIS
                           navbarMenu("Exploratory Analysis", icon = icon("compass"),
                                      
                                      tabPanel("Network descriptives",
                                               fluidRow(column(8, offset =2, align = "center",
                                                               tags$h2("Network descriptives"))),
                                               
                                               
                                               
                                               
                                               
                                               fluidRow(column(8, offset =2, align = "center",
                                                               tags$p("Welcome to the network descriptives page. On this page, you can see the basic descriptives of two networks built based on the data: first, a network displaying the connections between universities based on the number of students exchanged between them and second, a network between partner universities when they are only connected with one edge if there is at least one student exchanged between the two schools."
                                                               ))),
                                               tags$hr(),
                                               fluidRow(column(8, offset =2, align = "center",
                                                               tags$h5("Network number of universities - each edge represents one student"))),
                                               fluidRow(column(8, offset =2, align = "center",
                                                               tags$p("The first section summarizes a directed network with the universities as nodes, with every edge representing one exchange student. The clustering coefficient is quite low, as the universities that are located in the same country can never be connected, so triads can only be formed between three universities that are each located in a different country. The diameter is 10, meaning that the biggest distance between two universities in the network is 10 steps. The average degree shows that the average number of students a university sends or welcomes on exchange is 160."
                                                               ))),
                                               
                                               fluidRow(align = "center", tableOutput("networkdesc")),
                                               tags$br(),
                                               fluidRow(column(8, offset =2, align = "center",
                                                               tags$p("It can be seen on the histogram below that the majority of universities have a lower number of exchange students, with a few exceptions who have above 2000 students. Thus, we can classify it as an in-between network, since the majority of the nodes have a lower degree but there is a weak tail with a few nodes that have higher degree. However, every university has a degree of at least 1, because only those who participate in the Erasmus+ program are part of the dataset."
                                                               ))),
                                               tags$br(),
                                               fluidRow(column(6, plotOutput("networkdegree")),
                                                        column(6, plotOutput("network.degree.zoomed"))),
                                               tags$hr(),
                                               fluidRow(column(8, offset =2, align = "center",
                                                               tags$h5("Network number of universities - each edge exists if there is at least one incoming or outgoing student between two universities "))),
                                               fluidRow(column(8, offset =2, align = "center",
                                                               tags$p("The second section shows an undirected network between universities, where two universities are connected by an edge if there is at least one student exchanged between them, that is why the number of edges is lower than in the table above. The clustering coefficient is the same as before as the same connections exist in this network as in the one below. The average path length between two universities is slightly lower, as the direction of the exchange does not matter here. For the same reason, the diameter is also lower as in the directed network. The average degree is 45, meaning that universities have 45 partner institutions on average."
                                                               ))),
                                               tags$br(),
                                               fluidRow(align = "center", tableOutput("unitable")),
                                               tags$br(), 
                                               fluidRow(column(6, plotOutput("unidegree")),
                                                        column(6, plotOutput("uni.degree.zoomed"))),
                                               tags$hr(),
                                               fluidRow(
                                                 column(12, align = "center",
                                                        tags$img(height = 300,
                                                                 width = 700,
                                                                 src = "background-removebg.png")))),
                                      
                                      
                                      #Exploratory analysis - Partner universities by country
                                      tabPanel("Partner universities by country", 
                                               fluidRow(column(8, offset =2, align = "center",
                                                               tags$h2("Partner universities by country"))),
                                               fluidRow(column(8, offset =2, align = "center",
                                                               tags$p("Welcome to the network exploration page. Here you can see the relationship between universities in any two countries on an interactive network graph after selecting the countries you are interested in and setting a minimum number of students to visualize between each pair of universities."
                                                               ))),
                                               fluidRow(column(8, offset =2, align = "center",
                                                               tags$p("Additionally, you can find the centrality measures of the visualized graph in the table below, such as the number of clusters, betweenness and the directed and undirected diameters."
                                                               ))),
                                               fluidRow(column(8, offset =2, align = "center",
                                                               tags$p("Note: The clustering coefficient of the network below equals to 0 as two universities from the same country can never have a partnership within Erasmus+, so no triads can be formed."
                                                               ))),
                                               
                                               
                                               fluidRow(
                                                 column(3,
                                                        pickerInput(inputId = "home_country", choices = v.home.countries,
                                                                    label = "Select a country", selected = "Germany")
                                                 ),
                                                 column(3,
                                                        pickerInput(inputId = "host_country", choices = v.host.countries,
                                                                    label = "Select a second country", selected = "Denmark")
                                                 ),
                                                 column(3,
                                                        numericInput(inputId = "min_students",
                                                                     label = "Minimum number of students that did their exchange between every 2 universities (edge weight)",
                                                                     min = 0, value = 10)
                                                 ),
                                                 column(3,
                                                        awesomeCheckboxGroup(inputId = "level2.1",
                                                                             label = "Study Level",
                                                                             choices = c("Master", "Bachelor"),
                                                                             selected = c("Master", "Bachelor"))
                                                 ),
                                               ),
                                               tags$hr(),
                                               fluidRow(
                                                 visNetworkOutput("universities.countries")
                                               ),
                                               tags$hr(),
                                               fluidRow(
                                                 tableOutput("universities.countries.average.metrics")
                                               ),
                                               fluidRow(
                                                 column(12, align = "left",
                                                        tags$h3("Data dictionary and interpretation"
                                                        ))),
                                               fluidRow(
                                                 column(12, align = "left",
                                                        tags$p("Below you can find some explanations for the metrics displayed in the table:"
                                                        ))),
                                               fluidRow(
                                                 column(12, align = "left",
                                                        tags$p(tags$b("Average number of students per edge:"), "This metric shows how many students were exchanged on average between two universities displayed on the network graph."
                                                        ))),
                                               fluidRow(
                                                 column(12, align = "left",
                                                        tags$p(tags$b("Number of clusters:"), "A cluster in a network is intuitively defined as a set of densely connected nodes that is sparsely connected to other clusters in the graph. This metric shows how many smaller clusters are formed between the universities in the two displayed countries."
                                                        ))),
                                               
                                               fluidRow(
                                                 column(12, align = "left",
                                                        tags$p(tags$b("Number of clusters:"), "A cluster in a network is intuitively defined as a set of densely connected nodes that is sparsely connected to other clusters in the graph. This metric shows how many smaller clusters are formed between the universities in the two displayed countries."
                                                        ))),
                                               
                                               fluidRow(
                                                 column(12, align = "left",
                                                        tags$p(tags$b("Average betweenness:"), "Betweenness measures how well situated a node is in terms of the shortest paths that it lies on. In other words, the higher the betweenness of a university, the more well-connected it is to other universities in the network by being in the close neihborhood of many other universities."
                                                        ))),
                                               
                                               fluidRow(
                                                 column(12, align = "left",
                                                        tags$p(tags$b("Directed diameter:"), "The diameter shows the shortest distance between the two most distant universities in the network. The directed diameter only takes into account the directed edges, so is a university is sending at least as many exchange students to another university as the minimum set in the above field."
                                                        ))),
                                               
                                               fluidRow(
                                                 column(12, align = "left",
                                                        tags$p(tags$b("Undirected diameter:"), "The undirected diameter shows the distance between the two most distant universities in the network, regardless of the direction of the relatinship. This value is either equal to or higher than the directed diameter, as more universities are connected in an undirected way so there is a possibility the largest distance becomes bigger."
                                                        ))),
                                               
                                               fluidRow(
                                                 column(12, align = "left",
                                                        tags$p(tags$b("Clustering coefficient"), "a clustering coefficient is a measure of the degree to which nodes in a graph tend to cluster together. The clustering coefficient of each network equals to 0 as two universities from the same country can never have a partnership within Erasmus+, so no triads can be formed."
                                                        ))),
                                               
                                               
                                               tags$hr(),
                                               fluidRow(
                                                 plotOutput("universities.countries.detailed.metrics")
                                               ),
                                               # ),
                                               tags$hr(),
                                               fluidRow(
                                                 column(12, align = "center",
                                                        tags$img(height = 300,
                                                                 width = 700,
                                                                 src = "background-removebg.png"))),
                                      ),
                                      
                                      #Exploratory analysis - University exploration
                                      tabPanel("University exploration",
                                               fluidRow(column(8, offset =2, align = "center",
                                                               tags$h2("University exploration"))),
                                               fluidRow(column(8, offset =2, align = "center",
                                                               tags$p("In this section, you can find all the partner universities of any selected university. If you would like to see the network and a list of the partners of your home university and the number of incoming and outgoing exchange students to and from each partner, you only have to choose the name of your university from the dropdown list."
                                                               ))),
                                               fluidRow(column(8, offset =2, align = "center",
                                                               tags$p("Additionally, this section can also help you find your desired host university. By choosing a country and city from the dropdown menus, you can see all the universities at that certain location and visualize the network of each institution in a few clicks."
                                                               ))),
                                               fluidRow(column(8, offset =2, align = "center",
                                                               tags$p("If you wish to know if your home institution has an ongoing partnership with another institution, you can do so by selecting your host institution and using the 'search' feature at the top of the table to look for your home institution."
                                                               ))),
                                               tags$hr(),
                                               fluidRow(
                                                 column(4,align = "center",
                                                        pickerInput("country_ue", choices = v.home.countries, label = "Select a country")
                                                 ),
                                                 column(4,align = "center",
                                                        pickerInput("city_ue", choices = create.cities.per.country("Belgium"), label = "Select a city")
                                                 ),
                                                 column(4,align = "center",
                                                        pickerInput("university2.2", choices = create.univ.per.city("Brussels"), label = "Select a university")
                                                 )
                                               ),
                                               tags$hr(),
                                               fluidRow(align = "center",
                                                        visNetworkOutput("university")
                                               ),
                                               tags$hr(),
                                               fluidRow(align = "center",
                                                        dataTableOutput("university.summary")
                                               ),
                                               
                                               tags$br(),
                                               fluidRow(
                                                 column(12, align = "center",
                                                        tags$img(height = 300,
                                                                 width = 700,
                                                                 src = "background-removebg.png")))
                                      )),
                           
                           ##ADVANCED ANALYSIS
                           
                           #Advanced analysis - Link prediction
                           
                           navbarMenu("Advanced Analysis", icon = icon("globe-americas"),
                                      tabPanel("Link prediction", 
                                               fluidRow(align = "center", tags$h2("Link prediction")),
                                               fluidRow(align = "center", tags$h5("Find the most likely new exchange partner for your university")),
                                               tags$br(),
                                               fluidRow(column(8, offset = 2, align = "center",
                                                                tags$p("In this section, you can find the most likely new exchange partners of a university. The most possible new partners are generated by the method of link prediction of the exchange newtwork based on number of common neighbors. This can help student identify potential exchange destinations that are currently not a partner of their university."))),
                                                                
                                               fluidRow(column(8, offset = 2, align = "center",
                                                               tags$p("For students, this page provides an insight into the most possible future partner universities of their home university. So if you are a student planning your exchange in the next years, make sure to check it out!"))),
                                                                
                                               fluidRow(column(8, offset = 2, align = "center",
                                                               tags$p("For universities looking to expand their network of partners, this page helps identify the best options for future partners. The link prediction is based on common neighbours, so the suggested new partners are the ones that the university already has many partners in common with. Therefore, forming partnerships with the suggested universities will help create a more connected partnership network for the university."))),tags$br(),
                                               wellPanel(fluidRow(align = "center",
                                                                  column(6, pickerInput("lpcountry", choices = v.home.countries, label = "Select a country", selected = "")),
                                                                  column(6, pickerInput("lpcity", choices = create.cities.per.country("Austria"), label = "Select a city")),
                                                                  column(6, pickerInput(inputId = "checkuni", choices = v.lp.school.list, label = "Select a university")),
                                                                  column(6, numericInput("topx", label = "Number of most likely partner universities", value = 10, min = 1, max = 100)),
                                                                  fluidRow(align = "center", 
                                                                           column(4, offset = 4, actionButton(inputId = "go", label = "Calculate"))))), tags$br(),
                                               fluidRow(align = "center", tableOutput("topp")),
                                               fluidRow(
                                                 column(12, align = "center",
                                                        tags$img(height = 300,
                                                                 width = 700,
                                                                 src = "background-removebg.png")))
                                      ), 
                                      tabPanel("Grant prediction", 
                                               fluidRow(align = "center", tags$h2("Grant Prediction")),
                                               fluidRow(column(8, offset = 2, align = "center", "Here you can get an estimation of the grant you may receive, based on your home country, host country and the amount of time your exchange will take. Keep in mind that this is just an estimation based on data of one school year and thus may not be completely accurate. Your final grant will be assigned by the Erasmus+ organisation after checking your background.")),
                                               tags$br(),
                                               fluidRow(column(8, offset = 2, align = "center", "The grant is calculated mainly based on the difference of life costs between the home and host countries, and on the time spent for the exchange. Click ", tags$a(href = "https://erasmus-plus.ec.europa.eu/opportunities/individuals/students/studying-abroad", "here"), "if you want to know more about studying abroad with the Erasmus+ programme.")),
                                               tags$hr(),
                                               fluidRow(
                                                 column(4, align = "center",
                                                        pickerInput("home_gp", choices = v.home.countries, label = "Select a home country", selected = "Netherlands")
                                                 ),
                                                 column(4, align = "center",
                                                        pickerInput("host_gp", choices = v.host.countries, label = "Select a host country", selected = "Spain")
                                                 ),
                                                 column(4, align = "center",
                                                        pickerInput("length", choices = dt.study.length$V1,label = "Choose the length of your study exchange in months", selected = 5.0)
                                                 )
                                               ),
                                               fluidRow(align = "center",
                                                        tableOutput("grant")
                                               ),
                                               tags$br(),
                                               fluidRow(
                                                 column(12, align = "center",
                                                        tags$img(height = 300,
                                                                 width = 700,
                                                                 src = "background-removebg.png"))),
                                               
                                      ),
                           )
                )
)

