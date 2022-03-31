
ui <- fluidPage(theme = shinytheme("readable"),
                #setBackgroundImage(
                # src = "background-removebg.png"
                #),
                # setBackgroundColor("ghostwhite"),
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
                                      column(8, offset =2, align = "center", tags$p("On the Descriptive Statistics page, you can find information on all possible exchange locations. First, you can see a summary of the full dataset, the number of variables and some basic statistics. Then on the next pages, you only have to choose the country or city you are interested in, and you will see the basic characteristics of exchange students in that location, such as the number of male/female, bachelor/master students, their average age and the average amount of grant they have received. Finally, you can also see and compare all the universities in a certain city based on the characteristics of exchange students in that certain location."
                                      ))
                                    ),
                                    fluidRow(
                                      column(8, offset =2, align = "center", tags$p("The Exploratory Analysis page shows the connections between partner universities in the Erasmus+ program. You can see on an interactive plot which universities have sent exchange students to each other before. You can also check with which universities your home university has the strongest connections with."
                                      ))
                                    ),
                                    fluidRow(
                                      column(8, offset =2, align = "center", tags$p("Keep an eye out for the advanced network analysis as this will be released very soon! You sure don't want to miss it."
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
                                               fluidRow(
                                                 column(12, align = "center",
                                                        tags$img(height = 300,
                                                                 width = 700,
                                                                 src = "background-removebg.png"))),
                                      ),
                                      tabPanel("Preferred Country", 
                                               fluidRow(tags$h2("Preferred Country"), align = "center"),
                                               
                                               fluidRow(
                                                 column(8, offset =2, align = "center", "This page is for getting to know your potential host country. After choosing a country in the dropdown menu,
you will see what kind of students usually choose that country for their exchange in both a table and on graphs.")), 
                                               tags$br(),
                                               fluidRow(
                                                 selectInput(inputId = "hostc",
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
                                                 fluidRow(
                                                   selectInput(inputId = "desstat", label = "Choose a statistic", choices = v.choices),
                                                   plotOutput("countrysummarychart")
                                                 ),
                                               tags$br(),
                                               fluidRow(
                                                 column(12, align = "center",
                                                        tags$img(height = 300,
                                                                 width = 700,
                                                                 src = "background-removebg.png")))),
                                      
                                      tabPanel("Preferred City",
                                               fluidRow(align = "center", h3("Preferred City")),
                                               fluidRow(column(8, offset =2, align = "center",
                                                               tags$p("This page is for getting to know your potential host city. After choosing a city in the dropdown menu, you will see what kind of students usually choose that city for their exchange in both a table and on graphs.")
                                                               
                                               )),
                                               wellPanel(fluidRow(
                                                 pickerInput(inputId = "dreamcountry", label = "Choose your dream country", choices = v.host.countries, options = list(
                                                   title = "This is a placeholder")
                                                 ),
                                                 pickerInput(inputId = "hostcity", label = "Choose your dream city", choices = create_host_cities_per_country("Belgium"), options = list(
                                                   title = "This is a placeholder")
                                                 ),
                                                 tableOutput("hostcity"))),
                                               
                                               wellPanel(fluidRow(
                                                 pickerInput(inputId = "dreamcountry3", label = "Choose your dream country", choices = v.host.countries, options = list(
                                                   title = "This is a placeholder")
                                                 ),
                                                 pickerInput(inputId = "hostcity3", label = "Choose your dream city", choices = create_host_cities_per_country("Belgium"), options = list(
                                                   title = "This is a placeholder")
                                                 ),
                                                 tableOutput("hostcity3"))),
                                               
                                               
                                               tags$br(),
                                               fluidRow(
                                                 column(12, align = "center",
                                                        tags$img(height = 300,
                                                                 width = 700,
                                                                 src = "background-removebg.png"))),
                                      ),
                                      
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
                           
                           
                           navbarMenu("Exploratory Analysis", icon = icon("compass"),
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
                                                               tags$p("Note: The clustering coefficient of each network equals to 0 as two universities from the same country can never have a partnership within Erasmus+, so no triads can be formed."
                                                               ))),
                                               tags$hr(),
                                               #Tab 2, page 1 - Country exploration
                                               #wellPanel(
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
                                      
                                      #Tab 2, page 2 - University exploration
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
                                                        pickerInput("city_ue", choices = create_cities_per_country("Belgium"), label = "Select a city")
                                                        ),
                                                 column(4,align = "center",
                                                        pickerInput("university2.2", choices = create_univ_per_city("Brussels"), label = "Select a university")
                                                        )
                                               ),
                                               tags$hr(),
                                               fluidRow(align = "center",
                                                 visNetworkOutput("university"),
                                                  ),
                                               tags$hr(),
                                               fluidRow(align = "center",
                                                 dataTableOutput("university.summary"),
                                               ),
                                               
                                               tags$br(),
                                               fluidRow(
                                                 column(12, align = "center",
                                                        tags$img(height = 300,
                                                                 width = 700,
                                                                 src = "background-removebg.png"))),
                                      )    
                           ),
                           tabPanel("Advanced Analysis", icon = icon("globe-americas"),
                                    fluidRow(align = "center", tags$h2("Coming soon...")),
                                    tags$br(),
                                    fluidRow(
                                      column(12, align = "center",
                                             tags$img(height = 300,
                                                      width = 700,
                                                      src = "background-removebg.png"))),
                           ),
                           tabPanel("Grant prediction", 
                                    fluidRow(align = "center", tags$h2("Grant Prediction")),
                                    fluidRow(align = "center", "Here you can get an estimation of the grant you may receive, based on your home country, host country and the amount of time your exchange will take. Keep in mind that this is just an estimation based on data of previous years and may not be completely accurate. Your final grant will be assigned by the Erasmus+ organisation after checking your background."),
                                    fluidRow(align = "center", "The grant is calculated mainly based on the difference of life costs between the home and host countries, and on the time spent for the exchange."),
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
                           
                )
                )
) 
