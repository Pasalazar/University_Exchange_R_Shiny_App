# Loading the packages

library(shiny) 
library(rsconnect)
library(dplyr)
library(ggplot2)
library(igraph)
library(shinydashboard)
library(data.table)
library(shinyWidgets)
library(visNetwork)
library(shinythemes)
library(linkprediction)
library(bslib)
library(stargazer)


#setwd('C:\\Users\\p_sal\\OneDrive\\Escritorio\\RSM\\06.- B3 - Network Data Analytics\\01 Assignments\\Shiny App\\App')

dt.students.clean <- fread("processed-data.csv", encoding = "UTF-8")
  
##### Page 0: Home Page

##### Page 1: Descriptive Statistics
### Tab 1.1 Basic Descriptive


descriptive.names <- c("Number of students", "Number of Home Countries", "Number of Host Countries", 
                       "Number of Home Universities", "Number of Host Universities",
                       "Number of Placement Organisations", "Average Number of Students in Host University", "Number of Bachelor Stdent",
                       "Number of Master Student", "Number of Exchange Student on Fall Semester",
                       "Number of Exchange Student on Spring Semester", "Average Length of Study Period",
                       "Average Grant for Student", "Average Age of Student")
avg.host.uni.student <- (nrow(dt.students.clean) - nrow(dt.students.clean[MOBILITYTYPE == "P"]))/ length(unique(dt.students.clean$Host_organisation))
descriptive.stats <- c(nrow(dt.students.clean), 
                       length(unique(dt.students.clean$home_country)),
                       length(unique(dt.students.clean$host_country)),
                       length(unique(dt.students.clean$Home_organisation)),
                       length(unique(dt.students.clean$Host_organisation)), 
                       length(unique(dt.students.clean$PLACEMENTENTERPRISE)), 
                       format(round(avg.host.uni.student, 2), nsmall = 2),
                       nrow(dt.students.clean[study_level == "Bachelor"]),
                       nrow(dt.students.clean[study_level == "Master"]),
                       nrow(dt.students.clean[study_semester == "Fall"]),
                       nrow(dt.students.clean[study_semester == "Spring"]),
                       paste(format(round(mean(dt.students.clean$LENGTHSTUDYPERIOD), 2), nsmall = 2), " Months"),
                       paste("€ ", format(round(sum(dt.students.clean$STUDYGRANT, dt.students.clean$PLACEMENTGRANT) / nrow(dt.students.clean), 2), nsmall = 2)),
                       format(round(mean(dt.students.clean$AGE), 2), nsmall = 2)
)
descriptive.table <- setDT(as.data.frame(cbind(descriptive.names, descriptive.stats)))

descriptive.table <- setnames(descriptive.table, c("descriptive.names", "descriptive.stats"), c("Descriptive Statistic", "Value"))



### Tab 1.2: Preferred country

# Vector of all the unique host countries
dt.host.countries.sorted <- dt.students.clean[, "host_country"][!duplicated(host_country)]
dt.host.countries.sorted <- dt.host.countries.sorted[!host_country == "NA"]
dt.host.countries.sorted <- dt.host.countries.sorted[order(dt.host.countries.sorted$host_country)]



## Make a data table that shows in the rows the unique host countries
dt.host.country <- dt.students.clean[ , n_students_host_country := .N, by=host_country]
dt.host.country <- dt.host.country[, avg_grant := mean(STUDYGRANT), by=host_country]
dt.host.country <- dt.host.country[, avg_age := mean(AGE), by=host_country]
dt.host.country.summary <- dt.host.country[, list(n_students_host_country, avg_grant, avg_age), by = host_country]
dt.host.country.summary <- dt.host.country.summary[!duplicated(dt.host.country.summary[, c(host_country)]),]

# Make the function that subsets the data table by the input
host.country.table <- function(hostcountry) {
  dt.host.country.summary.unique <- dt.host.country.summary[host_country == hostcountry]
  
}


## Interactive chart
dt.host.country <- dt.students.clean[ , n_students_host_country := .N, by=host_country]
dt.host.country <- dt.host.country[, avg_grant := mean(STUDYGRANT), by=host_country]
dt.host.country <- dt.host.country[, avg_age := mean(AGE), by=host_country]
dt.host.country[, number_host_uni := length(unique(Host_organisation)), by=host_country]
dt.host.country.summary <- dt.host.country[, list(n_students_host_country, avg_grant, avg_age, number_host_uni), by = host_country]
dt.host.country.summary <- dt.host.country.summary[!duplicated(dt.host.country.summary[, c(host_country)]),]
dt.male <- dt.host.country[GENDER=="M"]
dt.male <- dt.male[, list(n_male = .N), by = host_country]
dt.host.country.summary <- left_join(dt.host.country.summary, dt.male, by = "host_country")
dt.female <- dt.host.country[GENDER=="F"]
dt.female <- dt.female[, list(n_female = .N), by = host_country]
dt.host.country.summary <- left_join(dt.host.country.summary, dt.female, by = "host_country")
dt.host.country.summary <- setnames(dt.host.country.summary, c("n_students_host_country", "avg_grant", "avg_age", "number_host_uni", "n_male", "n_female"),
                                    c("number of incoming exchange students", "average grant", "average age of student", "number of host universities", 
                                      "number of male student", "number of female student"))
dt.host.country.summary <- dt.host.country.summary[!host_country == ""]

v.choices <- c("number of incoming exchange students", "average grant", "average age of student", "number of host universities", 
               "number of male student", "number of female student")

# Create a function that returns a histogram
country.chart <- function(thestat) {
  return(ggplot(dt.host.country.summary, aes(x = host_country, y = .data[[thestat]])) + 
        geom_col(color = "blue", fill = "light blue") + labs(x = "Host Country") + 
        theme(panel.background = element_blank(), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 14), axis.text.y = element_text(size = 14)))
}

### Tab 1.3 Preferred City
## Descriptive statistics: Interactive table for dream host city
dt.host.city <- dt.students.clean[ , n_students_host_city := .N, by=host_city]
dt.host.city <- dt.host.city[, avg_grant := mean(STUDYGRANT), by=host_city]
dt.host.city <- dt.host.city[, avg_age := mean(AGE), by=host_city]
dt.host.city <- dt.host.city[, number_host_uni := length(unique(Host_organisation)), by=host_city]
dt.host.city.summary <- dt.host.city[, list(n_students_host_city, avg_grant, avg_age, number_host_uni), by = host_city]
dt.host.city.summary <- dt.host.city.summary[!duplicated(dt.host.city.summary[, c(host_city)])]
dt.male.host.city <- dt.host.city[GENDER == "M"]
dt.male.host.city <- dt.male.host.city[, list(n_male = .N), by=host_city]
dt.host.city.summary <- left_join(dt.host.city.summary, dt.male.host.city, by="host_city" )
dt.females.host.city <- dt.host.city[GENDER == "F"]
dt.females.host.city <- dt.females.host.city[, list(n_female = .N), by=host_city]
dt.host.city.summary <- left_join(dt.host.city.summary, dt.females.host.city, by="host_city" )

# Change the names of the columns
dt.host.city.summary <- setnames(dt.host.city.summary, c("n_students_host_city", "avg_grant", "avg_age", "number_host_uni", "n_male", "n_female"), 
                                 c("number of incoming exchange students", "average grant", "average age of student", "number of host universities", 
                                   "number of male student", "number of female student"))

# Make a vector of all the choices
v.host.cities.unordered = dt.host.city.summary[, host_city]
v.host.cities <- sort(v.host.cities.unordered)
# Create a function

host.city.summary.table <- function(hostcity) {
  dt.host.city.summary.unique <- dt.host.city.summary[host_city == hostcity]
  dt.host.city.summary.unique <- setnames(dt.host.city.summary.unique, "host_city", "Host city")
}




### Tab 1.4 Universities in City
# Descriptive statistics: Interactive table for dream university
dt.host.organisation <- dt.students.clean[ , n_students_host_organization := .N, by=Host_organisation]
dt.host.organisation <- dt.host.organisation[, avg_grant := mean(STUDYGRANT), by=Host_organisation]
dt.host.organisation <- dt.host.organisation[, avg_age := mean(AGE), by=Host_organisation]
dt.host.organisation.summary <- dt.host.organisation[, list(n_students_host_organization, avg_grant, avg_age, host_city), by = Host_organisation]
dt.host.organisation.summary <- dt.host.organisation.summary[!duplicated(dt.host.organisation.summary[, c(Host_organisation)])]
dt.male.host.organisation <- dt.host.organisation[GENDER == "M"]
dt.male.host.organisation <- dt.male.host.organisation[, list(n_male = .N), by=Host_organisation]
dt.host.organisation.summary <- left_join(dt.host.organisation.summary, dt.male.host.organisation, by="Host_organisation" )
dt.female.host.organisation <- dt.host.organisation[GENDER == "F"]
dt.female.host.organisation <- dt.female.host.organisation[, list(n_female = .N), by=Host_organisation]
dt.host.organisation.summary <- left_join(dt.host.organisation.summary, dt.female.host.organisation, by="Host_organisation" )
dt.n.bachelor.host.organisation <- dt.host.organisation[study_level == "Bachelor"]
dt.n.bachelor.host.organisation <- dt.n.bachelor.host.organisation[, list(n_bachelor = .N), by=Host_organisation]
dt.host.organisation.summary <- left_join(dt.host.organisation.summary, dt.n.bachelor.host.organisation, by="Host_organisation" )
dt.n.master.host.organisation <- dt.host.organisation[study_level == "Master"]
dt.n.master.host.organisation <- dt.n.master.host.organisation[, list(n_master = .N), by=Host_organisation] 
dt.host.organisation.summary <- left_join(dt.host.organisation.summary, dt.n.master.host.organisation, by="Host_organisation" )


# Change the names of the columns
dt.host.organisation.summary <- setnames(dt.host.organisation.summary, c("n_students_host_organization", "avg_grant", "avg_age", "host_city", "n_male", "n_female", "n_bachelor", "n_master"),
                                         c("number of incoming exchange students", "average grant", "average age of student", "city", 
                                           "number of male students", "number of female students", "number of bachelor students", "number of master students"))


# Create a function that inputs the host_city and outputs a table of all the unis 
dt.host.organisation.summary.city.input <- function(hostcityinput){
  dt.host.organisations.per.city <- dt.host.organisation.summary[city == hostcityinput]
  dt.host.organisations.per.city <- setnames(dt.host.organisations.per.city, "Host_organisation", "Host organisation")
}




###### Page 2: Exploratory Analysis 

# Create a graph with all data


# Values and vectors to be used
# List countries for host and home universities
v.home.countries <- dt.students.clean[, "home_country"][!duplicated(home_country)]
v.home.countries <- setorder(v.home.countries)
v.host.countries <- dt.students.clean[, "host_country"][!duplicated(host_country)]
v.host.countries <- setorder(v.host.countries)
v.host.countries <- v.host.countries[!host_country == ""]
v.all.cities <- rbind(dt.students.clean[,"home_city"], dt.students.clean[,"host_city"], use.names = FALSE)
v.all.cities <- v.all.cities[!duplicated(home_city)]
v.all.universities <- rbind(dt.students.clean[,"Home_organisation"], dt.students.clean[,"Host_organisation"], use.names = FALSE)
v.all.universities <- v.all.universities[!duplicated(Home_organisation)]
v.all.universities <- setorder(v.all.universities)
v.all.universities <- v.all.universities[-1]
dt.study.length <- as.data.table(seq(0, 13.25, .25))


######                                      Functions Page 2-1                                        #######

# Create and plot a new subgraph based on input. Default min_students = 10
create_subgraph <- function (home_coun, host_coun, min_students = 10, level = c("Master", "Bachelor")){
  dt.filtered <- dt.students.clean[(home_country == home_coun & host_country == host_coun)|
                                     (home_country == host_coun & host_country == home_coun)]
  dt.filtered <- dt.filtered[study_level %in% level]
  dt.filtered[, n_students := .N, by = c("Home_organisation", "Host_organisation")]
  dt.filtered <- dt.filtered[n_students >= min_students]
  dt.univ <- rbind(dt.filtered[, list(Home_organisation, home_country)],
                   dt.filtered[, list(Host_organisation, host_country)],
                   use.names = FALSE)
  dt.univ <- dt.univ[!duplicated(Home_organisation)]
  g.filtered <- graph.data.frame(dt.filtered[, list(Home_organisation, Host_organisation)],
                                 directed = TRUE,
                                 vertices = dt.univ)
  g.filtered <- simplify(g.filtered)
  
  if (components(g.filtered)[3]<1) {
    return(visIgraph(g.filtered))
  } else {
    V(g.filtered)[home_country == home_coun]$color <- "indianred"
    V(g.filtered)[home_country == host_coun]$color <- "cornflowerblue"
    V(g.filtered)$size <- degree(g.filtered, mode = "all")*7
    return(visIgraph(g.filtered))
  }
}

# Create a new subgraph based on input and calculate average metrics. Default min_students = 10.
create_subgraph_average_metrics <- function (home_coun, host_coun, min_students = 10, level = c("Master", "Bachelor")){
  dt.filtered <- dt.students.clean[(home_country == home_coun & host_country == host_coun)|
                                     (home_country == host_coun & host_country == home_coun)]
  dt.filtered <- dt.filtered[study_level %in% level]
  dt.filtered[, n_students := .N, by = c("Home_organisation", "Host_organisation")]
  dt.filtered <- dt.filtered[n_students >= min_students]
  v.universities <- rbind(dt.filtered[,"Home_organisation"], dt.filtered[,"Host_organisation"], use.names = FALSE)
  v.universities <- v.universities[!duplicated(Home_organisation)]
  g.filtered <- graph.data.frame(dt.filtered[, list(Home_organisation, Host_organisation)], directed = TRUE)
  dt.aux <- dt.filtered
  dt.aux[, "pasted" := paste(Home_organisation, Host_organisation)]
  dt.aux <- dt.aux[!duplicated(pasted)]
  dt.metrics <- data.table("Number of universities" = nrow(v.universities),
                           "Total number of students" = nrow(dt.filtered),
                           "Avg number of students per edge" = (nrow(dt.filtered)/nrow(dt.aux)),
                           "Number of clusters" = components(g.filtered)[3],
                           "Avg Betweenness" = mean(betweenness(g.filtered)),
                           "Directed Diameter" = diameter(g.filtered, directed = TRUE),
                           "Undirected Diameter" = diameter(g.filtered, directed = FALSE)#,
                           #"Clustering coefficient" = transitivity(g.filtered, "undirected")
                           )
  return(dt.metrics)
}


# Create a new subgraph based on input and plot detailed metrics. Default min_students = 10.
create_subgraph_detailed_metrics <- function (home_coun, host_coun, min_students = 10, level = c("Master", "Bachelor")){
  dt.filtered <- dt.students.clean[(home_country == home_coun & host_country == host_coun)|
                                     (home_country == host_coun & host_country == home_coun)]
  dt.filtered <- dt.filtered[study_level %in% level]
  dt.filtered[, n_students := .N, by = c("Home_organisation", "Host_organisation")]
  dt.filtered <- dt.filtered[n_students >= min_students]
  g.filtered <- graph.data.frame(dt.filtered[, list(Home_organisation, Host_organisation)], directed = TRUE)
  dt.metrics <- data.table("Number_of_students" = degree(g.filtered, mode = "all"))
  dt.aux <- ggplot(dt.metrics, aes(Number_of_students)) + geom_histogram(color = "blue", fill = "light blue") + 
    labs (title = "Degree distribution of universities in this graph", x = "Degree", y = "Frequency") + 
    theme(panel.background = element_blank())
  return(dt.aux)
}


######                                      Functions Page 2-2                                        #######

# Create subgraph with selected university and its neighbors
create_subgraph_university <- function (univ){
  dt.filtered <- dt.students.clean[(Home_organisation == univ | Host_organisation == univ)]
  dt.filtered[, n_students := .N, by = c("Home_organisation", "Host_organisation")]
  g.filtered <- graph.data.frame(dt.filtered[, list(Home_organisation, Host_organisation)], directed = TRUE)
  g.filtered <- simplify(g.filtered)
  return(visIgraph(g.filtered))
}

# Create a table with the top universities for the selected university in terms of degree
create_subgraph_university_metrics <- function (univ){
  dt.filtered <- dt.students.clean[((Home_organisation == univ | Host_organisation == univ) & (MOBILITYTYPE == "S" | MOBILITYTYPE == "C"))]
  dt.filtered[, n_students := .N, by = c("Home_organisation", "Host_organisation")]
  dt.partners <- rbind(dt.filtered[,c("Home_organisation", "home_country", "home_city")],
                       dt.filtered[,c("Host_organisation", "host_country", "host_city")],use.names = FALSE)
  dt.partners <- dt.partners[!Home_organisation == univ,]
  g.filtered <- graph.data.frame(dt.filtered[, list(Home_organisation, Host_organisation)], directed = TRUE)
  dt.metrics <- data.table("Institutions" = V(g.filtered)$name,
                           "Students to selected university (in-degree)" = degree(g.filtered, mode = "out"),
                           "Students from selected university (out-degree)" = degree(g.filtered, mode = "in"))
  if(univ == ""){
    return ()
  }else{
   dt.metrics <- left_join(dt.metrics, dt.partners, by = c("Institutions" = "Home_organisation"))
  dt.metrics <- dt.metrics[!duplicated(Institutions)]
  setnames (dt.metrics, "home_country", "Country")
  setnames (dt.metrics, "home_city", "City")
  dt.metrics <- setorder(dt.metrics, -'Students to selected university (in-degree)')
  dt.metrics <- dt.metrics[-1,]
    return(dt.metrics)
  }
}

# Dynamic dropdown lists                                  

# List the cities in the selected country
create_cities_per_country <- function (country){
  dt.filtered <- dt.students.clean[home_country == country]
  dt.filtered <- dt.filtered[!duplicated(home_city)]
  v.filtered <- as.vector(dt.filtered$home_city)
  v.filtered <- sort(v.filtered)
  return(v.filtered)
}


create_host_cities_per_country <- function(country){
  dt.filterted.host <- dt.students.clean[host_country == country]
  dt.filterted.host <- dt.filterted.host[!duplicated(host_city)]
  v.filtered.host <- as.vector(dt.filterted.host$host_city)
  v.filtered.host <- sort(v.filtered.host)
  return(v.filtered.host)
}


# List the universities in the selected city
create_univ_per_city <- function (city){
  dt.filtered <- dt.students.clean[home_city == city]
  dt.filtered <- dt.filtered[!duplicated(Home_organisation)]
  v.filtered <- as.vector(dt.filtered$Home_organisation)
  v.filtered <- sort(v.filtered)
  return(v.filtered)
}


######                                      Grant Prediction                                        #######

#Create a regression model
dt.reg <- dt.students.clean[!(MOBILITYTYPE == "P" | MOBILITYTYPE == "C")]
regmodel <- lm(study_grant_per_month ~ host_country + home_country, dt.reg)
dt.coefficients <- as.data.table(regmodel$coefficients)

#Create table with the columns needed to predict a grant
dt.prediction <- data.table()
dt.prediction[,"variable" := "Intercept"]

dt.austria <- data.table()
dt.austria[,"variable" := "Austria"]
dt.austria[,"V1" := 0]

dt.intercept <- as.data.table(rep("intercept", 1))
dt.host <- as.data.table(rep("host", 33))
dt.home <- as.data.table(rep("home", 33))
dt.aux.column <- rbind(dt.intercept, dt.host, dt.home)
setnames (dt.aux.column, "V1", "aux")

dt.prediction <- rbind(dt.prediction,v.home.countries[-1,],v.home.countries[-1,], use.names = FALSE)
dt.prediction <- cbind(dt.prediction, dt.coefficients)
dt.prediction <- rbind(dt.prediction[1,], dt.austria, dt.prediction[2:33,], dt.austria, dt.prediction[34:65,])
dt.prediction <- cbind(dt.aux.column, dt.prediction)
setnames(dt.prediction, "V1", "coefficient")

dt.zeros <- rep(0, 67)
dt.prediction <- cbind(dt.prediction, dt.zeros)
dt.prediction[1, 4] <- 1
setnames(dt.prediction, "dt.zeros", "value")
dt.prediction.host <- dt.prediction[2:34,] 
dt.prediction.home <- dt.prediction[35:67,] 

# Create the function to predict the grant based on user input
predict_grant <- function(home, host, length){
  length <- as.numeric(length)
  dt.prediction.host[variable == host,4] <- 1
  dt.prediction.home[variable == home,4] <- 1
  dt.prediction.merged <- rbind(dt.prediction[1,], dt.prediction.host, dt.prediction.home)
  prediction <- as.integer(crossprod(dt.prediction.merged$coefficient, dt.prediction.merged$value))
  dt.predicted.value <- data.table("Home country" = home,
                                   "Host country" = host,
                                   "Exchange length (months)" = length,
                                   "Predicted grant" = paste("€ ",prediction*length))
  if (prediction < 0){
    return(NULL)
  } else {
    return(dt.predicted.value)
  }
}

#predict_grant("Spain", "Germany")

#View(tapply(dt.students.clean$STUDYGRANT, dt.students.clean$LENGTHSTUDYPERIOD, mean))







