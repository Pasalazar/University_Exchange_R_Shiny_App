# Loading the packages
# Loading the packages
library(igraph)
library(shiny)
library(data.table)
library(rsconnect)
library(ggplot2)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(shiny) 
library(dplyr)
library(visNetwork)
library(linkprediction)

dt.students.clean <- fread("processed-data.csv", encoding = "UTF-8")

##### Page 0: Home Page

##### Page 1: Descriptive Statistics
### Tab 1.1 Basic Descriptive
## This table shows various descriptive statistics about the dataset and their values, so users can get familiar with the data we worked with


#First, we created the column listing the names of all the statistics

v.descriptive.names <- c("Number of students", "Number of student going on study exchange to a University", "Number of students going to do their exchange at an Enterprise", "Number of Male Students", "Number of Female Students", "Number of Home Countries", "Number of Host Countries",
                         "Number of Home Universities", "Number of Host Universities",
                         "Number of Placement Organisations", "Average Number of Students in Host University", "Number of Bachelor Students",
                         "Number of Master Students", "Number of Exchange Student in Fall Semester",
                         "Number of Exchange Student in Spring Semester", "Average Length of Study Period",
                         "Standard Deviation of Length of Study Period",
                         "Average Grant for Student", "Standard Devation of Grant for Student",
                         "Average Study Grant per Month", "Standard Deviation of Study Grant per Month",
                         "Average Age of Student", "Standard Deviation of Student Age")

avg.host.uni.student <- (nrow(dt.students.clean) - nrow(dt.students.clean[MOBILITYTYPE == "P"]))/ length(unique(dt.students.clean$Host_organisation))
n.students.to.uni <- dt.students.clean[!Host_organisation == ""]
n.students.to.enterprise <- dt.students.clean[!PLACEMENTENTERPRISE == ""]
n.students.with.study.grant <- dt.students.clean[!Host_organisation == ""]


avg.host.uni.student <- (nrow(dt.students.clean) - nrow(dt.students.clean[MOBILITYTYPE == "P"]))/ length(unique(dt.students.clean$Host_organisation))
dt.n.students.to.uni <- dt.students.clean[!Host_organisation == ""]
dt.n.students.to.enterprise <- dt.students.clean[!PLACEMENTENTERPRISE == ""]
dt.n.students.with.study.grant <- dt.students.clean[!Host_organisation == ""]

#Then, we calculated the values for all the statistics and stored them in another data table containing one column
dt.descriptive.stats <- c(nrow(dt.students.clean),
                          nrow(n.students.to.uni),
                          nrow(n.students.to.enterprise),
                          nrow(dt.students.clean[GENDER == "M"]),
                          nrow(dt.students.clean[GENDER == "F"]),
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
                          paste(format(round(sd(dt.students.clean$LENGTHSTUDYPERIOD), 2), nsmall = 2), " Months"),
                          paste("€ ", format(round(sum(dt.students.clean$STUDYGRANT, dt.students.clean$PLACEMENTGRANT) / nrow(dt.students.clean), 2), nsmall = 2)),
                          paste("€ ", format(round(sd(dt.students.clean$STUDYGRANT), 2), nsmall = 2)),
                          paste("€ ", format(round(sum(na.omit(n.students.with.study.grant$study_grant_per_month)) / nrow(n.students.with.study.grant), 2))),
                          paste("€ ", format(round(sd(na.omit(n.students.with.study.grant$study_grant_per_month)), 2), nsmall = 2)),
                          format(round(mean(dt.students.clean$AGE), 2), nsmall = 2),
                          format(round(sd(dt.students.clean$AGE), 2), nsmall = 2)
)

dt.descriptive.table <- setDT(as.data.frame(cbind(v.descriptive.names, dt.descriptive.stats)))

#Finally, we changed the column names in the table
dt.descriptive.table <- setnames(dt.descriptive.table, c("v.descriptive.names", "dt.descriptive.stats"), c("Descriptive Statistic", "Value"))


### Tab 1.2: Preferred country
##This page is for comparing host countries based on different variables. The first section is for seeing the variables of one specific country of interest in a table.

# We created a table with all the unique host countries
dt.host.countries.sorted <- dt.students.clean[, "host_country"][!duplicated(host_country)]
dt.host.countries.sorted <- dt.host.countries.sorted[!host_country == ""]
dt.host.countries.sorted <- dt.host.countries.sorted[order(dt.host.countries.sorted$host_country)]



# Then, we created a data table that shows the values of the variables by host country
dt.host.country <- dt.students.clean[ , n_students_host_country := .N, by=host_country]
dt.host.country <- dt.host.country[!host_country == ""]
dt.host.country <- dt.host.country[, avg_grant_per_month := mean(na.omit(study_grant_per_month)), by=host_country]
dt.host.country <- dt.host.country[, avg_age := mean(AGE), by=host_country]
dt.host.country.summary <- dt.host.country[, list(n_students_host_country, avg_grant_per_month, avg_age), by = host_country]
dt.host.country.summary <- dt.host.country.summary[!duplicated(dt.host.country.summary[, c(host_country)]),]

# Function that subsets the data table based on the country the user inputs
host.country.table <- function(hostcountry) {
  dt.host.country.summary.unique <- dt.host.country.summary[host_country == hostcountry]
  
}



## The second section is a histogram, that displays the values of all host countries for the different metrics
dt.host.country <- dt.students.clean[ , n_students_host_country := .N, by=host_country]
dt.host.country <- dt.host.country[, avg_grant_per_month := mean(na.omit(study_grant_per_month)), by=host_country]
dt.host.country <- dt.host.country[, avg_age := mean(AGE), by=host_country]
dt.host.country[, number_host_uni := length(unique(Host_organisation)), by=host_country]
dt.host.country.summary <- dt.host.country[, list(n_students_host_country, avg_grant_per_month, avg_age, number_host_uni), by = host_country]
dt.host.country.summary <- dt.host.country.summary[!duplicated(dt.host.country.summary[, c(host_country)]),]
dt.male <- dt.host.country[GENDER=="M"]
dt.male <- dt.male[, list(n_male = .N), by = host_country]
dt.host.country.summary <- left_join(dt.host.country.summary, dt.male, by = "host_country")
dt.female <- dt.host.country[GENDER=="F"]
dt.female <- dt.female[, list(n_female = .N), by = host_country]
dt.host.country.summary <- left_join(dt.host.country.summary, dt.female, by = "host_country")
dt.host.country.summary <- setnames(dt.host.country.summary, c("n_students_host_country", "avg_grant_per_month", "avg_age", "number_host_uni", "n_male", "n_female"),
                                    c("Number of incoming exchange students", "Average grant per month (€)", "Average age of student", "Number of host universities", 
                                      "Number of male students", "Number of female students"))
dt.host.country.summary <- dt.host.country.summary[!host_country == ""]

#We create a vector of the metrics the user can choose from
v.choices <- c("Number of incoming exchange students", "Average grant per month (€)", "Average age of student", "Number of host universities", 
               "Number of male students", "Number of female students")

# Function that returns a histogram based on the statistic the user chooses
country.chart <- function(thestat) {
  return(ggplot(dt.host.country.summary, aes(x = host_country, y = .data[[thestat]])) +
           geom_col(color = "blue", fill = "light blue") + labs(x = "Host Country") +
           theme(panel.background = element_blank(), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 14), axis.text.y = element_text(size = 14)))
}

#dynamic text
descriptive.text <- function(thestat) {
  if(thestat == "Number of incoming exchange students"){return("In the plot, we see that Spain has the most incoming exchange students. So, if you want an exchange where you can meet many other exchange students, you know where to go now! Moreover, this graph can also be interesting for universities. Countries like Spain are very popular, so it might be beneficial for universities outside Spain to establish partnerships with universities from Spain to better meet the desires of their students.")}
  else if(thestat == "Average grant per month (€)"){return("When selecting the average grant per month, we see that students on average receive the most money when they go to Liechtenstein. However, this does not necessarily mean that if you go to Liechtenstein, you will always receive the highest grant per month as the amount of grant you receive depends on multiple factors. Therefore, we made the grant prediction page which can be found under the advanced analysis in order to predict the grant that you will receive based on several inputs.")}
  else if(thestat == "Average age of student"){return("The average age of incoming exchange students is between 22 and 23 for all countries. Are you younger or older? Don’t worry, age is not important! Every student is on exchange to get the most out of their time abroad, regardless of how old you are!")}
  else if(thestat == "Number of host universities"){return("When looking at the number of host universities, we see that France has almost 400 host universities! So, there are lots of possibilities for exchange students to find a university in France! On the other hand, Luxembourg only has two host universities, but this country is also considerably smaller than France.")}
  else if (thestat == "Number of male students"){return("The visualization of the number of male students might be interesting for both students and universities. Students can now find out the place where they can meet the most students with a specific gender. Universities can find out whether on average a high or low number of students with a specific gender go to this country. As can be seen, Spain has the most incoming males, followed by Germany. ")}
  else {return("Spain has the most incoming females, just like it also has the most incoming male exchange students. However, France is the country with the second most incoming females, whereas Germany was the country with the second most incoming males. It might be interesting for universities or even governments of countries to try to get the number of incoming exchange students from a specific gender to go up. ")}
}





### Tab 1.3 Preferred City
## Descriptive statistics: Interactive table for dream host city

#Creating a data table with all the metrics summarized by host city
dt.host.city <- dt.students.clean[ , n_students_host_city := .N, by=host_city]
dt.host.city <- dt.host.city[, avg_grant := mean(na.omit(study_grant_per_month)), by=host_city]
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
dt.n.bachelor.host.city <- dt.host.city[study_level == "Bachelor"]
dt.n.bachelor.host.city <- dt.n.bachelor.host.city[, list(n_bachelor = .N), by=host_city]
dt.host.city.summary <- left_join(dt.host.city.summary, dt.n.bachelor.host.city, by="host_city")
dt.n.master.host.city <- dt.host.city[study_level == "Master"]
dt.n.master.host.city <- dt.n.master.host.city[, list(n_master = .N), by=host_city]
dt.host.city.summary <- left_join(dt.host.city.summary, dt.n.master.host.city, by="host_city")

# Changing the names of the columns
dt.host.city.summary <- setnames(dt.host.city.summary, c("n_students_host_city", "avg_grant", "avg_age", "number_host_uni", "n_male", "n_female", "n_bachelor", "n_master"), 
                                 c("number of incoming exchange students", "average grant per month", "average age of student", "number of host universities", 
                                   "number of male students", "number of female students", "number of bachelor students", "number of master students"))


# Creating a vector of all the host cities the user can choose from
v.host.cities.unordered = dt.host.city.summary[, host_city]
v.host.cities <- sort(v.host.cities.unordered)


## Creating a function that returns a table showing the metrics for two host cities chosen by the user
host.city.summary.table.two.cities <- function(hostcityone, hostcitytwo) {
  if((hostcityone == "")&(hostcitytwo == "")){
    dt.empty <- data.table()
    return (dt.empty)
  } else if ((hostcityone != "") & (hostcitytwo == "")){
  dt.host.city.summary.two.cities <- dt.host.city.summary[host_city == hostcityone]
  dt.host.city.summary.two.cities <- setnames(dt.host.city.summary.two.cities, "host_city", "Host city")
  return(dt.host.city.summary.two.cities)
  } else if ((hostcityone == "") & (hostcitytwo != "")){
    dt.host.city.summary.two.cities <- dt.host.city.summary[host_city == hostcitytwo]
    dt.host.city.summary.two.cities <- setnames(dt.host.city.summary.two.cities, "host_city", "Host city")
    return(dt.host.city.summary.two.cities)
  }else{
    dt.host.city.summary.two.cities <- dt.host.city.summary[host_city == hostcityone | host_city == hostcitytwo]
    dt.host.city.summary.two.cities <- setnames(dt.host.city.summary.two.cities, "host_city", "Host city")
    return(dt.host.city.summary.two.cities) 
  }
}



### Tab 1.4 Universities in City
# Descriptive statistics: Interactive table for dream university

#Creating a data table with all the metrics summarized by host university
dt.host.organisation <- dt.students.clean[ , n_students_host_organization := .N, by=Host_organisation]
dt.host.organisation <- dt.host.organisation[, avg_grant := round(mean(na.omit(study_grant_per_month)),2), by=Host_organisation]
dt.host.organisation <- dt.host.organisation[, avg_age := round(mean(AGE),2), by=Host_organisation]
dt.host.organisation.summary <- dt.host.organisation[, list(n_students_host_organization, avg_grant, avg_age, host_city), by = Host_organisation]
dt.host.organisation.summary <- dt.host.organisation.summary[!duplicated(dt.host.organisation.summary[, c(Host_organisation)])]
dt.male.host.organisation <- dt.host.organisation[GENDER == "M"]
dt.male.host.organisation <- dt.male.host.organisation[, list(n_male = .N), by=Host_organisation]
dt.host.organisation.summary <- left_join(dt.host.organisation.summary, dt.male.host.organisation, by="Host_organisation" )
dt.female.host.organisation <- dt.host.organisation[GENDER == "F"]
dt.female.host.organisation <- dt.female.host.organisation[, list(n_female = .N), by=Host_organisation]
dt.host.organisation.summary <- left_join(dt.host.organisation.summary, dt.female.host.organisation, by="Host_organisation" )
dt.fall.host.organisation <- dt.host.organisation[study_semester == "Fall"]
dt.fall.host.organisation <- dt.fall.host.organisation[, list(n_fall = .N), by=Host_organisation]
dt.host.organisation.summary <- left_join(dt.host.organisation.summary, dt.fall.host.organisation, by="Host_organisation" )
dt.spring.host.organisation <- dt.host.organisation[study_semester == "Spring"]
dt.spring.host.organisation <- dt.spring.host.organisation[, list(n_spring = .N), by=Host_organisation]
dt.host.organisation.summary <- left_join(dt.host.organisation.summary, dt.spring.host.organisation, by="Host_organisation" )
dt.n.bachelor.host.organisation <- dt.host.organisation[study_level == "Bachelor"]
dt.n.bachelor.host.organisation <- dt.n.bachelor.host.organisation[, list(n_bachelor = .N), by=Host_organisation]
dt.host.organisation.summary <- left_join(dt.host.organisation.summary, dt.n.bachelor.host.organisation, by="Host_organisation" )
dt.n.master.host.organisation <- dt.host.organisation[study_level == "Master"]
dt.n.master.host.organisation <- dt.n.master.host.organisation[, list(n_master = .N), by=Host_organisation] 
dt.host.organisation.summary <- left_join(dt.host.organisation.summary, dt.n.master.host.organisation, by="Host_organisation" )
dt.host.organisation.summary[is.na(dt.host.organisation.summary)] <- 0

# Changing the names of the columns
dt.host.organisation.summary <- setnames(dt.host.organisation.summary, c("n_students_host_organization", "avg_grant", "avg_age", "host_city", "n_male", "n_female", "n_fall", "n_spring", "n_bachelor", "n_master"),
                                         c("number of incoming exchange students", "average grant per month", "average age of student", "city", 
                                           "number of male students", "number of female students", "number of incoming students in the fall semester","number of incoming students in the spring semester", "number of bachelor students", "number of master students"))


# Creating a function that outputs a table of all the universities in the city the user chooses
dt.host.organisation.summary.city.input <- function(hostcityinput){
  dt.host.organisations.per.city <- dt.host.organisation.summary[city == hostcityinput]
  dt.host.organisations.per.city <- setnames(dt.host.organisations.per.city, "Host_organisation", "Host organisation")
}




###### Page 2: Exploratory Analysis 
##Creating the necessary objects for exploratory analysis

# Creating a directed graph between home and host universities
g.students <- graph.data.frame(dt.students.clean[, list(Home_organisation, Host_organisation)], directed = TRUE)

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



#### 2.1 Network descriptives


## Make a directed graph that only includes students that went on a study exchange
dt.students.only.study.exchanges <- dt.students.clean[!MOBILITYTYPE == "P"]
g.students.only.study.exchanges <- graph.data.frame(dt.students.only.study.exchanges[, list(Home_organisation, Host_organisation)], directed = TRUE)

#First, we created the column listing the names of all the statistics
dt.network.descriptives.names <- c("Number of edges", "Number of nodes", "Average path length", "Average clustering coefficient", "Diameter", "Average Degree")

#Then, we calculated the values for all the statistics and stored them in another data table containing one column
dt.network.descriptives.values <- c(
  length(E(g.students.only.study.exchanges)),
  length(V(g.students.only.study.exchanges)),
  round(mean_distance(g.students.only.study.exchanges), 2),
  round(transitivity(g.students.only.study.exchanges, type = "average"), 2),
  diameter(g.students.only.study.exchanges),
  round(mean(degree(g.students.only.study.exchanges)), 0))

dt.network.descriptives <- setDT(as.data.frame((cbind(dt.network.descriptives.names, dt.network.descriptives.values))))

# Change the names
dt.network.descriptives <- setnames(dt.network.descriptives, c("dt.network.descriptives.names", "dt.network.descriptives.values"), c("Descriptive statistic", "Value"))

# Make a degree distribution

# Plot degree distribution where nodes is the number of in and outgoing students

ggplot.degree.network <- ggplot() + geom_histogram(aes(x=degree(g.students.only.study.exchanges)), bins=30, color = "blue", fill = "light blue") + ggtitle("Degree distribution of the student exchange network") + labs(x = "Degree Centrality", y = "count") +
  theme(panel.background = element_blank()) 

ggplot.degree.network.zoomed <- ggplot() + geom_histogram(aes(x=degree(g.students.only.study.exchanges)), bins=30, color = "blue", fill = "light blue") + ggtitle("Zoomed in degree distribution of the student exchange network") + labs(x = "Degree Centrality", y = "count") +
  theme(panel.background = element_blank()) + xlim(0, 100)
## Now do the same analysis, but then for the university network. 
# In this network, there is only one edge between two universities. The edge exists when there is at least one student going to or from a university to another unviersity.

# Now make a ggplot that only shows the number of universities that are connected
dt.unique.combi.host.home <- dt.students.clean[, list(Home_organisation, Host_organisation)]
dt.unique.combi.host.home <- dt.unique.combi.host.home[!Host_organisation == ""]

# Get ids of uni's
all.uni.names <- as.data.table(as.vector(V(g.students.only.study.exchanges)$name))
all.uni.names <- setnames(all.uni.names, "V1", "Organisation")
all.uni.ids <- as.data.table(as.vector(V(g.students.only.study.exchanges)))
all.uni.ids <- setnames(all.uni.ids, "V1", "ids")
all.uni.names.ids <- cbind(all.uni.names, all.uni.ids)
# Left join
dt.unique.combi.host.home <- left_join(dt.unique.combi.host.home, all.uni.names.ids, by=c("Home_organisation" = "Organisation"))
dt.unique.combi.host.home <- setnames(dt.unique.combi.host.home, "ids", "id_home_organisation")

# Left join host organisation
dt.unique.combi.host.home <- left_join(dt.unique.combi.host.home, all.uni.names.ids, by=c("Host_organisation" = "Organisation"))
dt.unique.combi.host.home <- setnames(dt.unique.combi.host.home, "ids", "id_host_organisation")

dt.unique.combi.host.home <- dt.unique.combi.host.home[, two_id := ifelse(id_home_organisation > id_host_organisation, paste(id_host_organisation, id_home_organisation), paste(id_home_organisation, id_host_organisation))]
dt.unique.combi.host.home <- dt.unique.combi.host.home[!duplicated(two_id)]

# 56,907 unique combinations of universities
g.university <- graph.data.frame(dt.unique.combi.host.home, directed = FALSE)

#First, we created the column listing the names of all the statistics
dt.uni.network.descriptives.names <- c("Number of edges", "Number of nodes", "Average path length", "Average clustering coefficient", "Diameter", "Average Degree")

#Then, we calculated the values for all the statistics and stored them in another data table containing one column
dt.uni.network.descriptives.values <- c(
  length(E(g.university)),
  length(V(g.university)),
  round(mean_distance(g.university), 2),
  round(transitivity(g.university, type = "average"), 2),
  diameter(g.university),
  round(mean(degree(g.university)), 0))

dt.uni.network.descriptives <- setDT(as.data.frame((cbind(dt.uni.network.descriptives.names, dt.uni.network.descriptives.values))))

# Change the names
dt.uni.network.descriptives <- setnames(dt.uni.network.descriptives, c("dt.uni.network.descriptives.names", "dt.uni.network.descriptives.values"), c("Descriptive statistic", "Value"))

# Plot the degree distribution
ggplot.degree.universities <- ggplot() + geom_histogram(aes(x=degree(g.university)), bins=30, color = "blue", fill = "light blue") + ggtitle("Degree distribution of partner university network") + labs(x = "Degree Centrality", y = "count") +
  theme(panel.background = element_blank()) 

ggplot.degree.universities.zoomed <- ggplot() + geom_histogram(aes(x=degree(g.university)), bins=30, color = "blue", fill = "light blue") + ggtitle("Zoomed in degree distribution of partner university network") + labs(x = "Degree Centrality", y = "count") +
  theme(panel.background = element_blank()) + xlim(0, 100)


######                Functions Page 2-2 Partner Universities per country                   #######

# Create and plot a new subgraph based on input. Default min_students = 10
create.subgraph <- function (home_coun, host_coun, min_students = 10, level = c("Master", "Bachelor")){
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
create.subgraph.average.metrics <- function (home_coun, host_coun, min_students = 10, level = c("Master", "Bachelor")){
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
create.subgraph.detailed.metrics <- function (home_coun, host_coun, min_students = 10, level = c("Master", "Bachelor")){
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

######                         Functions Page 2-2      University exploration                       #######

# Create subgraph with selected university and its neighbors
create.subgraph.university <- function (univ){
  dt.filtered <- dt.students.clean[(Home_organisation == univ | Host_organisation == univ)]
  dt.filtered[, n_students := .N, by = c("Home_organisation", "Host_organisation")]
  g.filtered <- graph.data.frame(dt.filtered[, list(Home_organisation, Host_organisation)], directed = TRUE)
  g.filtered <- simplify(g.filtered)
  return(visIgraph(g.filtered))
}

# Create a table with the top universities for the selected university in terms of degree
create.subgraph.university.metrics <- function (univ){
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


##Creating functions for adaptive dropdown lists for choosing a country, a city within that country and a university within that city

# Listing the cities in the selected country
create.cities.per.country <- function (country){
  dt.filtered <- dt.students.clean[home_country == country]
  dt.filtered <- dt.filtered[!duplicated(home_city)]
  v.filtered <- as.vector(dt.filtered$home_city)
  v.filtered <- sort(v.filtered)
  return(v.filtered)
}


create.host.cities.per.country <- function(country){
  dt.filterted.host <- dt.students.clean[host_country == country]
  dt.filterted.host <- dt.filterted.host[!duplicated(host_city)]
  v.filtered.host <- as.vector(dt.filterted.host$host_city)
  v.filtered.host <- sort(v.filtered.host)
  return(v.filtered.host)
}


# Listing the universities in the selected city
create.univ.per.city <- function (city){
  dt.filtered <- dt.students.clean[home_city == city]
  dt.filtered <- dt.filtered[!duplicated(Home_organisation)]
  v.filtered <- as.vector(dt.filtered$Home_organisation)
  v.filtered <- sort(v.filtered)
  return(v.filtered)
}





### Advanced Analaysis: 
### Page 3.1: Link prediction


dt.all.school <- dt.students.clean[!Host_organisation == ""]
dt.all.school <- dt.all.school[!Home_organisation == ""]
g.all.school <- graph.data.frame(dt.all.school[, list(Host_organisation, Home_organisation)], directed = FALSE)
cl <- components(g.all.school)
v.excluded <- lapply(seq_along(cl$csize)[cl$csize > 1], function(x) V(g.all.school)$name[cl$membership %in% x])[[2]]
v.excluded <- c(v.excluded, lapply(seq_along(cl$csize)[cl$csize > 1], function(x) V(g.all.school)$name[cl$membership %in% x])[[3]])
v.excluded <- c(v.excluded, lapply(seq_along(cl$csize)[cl$csize > 1], function(x) V(g.all.school)$name[cl$membership %in% x])[[4]])
v.excluded <- c(v.excluded, lapply(seq_along(cl$csize)[cl$csize > 1], function(x) V(g.all.school)$name[cl$membership %in% x])[[5]])
v.excluded <- c(v.excluded, lapply(seq_along(cl$csize)[cl$csize > 1], function(x) V(g.all.school)$name[cl$membership %in% x])[[6]])
dt.school.network <- dt.all.school[!Host_organisation %in% v.excluded]
dt.school.network <- dt.all.school[!Home_organisation %in% v.excluded]
g.school.network <- graph.data.frame(dt.school.network[, list(Host_organisation, Home_organisation)], directed = FALSE)

school.network.name <- as.data.table(as.vector(V(g.school.network)$name))
school.network.name <- setnames(school.network.name, "V1", "school")
school.network.id <- as.data.table(as.vector(V(g.school.network)))
school.network.id <- setnames(school.network.id, "V1", "id")
school.network.id <- cbind(school.network.id, school.network.name)

m.lp <- proxfun(g.school.network, method = "cn", value = "edgelist")
m.lp <- left_join(m.lp, school.network.id, by = c("from" = "id"))
m.lp <- setnames(m.lp, "school", "school1")
m.lp <- left_join(m.lp, school.network.id, by = c("to" = "id"))
m.lp <- setnames(m.lp, "school", "school2")
m.lp <- setDT(m.lp)
m.lp <- m.lp[!(school1 == school2)]
m.lp <- m.lp[, two_id := ifelse(from > to, paste(to, from), paste(from, to))]
m.lp <- m.lp[!duplicated(two_id)]

dt.connection <- dt.students.clean[,list(Host_organisation, Home_organisation)]
dt.connection <- dt.connection[, connection := paste(Host_organisation, Home_organisation)]
dt.connection <- dt.connection[!duplicated(connection)]
dt.connection <- dt.connection[, connection1 := paste(Home_organisation, Host_organisation)]
v.connection <- c(dt.connection$connection, dt.connection$connection1)
m.lp <- m.lp[, connection := paste(school1, school2)]
m.lp <- m.lp[!connection %in% v.connection]

dt.school.country <- dt.students.clean[, list(Host_organisation, host_country)]
dt.school.country <- rbind(dt.school.country, dt.students.clean[, list(Home_organisation, home_country)], use.names=FALSE)
dt.school.country <- dt.school.country[!duplicated(Host_organisation)]

dt.school.city <- dt.students.clean[, list(Host_organisation, host_city)]
dt.school.city <- rbind(dt.school.city, dt.students.clean[, list(Home_organisation, home_city)], use.names=FALSE)
dt.school.city <- dt.school.city[!duplicated(Host_organisation)]

m.lp <- left_join(m.lp, dt.school.country, by = c("school1" = "Host_organisation"))
m.lp <- setnames(m.lp, "host_country", "country1")
m.lp <- left_join(m.lp, dt.school.country, by = c("school2" = "Host_organisation"))
m.lp <- setnames(m.lp, "host_country", "country2")
m.lp <- m.lp[!(country1 == country2)]

v.lp.school.list <- as.data.table(m.lp$school1)
v.lp.school.list2 <- as.data.table(m.lp$school2)
v.lp.school.list <- rbind(v.lp.school.list, v.lp.school.list2, use.names=FALSE)
v.lp.school.list <- v.lp.school.list[!duplicated(V1)][order(V1)]
v.lp.school.list <- as.vector(v.lp.school.list)
colnames(v.lp.school.list) <- c("Universities")

v.lp.school.list <- left_join(v.lp.school.list, dt.school.city, by = c("Universities" = "Host_organisation"))
v.lp.school.list[host_city == "",2] <- "Wiener Neustadt"

create.univ.per.city.lp <- function (city){
  dt.filtered.lp <- v.lp.school.list[host_city == city]
  v.filtered.lp <- as.vector(dt.filtered.lp$Universities)
  v.filtered.lp <- sort(v.filtered.lp)
  return(v.filtered.lp)
}

find.potential.partner <- function(uni, x){
  dt.potential.partner <- m.lp[school1 == uni | school2 == uni][order(-value)]
  dt.potential.partner <- dt.potential.partner[, partner := ifelse(school1 == uni, school2, school1)]
  dt.lp.result <- as.data.frame(dt.potential.partner$partner[1:x])
  dt.lp.result$Rank <- seq.int(nrow(dt.lp.result))
  dt.lp.result <- setnames(dt.lp.result, "dt.potential.partner$partner[1:x]", "Most Likely Partner Universities")
  dt.lp.result <- dt.lp.result[, c(2, 1)]
  dt.lp.result <- left_join(dt.lp.result, dt.school.country, by = c("Most Likely Partner Universities" = "Host_organisation"))
  dt.lp.result <- left_join(dt.lp.result, dt.school.city, by = c("Most Likely Partner Universities" = "Host_organisation"))
  dt.lp.result <- setnames(dt.lp.result, c("host_country", "host_city"), c("Country", "City"))
  return(dt.lp.result)
}


###### 3.2 Grant Prediction #####

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
predict.grant <- function(home, host, length){
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




