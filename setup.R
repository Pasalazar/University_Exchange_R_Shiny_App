library (data.table)
library (dplyr)
library (igraph)
library (stringr)

#setwd('C:\\Users\\p_sal\\OneDrive\\Escritorio\\RSM\\06.- B3 - Network Data Analytics\\01 Assignments\\Shiny App\\App')

dt.institutions <- fread ("Organizations.csv", encoding = "UTF-8")
dt.students <- fread ("student_1112.csv", encoding = "UTF-8")
dt.countries <- fread ("Country Code - Sheet1.csv")
dt.nationality <- fread ("Nationality Code - Sheet1.csv")
dt.home.cities <- fread("home-city.csv", encoding = "UTF-8")
dt.host.cities <- fread("host-city.csv", encoding = "UTF-8")

# Delete 1 repeated row from the institutions dataset
dt.institutions.clean <- dt.institutions[!duplicated(`Erasmus code`)]


# Bring the actual organisation name to the dataset for both host and home institutions
dt.students <- left_join(dt.students, dt.institutions.clean[, c("Erasmus code", "Organisation Name")], by = c("HOMEINSTITUTION" = "Erasmus code"))
setnames(dt.students, "Organisation Name", "Home_organisation")

dt.students <- left_join(dt.students, dt.institutions.clean[, c("Erasmus code", "Organisation Name")], by = c("HOSTINSTITUTION" = "Erasmus code"))
setnames(dt.students, "Organisation Name", "Host_organisation")

# Bring the name of the home and host cities
dt.students <- left_join(dt.students, dt.institutions.clean[, c("Erasmus code", "City")], by = c("HOMEINSTITUTION" = "Erasmus code"))
setnames(dt.students, "City", "Home_city")
dt.students$Home_city <- str_to_title(dt.students$Home_city)

dt.students <- left_join(dt.students, dt.institutions.clean[, c("Erasmus code", "City")], by = c("HOSTINSTITUTION" = "Erasmus code"))
setnames(dt.students, "City", "Host_city")
dt.students$Host_city <- str_to_title(dt.students$Host_city)

# New columns with full name of countries and nationalities
dt.students <- left_join(dt.students, dt.countries, by = c("COUNTRYCODEOFHOMEINSTITUTION" = "Country Code"))
setnames(dt.students, "Country", "home_country")

dt.students <- left_join(dt.students, dt.countries, by = c("COUNTRYCODEOFHOSTINSTITUTION" = "Country Code"))
setnames(dt.students, "Country", "host_country")

setnames(dt.students, "NATIONALITY", "NATIONALITYCODE")
dt.students <- left_join(dt.students, dt.nationality, by = c("NATIONALITYCODE" = "Nationality_Code"))

# We correct the names that had mistakes by finding the correct names related to their Erasmus Code online
dt.students[dt.students == "??????????? ??????? - PANEPISTIMIO EGEOU"] <- "UNIVERSITY OF THE AEGEAN - PANEPISTIMIO AIGAIOU"
dt.students[dt.students == "???????????? ?????????? (??????????? ??? ?????????? ?????????)"] <- "UNIVERSITY OF MACEDONIA"
dt.students[dt.students == "?STANBUL ?EH?R ÜN?VERS?TES?"] <- "ISTANBUL SEHIR ÜNIVERSITESI"
dt.students[dt.students == "?STANBUL GEL???M ÜN?VERS?TES?"] <- "ISTANBUL GELISIM ÜNIVERSITESI"
dt.students[dt.students == '????? ???????????? ??????? ""?. ?. ????????""'] <- "NIKOLA VAPTSAROV NAVAL ACADEMY"
dt.students[dt.students == "?????????? ???????????? ????????"] <- "NATIONAL ACADEMY OF ART"
dt.students[dt.students == '?????????? ????????? ???????? ""????.????? ??????????""'] <- "NATIONAL ACADEMY OF MUSIC 'PROF. PANCHO VLADIGEROV'"
dt.students[dt.students == "??????? ???????????? ???????"] <- "INTERNATIONAL HELLENIC UNIVERSITY"
dt.students[dt.students == "?????????? ????? ?? ????????? ? ??????????"] <- "INTERNATIONAL HELLENIC UNIVERSITY"
dt.students[dt.students == "??????? ???????????? ??????"] <- "OPEN UNIVERSITY OF CYPRUS"
dt.students[dt.students == "?????? ????? ???????? ??????"] <- "NATIONAL SCHOOL OF PUBLIC HEALTH"

# We delete 2,105 rows that had values "NA" for either home or host organisation and for which the real value was not found.
# Additionally, we only delete those with MOBILITYTYPE = S OR C, because the rest are placements
dt.students.clean <- dt.students[!((is.na(Host_organisation) == TRUE | is.na(Home_organisation) == TRUE) & (MOBILITYTYPE == "S" | MOBILITYTYPE == "C"))]

# We change the format of organisation names
dt.students.clean$Host_organisation <- str_to_title(dt.students.clean$Host_organisation)
dt.students.clean$Home_organisation <- str_to_title(dt.students.clean$Home_organisation)

# We create a column with the STUDY semester type (Fall or Spring) instead of just having months
dt.students.clean <- dt.students.clean %>% 
  mutate(study_semester = if_else(STUDYSTARTDATE == "", "", 
                                  if_else((substr(STUDYSTARTDATE,1,3) == "Jul") |
                                  (substr(STUDYSTARTDATE,1,3) == "Aug") |
                                  (substr(STUDYSTARTDATE,1,3) == "Sep") |
                                  (substr(STUDYSTARTDATE,1,3) == "Oct") |
                                  (substr(STUDYSTARTDATE,1,3) == "Nov") |
                                  (substr(STUDYSTARTDATE,1,3) == "Dec") , 
                                  "Fall", "Spring")))

# We create a column with the PLACEMENT semester type (Fall or Spring) instead of just having months
dt.students.clean <- dt.students.clean %>% 
  mutate(placement_semester = if_else(PLACEMENTSTARTDATE == "", "", 
                                  if_else((substr(PLACEMENTSTARTDATE,1,3) == "Jul") |
                                  (substr(PLACEMENTSTARTDATE,1,3) == "Aug") |
                                  (substr(PLACEMENTSTARTDATE,1,3) == "Sep") |
                                  (substr(PLACEMENTSTARTDATE,1,3) == "Oct") |
                                  (substr(PLACEMENTSTARTDATE,1,3) == "Nov") |
                                  (substr(PLACEMENTSTARTDATE,1,3) == "Dec") , 
                                  "Fall", "Spring")))

# We create a column for better understanding of the study levels
dt.students.clean <- dt.students.clean %>% 
  mutate(study_level = if_else(LEVELSTUDY == 1, "Bachelor", 
                               if_else(LEVELSTUDY == 2, "Master", 
                                       if_else(LEVELSTUDY == 3, "Doctoral", 
                                               "Short"))))

# Do a left_join to include the filtered home city and host city.
dt.students.clean <- left_join(dt.students.clean, dt.home.cities, by = c("Home_city" = "City"))
setnames(dt.students.clean, "standard_city", "home_city")
dt.students.clean <- left_join(dt.students.clean, dt.host.cities, by = c("Host_city" = "City"))
setnames(dt.students.clean, "host_cities", "host_city")

# Delete columns that we will not need anymore
dt.students.clean <- dt.students.clean[, !"HOMEINSTITUTION"]
dt.students.clean <- dt.students.clean[, !"COUNTRYCODEOFHOMEINSTITUTION"]
dt.students.clean <- dt.students.clean[, !"NATIONALITYCODE"]
dt.students.clean <- dt.students.clean[, !"LEVELSTUDY"]
dt.students.clean <- dt.students.clean[, !"COUNTRYCODEOFHOSTINSTITUTION"]
dt.students.clean <- dt.students.clean[, !"CONSORTIUMAGREEMENTNUMBER"]
dt.students.clean <- dt.students.clean[, !"HOSTINSTITUTION"]
dt.students.clean <- dt.students.clean[, !"Home_city"]
dt.students.clean <- dt.students.clean[, !"Host_city"]

## Add the grant per month column
dt.students.clean <- dt.students.clean[, study_grant_per_month := (STUDYGRANT / LENGTHSTUDYPERIOD)]

# Save the file to be used by the app

fwrite(dt.students.clean, file = "processed-data.csv")


