library(pdftools); library(zoo); library(stringr); library(tidyverse)
source("constants_functions.R")

doc <- pdf_text("Program-Results_2016_2020.pdf")
doc <- unlist(strsplit(doc, split = "\n"))
doc <- data_frame(text = as.vector(doc))

states <- data.frame("stateFull" = toupper(state.name), "State" = state.abb, stringsAsFactors = F)

old.data <- read.csv("matchData_2013-2019.csv", stringsAsFactors = F) %>% filter(Year == 2013 | Year == 2014 | Year == 2015) 

df <- doc %>%
  #Remove "\r"
  rowwise() %>%
  mutate(text = unlist(strsplit(text, split = "\r"))) %>%
  ungroup() %>%
  #Filter out non-data lines
  filter(!grepl("NRMP Program Results", text)) %>%
  filter(!grepl("Did not fill all available positions", text)) %>%
  filter(!grepl("Program ", text)) %>%
  filter(!grepl("Continued", text)) %>%
  #Identify state groups and assign as variable to each program
  mutate(upper = toupper(text)) %>%
  mutate(isState = text == upper) %>%
  rowwise() %>%
  mutate(stateFull = ifelse(isState == TRUE, text, NA)) %>%
  ungroup() %>%
  mutate(stateFull = na.locf(stateFull)) %>%
  left_join(states, by = "stateFull")%>%
  rowwise() %>% mutate(State = stateFinder(stateFull, State))%>%
  filter(!isState) %>%
  select(-isState, -upper, -stateFull) %>%
  #Identify program names and assign as variable to each program
  rowwise() %>%
  mutate(first = unlist(strsplit(as.character(text), "[ \t]{2,} | --"))[1]) %>%
  ungroup() %>%
  mutate(isProgram = text == first) %>%
  mutate(Program = ifelse(isProgram == TRUE, first, NA)) %>%
  mutate(Program = na.locf(Program)) %>%
  filter(!isProgram) %>%
  #Identify cities and assign as variable to each program
  mutate(isCity = grepl("Quota", text)) %>%
  mutate(City = ifelse(isCity == TRUE, first, NA)) %>%
  mutate(City = na.locf(City)) %>%
  filter(!isCity) %>%
  #Identify specialties and asign as variable
  mutate(isPGY2 = grepl("PGY2", text)) %>%
  mutate(Specialty = ifelse(isPGY2 == TRUE, paste0(first,"-PGY2"), first)) %>%
  select(-isPGY2, -isProgram, - isCity, -first) %>%
  #Expanding data in `text` to columns
  mutate(text = gsub("\\*", "", text)) %>%
  mutate(text = gsub("--", "0", text)) %>%
  mutate(text = gsub("PGY2", "PGY2 ", text)) %>%
  separate(text, sep = "([\\S])+([0-9])([A-z])([0-9]) ", into = c("P", "data"), remove = F) %>%
  separate(data, sep = "[ \t]{2,}", into = c("blank",
                                             "Quota.2020", "Matched.2020",
                                             "Quota.2019", "Matched.2019",
                                             "Quota.2018", "Matched.2018",
                                             "Quota.2017", "Matched.2017",
                                             "Quota.2016", "Matched.2016")) %>%
  #Extracting program number
  rowwise() %>%
  mutate(coord = paste0(str_locate(text, "([\\S])+([0-9])([A-z])([0-9])"), collapse = "-")) %>%
  separate(coord, sep = "-", into = c("start", "stop")) %>%
  mutate(Code = substr(text, start, stop)) %>%
  select(-blank, -P, -text, - start, -stop) %>%
  #SOAP calculation
  # mutate_if(is.numeric, funs(as.character))
  mutate_if(function(x) all(grepl("^[0-9]*$", x)), funs(as.numeric)) %>%
  mutate(SOAP.2020 = Quota.2020 - Matched.2020,
         SOAP.2019 = Quota.2019 - Matched.2019,
         SOAP.2018 = Quota.2018 - Matched.2018,
         SOAP.2017 = Quota.2017 - Matched.2017,
         SOAP.2016 = Quota.2016 - Matched.2016) %>%
  #Convert to long format
  pivot_longer(c(-State, -Program, -City, -Specialty, -Code), names_to = "Stat", values_to = "value") %>%
  separate(Stat, into = c("Stat", "Year"), sep = "\\.")

# Fix Washington DC and Puerto Rico
df[(is.na(df$State) & (df$City == "Washington")),1] <- "DC"
df[is.na(df$State),1] <- "PR"

df.final <- specialty.simple(df) %>% mutate(Year = as.numeric(Year)) %>% bind_rows(old.data)

write.csv(df.final, "matchData.csv", row.names = F)
