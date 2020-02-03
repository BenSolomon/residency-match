library(pdftools); library(dplyr); library(zoo); library(tidyr); library(stringr)

doc <- pdf_text("Program-Results-2015-2019.pdf")
doc <- unlist(strsplit(doc, split = "\n"))
doc <- data_frame(text = as.vector(doc))

states <- data.frame("stateFull" = toupper(state.name), "State" = state.abb, stringsAsFactors = F)

old.data <- read.csv("Program-Results-2013-2017v2.csv", stringsAsFactors = F)
old.data <- old.data %>%
  filter(Year == 2013 | Year == 2014) %>%
  mutate(Stat = paste(Year, Stat, sep = "-")) %>%
  select(Code, Stat, value) %>%
  # group_by(Code) %>%
  # summarise(summary = length(Stat)) %>%
  # arrange(desc(summary))
  spread(Stat, value)


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
  left_join(states, by = "stateFull") %>%
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
                                             "Quota.2019", "Matched.2019",
                                             "Quota.2018", "Matched.2018",
                                             "Quota.2017", "Matched.2017",
                                             "Quota.2016", "Matched.2016",
                                             "Quota.2015", "Matched.2015")) %>%
  #Extracting program number
  rowwise() %>%
  mutate(coord = paste0(str_locate(text, "([\\S])+([0-9])([A-z])([0-9])"), collapse = "-")) %>%
  separate(coord, sep = "-", into = c("start", "stop")) %>%
  mutate(Code = substr(text, start, stop)) %>%
  select(-blank, -P, -text, - start, -stop) %>%
  #SOAP calculation
  # mutate_if(is.numeric, funs(as.character))
  mutate_if(function(x) all(grepl("^[0-9]*$", x)), funs(as.numeric)) %>%
  mutate(SOAP.2019 = Quota.2019 - Matched.2019,
         SOAP.2018 = Quota.2018 - Matched.2018,
         SOAP.2017 = Quota.2017 - Matched.2017,
         SOAP.2016 = Quota.2016 - Matched.2016,
         SOAP.2015 = Quota.2015 - Matched.2015) %>%
  #Add 2013
  left_join(old.data, by = "Code") %>%
  #Reshape
  gather(Stat, value, -State, -Program, -Code, -City, -Specialty) %>%
  mutate(Stat = factor(Stat), 
         Stat = recode(Stat, 
                `2013-Quota` = "Quota.2013",
                `2013-Matched` = "Matched.2013",
                `2013-SOAP` = "SOAP.2013",
                `2014-Quota` = "Quota.2014",
                `2014-Matched` = "Matched.2014",
                `2014-SOAP` = "SOAP.2014")) %>%
  separate(Stat, sep = "\\.", into = c("Stat", "Year"))

df[(is.na(df$State) & (df$City == "Washington")),1] <- "DC"
df[is.na(df$State),1] <- "PR"

specialty.search <- list("Anesthesiology" = c("anes"),
                         "Child Neurology" = c("child ne"),
                         "Dermatology" = c("derm"), 
                         "Radiology-Diagnostic" = c("diag"), 
                         "Emergency-Medicine" = c("EM", "emerg"),
                         "Family Medicine" = c("FM", "family", "fam med"), 
                         "Internal Medicine" = c("internal", "int med", "medicine-", "medicine - prim", "medicine/ger", "IM ", "med-pri"), 
                         "Medical Genetics" = c("genetics"), 
                         "Radiology-Interventional" = c("interven"), 
                         "Radiology" = c("radiology"),
                         "Neurological Surgery" = c("neurologi"), 
                         "Neurology" = c("neurology"), 
                         "Nuclear Medicine" = c("nuclear"), 
                         "Obstetrics-Gynecology" = c("gyn"), 
                         "Orthopaedic Surgery" = c("ortho"),
                         "Otolaryngology" = c("oto"), 
                         "Pathology" = c("anat", "path"), 
                         "Pediatrics" = c("ped"), 
                         "Physical Medicine and Rehabilitation" = c("PM&R", "rehab"), 
                         "Plastic Surgery" = c("plast"),
                         "Preventive Medicine" = c("prevent"), 
                         "Psychiatry" = c("psych"), 
                         "Radiation Oncology" = c("onc"), 
                         "General Surgery" = c("general", "gen sur"), 
                         "Thoracic Surgery" = c("thorac"), 
                         "Urology" = c("/urology"), 
                         "Transitional Year" = c("trans"), 
                         "Preliminary Year" = c("prelim"), 
                         "Vascular Surgery" = c("vasc")
)

specialties <- names(specialty.search)

specialty.simple <- function(input) {
  matched.ls <- lapply(specialty.search, function(x) input[grepl(paste(x, collapse="|"), input$Specialty, ignore.case = T), ])
  names.ls <- lapply(seq_along(matched.ls), function(x) names(matched.ls)[[x]])
  df.ls <- lapply(seq_along(matched.ls), function(x){
    data.frame(matched.ls[[x]], "simpleSpecialty" = rep(names.ls[[x]], nrow(matched.ls[[x]])), stringsAsFactors = F)
  })
  bind_rows(df.ls)
}
df <- specialty.simple(df)

write.csv(df, "matchData.csv", row.names = F)
