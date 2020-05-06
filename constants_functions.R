stateFinder <- function(search, default) {
  sub <- str_sub(search, -3)
  if (grepl("^-", sub) == T){
    return(str_sub(sub,-2))
  } else {
    return(default)
  }
}

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