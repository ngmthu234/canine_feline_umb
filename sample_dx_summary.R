# Load neccessary libraries
library(tidyverse)     # to manipulate data
library(network3D)     # to create interactive Sankey graphs

# Load csv file into R
raw <- read.csv("~/Downloads/umb_log.csv")     # Change "~/Downloads/umb_log.csv" with your actual file path

# Data cleanup #1
raw$Species <- tolower(raw$Species)
raw$Diagnosis <- tolower(raw$Diagnosis)
data <- subset(raw, select = c(Sample.name, Species, Diagnosis))     # to create new datagrame data with certain columns from raw

# Data cleanup #2
data <- data %>% 
        filter((Sample.name != "") %>%     # to remove rows with empty string in column Sample.name
        mutate(Species == if_else(Species == "", "Need extracting from records", Species) %>%     # to change empty string with desired string in column Species
        mutate(diagnosis_status = ifelse(is.na(Diagnosis) | Diagnosis == "", "Need extracting diagnosis from records", "Have diagnosis on file"))

# Count the samples that need diagnosis extracted from medical records
dxcount_empty <- data %>% filter(diagnosis_status == "Need extracting diagnosis from records") %>% group_by(Species) %>% summarize(count = n(), .groups = 'drop') %>% mutate(diagnosis_status = "Need extracting diagnosis from records")

# Create a list of keywords for each diagnosis summary
dx_keywords <- list(`Neurological problems` = c("seizure", "epilepsy", "paralysis", "nerve", "stroke", "cognitive", "myelopathy", "brain", "ivdd", "intervertebral disc"), 
                    `UTI / FIC` = c("uti", "cystitis", "pyelonephritis", "fic", "flutd"), 
                    `Heart condition = c("mve", "mmvd", "myxomatous", "endocarditis", "mitral regurgitation", "valve regurgitation", "tricuspid regurgitation", "heart", "atrium"),
                    Urolithiasis = c("olith", "bladder stone", "urethral stone", "oxalate", "crystal"), 
                    Cancer = c("tumor", "neoplasia", "lymphoma", "carcinoma", "sarcoma", "cancer", "mass", "agasaca", "bisc" ), 
                    Diabetes = c("diabetes"), 
                    Healthy = c("healthy"), 
                    `Renal diseases` = c("ckd", "kidney disease", "renal failure", "renal degeneration"), 
                    Anemia = c("imha", "anemia"))

classify_term <- function(term) {
    for (dx_summary in names(dx_keywords)) {
        if (any(str_detect(term, dx_keywords[[dx_summary]))) {
            return(dx_summary) # Return the category name if a match is found
        }
    }
    return("Others") # If no specific match, classify as "Others"
}

termlevel_dx <- data %>% filter(diagnosis_status == "Have diagnosis on file") %>% separate_rows(Diagnosis, sep = ",") %>% mutate(dx_term = str_trim(Diagnosis)) %>% filter(dx_term != "") %>% rowwise() %>% mutate(diagnosis_summary = classify_term(dx_term)) %>% ungroup()

# Count the samples that have diagnosis
dxcount_dx <- termlevel_dx %>% group_by(Species, diagnosis_summary) %>% summarize(count = n(), .groups = 'drop')

data_plot <- bind_rows(dxcount_empty, dxcount_dx)

data_plot <- data_plot %>% mutate(diagnosis_summary = factor(diagnosis_summary, levels = c("Have diagnosis on file", names(dx_keywords), "Others"))) %>% replace_na(list(main_diagnosis = "Need extracting diagnosis from records"))

nodes <- data.frame(name = c(as.character(unique(data_plot$Species)), as.character(unique(data_plot$diagnosis_summary))))

links <- data_plot %>% mutate(source = match(Species, nodes$name) - 1, target = match(diagnosis_summary, nodes$name) - 1, value = count) %>% select(source, target, value)

# Create a Sankey diagram categorizing samples based on relevant diagnosis summary
sankeyNetwork(Links = links, Nodes = nodes, Source = "source", Target = "target", Value = "value", NodeID = "name", units = "samples", fontSize = 20, nodeWidth = 50, sinksRight = FALSE)

