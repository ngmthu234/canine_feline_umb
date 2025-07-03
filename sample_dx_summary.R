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

# Online sankeymatic diagram
https://sankeymatic.com/build/?i=PTAEFEDsBcFMCdQDEA2B7A7gZ1AI1tBrLJKAHJoAmsWANKCgJYDWso0AFo1gFwBQIUEOHCAymgCu8AMZsA2gEEAsgHkAqmQAqAXVCaAhvADmBPn01po%2BlKCz6AtgAcUNUHICMAVgCcADl2ijrDSjK4AxAAssADs%2Bvq%2BZoHBoTge7tEAbLoAImhGoGEAZgBMvrAAzNJ8SSGucqWeugDC%2BtAFhdG40rHVQbWp7uXRumTElKCwAB7Q8PrS0IyQ%2BYXwaPag8MFo8JRYZrn5cuUZEc36kLLwfAduvv7ksFJo6EaM0tagjqu4LvZ7N3JfOVdApILB7Ix9Nc8m4IsDQAAJWDWTgAT2hh3cXl0SMMbUo3GRWFgGLcWMaoDUmgAkqAwEgADJU7KkuQZLKU1ZMTiQrDcVnFCnZSH4OA4eywFDciT%2DGFHU6gJoAaRZAOKcJGYwm01m80Wy1W6020m2uzMLTacgiw0V50ufAtbgpoyeLzeHy%2BaB%2B4L2jvqFNB4MhDtatxxyJQaJDluKxXDeNABOJ%2BmJ0bcxXcuiptPpTM0LL92M5z0YPJT%2DL93hyIoIrglUtLMrTck8NuVBdDckymtg4ymMzmCyWoBWaw2Wx2ez5AC82BhQF4AAyLvhCDigDLLvj2QyvUg2dzFVcbBdHoRtdwJIS4UAr3D5E3oRBFQqv1%2DHgeQLCOQwkNpkPhICoOcFxXNdQE8MDbB%2DEJh2iTxj1wU0EFvD8OHBNhcGPR9tgKO58KvUA0Bg0tUQXPhCnQedpCkAA3VopDYRcADoIjPUBFnQ%2BBS1HdYgLBbDnlwsJvFEsTj2IuZSNvZiEJQfRUUkNpkMQKZB2PAArGUFkKRTuL3HAAKELSsB01ESF2chj02WiEGJIxZkcdcjNAVorGkLgLjWZxa1MtBQDBX9TL4eT8BQHAcOfZdoqgrhqCstdGCMDgmGStoWPg49CjQGBCjmNg7C%2DABaYluMKY8mDBb8pOHFj2M2eSFlsmc2CxKCdyMSBGF00CV1CyVIAcNh9EcIJDBwABNY8WoXDJjyIJKODaCIt36lB6JQCRhtG5F4EmrKJClL5kj5HLQCmoRHDQPkFjOsLMHm2BFuW1b9DCq6bsYM79AkaA0GsJLSCgrAPIw1zfrQLLGD2tp8GyzZEM2fRmCuxY2gQjatpHbYdzaAByWhmLx49jsKRhJlAPHiaELAJFfcnKeJzgMLQV9iTafRQEIm8oOkVChHGbxtwITmJRgL6v3OVhUVxt4EoYbgFnsXBrDtVwpvsNBbNAGoUlAIq6oyLxvHoFdNe1m4DdYxd0l8U3mMXWNHe3LW2EdK2Vtt%2Bgrcd4poiF822FGXttQHPVh148cTUnfWWNbdwTZk3wbfZF3tZdLk8ndGxPW9P5b29livFjaI07YQMIU5xdC%2BY8oIncE4y8RCM0QL2PmPSRvA%2BbhMkyJJia%2Dca1yiPbvszpZA82yNurcPcpvFH13i25Lhyxwav28XXxRIQ7vhTe3zQHraV15rxc4ThJv2xnljl08cp3CvtXEA3q3im8CJPAiIA
                                             
