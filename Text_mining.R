
# Project book of abstract NEOBIOTA & ICAIS? 
## Code by: Sergio, Ismael, Fran & Miguel
Sys.time()

install.packages("pacman")
pacman::p_load(dplyr, tidyr,pdftools, tesseract, httr, stringr,pdfsearch,xlsx, writexl,readxl)

suppressMessages({
  library(dplyr, quiet = TRUE, warn.conflicts = FALSE)
  library(tidyr)  
  library(stringr)
  require(httr)
  require(rvest)
  library(pdftools)
  library(pdfsearch)
  library(xlsx)
  library(readxl)
  library(writexl)
  library(devtools)
  Sys.setenv(LANGUAGE = "en")
})


## setwd
setwd("C:/Users/Propietario/Desktop/book_abstracts") # directory


### NEOBIOTA ----
# Notes: I fail to create a generalistic code for all the NEOBIOTA pdf as they have each a format totally different so the code will be specific for each edition -- much more boring

neo <- list.files(path = "./NEOBIOTA/", pattern=".pdf")

# Number of editions 
cat("Number of editions", length(neo), "/ 13")

# option 2 to extract species names
elza <- read_xlsx("C:/Users/Propietario/Desktop/ELZA/GLOBAL NNS DATA FINAL.xlsx")
hanno <- read_xlsx("C:/Users/Propietario/Desktop/GlobalAlienSpeciesFirstRecordDatabase_v3.1_freedata.xlsx", sheet = "FirstRecords")
hanno2 <- read.csv("C:/Users/Propietario/Downloads/SInAS_AlienSpeciesDB_2.5_FullTaxaList.csv")

all_species <- c(elza$Taxon, hanno$TaxonName, hanno$scientificName, hanno$OrigName)
elza_sp <- unique(all_species)

res <- data.frame(Edition =as.character(), Year = as.character(), 
                  Title = as.character(), Abstract =as.character(), Author = as.character(),
                  Affiliation = as.character(), Species =as.character(), Species_elza =as.character(),
                  Terminology = as.character(), Style = as.character(), Manually =as.character(), 
                  Person_double_check = as.character()) # generalist data for all the editions

count_words <- function(text){ str_count(text, "\\S+")  }


# NEOBIOTA 2ND EDITION ----
n <- neo[grep("2nd", neo)]
setwd("C:/Users/Propietario/Desktop/book_abstracts/NEOBIOTA") # need to change for the scaners
pdf_images <- pdf_convert(n, dpi = 300)
eng <- tesseract()

ocr_text <- lapply(pdf_images, function(image) {
  tesseract::ocr(image, engine = eng)
})


i<- 22
Style <- "Oral presentation"

for(i in 1:length(ocr_text)){
  abs <- ocr_text[[i]]
  part2 <- abs
  
  if( count_words(abs) < 150) { next }
  
  # Title
  title1 <- str_extract(part2, "^\\s*.*\n.*\n")
  title <- str_replace_all(title1, "\\s+", " ")
  title <- str_trim(title)
  title <- str_replace(title, "\\s+\\S+\\s+\\S+1.*$", "")
  
  if(!is.na(title) & title =="Investigating the adaptive potential of agricultural weeds to increased temperatures"){
    Style <<- "Poster presentation" }
  
  remainder <- sub(title1, "", part2, fixed = TRUE) 
  
  author1 <- sub("\n.*", "", remainder)
  
  pattern2 <- "\\b(Department of|Universita|School|University|Center|Centre|Institute|Colegio|Univesity)\\b"  # to extract the affiliation ..
  affi <- str_extract(remainder, paste0(pattern2, ".*"))
  
  if(is.na(affi)) {
    affi <- str_match(remainder, 
                      pattern = sprintf(".*(%s[^\\n]*)", pattern2))[1,2] 
  }
  
  # Species:
  pattern <- "(?<!\\.\\s)\\b[A-Z][a-z]{6,}\\s[a-z]{6,}\\b"
  sp1 <- unlist(regmatches(part2, gregexpr(pattern, part2, perl = TRUE)))
  sp1 <- paste(sp1, collapse = ", ")
  
  all_sp <- unlist(regmatches(part2, gregexpr("\\b[A-Z][a-z]+\\s[a-z]+(?:\\s[a-z]+)?\\b", part2))) #usinig elza data
  sp2 <- all_sp[all_sp %in% elza_sp]
  sp2 <- paste(sp2, collapse = ", ")
  
  # Terminology
  terms <- str_extract_all(tolower(title), "Non-Native|naturalized|naturalised|nonindigenous|non-indigenous|allochthonous|invasive|invader|alien|exotic|non-native|nonnative|invasive alien|invasive non-native", simplify = FALSE)
  terms <- unlist(terms)
  terms <- unique(terms)
  terms <- paste(terms, collapse = ", ")
  
  data <- data.frame(Edition =n, Year = "2002", 
                     Title = tolower(title), Abstract =part2, Author = author1,
                     Affiliation = affi, Species =sp1, Species_elza =sp2, 
                     Terminology = terms, Style = Style, Manually ="Need to check manually", 
                     Person_double_check = "NA")
  
  res <- rbind(res, data)
  print(i)
}




# NEOBIOTA 5TH EDITION ----
n <- neo[grep("5th", neo)]
setwd("C:/Users/Propietario/Desktop/book_abstracts/") # need to change for the scaners

edition <- file.path(path ="./NEOBIOTA/", n)
text <- pdf_text(edition)

abstracts <- strsplit(text, "(?<=\n)(?=[^\\n]*Oral presentation|Poster presentation|Key note talk)", perl=TRUE)
abstracts <- abstracts[sapply(abstracts, nchar) > 0]

i <- 27
for(i in 1:length(abstracts)) {
  # del 1 al 47 funciona bien
  if (i %in% 1:63) {
  abs <- abstracts[[i]]
  abs1 <- gsub("\r\n", "\n", abs)
  abs1 <- gsub("\r", "\n", abs1)
  
  if( count_words(abs1) < 150) { next }
  
  parts <- str_split(abs1, "(\\d+-\\d+\n\n\n)", simplify = FALSE)[[1]]
  Style <- parts[[1]][1] %>% trimws() 
  
  if (!grepl("Oral presentation|Poster presentation|Key note talk", Style)) { next }
  
  session_types <- c("Oral presentation", "Poster presentation", "Key note talk")
  matches <- sapply(session_types, function(x) if (grepl(x, Style)) x else NA)
  matches <- na.omit(matches)
  Style <- matches[1]
  Style <- unname(Style)
  
  part2<- parts 
  
  # Title
  #  pattern <- "(Oral presentation|Poster presentation|Key note talk)\\s*\\n+\\s*([^\\n]+(?:\\n\\s*[^\\n]+)?)"
  #matches <- str_match(part2, pattern)
  #title1 <- matches[1, 3]
  #title <- gsub("\n", " ", title1)  
  #title <- gsub("\\s+", " ", title) # Just to be sure...
  
  ##
  tit <- str_split(part2, "\\n\\n+", n = 2)
  title <- tit[[1]][2]  %>% trimws()
  title <- sub("\n\n.*", "", title)
  title <- sub("\n.*", "", title)
  title <- gsub("\n", " ", title)  
  title <- gsub("\\s+", " ", title)
  ###
  
  # last_title <- tail(unlist(str_split(title, "\\s+")), 2)
  #last_title <- paste(last_title, collapse=" ")
  #pos_title <- str_detect(part2, regex(paste0("\\b", last_title, "\\b"))) 
  
  # Authors
  remainder <- sub(title, "", part2, fixed = TRUE) 
  
  author1 <- gsub("Oral presentation|Poster presentation|Key note talk", "", remainder)
  author1 <- gsub("[0-9]+", "", author1)
  author1 <- gsub(",", "", author1) 
  author1 <- trimws(author1)
  author1 <- sub("\n.*", "", author1)
  
  #author1 <- str_replace_all(str_trim(remainder), "\\s+", " ")
  #author1 <- gsub("Oral presentation|Poster presentation|Key note talk", "", author1)
  #author1 <- gsub("[0-9]+", "", author1)
  #author1 <- gsub("\\s+", " ", author1)
  #author1 <- gsub(",", "", author1) 
  #author1 <- trimws(author1)
  #author1 <- sub("^((([A-Z][a-zA-Z\\.]*\\s*){3})).*", "\\1", author1)
  #author1 <- trimws(author1)
  
  
  # Affiliation:
  pattern <- "\\b(Department of|School|University|Center|Centre|Institute|Colegio|Univesity)\\b"  # to extract the affiliation ..
  affi <- str_match(part2, pattern = sprintf(".*(%s[^\\n]*)", pattern))[1,2]
  
  # Species:
  pattern <- "(?<!\\.\\s)\\b[A-Z][a-z]{6,}\\s[a-z]{6,}\\b"
  sp1 <- unlist(regmatches(part2, gregexpr(pattern, part2, perl = TRUE)))
  sp1 <- paste(sp1, collapse = ", ")
  
  all_sp <- unlist(regmatches(part2, gregexpr("\\b[A-Z][a-z]+\\s[a-z]+(?:\\s[a-z]+)?\\b", part2))) #usinig elza data
  sp2 <- all_sp[all_sp %in% elza_sp]
  sp2 <- paste(sp2, collapse = ", ")
  
  # Terminology
  terms <- str_extract_all(title, "introduced|naturalized|naturalised|nonindigenous|non-indigenous|allochthonous|invasive|invader|alien|exotic|non-native|nonnative|invasive alien|invasive non-native|neobiota|adventive|translocated|noxious|pest|weed|colonizer|established|feral", simplify = FALSE)
  terms <- unlist(terms)
  terms <- unique(terms)
  terms <- paste(terms, collapse = ", ")
  
  data <- data.frame(Edition =n, Year = "2008", 
                     Title = title, Abstract =part2, Author = author1,
                     Affiliation = affi, Species =sp1, Species_elza =sp2, 
                     Terminology = terms, Style = Style, Manually ="No", 
                     Person_double_check = "NA")
  
  res <- rbind(res, data)
  print(i)
  } else {
    abs <- abstracts[[i]]
    abs1 <- gsub("\r\n", "\n", abs)
    abs1 <- gsub("\r", "\n", abs1)
    
    parts <- str_split(abs1, "(\\d+-\\d+\n\n\n)", simplify = FALSE)[[1]]
    Style <- parts[[1]][1] %>% trimws() 
    
    if (!grepl("Oral presentation|Poster presentation|Key note talk", Style)) { next }
    
    session_types <- c("Oral presentation", "Poster presentation", "Key note talk")
    matches <- sapply(session_types, function(x) if (grepl(x, Style)) x else NA)
    matches <- na.omit(matches)
    Style <- matches[1]
    Style <- unname(Style)
    
    part2<- parts[2] 
    
    # Title
    tit <- str_split(part2, "\\n\\n+", n = 2)
    title <- tit[[1]][1]  %>% trimws()
    title <- gsub("\n", " ", title)  
    title <- gsub("\\s+", " ", title)
  
     if(title==""){
       title <- str_extract(part2, "(?<=\\n\\n)\\s*[^\\n]*")
       title <- gsub("\n", " ", title)  %>% trimws()
       title <- gsub("\\s+", " ", title) 
   }
    
    # Authors
    remainder <- sub(title, "", part2, fixed = TRUE) 
    
    author1 <- gsub("Oral presentation|Poster presentation|Key note talk", "", remainder)
    author1 <- gsub("[0-9]+", "", author1)
    author1 <- gsub(",", "", author1) 
    author1 <- trimws(author1)
    author1 <- sub("\n.*", "", author1)
    
    # Affiliation:
    pattern <- "\\b(Department of|School|University|Center|Centre|Institute|Colegio|Univesity)\\b"  # to extract the affiliation ..
    affi <- str_match(part2, pattern = sprintf(".*(%s[^\\n]*)", pattern))[1,2]
    
    # Species:
    pattern <- "(?<!\\.\\s)\\b[A-Z][a-z]{6,}\\s[a-z]{6,}\\b"
    sp1 <- unlist(regmatches(part2, gregexpr(pattern, part2, perl = TRUE)))
    sp1 <- paste(sp1, collapse = ", ")
    
    all_sp <- unlist(regmatches(part2, gregexpr("\\b[A-Z][a-z]+\\s[a-z]+(?:\\s[a-z]+)?\\b", part2))) #usinig elza data
    sp2 <- all_sp[all_sp %in% elza_sp]
    sp2 <- paste(sp2, collapse = ", ")
    
    # Terminology
    terms <- str_extract_all(tolower(title), "Non-Native|naturalized|naturalised|nonindigenous|non-indigenous|allochthonous|invasive|invader|alien|exotic|non-native|nonnative|invasive alien|invasive non-native", simplify = FALSE)
    terms <- unlist(terms)
    terms <- unique(terms)
    terms <- paste(terms, collapse = ", ")
    
    data <- data.frame(Edition =n, Year = "2008", 
                       Title = title, Abstract =part2, Author = author1,
                       Affiliation = affi, Species =sp1, Species_elza =sp2, 
                       Terminology = terms, Style = Style, Manually ="No", 
                       Person_double_check = "NA")
    
    res <- rbind(res, data)
    print(i)
}
}





# NEOBIOTA 6TH EDITION ----
n <- neo[grep("6th", neo)]
setwd("C:/Users/Propietario/Desktop/book_abstracts/NEOBIOTA/")
pdf_images <- pdf_convert(n, dpi = 300)
eng <- tesseract()

ocr_text <- lapply(pdf_images, function(image) {
  tesseract::ocr(image, engine = eng)
})


i<- 22
Style <- "Oral presentation"

for(i in 1:length(ocr_text)){
  abs <- ocr_text[[i]]
  part2 <- abs
  
  if( count_words(abs) < 150) { next }
  
  # Title
  title1 <- str_extract(part2, "^\\s*.*\n.*\n")
  title <- str_replace_all(title1, "\\s+", " ")
  title <- str_trim(title)
  title <- str_replace(title, "\\s+\\S+\\s+\\S+1.*$", "")
  
  if(!is.na(title) & title =="Investigating the adaptive potential of agricultural weeds to increased temperatures"){
    Style <<- "Poster presentation" }
  
  remainder <- sub(title1, "", part2, fixed = TRUE) 
  
  author1 <- sub("\n.*", "", remainder)
  
  pattern2 <- "\\b(Department of|Universita|School|University|Center|Centre|Institute|Colegio|Univesity)\\b"  # to extract the affiliation ..
  affi <- str_extract(remainder, paste0(pattern2, ".*"))
  
  if(is.na(affi)) {
    affi <- str_match(remainder, 
                      pattern = sprintf(".*(%s[^\\n]*)", pattern2))[1,2] 
  }
  
  # Species:
  pattern <- "(?<!\\.\\s)\\b[A-Z][a-z]{6,}\\s[a-z]{6,}\\b"
  sp1 <- unlist(regmatches(part2, gregexpr(pattern, part2, perl = TRUE)))
  sp1 <- paste(sp1, collapse = ", ")
  
  all_sp <- unlist(regmatches(part2, gregexpr("\\b[A-Z][a-z]+\\s[a-z]+(?:\\s[a-z]+)?\\b", part2))) #usinig elza data
  sp2 <- all_sp[all_sp %in% elza_sp]
  sp2 <- paste(sp2, collapse = ", ")
  
  # Terminology
  terms <- str_extract_all(tolower(title), "Non-Native|naturalized|naturalised|nonindigenous|non-indigenous|allochthonous|invasive|invader|alien|exotic|non-native|nonnative|invasive alien|invasive non-native", simplify = FALSE)
  terms <- unlist(terms)
  terms <- unique(terms)
  terms <- paste(terms, collapse = ", ")
  
  data <- data.frame(Edition =n, Year = "2002", 
                     Title = tolower(title), Abstract =part2, Author = author1,
                     Affiliation = affi, Species =sp1, Species_elza =sp2, 
                     Terminology = terms, Style = Style, Manually ="Need to check manually", 
                     Person_double_check = "NA")
  
  res <- rbind(res, data)
  print(i)
}




# NEOBIOTA 7TH EDITION ----
n <- neo[grep("7th", neo)]
setwd("C:/Users/Propietario/Desktop/book_abstracts/") 

edition <- file.path(path ="./NEOBIOTA/", n)
text <- pdf_text(edition)

pattern <- paste0("(",  paste(c("Oral session 1 Climate change interactions with biological invasions",
                          "Oral session 2 New tools for prevention and early detection of invasive species",
                          "Oral session 3 Modelling the success of alien species",
                          "Oral session 4 Risk analysis of biological invasions",
                          "Oral session 5 Ecological impacts of biological invasions",
                          "Oral session 6 Successful controls and eradications of invasive species",
                          "Oral session 7 Exploring the routes, pathways and vectors of invasion",
                          "Oral session 8 Species traits conferring invasiveness",
                          "Oral session 9 Biotic and environmental control of biological invasions",
                          "Oral session 10 Genetics and evolution of introduced and native populations",
                          "Poster session 1 Climate change interactions with biological invasions",
                          "Poster session 2 New tools for prevention and early detection of invasive species",
                          "Poster session 3 Modelling the success of alien species",
                          "Poster session 4 Risk analysis of biological invasions",
                          "Poster session 5 Ecological impacts of biological invasions",
                          "Poster session 6 Successful controls and eradications of invasive species",
                          "Poster session 7 Exploring the routes, pathways and vectors of invasion",
                          "Poster session 8 Species traits conferring invasiveness",
                          "Poster session 9 Biotic and environmental control of biological invasions",
                          "Poster session 10 Genetics and evolution of introduced and native populations"),
                        collapse = "|"), ")")

abstracts <- unlist(strsplit(text, split = pattern, perl = TRUE))
abstracts <- abstracts[sapply(abstracts, nchar) > 0]

i<- 305
Style <- "Oral presentation"

for(i in 1:length(abstracts)){
  abs <- abstracts[[i]]
  abs1 <- paste(abs, collapse = " ")
  part2 <- abs1
  

  if( count_words(part2) < 150) { next }
  
  # Title
  title1 <- str_extract(part2, "^\\s*.*\n.*\n")
  title <- str_replace_all(title1, "\\s+", " ")
  title <- str_trim(title)
  title <- str_replace(title, "\\s+\\S+\\s+\\S+1.*$", "")
  
  if(!is.na(title) & title =="Investigating the adaptive potential of agricultural weeds to increased temperatures"){
    Style <<- "Poster presentation" }
  
  author <- str_extract(part2, "(?<=\n)[^\n,]+(?=1,)")
  author <- author %>% trimws()
    
  remainder <- sub(title1, "", part2, fixed = TRUE) 
  
  if(is.na(author)){
    author1 <- sub(",.*", "", remainder)  }
  
  pattern2 <- "\\b(Department of|School|University|Center|Centre|Institute|Colegio|Univesity)\\b"  # to extract the affiliation ..
  affi <- str_extract(remainder, paste0(pattern2, ".*"))
  
  if(is.na(affi)) {
    affi <- str_match(remainder, 
                      pattern = sprintf(".*(%s[^\\n]*)", pattern2))[1,2] 
  }
  
  # Species:
  pattern <- "(?<!\\.\\s)\\b[A-Z][a-z]{6,}\\s[a-z]{6,}\\b"
  sp1 <- unlist(regmatches(part2, gregexpr(pattern, part2, perl = TRUE)))
  sp1 <- paste(sp1, collapse = ", ")
  
  all_sp <- unlist(regmatches(part2, gregexpr("\\b[A-Z][a-z]+\\s[a-z]+(?:\\s[a-z]+)?\\b", part2))) #usinig elza data
  sp2 <- all_sp[all_sp %in% elza_sp]
  sp2 <- paste(sp2, collapse = ", ")
  
  # Terminology
  terms <- str_extract_all(tolower(title), "Non-Native|naturalized|naturalised|nonindigenous|non-indigenous|allochthonous|invasive|invader|alien|exotic|non-native|nonnative|invasive alien|invasive non-native", simplify = FALSE)
  terms <- unlist(terms)
  terms <- unique(terms)
  terms <- paste(terms, collapse = ", ")
  
  data <- data.frame(Edition =n, Year = "2012", 
                     Title = tolower(title), Abstract =part2, Author = author1,
                     Affiliation = affi, Species =sp1, Species_elza =sp2, 
                     Terminology = terms, Style = Style, Manually ="Check keynotes", 
                     Person_double_check = "NA")
  
  res <- rbind(res, data)
  print(i)
}



# NEOBIOTA 8TH EDITION ----
n <- neo[grep("8th", neo)]

edition <- file.path(path ="./NEOBIOTA/", n)
text <- pdf_text(edition)


pattern <- "8th International Conference on Biological Invasions from understanding to action\n\n"

abstracts <- strsplit(text, pattern, perl=TRUE)
abstracts <- abstracts[nchar(abstracts) > 0]
#abstracts <- sapply(abstracts, function(x) paste0(pattern, x), USE.NAMES = FALSE)

i <- 163
Style <- "Oral presentation"

for(i in 1:length(abstracts)){
  abs <- abstracts[[i]]
  if( length(abs) >1 ) { 
 abs1 <- abs[[2]]
  } else { next }
 part2 <- abs1
 
 # Title
 title <- str_extract(abs1, "^\\s*.*\n.*\n")
 title <- str_replace_all(title, "\\s+", " ")
 title <- str_trim(title)
 
 if(title=="The Distribution of the Eastern Mosquitofish (Gambusia holbrooki) and Endemic Aphanius villwocki in the Upper Sakarya River Basin"){
   Style <<- "Poster presentation"
 }
 
 
 # Extract authors and their indices
 author1 <- str_replace_all(str_trim(abs1), "\\s+", " ")
 author1 <- str_extract(author1, "\\b\\w+ \\w+(?=1)")
 
 # Extract affiliations linked by indices
 #affi <- str_extract_all(abs1, "\\d\\n.*\\n*.*(?=\\n\\d\\n|\\n\\n)", simplify = TRUE)
 #affi <- vapply(affi, function(x) {
 # str_replace_all(x, "\n", " ")
 #}, character(1))[1]

 #pat2 <- "(?<=1\\n)[\\s\\S]*?(?=e-mail|\\n\\n)"
 #affi <- str_extract(abs1, pat2)
 pattern <- "\\b(Department of|School|University|Center|Centre|Institute|Colegio|Univesity)\\b"  # to extract the affiliation ..
 affi <- str_match(part2, pattern = sprintf(".*(%s[^\\n]*)", pattern))[1,2]
 
 # Species:
 pattern <- "(?<!\\.\\s)\\b[A-Z][a-z]{6,}\\s[a-z]{6,}\\b"
 sp1 <- unlist(regmatches(part2, gregexpr(pattern, part2, perl = TRUE)))
 sp1 <- paste(sp1, collapse = ", ")
 
 all_sp <- unlist(regmatches(part2, gregexpr("\\b[A-Z][a-z]+\\s[a-z]+(?:\\s[a-z]+)?\\b", part2))) #usinig elza data
 sp2 <- all_sp[all_sp %in% elza_sp]
 sp2 <- paste(sp2, collapse = ", ")
 
 # Terminology
 terms <- str_extract_all(tolower(title), "Non-Native|naturalized|naturalised|nonindigenous|non-indigenous|allochthonous|invasive|invader|alien|exotic|non-native|nonnative|invasive alien|invasive non-native", simplify = FALSE)
 terms <- unlist(terms)
 terms <- unique(terms)
 terms <- paste(terms, collapse = ", ")
 
 
 data <- data.frame(Edition =n, Year = "2014", 
                    Title = title, Abstract =part2, Author = author1,
                    Affiliation = affi, Species =sp1, Species_elza =sp2, 
                    Terminology = terms, Style = Style, Manually ="No", 
                    Person_double_check = "NA")
 
 res <- rbind(res, data)
 res <- res %>% filter(!Title %in% c("ORAL PRESENTATIONS",'"NOT ATTENDED"',"POSTER PRESENTATIONS"))
 print(i)
}






# NEOBIOTA 9TH EDITION ----
n <- neo[grep("9th", neo)]

edition <- file.path(path ="./NEOBIOTA/", n)
text <- pdf_text(edition)

pattern <- "\\b(?:Mo|Tu|We|Th|Fr|Sa|Su)-?[A-Z]?\\d+-\\d+\\b"

abstracts <- str_split(text, pattern)

i <- 30
Style <- "Oral presentation"

for(i in 1:length(abstracts)){
  abs <- abstracts[[i]]
  abs1 <- abs
  if( length(abs1) >1 ) { 
    abs1 <- abs1[[2]]
  } 
  part2 <- abs1
  
  # Title
  title1 <- str_extract(abs1, "^\\s*.*\n.*\n")
  title <- str_replace_all(title1, "\\s+", " ")
  title <- str_trim(title)
  
  if(!is.na(title) & title =="Poster presentations"){
    Style <<- "Poster presentation" }
  
  if((grepl("^P-\\d+", title))) { 
    title1 <- gsub("([][{}()+*^$\\|?.])", "\\\\\\1", title1)
    remainder <- sub(title1, "", part2)
  }  else {
    remainder <- sub(title1, "", part2) 
  }

  author1 <- str_replace_all(str_trim(abs1), "\\s+", " ")
  author1 <- str_extract(author1, "\\b\\w+ \\w+(?=1)")
  

  if(is.na(author1)){
    author1 <- sub(",.*", "", remainder)  }
  
  #affi <- sub("^[^\n]*\n", "", remainder)
  #affi <- sub("(.*?)(\\n).*", "\\1", affi)
  pattern <- "\\b(Department of|School|University|Center|Centre|Institute|Colegio|Univesity)\\b"  # to extract the affiliation ..
  affi <- str_match(part2, pattern = sprintf(".*(%s[^\\n]*)", pattern))[1,2]
  
  # Species:
  pattern <- "(?<!\\.\\s)\\b[A-Z][a-z]{6,}\\s[a-z]{6,}\\b"
  sp1 <- unlist(regmatches(part2, gregexpr(pattern, part2, perl = TRUE)))
  sp1 <- paste(sp1, collapse = ", ")
  
  all_sp <- unlist(regmatches(part2, gregexpr("\\b[A-Z][a-z]+\\s[a-z]+(?:\\s[a-z]+)?\\b", part2))) #usinig elza data
  sp2 <- all_sp[all_sp %in% elza_sp]
  sp2 <- paste(sp2, collapse = ", ")
  
  # Terminology
  terms <- str_extract_all(tolower(title), "Non-Native|naturalized|naturalised|nonindigenous|non-indigenous|allochthonous|invasive|invader|alien|exotic|non-native|nonnative|invasive alien|invasive non-native", simplify = FALSE)
  terms <- unlist(terms)
  terms <- unique(terms)
  terms <- paste(terms, collapse = ", ")
  
  
  data <- data.frame(Edition =n, Year = "2016", 
                     Title = title, Abstract =part2, Author = author1,
                     Affiliation = affi, Species =sp1, Species_elza =sp2, 
                     Terminology = terms, Style = Style, Manually ="No", 
                     Person_double_check = "NA")
  
  res <- rbind(res, data)
  res <- res %>% filter(!Title %in% c("ORAL PRESENTATIONS",'"NOT ATTENDED"',"POSTER PRESENTATIONS"))
  print(i)
}





# NEOBIOTA 10TH EDITION ----
n <- neo[grep("10th", neo)]

edition <- file.path(path ="./NEOBIOTA/", n)
text <- pdf_text(edition)

pattern <- "\\b[A-Za-z]+\\d+\\b"
abstracts <- str_split(text, pattern)

pattern <- "\\b[A-Za-z]+\\d+\\b"
abstracts <- str_split(text, pattern, simplify = FALSE)

i<- 61
Style <- "Oral presentation"

for(i in 1:length(abstracts)){
  abs <- abstracts[[i]]
  #abs1 <- paste(abs, collapse = " ")
  #part2 <- abs1
  
  pattern <- "^\\n+.*"
  papers <- abs[str_detect(abs, pattern)]

  if(length(papers) <1 ) { next}
  
  combine_lines <- function(abs) {
    combined <- c()
    i <- 1
    while (i <= length(abs)) {
      if (count_words(abs[i]) < 150) {
        if (i < length(abs)) {
          combined <- c(combined, paste(abs[i], abs[i + 1], sep = " "))
          i <- i + 2  
        } else {
          combined <- c(combined, abs[i])  
          i <- i + 1
        }
      } else {
        combined <- c(combined, abs[i])  
        i <- i + 1 
      } 
    } 
    return(combined) }
  
  papers2 <- combine_lines(papers)
  
  for(j in 1:length(papers2)){
    papers3 <- papers2[[j]] 
    part2 <- papers3 
    # Title
    title1 <- str_extract(papers3, "^\\s*.*\n.*\n")
    title <- str_replace_all(title1, "\\s+", " ")
    title <- str_trim(title)
    
    if(!is.na(title) & title =="The role of the invasive weed Panicum miliaceum in the epidemiology of cereal viruses Gyorgy Pasztor, Rita Szabo, Erzsébet Nadasy, Andras Takacs the autumn 2014 and 2015. After collection, the samples were"){
      Style <<- "Poster presentation" }
    
    if((grepl("^P\\d+", title))) { 
      title1 <- gsub("([][{}()+*^$\\|?.])", "\\\\\\1", title1)
      remainder <- sub(title1, "", part2)
    }  else {
      remainder <- sub(title1, "", part2) 
    }
    
    author1 <- str_replace_all(str_trim(remainder), "\\s+", " ")
    author1 <- sub(",.*", "", author1)
    
    if(is.na(author1)){
      author1 <- sub(",.*", "", remainder)  }
    
    pattern <- "\\b(Department of|School|University|Center|Centre|Institute|Colegio|Univesity)\\b"  # to extract the affiliation ..
    affi <- str_match(part2, pattern = sprintf(".*(%s[^\\n]*)", pattern))[1,2]
    
    # Species:
    pattern <- "(?<!\\.\\s)\\b[A-Z][a-z]{6,}\\s[a-z]{6,}\\b"
    sp1 <- unlist(regmatches(part2, gregexpr(pattern, part2, perl = TRUE)))
    sp1 <- paste(sp1, collapse = ", ")
    
    all_sp <- unlist(regmatches(part2, gregexpr("\\b[A-Z][a-z]+\\s[a-z]+(?:\\s[a-z]+)?\\b", part2))) #usinig elza data
    sp2 <- all_sp[all_sp %in% elza_sp]
    sp2 <- paste(sp2, collapse = ", ")
    
    # Terminology
    terms <- str_extract_all(tolower(title), "Non-Native|naturalized|naturalised|nonindigenous|non-indigenous|allochthonous|invasive|invader|alien|exotic|non-native|nonnative|invasive alien|invasive non-native", simplify = FALSE)
    terms <- unlist(terms)
    terms <- unique(terms)
    terms <- paste(terms, collapse = ", ")
    
    data <- data.frame(Edition =n, Year = "2018", 
                       Title = title, Abstract =part2, Author = author1,
                       Affiliation = affi, Species =sp1, Species_elza =sp2, 
                       Terminology = terms, Style = Style, Manually ="No", 
                       Person_double_check = "NA")
    
    res <- rbind(res, data)
    res <- res %>% filter(!Title %in% c("ORAL PRESENTATIONS",'"NOT ATTENDED"',"POSTER PRESENTATIONS"))
    print(i)
  }
}




# NEOBIOTA 11th EDITION ----
n <- neo[grep("11th", neo)]

edition <- file.path(path ="./NEOBIOTA/", n)
text <- pdf_text(edition)

pattern <- "(O\\d+|P\\d+)"
abstracts <- unlist(strsplit(text, split = pattern, perl = TRUE))

i<- 289
Style <- "Oral presentation"

for(i in 1:length(abstracts)){
  abs <- abstracts[[i]]
  abs1 <- paste(abs, collapse = " ")
  part2 <- strsplit(abs1, split = "\nAbstract\n", fixed = TRUE)
  part3 <- part2[[1]][2] # abstract clean 
  

    if(count_words(part2) < 150) { next}
      
    info <- part2[[1]][1]
    
    # Title
    title1 <- str_extract(info, "^\\s*.*\n.*\n")
    title <- str_replace_all(title1, "\\s+", " ")
    title <- str_trim(title)
    
    if(!is.na(title) & title =="Current distribution of alien crop pests (Insecta) detected during the period 2016- 2019 in Bulgaria"){
      Style <<- "Poster presentation" }
    
    remainder <- sub(title1, "", info, fixed = TRUE) 
    
    author1 <- str_replace_all(str_trim(remainder), "\\s+", " ")
    author1 <- sub(",.*", "", author1)
    
    if(is.na(author1)){
      author1 <- sub(",.*", "", remainder)  }
    
    pattern2 <- "\\b(Department of|School|University|Center|Centre|Institute|Colegio|Univesity)\\b"  # to extract the affiliation ..
    affi <- sub(".*\\n1", "", remainder)
    affi <- sub("\n*", "", affi)
    
    if (grepl("2", affi)) {
      affi <- sub(".*\\n1(.*?)2.*", "\\1", remainder)
    } else {
      affi <- sub(".*\\n1", "", remainder)
    }
  
    if(is.na(affi)) {affi <- str_match(remainder, 
      pattern = sprintf(".*(%s[^\\n]*)", pattern2))[1,2] }
    
    # Species:
    pattern <- "(?<!\\.\\s)\\b[A-Z][a-z]{6,}\\s[a-z]{6,}\\b"
    sp1 <- unlist(regmatches(part3, gregexpr(pattern, part3, perl = TRUE)))
    sp1 <- paste(sp1, collapse = ", ")
    
    all_sp <- unlist(regmatches(part3, gregexpr("\\b[A-Z][a-z]+\\s[a-z]+(?:\\s[a-z]+)?\\b", part3))) #usinig elza data
    sp2 <- all_sp[all_sp %in% elza_sp]
    sp2 <- paste(sp2, collapse = ", ")
    
    # Terminology
    terms <- str_extract_all(tolower(title), "Non-Native|naturalized|naturalised|nonindigenous|non-indigenous|allochthonous|invasive|invader|alien|exotic|non-native|nonnative|invasive alien|invasive non-native", simplify = FALSE)
    terms <- unlist(terms)
    terms <- unique(terms)
    terms <- paste(terms, collapse = ", ")
    
    data <- data.frame(Edition =n, Year = "2020", 
                       Title = title, Abstract =part3, Author = author1,
                       Affiliation = affi, Species =sp1, Species_elza =sp2, 
                       Terminology = terms, Style = Style, Manually ="Please add keynote manually", 
                       Person_double_check = "NA")
    
    res <- rbind(res, data)
    res <- res %>% filter(!Title %in% c("ORAL PRESENTATIONS",'"NOT ATTENDED"',"POSTER PRESENTATIONS"))
    print(i)
}






# NEOBIOTA 12th EDITION ----
n <- neo[grep("12th", neo)]

edition <- file.path(path ="./NEOBIOTA/", n)
text <- pdf_text(edition)

pattern <- "([A-Z]\\d+)"
abstracts <- unlist(strsplit(text, split = pattern, perl = TRUE))

i<- 290
Style <- "Oral presentation"

for(i in 1:length(abstracts)){
  abs <- abstracts[[i]]
  abs1 <- paste(abs, collapse = " ")
  part2 <- abs1
  
  if(count_words(part2) < 150) { next}
  
  # Title
  title1 <- str_extract(part2, "^\\s*.*\n.*\n")
  title <- str_replace_all(title1, "\\s+", " ")
  title <- str_trim(title)
  title <- str_replace(title, "\\s+\\S+\\s+\\S+1.*$", "")
  
  if(!is.na(title) & title =="Gastrointestinal parasites of Egyptian geese Alopochen aegyptiaca (Linnaeus, 1766) in Western Europe"){
    Style <<- "Poster presentation" }
  
  author <- str_extract(part2, "(?<=\n)[^\n,]+(?=1,)")
  
  remainder <- sub(title1, "", part2, fixed = TRUE) 
  
  if(is.na(author)){
    author1 <- sub(",.*", "", remainder)  }
  
  pattern2 <- "\\b(Department of|School|University|Center|Centre|Institute|Colegio|Univesity)\\b"  # to extract the affiliation ..
  affi <- str_extract(remainder, paste0(pattern2, ".*"))

  
  if(is.na(affi)) {
    affi <- str_match(remainder, 
    pattern = sprintf(".*(%s[^\\n]*)", pattern2))[1,2] }
  
  # Species:
  pattern <- "(?<!\\.\\s)\\b[A-Z][a-z]{6,}\\s[a-z]{6,}\\b"
  sp1 <- unlist(regmatches(part2, gregexpr(pattern, part2, perl = TRUE)))
  sp1 <- paste(sp1, collapse = ", ")
  
  all_sp <- unlist(regmatches(part2, gregexpr("\\b[A-Z][a-z]+\\s[a-z]+(?:\\s[a-z]+)?\\b", part3))) #usinig elza data
  sp2 <- all_sp[all_sp %in% elza_sp]
  sp2 <- paste(sp2, collapse = ", ")
  
  # Terminology
  terms <- str_extract_all(tolower(title), "Non-Native|naturalized|naturalised|nonindigenous|non-indigenous|allochthonous|invasive|invader|alien|exotic|non-native|nonnative|invasive alien|invasive non-native", simplify = FALSE)
  terms <- unlist(terms)
  terms <- unique(terms)
  terms <- paste(terms, collapse = ", ")
  
  data <- data.frame(Edition =n, Year = "2022", 
                     Title = title, Abstract =part2, Author = author1,
                     Affiliation = affi, Species =sp1, Species_elza =sp2, 
                     Terminology = terms, Style = Style, Manually ="Check keynotes", 
                     Person_double_check = "NA")
  
  res <- rbind(res, data)
  print(i)
}






# NEOBIOTA 13th EDITION ----
n <- neo[grep("13th", neo)]

edition <- file.path(path ="./NEOBIOTA/", n)
text <- pdf_text(edition)

pattern <- paste0("(", 
                  paste(c("Session 1 – Risk assessment and management of invasive species",
                          "Session 2 – New tools and approaches for detection and monitoring",
                          "Session 3 – Global change and invasions",
                          "Session 4 – Socioeconomic impacts of invasions",
                          "Session 5 – Conservation issues and biological invasions",
                          "Session 6 – Pathways and dispersal of invasive species"),
                        collapse = "|"),
                  ")")

abstracts <- unlist(strsplit(text, split = pattern, perl = TRUE))


i<- 4
Style <- "Oral presentation"

for(i in 1:length(abstracts)){
  abs <- abstracts[[i]]
  abs1 <- paste(abs, collapse = " ")
  part2 <- abs1
  
  if( count_words(part2) < 150) { next }
  
  # Title
  title1 <- str_extract(part2, "^\\s*.*\n.*\n")
  title <- str_replace_all(title1, "\\s+", " ")
  title <- str_trim(title)
  title <- str_replace(title, "\\s+\\S+\\s+\\S+1.*$", "")
  
  if(!is.na(title) & title =="APHID INFESTATION ON INVASIVE IMPATIENS L. SPECIES IN EUROPE: IMPLICATIONS FOR PLANT-INSECT INTERACTIONS AND INVASION ECOLOGY"){
    Style <<- "Poster presentation" }
  
  author <- str_extract(part2, "(?<=\n)[^\n,]+(?=1,)")
  
  remainder <- sub(title1, "", part2, fixed = TRUE) 
  
  if(is.na(author)){
    author1 <- sub(",.*", "", remainder)  }
  
  pattern2 <- "\\b(Department of|School|University|Center|Centre|Institute|Colegio|Univesity)\\b"  # to extract the affiliation ..
  affi <- str_extract(remainder, paste0(pattern2, ".*"))
  
  if(is.na(affi)) {
    affi <- str_match(remainder, 
                      pattern = sprintf(".*(%s[^\\n]*)", pattern2))[1,2] 
    }
  
  # Species:
  pattern <- "(?<!\\.\\s)\\b[A-Z][a-z]{6,}\\s[a-z]{6,}\\b"
  sp1 <- unlist(regmatches(part2, gregexpr(pattern, part2, perl = TRUE)))
  sp1 <- paste(sp1, collapse = ", ")
  
  all_sp <- unlist(regmatches(part2, gregexpr("\\b[A-Z][a-z]+\\s[a-z]+(?:\\s[a-z]+)?\\b", part2))) #usinig elza data
  sp2 <- all_sp[all_sp %in% elza_sp]
  sp2 <- paste(sp2, collapse = ", ")
  
  # Terminology
  terms <- str_extract_all(tolower(title), "Non-Native|naturalized|naturalised|nonindigenous|non-indigenous|allochthonous|invasive|invader|alien|exotic|non-native|nonnative|invasive alien|invasive non-native", simplify = FALSE)
  terms <- unlist(terms)
  terms <- unique(terms)
  terms <- paste(terms, collapse = ", ")
  
  data <- data.frame(Edition =n, Year = "2022", 
                     Title = tolower(title), Abstract =part2, Author = author1,
                     Affiliation = affi, Species =sp1, Species_elza =sp2, 
                     Terminology = terms, Style = Style, Manually ="Check keynotes", 
                     Person_double_check = "NA")
  
  res <- rbind(res, data)
  print(i)
}

