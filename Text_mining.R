
# Project book of abstract NEOBIOTA & ICAIS
## Code by: Sergio, Ismael
Sys.time()

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
elza_sp <- unique(elza$Taxon)


res <- data.frame(Edition =as.character(), Year = as.character(), 
                  Title = as.character(), Abstract =as.character(), Author = as.character(),
                  Affiliation = as.character(), Species =as.character(), Species_elza =as.character(),
                  Terminology = as.character(), Style = as.character(), Manually =as.character(), 
                  Person_double_check = as.character()) # generalist data for all the editions


# NEOBIOTA 2ND EDITION ----
n <- neo[5]

edition <- file.path(path ="./NEOBIOTA/", n)
text <- pdf_text(edition)

# !!! Its a scan so can i not mine it, extrat manually -- luckily they are few



# NEOBIOTA 5TH EDITION ----
n <- neo[6]

edition <- file.path(path ="./NEOBIOTA/", n)
text <- pdf_text(edition)

abstracts <- strsplit(text, "(?<=\n)(?=[^\\n]*Oral presentation|Poster presentation|Key note talk)", perl=TRUE)
abstracts <- abstracts[sapply(abstracts, nchar) > 0]

for(i in 1:length(abstracts)) {
  # del 1 al 47 funciona bien
  if (i %in% 1:63) {
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
  
  part2<- parts 
  
  # Title
  pattern <- "(Oral presentation|Poster presentation|Key note talk)\\s*\\n+\\s*([^\\n]+(?:\\n\\s*[^\\n]+)?)"
  matches <- str_match(part2, pattern)
  title <- matches[1, 3]
  title <- gsub("\n", " ", title)  
  title <- gsub("\\s+", " ", title) # Just to be sure...
  
  
  last_title <- tail(unlist(str_split(title, "\\s+")), 2)
  last_title <- paste(last_title, collapse=" ")
  #pos_title <- str_detect(part2, regex(paste0("\\b", last_title, "\\b"))) 
  
  # Authors
  author1 <- str_extract(part2, pattern = sprintf("(?<=%s)[\\s\\S]*?(,|&)",str_escape(last_title)))%>% trimws() 
  author1 <- str_extract(author1, "^[^\n]*")
  author1 <- gsub(",", "", author1) 
  author1 <- gsub("[0-9]+", "", author1)
  author1 <- gsub("\\s+", " ", author1)
  author1 <- trimws(author1)
  
  # Affiliation:
  pattern <- "\\b(University|Center|Centre|Institute|Colegio|Univesity)\\b"  # to extract the affiliation ..
  affi <- str_match(part2, pattern = sprintf(".*(%s[^\\n]*)", pattern))[1,2]
  
  # Species:
  pattern <- "(?<!\\.\\s)\\b[A-Z][a-z]{6,}\\s[a-z]{6,}\\b"
  sp1 <- unlist(regmatches(part2, gregexpr(pattern, part2, perl = TRUE)))
  sp1 <- paste(sp1, collapse = ", ")
  
  all_sp <- unlist(regmatches(part2, gregexpr("\\b[A-Z][a-z]+\\s[a-z]+(?:\\s[a-z]+)?\\b", part2))) #usinig elza data
  sp2 <- all_sp[all_sp %in% elza_sp]
  sp2 <- paste(sp2, collapse = ", ")
  
  # Terminology
  terms <- str_extract_all(title, "naturalized|naturalised|nonindigenous|non-indigenous|allochthonous|invasive|invader|alien|exotic|non-native|nonnative|invasive alien|invasive non-native", simplify = FALSE)
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
    
    last_title <- tail(unlist(str_split(title, "\\s+")), 2)
    last_title <- paste(last_title, collapse=" ")
    #pos_title <- str_detect(part2, regex(paste0("\\b", last_title, "\\b"))) 
    
    # Authors
    author1 <- str_extract(part2, pattern = sprintf("(?<=%s)[\\s\\S]*?(,|&)",str_escape(last_title)))%>% trimws() 
    author1 <- str_extract(author1, "^[^\n]*")
    author1 <- gsub(",", "", author1) 
    author1 <- gsub("[0-9]+", "", author1)
    author1 <- gsub("\\s+", " ", author1)
    author1 <- trimws(author1)
    
    # Affiliation:
    pattern <- "\\b(University|Center|Centre|Institute|Colegio|Univesity)\\b"  # to extract the affiliation ..
    affi <- str_match(part2, pattern = sprintf(".*(%s[^\\n]*)", pattern))[1,2]
    
    # Species:
    pattern <- "(?<!\\.\\s)\\b[A-Z][a-z]{6,}\\s[a-z]{6,}\\b"
    sp1 <- unlist(regmatches(part2, gregexpr(pattern, part2, perl = TRUE)))
    sp1 <- paste(sp1, collapse = ", ")
    
    all_sp <- unlist(regmatches(part2, gregexpr("\\b[A-Z][a-z]+\\s[a-z]+(?:\\s[a-z]+)?\\b", part2))) #usinig elza data
    sp2 <- all_sp[all_sp %in% elza_sp]
    sp2 <- paste(sp2, collapse = ", ")
    
    # Terminology
    terms <- str_extract_all(title, "naturalized|naturalised|nonindigenous|non-indigenous|allochthonous|invasive|invader|alien|exotic|non-native|nonnative|invasive alien|invasive non-native", simplify = FALSE)
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






# NEOBIOTA 8TH EDITION ----
n <- neo[7]

edition <- file.path(path ="./NEOBIOTA/", n)
text <- pdf_text(edition)

pattern <- "POSTER PRESENTATIONS\\n\\nSessions and Titles\\s*([\\s\\S]*)"
poster <- str_extract(text, pattern)


POSTER PRESENTATIONS & ORAL PRESENTATION











#for (j in 1:length(text)) {
    tryCatch({
      if(n =="10th_2018.pdf") {
        abstract <- sub("^.*?(FP10\\s.*?)(?=FP11|\\Z)", "\\1", text, perl = TRUE)
      } else if(n =="11th_2020.pdf") {
        abstract <- sub(".*?\\n\\nAbstract\\n\\n(.*?)\\n\\n\\d+\\n.*", "\\1", text, perl=TRUE)
      } else if(n=="12th_2022.pdf"){
        abstract <- sub(".*?\\n\\nAbstract\\n\\n(.*?)\\n\\nO58.*", "\\1", text, perl = TRUE)
      }
      
      for (abs in abstract) {
      
        #  title
        title <- trimws(gsub("\n.*", "", abs))
        
        #  authors
        lines <- strsplit(abs, "\n")[[1]]
        authors_line <- grep("\\w+\\d", lines)  
        

        author1 <- str_extract(lines[authors_line[1]], "^[^,]*")
        author2 <- str_extract(lines[authors_line[2]], "^[^,]*")
        
        #  Affiliation
        affi_lines <- grep("\\b(University|Center|Centre|Department|Institute)\\b", lines, ignore.case = TRUE)
        first_affi <- if (length(affi_lines) > 0) trimws(lines[affi_lines[1]]) else NA
        
        #  species names1
        sp1 <- unlist(regmatches(abs, gregexpr("\\b[A-Z][a-z]{6,}\\s[a-z]{6,}\\b", abs)))
        sp1 <- paste(sp1, collapse = ", ")
        
        #  Species name 2.0 using the elza database:
        all_sp <- unlist(regmatches(abs, gregexpr("\\b[A-Z][a-z]+\\s[a-z]+(?:\\s[a-z]+)?\\b", abs)))
        sp2 <- all_sp[all_sp %in% elza_sp]
        sp2 <- paste(sp2, collapse = ", ")
        
        res <- rbind(res, data.frame(Full_abstract = abs, Title = title,
                                     First_author = author1, Second_author = author2,
                                     Affiliation = first_affi, 
                                     Species1 = sp1, Species2 = sp2,
                                     Conference = n, stringsAsFactors = FALSE))
      #}
    }, error = function(e) {
      cat("Error:", n, "\n")
    })
  }
  cat("Done:", n, "\n")
}


  
  



   
