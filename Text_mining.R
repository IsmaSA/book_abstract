
# Project book of abstract NEOBIOTA & ICAIS
## Code by: Sergio, Ismael, Fran & Miguel 
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
setwd("C:/Users/Propietario/Desktop/book_abstracts")


### NEOBIOTA ----
neo <- list.files(path = "./NEOBIOTA/", pattern=".pdf")

# Number of editions 
cat("Number of editions", length(neo), "/ 13")

n <- neo[3]
elza <- read_xlsx("C:/Users/Propietario/Desktop/ELZA/GLOBAL NNS DATA FINAL.xlsx")
elza_sp <- unique(elza$Taxon)
### notes
# Maybe we need an specific pattern for eeach conference...
# patterns:
# title <- sub("\n.*", "", sub("\n", "", abstract)) --> 12 th


#
res <- data.frame()
n <- neo[1]
for (n in neo) {
  edition <- file.path(path ="./NEOBIOTA/", n)
  text <- pdf_text(edition)
  
  for (j in 1:length(text)) {
    tryCatch({
      abstracts <- strsplit(text[j], "\\s*(?=\\[\\d+\\]\\s*[A-Z]\\d+)", perl = TRUE)[[1]]
      
      for (abstract in abstracts) {
      
        #  title
        title <- trimws(gsub("\n.*", "", abstract))
        
        #  authors
        lines <- strsplit(abstract, "\n")[[1]]
        authors_line <- grep("\\w+\\d", lines)  
        

        author1 <- str_extract(lines[authors_line[1]], "^[^,]*")
        author2 <- str_extract(lines[authors_line[2]], "^[^,]*")
        
        #  Affiliation
        affi_lines <- grep("\\b(University|Center|Centre|Department|Institute)\\b", lines, ignore.case = TRUE)
        first_affi <- if (length(affi_lines) > 0) trimws(lines[affi_lines[1]]) else NA
        
        #  species names1
        sp1 <- unlist(regmatches(abstract, gregexpr("\\b[A-Z][a-z]{6,}\\s[a-z]{6,}\\b", abstract)))
        sp1 <- paste(sp1, collapse = ", ")
        
        #  Species name 2.0 using the elza database:
        all_sp <- unlist(regmatches(abstract, gregexpr("\\b[A-Z][a-z]+\\s[a-z]+(?:\\s[a-z]+)?\\b", abstract)))
        sp2 <- all_sp[all_sp %in% elza_sp]
        sp2 <- paste(sp2, collapse = ", ")
        
        res <- rbind(res, data.frame(Full_abstract = abstract, Title = title,
                                     First_author = author1, Second_author = author2,
                                     Affiliation = first_affi, 
                                     Species1 = sp1, Species2 = sp2,
                                     Conference = n, stringsAsFactors = FALSE))
      }
    }, error = function(e) {
      cat("Error:", n, "\n")
    })
  }
  cat("Done:", n, "\n")
}


  
  



   
