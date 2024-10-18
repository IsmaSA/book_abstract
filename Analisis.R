
# Analyses Abstract Neobiota

setwd("C:/Users/Propietario/Desktop/book_abstracts")

neobiota <- read_xlsx("Neobiota.xlsx", sheet = "Sheet1")

neobiota <- neobiota[,c(1,2,3,6,8,9,10)]


neobiota <- neobiota %>% 
  mutate(Species = sapply(Species_elza, function(x) {
    if (is.na(x)) {
      return(NA)
    } else {
      paste(unique(strsplit(x, ",\\s*")[[1]]), collapse = ", ")
    }
  }))

table(neobiota$Affiliation)
length(unique(neobiota$Affiliation))

# Countries -----
countries <- neobiota %>% group_by(Affiliation) %>% summarise(n=n()) %>% arrange(-n)


# Total number of species ----
capitalize_first_word <- function(x) {
  words <- strsplit(x, " ")[[1]]  # Split the string into words
  words[1] <- paste0(toupper(substr(words[1], 1, 1)), tolower(substr(words[1], 2, nchar(words[1]))))  # Capitalize the first word
  words[-1] <- tolower(words[-1])  
  paste(words, collapse = " ")  
}

sp <- neobiota %>%
  filter(!is.na(Species)) %>%   mutate(Species= tolower(Species) ) %>%
  separate_rows(Species, sep = ",\\s*") %>% 
  distinct(Species) %>%     
  arrange(Species)             

sp <- sp %>% filter(Species != "" & !is.na(Species))
sp$Species <- sapply(sp$Species, capitalize_first_word)

nsp <- nrow(sp) - 6


### Get the groups ------
elza <- read_xlsx("C:/Users/Propietario/Desktop/Native range/Final_data/Native_Range.v1.1.xlsx", sheet = "Elza_sp")
elza2<- read_xlsx("C:/Users/Propietario/Desktop/ELZA/GLOBAL NNS DATA FINAL.xlsx")

sp2<- sp %>% left_join(elza[,c(1,4)], by =c("Species"="Taxon") )

sp2_na <- sp2 %>% filter(is.na(Group))

sp2 <- sp2 %>%
  mutate(Group = case_when(
    Species == "Acacia cyanophylla" ~ "Vascular plants",
    Species == "Acacia sp." ~ "Vascular plants",
    Species == "Acacia visco" ~ "Vascular plants",
    Species == "Acer ginnala" ~ "Vascular plants",
    Species == "Ambrosia artemisifolia" ~ "Vascular plants",
    Species == "Astacus leptodactylus" ~ "Crustaceans",
    Species == "Anodonta woodiana" ~ "Molluscs",
    Species == "Gambusia spp." ~ "Fishes",
    Species == "Fallopia japonica" ~ "Vascular plants",
    Species == "Orconectes limosus" ~ "Crustaceans",
    Species == "Helianthus annuus subsp. annuus" ~ "Vascular plants",
    Species == "Dreissena rostriformis bugensis" ~ "Molluscs",
    Species == "Haliaeetus albicilla" ~ "Birds",
    Species == "Acroptilon repens" ~ "Vascular plants",
    Species == "Adenocaulon adhaerescens" ~ "Vascular plants",
    Species == "Amaranthus sp." ~ "Vascular plants",
    Species == "Anisantha diandra" ~ "Vascular plants",
    Species == "Anisantha tectorum" ~ "Vascular plants",
    Species == "Aphalara itadori" ~ "Insects",
    Species == "Aronia prunifolia" ~ "Vascular plants",
    Species == "Berberis asiatica" ~ "Vascular plants",
    Species == "Bidens frondosus" ~ "Vascular plants",
    Species == "Boletellus projectellus" ~ "Fungi",
    Species == "Brachytheciastrum velutinum" ~ "Bryophytes",
    Species == "Bucephalus polymorphus" ~ "Invertebrates (excl. Arthropods, Molluscs)",
    Species == "Bursaphelenchus xylophilus" ~ "Nematodes",
    Species == "Canis familiaris" ~ "Mammals",
    Species == "Cardaria draba" ~ "Vascular plants",
    Species == "Carpobrotus ecotypes" ~ "Vascular plants",
    Species == "Centaurea x moncktonii" ~ "Vascular plants",
    Species == "Chamomilla suaveolens" ~ "Vascular plants",
    Species == "Chrysanthemoides monilifera" ~ "Vascular plants",
    Species == "Consolida orientalis" ~ "Vascular plants",
    Species == "Coronopus squamatus" ~ "Vascular plants",
    Species == "Corophium curvispinum" ~ "Crustaceans",
    Species == "Crambe abyssinica" ~ "Vascular plants",
    Species == "Crassostrea gigas" ~ "Molluscs",
    Species == "Diabrotica vergifera" ~ "Insects",
    Species == "Echinostoma revolutum" ~ "Invertebrates (excl. Arthropods, Molluscs)",
    Species == "Echinuria uncinata" ~ "Invertebrates (excl. Arthropods, Molluscs)",
    Species == "Eichornia crassipes" ~ "Vascular plants",
    Species == "Epilobium adenocaulon" ~ "Vascular plants",
    Species == "Eragrostis albensis" ~ "Vascular plants",
    Species == "Erechtites hieraciifolius" ~ "Vascular plants",
    Species == "Fallopia sp" ~ "Vascular plants",
    Species == "Fallopia x bohemica" ~ "Vascular plants",
    Species == "Galeobdolon argentatum" ~ "Vascular plants",
    Species == "Galinsoga ciliata" ~ "Vascular plants",
    Species == "Gammarus roeseli" ~ "Crustaceans",
    Species == "Garrulus glandarius" ~ "Birds",
    Species == "Glossogobius callidus" ~ "Fishes",
    Species == "Gonyostomum semen" ~ "Algae",
    Species == "Guignardia aesculi" ~ "Fungi",
    Species == "Gymnocephalus cernuus" ~ "Fishes",
    Species == "Halophytophthora avicenniae" ~ "Oomycetes",
    Species == "Haramonia axyridis" ~ "Insects",
    Species == "Hedysarum coronarium" ~ "Vascular plants",
    Species == "Hieracium aurantiacum" ~ "Vascular plants",
    Species == "Hieracium pilosella" ~ "Vascular plants",
    Species == "Hystrichis tricolor" ~ "Nematodes",
    Species == "Kochia scoparia" ~ "Vascular plants",
    Species == "Lamium argentatum" ~ "Vascular plants",
    Species == "Lemna punctata" ~ "Vascular plants",
    Species == "Lenna minuta" ~ "Vascular plants",
    Species == "Linaria loeselii" ~ "Vascular plants",
    Species == "Lonicera henryi" ~ "Vascular plants",
    Species == "Lonsdalea quercina" ~ "Bacteria",
    Species == "Ludwigia sp." ~ "Vascular plants",
    Species == "Lupinos polyphyllus" ~ "Vascular plants",
    Species == "Lupinus polyphyllos" ~ "Vascular plants",
    Species == "Lycopersicon esculentum" ~ "Vascular plants",
    Species == "Meles meles" ~ "Mammals",
    Species == "Melilotus alba" ~ "Vascular plants",
    Species == "Merizodus soledadinus" ~ "Insects",
    Species == "Mustela furo" ~ "Mammals",
    Species == "Mycosphaerella dearnessi" ~ "Fungi",
    Species == "Neogobius gymnotrachelus" ~ "Fishes",
    Species == "Neovison vison" ~ "Mammals",
    Species == "Ophiostoma novo ulmi" ~ "Fungi",
    Species == "Opuntia maxima" ~ "Vascular plants",
    Species == "Oxalis fontana" ~ "Vascular plants",
    Species == "Oxybaphus nyctagineus" ~ "Vascular plants",
    Species == "Pacifastacus leniusulus" ~ "Crustaceans",
    Species == "Pantomorus cervinus" ~ "Insects",
    Species == "Parkinsonia aculeate" ~ "Vascular plants",
    Species == "Parthenocissus inserta" ~ "Vascular plants",
    Species == "Petasites fragrans" ~ "Vascular plants",
    Species == "Phytophthora gemini" ~ "Oomycetes",
    Species == "Polygonum perfoliatum" ~ "Vascular plants",
    Species == "Polymorphus minutus" ~ "Invertebrates (excl. Arthropods, Molluscs)",
    Species == "Pomacea insularum" ~ "Molluscs",
    Species == "Pontederia crassipes" ~ "Vascular plants",
    Species == "Populus euramericana" ~ "Vascular plants",
    Species == "Populus x canadensis" ~ "Vascular plants",
    Species == "Potamonautes sidneyi" ~ "Crustaceans",
    Species == "Prorocentrum minimum" ~ "Dinoflagellata",
    Species == "Prosopis sp." ~ "Vascular plants",
    Species == "Proterorhinus nasalis" ~ "Fishes",
    Species == "Reynoutria x bohemica" ~ "Vascular plants",
    Species == "Robinia pseudacacia" ~ "Vascular plants",
    Species == "Sabellaria alveolata" ~ "Polychaetes",
    Species == "Schedonorus pratensis" ~ "Vascular plants",
    Species == "Setaria pumila" ~ "Vascular plants",
    Species == "Solidago sp" ~ "Vascular plants",
    Species == "Sophora japonica" ~ "Vascular plants",
    Species == "Spartina maritima" ~ "Vascular plants",
    Species == "Symphyotrichum squamatum" ~ "Vascular plants",
    Species == "Tagetes patula" ~ "Vascular plants",
    Species == "Taraxacum lacerum" ~ "Vascular plants",
    Species == "Urochloa arrecta" ~ "Vascular plants",
    Species == "Urochloa decumbens" ~ "Vascular plants",
    Species == "Vanellus vanellus" ~ "Birds",
    Species == "Vespa crabro" ~ "Insects",
    Species == "Vulpes sp." ~ "Mammals",
    Species == "chelicorophium curvispinum" ~ "Crustaceans",
    Species == "dikerogammarus villosus" ~ "Crustaceans",
    Species == "fallopia sachalinensis" ~ "Vascular plants",
    Species == "heracleum mantegazzianum" ~ "Vascular plants",
    Species == "impatiens glandulifera" ~ "Vascular plants",
    Species == "impatiens parviflora" ~ "Vascular plants",    
    Species == "Margionys village" ~ "Birds",
    Species == "Neogobius\r\ngymnotrachelus" ~ "Fishes",
    Species == "Solanum\r\nelaeagnifolium" ~ "Vascular plants",
    Species == "Solenopsis" ~ "Insects",
    Species == "Solidago graminifolia" ~ "Vascular plants",
    Species == "Solidago x niederederi" ~ "Vascular plants",
    Species == "Spartina densiflora x martima" ~ "Vascular plants",
    Species == "Spartina versicolor" ~ "Vascular plants",
    Species == "Svizzera italiana" ~ "Molluscs",  
    TRUE ~ Group  
  ))

groups <- sp2 %>% group_by(Group) %>% summarise(n=n()) %>% arrange(-n) 


## Now the same but with habitat:  ----------

sp3<- sp2 %>% left_join(elza2[,c(6,13)], by =c("Species"="New_names") )
sp3<- sp3[!duplicated(sp3$Species), ]

sp3 <- sp3 %>%
  mutate(Habitat = case_when(
    # Freshwater species
    Species %in% c("Anodonta woodiana", "Corophium curvispinum", "Crassostrea gigas", "Bucephalus polymorphus",
                   "Dreissena rostriformis bugensis", "Gambusia spp.", "Glossogobius callidus", 
                   "Gymnocephalus cernuus", "Neogobius gymnotrachelus", "Astacus leptodactylus",
                   "Proterorhinus nasalis", "Potamonautes sidneyi",
                  "Neogobius gymnotrachelus", "Lemna minuta", "Trichocorixa verticalis verticalis",
                   "Gammarus roeseli", "Neogobius gymnotrachelus", "Orconectes limosus", 
                   "Pacifastacus leniusulus", "Elodea canadensis", "Lemna punctata", "Gonyostomum semen",
                   "Lenna minuta", "Pontederia crassipes") ~ "FRESHWATER",
    
    # Marine species
    Species %in% c("Spartina densiflora x martima", "Spartina maritima", "Sabellaria alveolata", "Spartina versicolor",
                   "Phytophthora gemini", "Pomacea insularum", "Halophytophthora avicenniae","Svizzera italiana","Polymorphus minutus", "Prorocentrum minimum") ~ "MARINE",
    
    # Terrestrial species
    Species %in% c("Acacia cyanophylla", "Acacia sp.", "Acacia visco", "Acer ginnala", 
                   "Ambrosia artemisifolia", "Anisantha diandra", "Anisantha tectorum", "Vanellus vanellus",
                   "Aphalara itadori", "Aronia prunifolia", "Astacus leptodactylus", 
                   "Berberis asiatica", "Bidens frondosus", "Boletellus projectellus", 
                   "Cardaria draba", "Centaurea x moncktonii", "Chamomilla suaveolens", 
                   "Chrysanthemoides monilifera", "Consolida orientalis", "Coronopus squamatus", 
                   "Fallopia japonica", "Fallopia sp", "Fallopia x bohemica", "Felis silvestris catus", 
                   "Garrulus glandarius", "Haliaeetus albicilla", "Helianthus annuus subsp. annuus", 
                   "Hieracium aurantiacum", "Hieracium pilosella", "Kochia scoparia", "Lamium argentatum", 
                   "Lonicera henryi", "Melilotus alba", "Meles meles", "Mustela furo", "Neovison vison", 
                   "Phalacrocorax carbo sinensis", "Polygonum perfoliatum", "Robinia pseudacacia", 
                   "Sophora japonica", "Symphyotrichum squamatum", "Taraxacum lacerum", 
                   "Trachemys scripta troostii", "Vanellus vanellus", "Vespa crabro", "Vulpes sp.",
                   "Acroptilon repens", "Adenocaulon adhaerescens", "Amaranthus sp.", 
                   "Brachytheciastrum velutinum", "Canis familiaris", "Carpobrotus ecotypes", 
                   "Crambe abyssinica", "Diabrotica vergifera", "Diabrotica virgifera virgifera", 
                   "Echinostoma revolutum", "Echinuria uncinata", "Eichornia crassipes", 
                   "Epilobium adenocaulon", "Eragrostis albensis", "Erechtites hieraciifolius", 
                   "Galeobdolon argentatum", "Galinsoga ciliata", "Guignardia aesculi", 
                   "Haramonia axyridis", "Hedysarum coronarium", "Hystrichis tricolor", 
                   "Linaria loeselii", "Lonsdalea quercina", "Ludwigia sp.", "Lupinos polyphyllus", 
                   "Lupinus polyphyllos", "Lycopersicon esculentum", "Margionys village", 
                   "Merizodus soledadinus", "Mycosphaerella dearnessi", "Ophiostoma novo ulmi", 
                   "Opuntia maxima", "Oxalis fontana", "Pantomorus cervinus", 
                   "Parkinsonia aculeate", "Parthenocissus inserta", "Petasites fragrans", 
                   "Populus euramericana", "Populus x canadensis", "Prosopis sp.", 
                   "Reynoutria x bohemica", "Schedonorus pratensis", "Setaria pumila", 
                   "Solanum elaeagnifolium", "Solenopsis", "Solidago graminifolia", "Solanum elaeagnifolium",
                   "Solidago sp", "Solidago x niederederi", "Tagetes patula", 
                   "Urochloa arrecta", "Urochloa decumbens", "Vespa velutina nigrithorax") ~ "TERRESTRIAL",
    
    # Both freshwater and terrestrial species
    Species %in% c( "Trachemys scripta troostii") ~ "FRESHWATER|TERRESTRIAL)",
    
    TRUE ~ Habitat  
  ))
sp3_na <- sp3 %>% filter(is.na(Habitat))

sp3 <- sp3[!duplicated(sp3$Species), ]
# make this shit with the fix because not works
sp3$Habitat[sp3$Species=="Solanum elaeagnifolium"] <- "TERRESTRIAL"
sp3$Habitat[sp3$Species=="Neogobius gymnotrachelus"] <- "FRESHWATER"

unique(sp3$Habitat)

sp3$Habitat[sp3$Habitat=="TERRESTRIAL|HOST"] <- "TERRESTRIAL"
sp3$Habitat[sp3$Habitat=="FRESHWATER|MARINE|MARINE"] <- "FRESHWATER|MARINE"
sp3$Habitat[sp3$Habitat=="FRESHWATER|MARINE|HOST"] <- "FRESHWATER|MARINE"
sp3$Habitat[sp3$Habitat=="FRESHWATER|HOST"] <- "FRESHWATER"
sp3$Habitat[sp3$Habitat=="MARINE|HOST"] <- "MARINE"
sp3$Habitat[sp3$Habitat=="TERRESTRIAL|HOST"] <- "TERRESTRIAL"
sp3$Habitat[sp3$Habitat=="TERRESTRIAL|FRESHWATER|MARINE|MARINE"] <- "TERRESTRIAL|FRESHWATER|MARINE"
sp3$Habitat[sp3$Habitat=="TERRESTRIAL|FRESHWATER|MARINE|HOST"] <- "TERRESTRIAL|FRESHWATER|MARINE"
sp3$Habitat[sp3$Habitat=="MARINE|MARINE"] <- "MARINE"
sp3$Habitat[sp3$Habitat=="TERRESTRIAL|FRESHWATTER|MARINE"] <- "TERRESTRIAL|FRESHWATER|MARINE"

table(sp3$Habitat)


