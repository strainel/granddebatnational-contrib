
setwd("C:/Users/stephane.trainel/Documents/Developpements/granddebat-contrib/")

# Contruction du corpus de mots
require(stopwords)
require(tidytext)
require(tidyverse)
require(corrplot)
require(qdapTools)

myTM <- function (d, v, sparsetx = 0.98, maxword = 40, qtx= .8) {
  corpus <- d %>%
    mutate(txt = stringr::str_replace_all(v, "’", " ")) %>% 
    mutate(txt = stringr::str_replace_all(txt, "'", " ")) %>% 
    mutate(txt = stringr::str_replace_all(txt, '"', " ")) %>% 
    mutate(txt = stringr::str_replace_all(txt, '“', " ")) %>%
    mutate(txt = stringr::str_replace_all(txt, '´', " ")) %>%
    mutate(txt = stringr::str_replace_all(txt, '«', " ")) %>%
    mutate(txt = stringr::str_replace_all(txt, '»', " ")) %>%
    .$txt %>% 
    VectorSource()%>%
    VCorpus()
  corpus <- corpus %>%
    tm_map(content_transformer(tolower))%>%
    tm_map(stripWhitespace) %>%
    tm_map(removeNumbers)%>%
    tm_map(removePunctuation)%>%
    tm_map(removeWords, c("ainsi","afin","alors","ans","avoir","aussi","autre","autres","car","ceux","chaque","comme",
                          "déjà","donc","dont","etc","doivent","doit","entre","etre","être",
                          "faut","faudrait","fait","falloir","faire","fais",
                          "grand","grande","grands","huit",
                          "mettre","mode","manière","notamment","nombre","niveau",
                          "prendre","place","peut","points","quand",
                          "surtous","vers")) %>%
    tm_map(removeWords, stopwords("french")) %>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "ecologie", replacement = "écologie")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "écologique", replacement = "écologie")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "ecologique", replacement = "écologie")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "écologiques", replacement = "écologie")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "politiques", replacement = "politique")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "canicules", replacement = "canicule")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "kerosene", replacement = "kérosène")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "kerozene", replacement = "kérosène")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "croisières", replacement = "croisière")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "énergétique", replacement = "énergie")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "énergies", replacement = "énergie")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "energies", replacement = "énergie")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "energie", replacement = "énergie")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "développement", replacement = "développer")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "transports", replacement = "transport")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "toutes", replacement = "tous")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "aides", replacement = "aide")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "tva", replacement = "taxe")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "taxer", replacement = "taxe")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "taxes", replacement = "taxe")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "produits", replacement = "produit")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "production", replacement = "produire")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "toutes", replacement = "tous")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "toute", replacement = "tous")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "tout", replacement = "tous")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "solutions", replacement = "solution")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "énergies", replacement = "énergie")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "villes", replacement = "ville")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "modes", replacement = "mode")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "véhicules", replacement = "véhicule")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "voitures", replacement = "véhicule")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "voiture", replacement = "véhicule")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "trains", replacement = "train")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "vélos", replacement = "vélo")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "polluantes", replacement = "pollution")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "polluants", replacement = "pollution")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "pollutions", replacement = "pollution")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "polluent", replacement = "pollueurs")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "électriques", replacement = "électrique")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "gens", replacement = "particuliers")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "personnes", replacement = "particuliers")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "industriels", replacement = "industrie")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "industries", replacement = "industrie")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "aujourd", replacement = "aujourd'hui")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "trie", replacement = "tri")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "utiliser", replacement = "utilisation")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "utilise", replacement = "utilisation")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "locaux", replacement = "local")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "achète", replacement = "acheter")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "achats", replacement = "acheter")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "achat", replacement = "acheter")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "aides", replacement = "aide")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "collectivités", replacement = "collectivité")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "communess", replacement = "commune")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "communes", replacement = "commune")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "communautés", replacement = "communauté")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "entreprises", replacement = "entreprise")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "etat", replacement = "état")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "mairie", replacement = "commune")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "mairies", replacement = "commune")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "départements", replacement = "département")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "régions", replacement = "région")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "comportements", replacement = "comportement")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "solaires", replacement = "solaire")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "impôts", replacement = "impôt")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "importants", replacement = "important")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "climatiques", replacement = "climat")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "climatique", replacement = "climat")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "liés", replacement = "lié")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "européenne", replacement = "européen")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "européens", replacement = "européen")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "financières", replacement = "financière")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "dérèglements", replacement = "dérèglement")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "problemes", replacement = "problème")))%>%
    tm_map(content_transformer(function(x) gsub(x, pattern = "problèmes", replacement = "problème")))
  tdm <- corpus %>%
    DocumentTermMatrix() %>%
    removeSparseTerms(sparsetx) %>%
    as.matrix()
  tdm[tdm>1] <- 1
  tm <- t(tdm) %*% tdm  
  
  maxw <- min(maxword,nrow(tm))
  rang <- which(diag(tm)<sort(diag(tm), decreasing=TRUE)[maxw])
  if (length(rang) >0) tm <- tm[-rang,-rang]
  diag <- diag(tm)
  diag(tm) <- 0
  tm[tm<quantile(tm, qtx)] <- 0
  diag(tm) <- diag
  
  rang <- which(colSums(tm)-diag(tm)==0)
  if (length(rang) >0) tm <- tm[-rang,-rang]
  tm
}

#tmq_test <- myTM(data[1:1000,], data[1:1000,]$Q1)

tmq1 <- myTM(data, data$Q1, sparsetx = 0.997)
tmq2 <- myTM(data, data$Q2)
tmq4 <- myTM(data, data$Q4, sparsetx = 0.996)
tmq6 <- myTM(data, data$Q6)
tmq7 <- myTM(data, data$Q7)
tmq8 <- myTM(data, data$Q8, sparsetx = 0.996)
tmq10 <- myTM(data, data$Q10, sparsetx = 0.996)
tmq12 <- myTM(data, data$Q12, sparsetx = 0.996)
tmq14 <- myTM(data, data$Q14, sparsetx = 0.996)
tmq15 <- myTM(data, data$Q15)
tmq16 <- myTM(data, data$Q16)


save(tmq1,tmq2,tmq4,tmq6,tmq7,tmq8,tmq10,tmq12,tmq14,tmq15,tmq16, file="data_tmq.Rdata")
# load(file = "data2.Rdata")


