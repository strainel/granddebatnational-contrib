
setwd("C:/Users/stephane.trainel/Documents/Developpements/granddebat-contrib/")

list_module <- c("rjson","httr", "jsonlite", "dplyr", "data.table","tm")
options(warn = -1)
require(rjson, quietly = T)
require(httr, quietly = T)
require(jsonlite, quietly = T)
require(dplyr, quietly = T)
require(data.table, quietly = T) 
require(tm, quietly = T)
require(arules)

# Lecture du fichier des contributions
file <- "data/LA_TRANSITION_ECOLOGIQUE.csv"
pre <- fread(file, sep = ",", header= TRUE, encoding = 'UTF-8', colClasses = 'character')

# Lecture de statistiques
file <- "data/stats_dep.csv"
stats <- fread(file, sep = ";", header= TRUE, encoding = 'UTF-8')

# Renommage des colonnes
vars <- c("id","reference","title","createdAt","publishedAt","updatedAt","trashed","trashedStatus","authorId","authorType","authorZipCode")
vars2_value <- setdiff(names(pre),vars)
vars2 <- paste('Q', 1:16, sep='')
names(pre) <- c(vars, vars2)

# Suppression des réponses en doublon
rang <- which(duplicated(data))
pre <- pre[-rang,]

# Mise au format date
pre$createdAt <- as.POSIXct(pre$createdAt, format="%Y-%m-%d %H:%M:%S")
pre$publishedAt <- as.POSIXct(pre$publishedAt, format="%Y-%m-%d %H:%M:%S")
pre$updatedAt <- as.POSIXct(pre$updatedAt, format="%Y-%m-%d %H:%M:%S")

pre$authorZipCode[nchar(pre$authorZipCode)!= 5] <- NA
pre$authorZipCode[pre$authorZipCode <= 0] <- NA
pre$authorZipCode[pre$authorZipCode == "00000"] <- NA
pre$authorZipCode[pre$authorZipCode <= "01000"] <- NA
pre$authorZipCode[pre$authorZipCode == "99999"] <- NA

pre$DEP <- substr(pre$authorZipCode, 0, 2)

pre <- pre %>%
  mutate(Q1F = case_when(Q1 == "La pollution de l'air"~ Q1,
                         Q1 == "La biodiversité et la disparition de certaines espèces" ~ Q1,
                         Q1 == "L'érosion du littoral" ~ Q1,
                         Q1 == "Les dérèglements climatiques (crue, sécheresse)" ~ Q1,
                         TRUE ~ "Autre")) %>%
  mutate(Q1 = case_when(Q1F == "La pollution de l'air"~ "",
                        Q1F == "La biodiversité et la disparition de certaines espèces" ~ "",
                        Q1F == "L'érosion du littoral" ~ "",
                        Q1F == "Les dérèglements climatiques (crue, sécheresse)" ~ "",
                         TRUE ~ Q1)) %>%
  mutate(Q1F = case_when(nchar(Q1)==0 & Q1F =="Autre" ~ "", TRUE ~ Q1F)) %>%
  mutate(Q13_TC = case_when(grepl("Les transports en commun", Q13) ~ "Oui",
                            TRUE ~ "Non")) %>%
  mutate(Q13_CO = case_when(grepl("Le covoiturage", Q13) ~ "Oui",
                            TRUE ~ "Non")) %>%
  mutate(Q13_AU = case_when(grepl("L'auto partage", Q13) ~ "Oui",
                            TRUE ~ "Non")) %>%
  mutate(Q13_OD = case_when(grepl("Le transport à la demande", Q13) ~ "Oui",
                            TRUE ~ "Non")) %>%
  mutate(Q13_VE = case_when(grepl("Le vélo", Q13) ~ "Oui",
                            TRUE ~ "Non")) %>%
  mutate(Q13_OT = case_when(Q13 == "" ~ "Non", 
                            !grepl("Le vélo|Le transport à la demande|L'auto partage|Le covoiturage|Les transports en commun", Q13) ~ "Oui",
                            TRUE ~ "Non")) 



data <- pre

save(vars2_value, data, stats, file="data.Rdata")
source("create_tmq.R", encoding = "UTF-8")

