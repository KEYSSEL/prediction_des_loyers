library(readxl)
library(dplyr)
library(lubridate)

rm(list=ls())

#ce script r a pour but de créer notre base de données
#dedans nous allons filtrer les données de notre début de base faite sur l'observatoire
#des territoires sur les communes appartenant aux métropoles, puis nous allons créer la variable 
#indicatrice indiquant si la ville est la ville principale de la métropole
#nous allons ensuite ajouter la variable qui donne le temps de trajet en TGV depuis Paris

epcicom <- read_excel("donnees/epcicom2024.xlsx") #la base qui lie les epci aux communes
data <- read.csv2("donnees/odt.csv", skip = 2) #notre socle de base de données finale
duree_tgv <- read.csv("donnees/tgv.csv", sep = ",", fileEncoding = "Latin1", col.names = c("Ville", "temps_tgv_paris"))
med_revenu <- read.csv2("donnees/med_revenu.csv", skip = 2)
med_revenu$Médiane.du.niveau.de.vie.2021 <- as.numeric(med_revenu$Médiane.du.niveau.de.vie.2021)
data$Médiane.du.revenu.disponible.par.UC.2020 <- as.numeric(data$Médiane.du.revenu.disponible.par.UC.2020)

med_revenu_2021 <- med_revenu$Médiane.du.niveau.de.vie.2021

data$Médiane.du.revenu.disponible.par.UC.2020 <- med_revenu_2021
epcicom <- epcicom[,-c(1,2,5:9,11:14)]
df <- merge(data, epcicom, by.x = "Code", by.y = "insee") #on fusionne nos bases

df_metro <- df[df$nature_juridique %in% c("METRO","MET69"),] #on garde seulement les métropoles

df_metro$metropole <- as.factor(mapply(function(libelle, raison_sociale) { 
  ifelse(grepl(libelle, raison_sociale, ignore.case = TRUE), 1, 0)
}, df_metro$Libellé, df_metro$raison_sociale)) #on créé la variable qui nous dit si la ville est la ville principale de la metropole
#pour ce faire on  regarde si le nom de la ville est dans le nom de la métropole, si oui on assigne 1 sinon 0

df_metro[df_metro$Libellé == "Clermont-Ferrand",]$metropole <- 1 #on ajuste à la main pour les cas particuliers
df_metro[df_metro$Libellé == "Saint-Étienne",]$metropole <- 1
summary(df_metro$metropole)

df_metro$ville_principale <- with(df_metro, ifelse(metropole == 1, Libellé, NA)) 
#on crée une variable qui nous donne le nom de la variable principale

df_metro <- within(df_metro, {
  ville_principale <- ave(ville_principale, raison_sociale, FUN = function(x) {
    if (all(is.na(x))) x else rep(na.omit(x)[1], length(x)) 
  }) #pour ce faire on a associé à chaque ville d'une meme métropole le nom de la ville pour laquelle 
})   #la variable métropole est égale à 1

df_metro$si_paris <- as.factor(ifelse(df_metro$ville_principale == "Paris", 1, 0)) 

duree_tgv$temps_tgv_paris <- paste0(duree_tgv$temps_tgv_paris, ":00")
duree_tgv$temps_tgv_paris <- as.numeric(hms::as_hms(duree_tgv$temps_tgv_paris)) / 60


df_final <- merge(df_metro, duree_tgv, by.x = "ville_principale", by.y = "Ville")

df_final <- df_final %>% 
  rename(loyer_m2 = Loyer.d.annonce.par.m..charges.comprises.pour.un.appartement.type.du.parc.privé.locatif.2023,
         logements_vacants = Part.des.logements.vacants.2021,
         med_revenu = Médiane.du.revenu.disponible.par.UC.2020,
         jeunes = Part.des.18.24.ans.2021,
         densite_pop = Densité.de.population.2021,
         nouveaux_menages = Part.des.ménages.ayant.emménagé.depuis.moins.de.2.ans.2021,
         beneficiaires_rsa = Part.d.allocataires.du.RSA..communes..2022,
         )

df_final <- df_final[,-c(11, 12)] 
