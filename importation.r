library(readr)
library(data.table)
library(dplyr)
library(stringr)

#------------------------------------------------#
#----------------Import des bases----------------#
#------------------------------------------------#

#On lit les bases de données 

abs_employes <- read_csv("DONNEES/employeeAbsenteeism.csv")
votes <- read_csv("DONNEES/votes.csv")
interactions <- read_csv("DONNEES/commentInteractions.csv")
derniere_participation <- read_csv("DONNEES/lastParticipationExists.csv")
info_commentaire <- read_csv("DONNEES/comments_by_employees_in_anonymous_forum.csv")

#-----------------------------------------#
#----------------Questions----------------#
#-----------------------------------------#

#Part du travail lié à l'absentéisme :

setDT(abs_employes)
setDT(votes)
setDT(raison_abs)
setDT(derniere_participation)
setDT(info_commentaire)

raison_abs <- abs_employes[, .N, reason]
raison_abs$pourcentage <- (raison_abs$N*100) / sum(raison_abs$N)
raison_abs[reason == "Common sickness  or accident not related to the job"]$reason = "Maladie"
raison_abs[reason == "Workplace accident"]$reason = "Accident du travail"
raison_abs[reason == "Long term sick leave"]$reason = "Longue maladie"
raison_abs[reason == "Non job related sickness"]$reason = "Autres"

#Duree moyenne d'absentéisme d'un salarié :

#On traite le format des dates en l'adaptant à un format exploitable (bien galère)

abs_employes$from = str_sub(abs_employes$from, 1,7)
abs_employes$to = str_sub(abs_employes$to, 1,8)
abs_employes$from = str_replace_all(abs_employes$from, "/", "-")
abs_employes$to = str_replace_all(abs_employes$to, "/", "-")
abs_employes[employee %in% c("338", "17r", "yKX", "g6K", "ONv"),]$to = str_sub(abs_employes[employee %in% c("338", "17r", "yKX", "g6K", "ONv")]$to, 1, 7)
abs_employes$from <- mdy(abs_employes$from)
abs_employes$to <- mdy(abs_employes$to)
abs_employes$from <- as.character(abs_employes$from)
abs_employes$to <- as.character(abs_employes$to)
abs_employes[str_sub(abs_employes$from,1,4) == "2001",]$from = str_replace_all(abs_employes[str_sub(abs_employes$from,1,4) == "2001",]$from,"2001","2018")
abs_employes[str_sub(abs_employes$to,1,4) == "2001",]$to = str_replace_all(abs_employes[str_sub(abs_employes$to,1,4) == "2001",]$to,"2001","2018")

#On calcule la différence en jour entre les dates de début et de fin d'arrêt, puis on calcule la moyenne

abs_employes$duree_abs <- difftime(as.Date(abs_employes$to), as.Date(abs_employes$from), units = "days")
abs_employes$duree_abs <- str_sub(abs_employes$duree_abs,1,3)

duree_moyenne_abs <- mean(as.integer(abs_employes$duree_abs))
graph_duree_moyenne_abs <- abs_employes[,.(nombre = .N),duree_abs]
graph_duree_moyenne_abs$duree_abs <- as.integer(graph_duree_moyenne_abs$duree_abs)
graph_duree_moyenne_abs <- graph_duree_moyenne_abs[order(duree_abs),]
            
#Quelle est la raison d'absentéisme la plus fréquente ?

#Quelle période est la plus sujette à l'absentéisme ?

abs_employes$mois_debut_arret = ""
abs_employes[str_sub(abs_employes$from,6,7) == "06",]$mois_debut_arret = "juin"
abs_employes[str_sub(abs_employes$from,6,7) == "07",]$mois_debut_arret = "juillet"
abs_employes[str_sub(abs_employes$from,6,7) == "08",]$mois_debut_arret = "aout"
abs_employes[str_sub(abs_employes$from,6,7) == "09",]$mois_debut_arret = "septembre"
abs_employes[str_sub(abs_employes$from,6,7) == "10",]$mois_debut_arret = "octobre"

periode_abs <- abs_employes[,.(nombre = .N),mois_debut_arret]                           

#Corrélation entre leurs réponses à la question "are you happy" et leur absentéisme ?

moyenne_vote_employe <- votes[,.( moyenne_vote = mean(vote)),employee]
abs_employes <- merge(abs_employes, moyenne_vote_employe[,.(employee, moyenne_vote)], by = "employee", all.x = TRUE)

#On calcule la moyenne des notes des employés en général puis la moyenne des notes des employés ayant déclaré un arrêt

moyenne_vote_employe_general <- mean(as.numeric(moyenne_vote_employe$moyenne_vote))
moyenne_vote_employe_abs <- mean(abs_employes$moyenne_vote, na.rm = T)

nombre_vote_employe_general <- votes[,.(nombre = .N),vote]
nombre_vote_employe_general$pourcentage = (nombre_vote_employe_general$nombre*100) / sum(nombre_vote_employe_general$nombre)

nombre_vote_employe_abs <- merge(abs_employes, votes[,.(employee, vote)], by = "employee", all.x = TRUE)
nombre_vote_employe_abs <- nombre_vote_employe_abs[!is.na(vote),.(nombre = .N),vote]
nombre_vote_employe_abs$pourcentage = (nombre_vote_employe_abs$nombre*100) / sum(nombre_vote_employe_abs$nombre)

#Corrélation entre la dernière participation et la réponse à “are you happy” → les gens heureux participent sûrement moins que les autres ? 

#Corrélation entre la longueur des commentaires et la réponse à “are you happy ?” 

moyenne_longueur_comm_employe <- info_commentaire[,.(moyenne_longueur = mean(commentLength)),employee]
moyenne_longueur_comm_employe <- merge(moyenne_longueur_comm_employe, moyenne_vote_employe[,.(employee, moyenne_vote)], by = "employee", all.x = TRUE)

# Corrélation entre like / dislikes / reply et la réponse à “are you happy ?” → personne pas contente a plus tendance à réagir aux posts des autres ?

moyenne_interaction_employe <- info_commentaire[,.(moyenne_like = mean(likes), moyenne_dislike = mean(dislikes)),employee]
moyenne_interaction_employe <- merge(moyenne_interaction_employe, moyenne_vote_employe[,.(employee, moyenne_vote)], by = "employee", all.x = TRUE)

# Quels types de feedback ?

type_feedback <- info_commentaire[,.(nombre = .N), feedbackType]

save(abs_employes, derniere_participation, info_commentaire, moyenne_interaction_employe, moyenne_longueur_comm_employe,
     periode_abs, raison_abs, votes, graph_duree_moyenne_abs, nombre_vote_employe_abs, nombre_vote_employe_general, type_feedback,
     file = "donnees.Rdata")
