# Projet shiny _ MPG
library(shiny)
library(readxl)
library(dplyr)
require(data.table)
library(tidyverse)

# pour optimisation lineaire 
library(dplyr)
library(ROI)
library(ROI.plugin.glpk)
library(ompr)
library(ompr.roi)

library(DT)       # tableau stylé dans factoshiny
library(shinyjs)  # Pour cacher le sidebarPanel


setwd("~/Projets_M2/Projet_MPG")



###################################################
############### Importation jdd ###################
###################################################

sheet_names <- excel_sheets("Stats-MPG-saison7MPG.xlsx")
note_mpg <- NULL
for (i in 2:length(sheet_names)){
  name <- sheet_names[i] # récupère le nom de la feuille
  name2 <- paste0("sheet_",name) #nouvelle variable name
  data <- read_excel("Stats-MPG-saison7MPG.xlsx", sheet = i) # lecture et stockage
  data <- data[7:(nrow(data)-1),]
  colnames(data) <- NULL
  data$club <- name # ajout d'une variable avec le nom original de la feuille
  note_mpg <- rbind(note_mpg,data)
}
note_mpg = as.data.frame(note_mpg)

# Renommer les colonnes
vec=NULL
for (k in 1:26){
  vec[k] = paste("J-",k)
}
colnames(note_mpg) = c("Poste", "Cote", "Joueur", "Titu.", "Entrees", "Buts", "Moyenne_note", vec, "Club")


## Convert variables
note_mpg$Poste = as.factor(note_mpg$Poste)
note_mpg$Cote = as.numeric(note_mpg$Cote)
note_mpg$Titu. = as.numeric(note_mpg$Titu.)
note_mpg$Buts = as.numeric(note_mpg$Buts)
note_mpg$Entrees = as.numeric(note_mpg$Entrees)
note_mpg$Moyenne_note = as.numeric(note_mpg$Moyenne_note)
note_mpg$Club = as.factor(note_mpg$Club)

# Convert variables starting with "J-"
colJ_num = grepl("J-", names(note_mpg))
note_mpg[ , colJ_num] =  apply( note_mpg[ ,colJ_num], MARGIN = 2 , FUN = as.numeric)

### on enlève les joueurs qui ont des NA en cote (c'est suite a un transfert) ###
note_mpg<-note_mpg[complete.cases(note_mpg[,2]),]

#######################################
######### mutliplicateur de perf ######
#######################################

#### On cherche a apporter plus d'importance aux performances recente 
jour = seq(1,26)
beta =log((jour+1)) # beta est notre conficient multiplicateur de performance par journee
plot_beta = plot(jour,beta, main = "Coefficient beta en fonction de la journée de ligue 1")

note = note_mpg[ , colJ_num]                     #tableau avec les note par journee 
note_beta = data.frame(mapply(`*`,note,beta))    #tableau avec les note * beta par journee

# on applique mtn la fonction sum par ligne pour avoir la perf total du joueur : 
performance_beta = apply(note_beta ,MARGIN = 1, FUN = sum, na.rm = TRUE)
performance = apply(note ,MARGIN = 1, FUN = sum, na.rm = TRUE)

# on concatene nos vecteurs performance, performance beta par joueurs
perf = cbind(note_mpg$Joueur, performance, performance_beta)
perf= as.data.frame(perf)
perf$V1 = as.factor(perf$V1)
perf$performance = as.numeric(perf$performance)
perf$performance_beta = as.numeric(perf$performance_beta)

# Puis on merge sur le data frame
note_mpg = merge(note_mpg, perf, by.x = "Joueur", by.y = "V1" )

perf = perf[order(-perf$performance_beta),]

###########################################
######### Multiplicateur de cote ##########
###########################################

## on souhaite que l'enchère sur les joueurs les plus cotés soit plus important que les autres ###

### on sélectionne la plage des x ###
x_cote=seq(1,max(note_mpg$Cote))

## choix de la pondération à partir des cotes joueurs ###
alpha=exp(x_cote/20)/10 +1.3
plot_alpha = plot(x_cote,alpha, main = "Coefficient Alpha en fonction de la cote initial du joueur")


## petite boucle qui permet de multiplier la cote du joueur au alpha qui correspond ###
cote_alpha <- NULL
for (i in 1:length(note_mpg$Cote)){
  cote_alpha[i] <- note_mpg$Cote[i] * alpha[note_mpg$Cote[i]]
}

## on arrondi les cotes ##
cote_alpha = round(cote_alpha,0)


# On rajoute la cote_alpha au data frame note_mpg
note_mpg$cote_alpha = cote_alpha


########################################
######### Classement - TOP 10 ##########
########################################

# Top 10 des Buteurs ---
n = 10
top_buteur = note_mpg[,c("Poste", "Joueur", "Club", "Buts", "Moyenne_note")]
top_buteur = top_buteur[order(-top_buteur$Buts),]
top_buteur = top_buteur[1:n,]

# Top 10 des performant ---
n = 10
top_perf = note_mpg[,c("Poste", "Joueur", "Club", "performance_beta", , "Moyenne_note")]
top_perf = top_perf[order(-top_perf$performance_beta),]
top_perf = top_perf[1:n,]

# Top 10 des Joueurs les plus chers ---
n = 10
top_cote = note_mpg[,c("Poste", "Joueur", "Club", "Cote", "cote_alpha")]
top_cote = top_cote[order(-top_cote$cote_alpha),]
top_cote = top_cote[1:n,]


# Top 10 des Joueurs les plus reguliers --
n = 10
top_moy = note_mpg[,c("Poste", "Joueur", "Club", "Moyenne_note", "performance_beta")]
top_moy = top_moy[order(-top_moy$performance_beta),]
top_moy = top_moy[1:n,]


# TOP 10 des remplacants les plus utilisés --
n = 10
top_entrees = note_mpg[,c("Poste", "Joueur", "Club", "Entrees")]
top_entrees = top_entrees[order(-top_entrees$Entrees),]
top_entrees = top_entrees[1:n,]


# TOP 5 des supersub --
n = 5
entree_titu = note_mpg[,c("Poste","Joueur", "Club", "Buts", "Entrees", "Titu.", "Moyenne_note")]
entree_titu$Ratio = entree_titu$Entrees/entree_titu$Titu.
top_supersub = entree_titu[c(entree_titu$Ratio>=1.5),]
top_supersub$Ratio = top_supersub$Buts/top_supersub$Entrees
top_supersub = top_supersub[order(-top_supersub$Ratio),]
top_supersub = top_supersub[1:n,]


## Top 10 des gardiens--
n = 10
top_G = note_mpg[c(note_mpg$Poste=="G"),c("Poste","Joueur", "Club","performance_beta", "Moyenne_note")]
top_G = top_G[order(-top_G$performance_beta),]
top_G = top_G[1:n,]


## TOP 10 des déf ---
top_def = note_mpg[c(note_mpg$Poste=="D"),c("Poste","Joueur", "Club","performance_beta", "Moyenne_note")]
top_def = top_def[order(-top_def$performance_beta),]
top_def = top_def[1:n,]


## TOP 10 des milieux --
n = 10
top_mil = note_mpg[c(note_mpg$Poste=="M"),c("Poste","Joueur", "Club", "performance_beta", "Moyenne_note")]
top_mil = top_mil[order(-top_mil$performance_beta),]
top_mil = top_mil[1:n,]


## TOP 10 des attaquants ---
n = 10
top_att = note_mpg[c(note_mpg$Poste=="A"),c("Poste","Joueur", "Club", "performance_beta", "Moyenne_note")]
top_att = top_att[order(-top_att$performance_beta),]
top_att = top_att[1:n,]


# TOP 10 des perles rares (cote basses, buts ou moyenne bonne)--
n= 10
perle =  note_mpg[,c("Poste","Joueur", "Club", "performance_beta","Cote")]
perle$Ratio = perle$performance_beta/perle$Cote
top_perle= perle[order(-perle$Ratio),]
top_perle = top_perle[1:n,]


# TOP joueur prolifiques (ratio Matchs joués/buts) ---
n = 10
prolifique = note_mpg[,c("Poste","Joueur", "Club", "Entrees", "Titu.","Buts")]
prolifique[is.na(prolifique)] <- 0
prolifique$Matchs = prolifique$Entrees+prolifique$Titu.
top_prolifique = prolifique[c(prolifique$Matchs>2),c("Poste","Joueur", "Club", "Matchs","Buts")]

# A discuter sur la selection individus MAtchs> 1 et consideré comme titu
top_prolifique$ratio = top_prolifique$Buts/top_prolifique$Matchs
top_prolifique = top_prolifique[order(-top_prolifique$ratio),]
top_prolifique = top_prolifique[1:n,]



#### Optimisation lienaire : OMPR 
n = dim(note_mpg)[1]
nb_joueurs = 18
maxG = 15
# Pour selectionner les joueurs desiré
liste = list( "Zohi Kévin","Touré Abdoulaye")
row_jpref = as.numeric(rownames(note_mpg[note_mpg$Joueur %in% liste,]))

perf = scale(note_mpg$performance_beta)
perf[row_jpref,] = 100
cote = note_mpg$cote_alpha
poste = as.character(note_mpg$Poste)
note_mpg$Buts[is.na(note_mpg$Buts)] <- 0
buts = scale(note_mpg$Buts)
buts[row_jpref,] = 100

results = MIPModel() %>%
  add_variable(z[i], i = 1:n, type = "binary") %>%
  set_objective(sum_expr((perf[i] + buts[i]) * z[i], i = 1:n), "max") %>%
  add_constraint(sum_expr(z[i], i = 1:n) == nb_joueurs) %>%
  add_constraint(sum_expr(cote[i] * z[i], i = 1:n) <= 500)  %>%
  add_constraint( sum_expr(z[i], i = 1:n, poste[i] == "G") == 2) %>%
  add_constraint( sum_expr(z[i], i = 1:n, poste[i] == "D") == 6) %>%
  add_constraint( sum_expr(z[i], i = 1:n, poste[i] == "M") == 6) %>%
  add_constraint( sum_expr(z[i], i = 1:n, poste[i] == "A") == 4) 


contraint3 = as.expression(sum_expr(z[i], i = 1:n, poste[i] == "G"))
contraint4 = as.expression(sum_expr(z[i], i = 1:n, poste[i] == "D"))
contraint5 = as.expression(sum_expr(z[i], i = 1:n, poste[i] == "M"))
contraint6 = as.expression(sum_expr(z[i], i = 1:n, poste[i] == "A"))


results$constraints[[3]]$lhs =contraint3
results$constraints[[4]]$lhs =contraint4
results$constraints[[5]]$lhs =contraint5
results$constraints[[6]]$lhs =contraint6


results = solve_model(results , with_ROI(solver = "glpk"))
results = get_solution(results, z[i])
results = filter(results, value > 0)


mercatoEx = note_mpg[results$i,c("Poste", "Joueur", "Club", "performance_beta", "Cote", "cote_alpha", "Buts", "Titu.")]
mercatoEx = mercatoEx[order(mercatoEx$Poste),]
mercatoEx

