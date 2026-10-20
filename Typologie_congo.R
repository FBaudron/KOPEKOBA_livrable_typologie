
# CHARGEMENT DES PACKAGES NECESSAIRES------------------------------------------

library(openxlsx)
library(vegan)
library(ade4)
library(cluster)
library(factoextra)
library(gtsummary)
library(randomForest)


# DEFINITION DU REPERTOIRE ET CHARGEMENT DES DONNEES----------------------------

setwd("D:\\Mes Donnees\\1. Cirad\\6. KOPEKOBA\\Livrables\\Typologie\\")

typo = read.xlsx("donnees_fermes_savanes_congo.xlsx", sheet = 2)


# Transformation des variables continues

data_typo = typo

summary(typo$revenu_agricole)
typo$revenu_agricole[is.na(typo$revenu_agricole)] = 0
sum(typo$revenu_agricole == 0)

summary(typo$revenu_res_nat)
typo$revenu_res_nat[is.na(typo$revenu_res_nat)] = 0
sum(typo$revenu_res_nat == 0)
typo$revenu_res_nat = ifelse(typo$revenu_res_nat > 0, 1, 0)

summary(typo$revenu_autre)
typo$revenu_autre[is.na(typo$revenu_autre)] = 0
sum(typo$revenu_autre == 0)
typo$revenu_autre = ifelse(typo$revenu_autre > 0, 1, 0)

typo$previous_vegetation = ifelse(typo$previous_vegetation == "Savane", 0, 1)

# hist(typo$taille_famille) # Histogramme de la taille du ménage
# hist(typo$age) # Histogramme de l’âge
# hist(typo$cultivated_area) # Histogramme de la superficie cultivée
typo$cultivated_area = log10(typo$cultivated_area + (0.5 * min(typo$cultivated_area[typo$cultivated_area > 0])))
# hist(typo$cultivated_area) # Histogramme de la superficie cultivée
# hist(typo$diversite_annual) # Histogramme de la diversité des cultures annuelles
typo$diversite_annual = log10(typo$diversite_annual + (0.5 * min(typo$diversite_annual[typo$diversite_annual > 0])))
# hist(typo$diversite_annual) # Histogramme de la diversité des cultures annuelles
# hist(typo$diversite_perenial) # Histogramme de la diversité des cultures pérennes
typo$diversite_perenial = log10(typo$diversite_perenial + (0.5 * min(typo$diversite_perenial[typo$diversite_perenial > 0])))
# hist(typo$diversite_perenial) # Histogramme de la diversité des cultures pérennes
# hist(typo$diversite_total) # Histogramme de la diversité totale des cultures
# hist(typo$revenu_agricole) # Histogramme du revenu agricole
typo$revenu_agricole = log10(typo$revenu_agricole + (0.5 * min(na.omit(typo$revenu_agricole)[na.omit(typo$revenu_agricole) > 0])))
# hist(typo$revenu_agricole) # Histogramme du revenu agricole
# hist(typo$distance_route) # Histogramme de la distance à la route
typo$distance_route = log10(typo$distance_route + (0.5 * min(na.omit(typo$distance_route)[na.omit(typo$distance_route) > 0])))
# hist(typo$distance_route) # Histogramme de la distance à la route

typo$fallow_duration_simplifie = as.factor(typo$fallow_duration_simplifie)

typo = typo[, c("id", "taille_famille", "age_chef_fam", "cultivated_area", "diversite_total", "diversite_annual",
                 "diversite_perenial", "revenu_agricole", "revenu_res_nat", "revenu_autre",
                 "foyer_femme", "education_chef_fam", "indigene", "jardin", "verger", 
                 "jachere", "surface_louee", "bovins", "porcs", "petits_ruminants", "volailles",
                 "securite_alim", "alim", "entraide", "main_doeuvre_ext", "groupement",
                 "contrat", "reprise", "manioc", "mais", "arachide", "safoutier", "cacao","cafe", "plantain",
                 "palmier", "evolution_surf_cult_passe", "evolution_surf_cult_future",
                 "charbon", "peche", "chasse", "orientation", "previous_vegetation",
                 "fallow_duration_simplifie", "statut_champ", "distance_route", "tracteur_usage", "voit_kav_mot_usage")]

typo = na.omit(typo)


# ANALYSE EN COORDONNEES PRINCIPALES ET DELIMITATION DES GROUPES-----------------

# Matrice de dissimilarité
diss = daisy(typo[, c("taille_famille", "age_chef_fam", "cultivated_area", "diversite_total",
                      "revenu_agricole", "distance_route",
                      "foyer_femme", "education_chef_fam", "indigene",
                      "jardin", "verger", 
                      "jachere", "surface_louee", "petits_ruminants", 
                      "securite_alim", "entraide", "main_doeuvre_ext", "groupement",
                      "contrat", "reprise", "mais", "arachide", "safoutier", "plantain",
                      "palmier", "evolution_surf_cult_passe", "evolution_surf_cult_future", "statut_champ", "orientation",
                      "charbon", "peche", "chasse", "tracteur_usage", "voit_kav_mot_usage", "previous_vegetation",
                      "fallow_duration_simplifie"
                      )],
             metric = "gower",
             type = list(asymm = c("foyer_femme", "education_chef_fam", "indigene",
                                   "jardin", "verger", 
                                   "jachere", "surface_louee", "petits_ruminants", 
                                   "securite_alim", "entraide", "main_doeuvre_ext", "groupement",
                                   "contrat", "reprise", "mais", "arachide", "safoutier", "plantain",
                                   "palmier", "evolution_surf_cult_passe", "evolution_surf_cult_future", "statut_champ", "orientation",
                                   "charbon", "peche", "chasse", "tracteur_usage", "voit_kav_mot_usage", "previous_vegetation"))) 

pco = cmdscale(diss, eig = TRUE, k = 10) 
barplot(pco$eig[1:20])

# Sélection du nombre d’axes permettant d’expliquer au moins 50 % de la variabilité
cum_var = cumsum(pco$eig) / sum(pco$eig)
k_axes = which(cum_var >= 0.5)[1]
k_axes
pco_var = pco$points[, 1:k_axes]

# Détermination du nombre optimal de groupes (de 3 à 6) à partir de la largeur moyenne de silhouette
sil_width = numeric(6)
for (k in 3:6) {
  pam_fit = pam(dist(pco_var), k = k)
  sil_width[k] = pam_fit$silinfo$avg.width
}
best_k = which.max(sil_width)
plot(2:6, sil_width[2:6], type = "b", pch = 19,
     xlab = "Nombre de groupes", ylab = "Largeur moyenne de silhouette")
abline(v = best_k, col = "red", lty = 2)

# Classification autour de médoïdes
final_pam = pam(pco_var, k = best_k, diss = FALSE)

typo$Type = factor(final_pam$clustering)

write.xlsx(typo[, c(1, 49)], "Typologie - Congo.xlsx", rowNames = F)


plot(pco$points[,1], pco$points[,2], col = final_pam$clustering)
plot(pco$points[,1], pco$points[,3], col = final_pam$clustering)
plot(pco$points[,2], pco$points[,3], col = final_pam$clustering)


data = merge(data_typo, typo[, c(1, 49)], by = "id")


# INTERPRETATION DES TYPES D’EXPLOITATION--------------------------------------

# Tableau récapitulatif

tbl = data[, c(56, 8:17, 19, 21:22, 25:34, 37:42, 45:53, 55)] %>%
  tbl_summary(
    by = Type,
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{p}%"),
    digits = all_continuous() ~ 1)%>%
  add_p(test = list(all_continuous() ~ "kruskal.test",
                    all_categorical() ~ "chisq.test"))%>%
  add_overall() 

tbl


# Forêt aléatoire

data$Type = as.factor(data$Type)

rf_type = randomForest(Type ~ ., data = data[, c(56, 8:17, 19, 21:22, 25:34, 37:42, 45:53, 55)], ntree = 1500)
print(rf_type)

varImpPlot(rf_type)



