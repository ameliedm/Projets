#Packages nécessaires
library(dplyr)
library(ggplot2)
library(lme4)
library(lmerTest)
#Pour la carte à la fin
library(sf)
library(osmdata)
library(dplyr)
library(ggspatial)


#Chargement des données
load("data_Talence.Rdata")

#---------------
#Partie 1 : Maniuplation et exploration des données 
#---------------

head(data_talence) #Aperçu des premières lignes
str(data_talence) # Structure des variables
summary(data_talence) # Résumé statistique

#Remplacer les chaînes "None" ou "NaN" par NA
data_talence[data_talence == "None"] <- NA
data_talence[data_talence == "NaN"] <- NA

#Vérifier le nombre de valeurs manquantes par variable
colSums(is.na(data_talence))

#Suppression des variables non pertinentes qui n'apportent rien pour l'analyse
#du prix du bien immobilier 
data_talence <- data_talence %>%
  select(-c(adresse_suffixe, ancien_code_commune,
            ancien_nom_commune, ancien_id_parcelle,
            numero_volume, lot1_numero,lot1_surface_carrez,
            lot2_numero, lot2_surface_carrez, lot3_numero,
            lot3_surface_carrez, lot4_numero, lot4_surface_carrez,
            lot5_numero, lot5_surface_carrez, code_nature_culture,
            code_departement, nature_culture, code_nature_culture_speciale,
            nature_culture_speciale))
#Nous passons donc de 42 variables à 22

#Conversion des types de variables
#Date
data_talence$date_mutation <- as.Date(data_talence$date_mutation)

#Variables catégorielles
data_talence$nature_mutation <- as.factor(data_talence$nature_mutation)
data_talence$type_local <- as.factor(data_talence$type_local)
data_talence$nom_commune <- as.factor(data_talence$nom_commune)
data_talence$Quartier <- as.factor(data_talence$Quartier)
data_talence$id_parcelle <- as.factor(data_talence$id_parcelle)

#Variables numériques
num_vars <- c("valeur_fonciere", "surface_reelle_bati",
              "nombre_pieces_principales", "surface_terrain",
              "longitude", "latitude", "adresse_numero")

#Conversion en numérique
data_talence[num_vars] <- lapply(data_talence[num_vars], as.numeric)

#Détection et traitement des valeurs aberrantes
#On a remarqué qu'il y avait une valeurs aberrantes de 32 670 000€ qui correspond
#au prix de l'immeuble et non au prix de l'appartement, alors pour nous il faut l'enlever.

#Aperçu des valeurs extrêmes
summary(data_talence$valeur_fonciere)

#Suppression des valeurs foncières aberrantes (ex: > 30000000 millions)
data_talence <- data_talence %>%
  filter(valeur_fonciere < 30000000)

#Vérification rapide
#Distirbution du prix de vente
ggplot(data_talence, aes(x = valeur_fonciere)) +
  geom_histogram(bins = 50, fill = "lightblue", color = "grey") +
  labs(title = "Distribution du prix des biens à Talence", x = "Valeur foncière (€)", y = "Fréquence")

#Statistiques descriptives de base

#Prix moyen par type de bien
data_talence %>%
  group_by(type_local) %>%
  summarise(prix_moyen = mean(valeur_fonciere, na.rm = TRUE),
            surface_moyenne = mean(surface_reelle_bati, na.rm = TRUE),
            n = n())

#Prix vs Surface
ggplot(data_talence, aes(x = surface_reelle_bati, y = valeur_fonciere, color = type_local)) +
  geom_point(alpha = 0.6) +
  labs(title = "Relation entre surface et prix de vente",
       x = "Surface réelle (m²)", y = "Valeur foncière (€)")

#Le nuage de points montre une forte concentration de biens avec de petites
#surfaces et des prix relativement "bas", principalement pour les appartements 
#et maisons par rapport aux locaux industirels. En effet, quelques biens 
#de très grande surface ou de prix très élevés créent des valeurs aberrantes,
#surtout pour les locaux industriels. 
#La relation surface-prix est globalement positive, mais très dispersée, 
#indiquant que la surface seule n'explique pas bien la valeur foncière mais
#elle y joue un rôle dans son évolution.

# Évolution temporelle des ventes
ggplot(data_talence, aes(x = date_mutation, y = valeur_fonciere)) +
  geom_point(alpha = 0.4, color = "darkblue") +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(title = "Évolution du prix de vente dans le temps à Talence")

#Le graphique montre une forte dispersion des prix au fil du temps, avec 
#quelques ventes extrêmement élevées qui créent des valeurs aberrantes. 
#La tendance lissée indique une légère hausse des prix autour de 2022-2023, 
#suivie d'une baisse progressive.

#Sélection des types de biens pertinents
#Avant de passer au modèle linéaire mixte on va se limiter aux maisons 
#et appartements.
#On enlève dépendance, terrains nus et locaux commerciaux qui peuvent
#pertuber le modèle.

#Garder uniquement les biens de type "Maison" ou "Appartement"
data_talence <- data_talence %>%
  filter(type_local %in% c("Appartement", "Maison"))

#Vérification
table(data_talence$type_local)

#Vérification des valeurs manquantes après filtrage
colSums(is.na(data_talence))

#On remarque que la variable surface_terrain contient 1935 NA ce qui parraît
#normale car les appartement (ici on en a 2000) n'ont généralement pas de 
#terrain associé. Donc, NA n’est pas une donnée manquante aléatoire, 
#c’est une absence logique.
#Ainsi la variable surface_terrain ne sera pertinente que pour les maisons.
#On décide donc de remplacer les NA par 0 pour les appartements,
#interprété comme "pas de terrain".

#Pour les appartements, remplacer NA par 0 (pas de terrain)
data_talence <- data_talence %>%
  mutate(surface_terrain = ifelse(is.na(surface_terrain) & type_local == "Appartement", 0, surface_terrain))

#Vérification
summary(data_talence$surface_terrain)

#Visualisation
ggplot(data_talence, aes(x = surface_terrain, fill = type_local)) +
  geom_histogram(bins = 40, position = "identity", alpha = 0.6) +
  labs(title = "Distribution de la surface de terrain selon le type de bien",
       x = "Surface du terrain (m²)", y = "Fréquence") +
  theme_minimal()
#Ce graphique nous montre que la surface des appartements a été bien mit à 0
#De plus, on peut voir que les maisons on des surfaces comprises entre 0 et 1000m²
#et qu'il existe une maison avec une grande surface (11607m²).

#---------------
#Partie 2 : Modèle linéaire mixte
#---------------

#Tout d'abord, nous allons enlever les NA manquants dans surface_terrain
data_model <- data_talence %>%
  filter(!is.na(surface_terrain))

#Nous commençons par un modèle à intercept aléatoire par quartier

#/!\ dans type local on a deux facteurs et lmer la convertit automatiquement
#en variable indicatrice (prend par ordre alaphabétique)
#Ainsi : type_local = 1 si maison et type_local = 0 si appartement
mod1 <- lmer(valeur_fonciere ~ surface_reelle_bati + nombre_pieces_principales +
               surface_terrain + type_local + (1 | Quartier),
             data = data_model, REML = FALSE)

summary(mod1)

ICC <- 3.659e11 / (3.659e11 + 3.077e12)
ICC #11% 
#soit environ 11% de la variabilité totale du prix est due aux différences 
#entre quartiers.


#Le modèle linéaire mixte ajusté sur les 3 121 ventes (28 quartiers) montre que
#environ 10 % de la variabilité du prix des biens s’explique par le quartier ;
#le nombre de pièces et la surface du terrain influencent significativement 
#le prix.
#L’effet du type de bien (maison/appartement) n’est pas significatif une 
#fois les autres caractéristiques prises en compte.
#La surface bâtie n’apporte pas d’information supplémentaire au-delà du 
#nombre de pièces (forte colinéarité).

#---------------
#On passe sur le jeu de données newData_talence pour prédire le prix
#immobiliers de certains biens en sachant tout ce qu'on a vu auparavant.

#Remplacer les chaînes "None" ou "NaN" par NA
newData_talence[newData_talence == "None"] <- NA
newData_talence[newData_talence == "NaN"] <- NA

#Vérifier le nombre de valeurs manquantes par variable
colSums(is.na(newData_talence))

#Nettoyage du jeu de données à prédire, on ne garde que les
#variables qu'on souhaite
data_prediction <- newData_talence %>%
  select(c(surface_reelle_bati, nombre_pieces_principales,
           surface_terrain, type_local, Quartier))

colSums(is.na(data_prediction))

#Garder uniquement les biens de type "Maison" ou "Appartement"
data_prediction <- data_prediction %>%
  filter(type_local %in% c("Appartement", "Maison"))
#dans data_prediction (notre nouveau newData_talence), nous passons de
#1000 observations à 445.

#Vérification
table(data_prediction$type_local)

#Vérification des valeurs manquantes après filtrage
colSums(is.na(data_prediction))

#Tout d'abord, nous allons enlever les NA manquants dans surface_terrain
data_prediction <- data_prediction %>%
  filter(!is.na(surface_terrain))
#après nettoyage des NA dans surface_terrain, nous sommes à 200 observations.

#S'assurer que les variables ont les mêmes noms et types que dans data_talence
data_prediction <- data_prediction %>%
  mutate(
    surface_reelle_bati = as.numeric(surface_reelle_bati),
    nombre_pieces_principales = as.numeric(nombre_pieces_principales),
    surface_terrain = as.numeric(surface_terrain),
    type_local = factor(type_local, levels = levels(data_model$type_local)),
    Quartier = factor(Quartier, levels = levels(data_model$Quartier))
  )

#Prédiction pour les nouveaux biens (quartiers connus)
data_prediction$pred_prix <- predict(mod1, data_prediction, re.form = NULL)

#Boxplot par quartier et type de bien
#Ce graphique montre la distribution des prix prédits selon les deux facteurs

ggplot(data_prediction, aes(x = Quartier, y = pred_prix, fill = type_local)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  labs(title = "Prix prédits en fonction du quartier et du type de bien",
       x = "Quartier",
       y = "Prix prédit (€)",
       fill = "Type de bien") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Le graphique montre une forte variabilité des prix prédits selon les quartiers.
#Certains quartiers présentent des prix nettement plus élevés 
#(ex : AO, AZ, BD), tandis que d'autres restent moyens. 
#Les maisons ont en général des prix prédits plus élevés que 
#les appartements, avec une dispersion plus importante.
#Cette hétérogénéité confirme que le quartier et le type de bien influencent
#fortement les prédictions du modèle.

#Prix par type de bien uniquement (entre maison et appartement)
ggplot(data_prediction, aes(x = type_local, y = pred_prix, fill = type_local)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Prix prédits par type de bien",
       x = "Type de bien",
       y = "Prix prédit (€)") +
  theme_minimal()

#Le graphique montre que les prix prédits pour les maisons sont nettement 
#plus élevés et plus dispersés que ceux des appartements.
#Les appartements présentent des valeurs plus concentrées et globalement 
#plus basses (sans doute dû au nombre faible de données sur les appartements
#dans le jeu de données), tandis que les maisons affichent
#une variabilité importante, avec de nombreuses valeurs à très haut prix.
#Cela confirme que le type de bien influence fortement les prédictions du modèle.

#---------------
#Prédiction des prix pour le nouveau quartier AK

#Préparation des données du quartier AK
#On ne garde ici aussi que Appartement et Maison
data_AK <- quartier_AK_missing %>%
  select(valeur_fonciere, surface_reelle_bati, nombre_pieces_principales,
         surface_terrain, type_local, Quartier) %>%
  filter(type_local %in% c("Appartement", "Maison"))

#On passe de 413 à 181 observations

#Suppression des NA dans surface_terrain pour maison et remplacement par 0 
#pour les appartements
data_AK <- data_AK %>%
  mutate(surface_terrain = ifelse(is.na(surface_terrain) & type_local == "Appartement", 0, surface_terrain)) %>%
  filter(!is.na(surface_terrain))

#On passe de 181 à 158 observations

#Harmonisation des types et niveaux de facteurs

data_AK <- data_AK %>%
  mutate(
    surface_reelle_bati = as.numeric(surface_reelle_bati),
    nombre_pieces_principales = as.numeric(nombre_pieces_principales),
    surface_terrain = as.numeric(surface_terrain),
    type_local = factor(type_local, levels = levels(data_model$type_local)),
    Quartier = factor(Quartier, levels = c(levels(data_model$Quartier), "AK"))
  )

#Ici on met allow.new.levels = TRUE car AK n'existe pas dans mod1
data_AK$pred_prix <- predict(mod1, data_AK, allow.new.levels = TRUE)

#Analyse des résultats
#On a 2 cas dans data_AK :
# - le cas où le prix est connu (permet la validation prédictive)
# - le cas où le prix est manquant (prédiction utile)

AK_connus <- data_AK %>% filter(!is.na(valeur_fonciere))
AK_manquants <- data_AK %>% filter(is.na(valeur_fonciere))

#Graphique prix observés vs prix prédits
ggplot(AK_connus, aes(x = valeur_fonciere, y = pred_prix)) +
  geom_point(alpha = 0.7, size = 3) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Validation prédictive – Quartier AK",
       x = "Prix observé (€)",
       y = "Prix prédit (€)") +
  theme_minimal()

#Le nuage de points compare les prix observés et les prix prédits pour 
#le quartier AK.
#On observe une corrélation globale positive : les points suivent 
#approximativement la ligne rouge (y = x), mais avec une dispersion 
#importante, surtout pour les biens chers.
#Le modèle a tendance à sur-estimer les prix élevés. 
#Cela indique une performance correcte pour les biens courants, 
#mais une moins bonne précision sur les valeurs extrêmes.

#Visaulisation par type de bien
ggplot(data_AK, aes(x = type_local, y = pred_prix, fill = type_local)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Prix prédits – Quartier AK",
       x = "Type de bien",
       y = "Prix prédit (€)") +
  theme_minimal()

#Le graphique montre que, dans le quartier AK, les prix prédits des maisons 
#sont nettement plus élevés et plus dispersés que ceux des appartements.
#Les appartements présentent des valeurs prédictives plus basses et concentrées,
#tandis que les maisons affichent une grande variabilité avec quelques 
#valeurs extrêmes.
#Le type de bien influence donc fortement les niveaux de prix prédits 
#dans le quartier AK.

#Calcul du RMSE
AK_connus <- data_AK %>%
  filter(!is.na(valeur_fonciere))

RMSE_AK <- sqrt(mean((AK_connus$pred_prix - AK_connus$valeur_fonciere)^2))
RMSE_AK #310095.8
#Ici on a un écart de 310 000€ par rapport aux prix moyens des biens
#ce qui est beaucoup trop élevé.

#---------------
#Amélioration du modèle :
#On souhaite ajouter la latitude et la longitude dans notre modèle
#Avant cela on va remplacer les NA par la moyenne du quartier donné

#Remplissage des coordonnées manquantes par la moyenne du quartier
data_model <- data_model %>%
  group_by(Quartier) %>%
  mutate(
    longitude = ifelse(is.na(longitude),
                       mean(longitude, na.rm = TRUE), longitude),
    latitude = ifelse(is.na(latitude),
                      mean(latitude, na.rm = TRUE), latitude)
  ) %>%
  ungroup()

#La localisation est fortement liée à la valeur immobilière.
#Il vaut mieux une approximation cohérente que de supprimer l’observation.

#Ensuite on veut transformer la variable cible en log prix
#car les prix immobiliers ont une grande variabilité
#distribution asymétrique avec des valeurs extrêmes : mauvaise normalité 
#des résidus.
#Le modèle devient plus stable et moins sensible aux gros biens.

data_model <- data_model %>%
  mutate(log_prix = log(valeur_fonciere))

#Nouveau modèle avec modification (sans pente aléatoire)
mod_final_v1 <- lmer(log_prix ~ 
                       surface_reelle_bati +
                       nombre_pieces_principales +
                       surface_terrain +
                       type_local +
                       longitude + latitude +
                       (1 | Quartier),
                     data = data_model, REML = FALSE)

summary(mod_final_v1)

#POUR DATA_PREDICTION :

#Ajout de la latitude et longitude dans data_prediction
data_prediction <- newData_talence %>%
  select(c(surface_reelle_bati, nombre_pieces_principales,
           surface_terrain, type_local,longitude, latitude, Quartier))

#Correction des NA de longitude et latitude
data_prediction <- data_prediction %>%
  group_by(Quartier) %>%
  mutate(
    longitude = ifelse(is.na(longitude),
                       mean(longitude, na.rm = TRUE), longitude),
    latitude = ifelse(is.na(latitude),
                      mean(latitude, na.rm = TRUE), latitude)
  ) %>%
  ungroup()

# Garder uniquement les biens de type "Maison" ou "Appartement"
data_prediction <- data_prediction %>%
  filter(type_local %in% c("Appartement", "Maison"))

#Tout d'abord, nous allons enlever les NA manquants dans surface_terrain
data_prediction <- data_prediction %>%
  filter(!is.na(surface_terrain))

#S'assurer que les variables ont les mêmes noms et types que dans data_talence
data_prediction <- data_prediction %>%
  mutate(
    surface_reelle_bati = as.numeric(surface_reelle_bati),
    nombre_pieces_principales = as.numeric(nombre_pieces_principales),
    surface_terrain = as.numeric(surface_terrain),
    type_local = factor(type_local, levels = levels(data_model$type_local)),
    longitude = as.numeric(longitude),
    latitude = as.numeric(latitude),
    Quartier = factor(Quartier, levels = levels(data_model$Quartier))
  )


#POUR DATA_AK :

#Ajout de la latitude et longitude dans data_AK
data_AK <- quartier_AK_missing %>%
  select(valeur_fonciere, surface_reelle_bati, nombre_pieces_principales,
         surface_terrain, type_local, longitude, latitude, Quartier) %>%
  filter(type_local %in% c("Appartement", "Maison"))

#Pour data_AK ces NA, Quartier = AK jamais vu dans data_model alors
#pas de moyenne par quartier possible
#donc médiane globale directement
data_AK$longitude[is.na(data_AK$longitude)] <- median(data_model$longitude, na.rm = TRUE)
data_AK$latitude[is.na(data_AK$latitude)] <- median(data_model$latitude, na.rm = TRUE)

#Suppression des NA dans surface_terrain pour maison et remplacement par 0 
#pour les appartements
data_AK <- data_AK %>%
  mutate(surface_terrain = ifelse(is.na(surface_terrain) & type_local == "Appartement", 0, surface_terrain)) %>%
  filter(!is.na(surface_terrain))

#Harmonisation des types et niveaux de facteurs

data_AK <- data_AK %>%
  mutate(
    surface_reelle_bati = as.numeric(surface_reelle_bati),
    nombre_pieces_principales = as.numeric(nombre_pieces_principales),
    surface_terrain = as.numeric(surface_terrain),
    type_local = factor(type_local, levels = levels(data_model$type_local)),
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude),
    Quartier = factor(Quartier, levels = c(levels(data_model$Quartier), "AK"))
  )


#Prédiction avec mod_final_v1
data_prediction$pred_prix <- exp(predict(mod_final_v1, data_prediction, allow.new.levels = TRUE))
data_AK$pred_prix <- exp(predict(mod_final_v1, data_AK, allow.new.levels = TRUE))

#Calcul du RMSE pour data_AK avec mod_final_v1
AK_connus <- data_AK %>% filter(!is.na(valeur_fonciere))
RMSE_AK <- sqrt(mean((AK_connus$pred_prix - AK_connus$valeur_fonciere)^2))
RMSE_AK #189 548.6

#Autre modèle avec pente aléatoire selon la surface
mod_final_v2 <- lmer(log_prix ~ 
                       surface_reelle_bati +
                       nombre_pieces_principales +
                       surface_terrain +
                       type_local +
                       longitude + latitude +
                       (1 + surface_reelle_bati | Quartier),
                     data = data_model, REML = FALSE)
summary(mod_final_v2)

#Prédiction avec mod_final_v2
data_prediction$pred_prix <- exp(predict(mod_final_v2, data_prediction, allow.new.levels = TRUE))
data_AK$pred_prix <- exp(predict(mod_final_v2, data_AK, allow.new.levels = TRUE))

#Calcul du RMSE pour data_AK avec mod_final_v2
AK_connus <- data_AK %>% filter(!is.na(valeur_fonciere))
RMSE_AK <- sqrt(mean((AK_connus$pred_prix - AK_connus$valeur_fonciere)^2))
RMSE_AK #185 393.1

#L’effet de la surface bâtie varie selon les quartiers.
#Et on a un RMSE largement amélioré par rapport à notre tout premier modèle.

#On a eu un message d'avis concernant un problème numérique pour mod_final_v2
#Avant d'ajouter une pente aléatoire, comme fait avant, il faut centrer et réduire 
#les variables pour stabiliser l'estimation
#Création de variables centrées-réduites
data_model <- data_model %>%
  mutate(across(c(surface_reelle_bati, nombre_pieces_principales,
                  surface_terrain, longitude, latitude),
                scale, .names = "sc_{col}"))

#Nouveau modèle final avec pente aléatoire
mod_final <- lmer(log_prix ~ 
                    sc_surface_reelle_bati +
                    sc_nombre_pieces_principales +
                    sc_surface_terrain +
                    type_local +
                    sc_longitude + sc_latitude +
                    (1 + sc_surface_reelle_bati | Quartier),
                  data = data_model, REML = FALSE)

summary(mod_final)

#Avant de faire la prédiction il faut aussi centrer et réduire dans nos autres
#jeu de données

#Stocker les paramètres de centrage-réduction du modèle
# On mémorise les moyennes et sd de data_model
means <- apply(data_model[, c("surface_reelle_bati",
                              "nombre_pieces_principales",
                              "surface_terrain",
                              "longitude",
                              "latitude")],
               2, mean, na.rm = TRUE)

sds <- apply(data_model[, c("surface_reelle_bati",
                            "nombre_pieces_principales",
                            "surface_terrain",
                            "longitude",
                            "latitude")],
             2, sd, na.rm = TRUE)

#Appliquer la transformation aux nouveaux biens
#Pour data_prediction
data_prediction <- data_prediction %>%
  mutate(
    sc_surface_reelle_bati =
      (surface_reelle_bati - means["surface_reelle_bati"]) / sds["surface_reelle_bati"],
    sc_nombre_pieces_principales =
      (nombre_pieces_principales - means["nombre_pieces_principales"]) / sds["nombre_pieces_principales"],
    sc_surface_terrain =
      (surface_terrain - means["surface_terrain"]) / sds["surface_terrain"],
    sc_longitude =
      (longitude - means["longitude"]) / sds["longitude"],
    sc_latitude =
      (latitude - means["latitude"]) / sds["latitude"]
  )

#Pour data_AK
data_AK <- data_AK %>%
  mutate(
    sc_surface_reelle_bati =
      (surface_reelle_bati - means["surface_reelle_bati"]) / sds["surface_reelle_bati"],
    sc_nombre_pieces_principales =
      (nombre_pieces_principales - means["nombre_pieces_principales"]) / sds["nombre_pieces_principales"],
    sc_surface_terrain =
      (surface_terrain - means["surface_terrain"]) / sds["surface_terrain"],
    sc_longitude =
      (longitude - means["longitude"]) / sds["longitude"],
    sc_latitude =
      (latitude - means["latitude"]) / sds["latitude"]
  )

#Prédictions
data_prediction$pred_prix <- exp(predict(mod_final, data_prediction, allow.new.levels = TRUE))
data_AK$pred_prix <- exp(predict(mod_final, data_AK, allow.new.levels = TRUE))

#RMSE
AK_connus <- data_AK %>% filter(!is.na(valeur_fonciere))
RMSE_AK_final <- sqrt(mean((AK_connus$pred_prix - AK_connus$valeur_fonciere)^2))
RMSE_AK_final #188 055.4
#en ayant centré et réduits les variables ont retrouve à peu près le 
#même RMSE

#Graphiques pour les quartiers connus

#Boxplots prix prédits : Quartier × Type de bien
ggplot(data_prediction, aes(x = Quartier, y = pred_prix, fill = type_local)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  labs(title = "Prix prédits par quartier et type de bien",
       x = "Quartier",
       y = "Prix prédit (€)",
       fill = "Type de bien") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Ce graphique montre que, avec un modèle incluant une pente aléatoire 
#de la surface bâtie selon les quartiers (et après centrage/réduction des
#variables), les prix prédits varient toujours fortement d’un quartier 
#à l’autre. Les maisons restent plus chères que les appartements dans la 
#plupart des zones, mais les écarts sont moins extrêmes
#que dans le modèle précédent, ce qui suggère une meilleure prise en 
#compte des effets de la surface selon les quartiers. 
#La variabilité intra-quartier diminue, indiquant que la structure 
#hiérarchique améliore la cohérence des prédictions.

#Barplot : Prix moyens prédits par quartier
prix_quartier <- data_prediction %>%
  group_by(Quartier) %>%
  summarise(prix_moyen = mean(pred_prix, na.rm = TRUE))

ggplot(prix_quartier, aes(x = reorder(Quartier, prix_moyen), y = prix_moyen)) +
  geom_col(fill = "steelblue") +
  labs(title = "Prix moyen prédit par quartier",
       x = "Quartier",
       y = "Prix moyen prédit (€)") +
  coord_flip() +
  theme_minimal()

#Ce graphique nous permet de voir où se situe les biens avec les prix les 
#plus élevés. En effet, les quartiers AO et AZ présentent les biens
#ayant des valeurs foncières élevées. Les autres quartiers présentent une
#moyenne de 400000€ et on voit que les quartiers AH et BE sont les quartier
#avec les biens les moins chers.

#Heatmap prédite par quartier
ggplot(prix_quartier, aes(x = Quartier, y = 1, fill = prix_moyen)) +
  geom_tile() +
  scale_fill_gradient(low = "yellow", high = "red") +
  labs(title = "Heatmap du prix moyen prédit par quartier",
       x = "Quartier",
       y = "",
       fill = "Prix moyen (€)") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))


#Graphiques pour le nouveau quartier
ggplot(AK_connus, aes(x = valeur_fonciere, y = pred_prix)) +
  geom_point(alpha = 0.6, size = 3, color = "steelblue") +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "Validation prédictive – Quartier AK",
       x = "Prix observé (€)",
       y = "Prix prédit (€)") +
  theme_minimal()

#Le graphique montre une amélioration de la relation entre prix observés et 
#prix prédits pour le quartier AK avec le modèle à pente aléatoire : 
#les points suivent mieux la diagonale, signe d’une meilleure adéquation. 
#Cependant, le modèle sous-estime encore les biens très chers.
#La dispersion est encore présente, mais la cohérence globale des 
#prédictions est renforcée.

ggplot(data_AK, aes(x = type_local, y = pred_prix, fill = type_local)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Prix prédits – Quartier AK")

#Comme avant, on remarque une plus grande variablité des prix pour les maisons
#avec présence de fortes valeurs. Tandis que pour les appartements, les prix
#sont plus concentrés (sans doute dû au nombre réduit des appartements dans
#les donées).

#------------
#Carte des prix immobiliers de Talence
#Calcul du centroïde moyen des ventes par quartier
carte_quartier <- data_model %>%
  group_by(Quartier) %>%
  summarise(
    lon = mean(longitude, na.rm = TRUE),
    lat = mean(latitude, na.rm = TRUE),
    prix_moyen = mean(valeur_fonciere, na.rm = TRUE)
  )

AK_centroid <- data_AK %>%
  summarise(
    lon = mean(longitude, na.rm = TRUE),
    lat = mean(latitude, na.rm = TRUE),
    prix_moyen = mean(pred_prix, na.rm = TRUE)
  )
AK_centroid

#Carte (points par quartier)
#Avec couleur indiquant le prix moyen du quartier
ggplot(carte_quartier, aes(x = lon, y = lat, color = prix_moyen)) +
  geom_point(size = 6) +
  # Ajout du point du quartier AK
  geom_point(data = AK_centroid, aes(x = lon, y = lat),
             size = 8, shape = 8, color = "blue") +
  annotate("text", x = AK_centroid$lon, y = AK_centroid$lat + 0.0005,
           label = "Quartier AK", color = "blue", fontface = "bold") +
  geom_text(aes(label = Quartier), vjust = -1, size = 4) +
  scale_color_gradient(low = "yellow", high = "red") +
  labs(title = "Carte des prix immobiliers par quartier - Talence",
       subtitle = "Quartier AK prédit avec modèle linéaire mixte",
       x = "Longitude",
       y = "Latitude",
       color = "Prix moyen (€)") +
  theme_minimal()

#Cette carte permet de visualiser les prix moyens des biens immobiliers (apparemtents
#et maison) selon les quartiers. On peut voir une différence notamment pour
#les quartiers AO et BD qui présentent des prix élevés (comme vu avant)

#On va ensuite réaliser la carte des prix prédits à Talence

#Calcul du centroïde moyen des ventes par quartier (pour les prix prédits)
carte_quartier_pred <- data_prediction %>%
  group_by(Quartier) %>%
  summarise(
    lon = mean(longitude, na.rm = TRUE),
    lat = mean(latitude, na.rm = TRUE),
    prix_moyen = mean(pred_prix, na.rm = TRUE)
  )

# On ajoute le nouveau quartier AK
AK_centroid <- data_AK %>%
  summarise(
    Quartier = "AK",
    lon = mean(longitude, na.rm = TRUE),
    lat = mean(latitude, na.rm = TRUE),
    prix_moyen = mean(pred_prix, na.rm = TRUE)
  )

# Fusion pour avoir TOUS les quartiers (anciens + nouveau)
carte_all <- bind_rows(carte_quartier_pred, AK_centroid)


ggplot(carte_all, aes(x = lon, y = lat, color = prix_moyen)) +
  geom_point(size = 6) +
  geom_text(aes(label = Quartier), vjust = -1, size = 4) +
  scale_color_gradient(low = "yellow", high = "red") +
  labs(title = "Carte prédictive des prix immobiliers par quartier - Talence",
       subtitle = "Le quartier AK (nouveau) coloré selon son prix prédit",
       x = "Longitude",
       y = "Latitude",
       color = "Prix moyen (€)") +
  theme_minimal()

#Cette carte permet de visualiser les quartiers ayant les prix prédits moyens
#des biens immobiliers (ici appartements et maisons), les plus faibles aux 
#plus fortes.
#On remarque que nos prédictions sont assez élevées, comparées aux prix immobiliers
#donnés qu'on avait dans ces mêmes quartiers. Mais nous voyons que le quartier
#AO est toujours celui ayant des prix les plus élevés.
#De plus, on a l'ajout du quartier AK qui permet de voir si c'est un quartier
#avec de faibles ou de forts prix moyens immobiliers. On voit que ce quartier
#fait partie des quartiers avec des prix moyens plutôt "bas" comparés 
#aux autres. Cela veut dire qu'on prédire de plus faibles valeurs foncières
#pour le quartier AK.

#------------
#Cartes de Talence avec Quartier
#------------

#1) Bornes et polygone administratif de Talence (OSM)
bb_talence <- getbb("Talence, Gironde, France")  # matrice 2x2 : x/y × min/max

#Requête OSM : frontière administrative "Talence"
talence_poly <- opq(bbox = bb_talence) |>
  add_osm_feature(key = "boundary", value = "administrative") |>
  add_osm_feature(key = "name", value = "Talence") |>
  osmdata_sf()

#Combiner multi-polygones et polygones s'ils existent, sélectionner colonnes présentes
talence_layers <- list(talence_poly$osm_multipolygons, talence_poly$osm_polygons)
talence_layers <- talence_layers[!vapply(talence_layers, function(x) is.null(x) || nrow(x) == 0, logical(1))]

if (length(talence_layers) > 0) {
  talence_bound <- do.call(rbind, lapply(talence_layers, function(x) {
    x |>
      dplyr::select(dplyr::any_of(c("osm_id","name","geometry"))) |>
      sf::st_make_valid()
  }))
} else {
  # Fallback : rectangle de la bbox si OSM ne renvoie pas le polygone
  talence_bound <- st_as_sfc(st_bbox(c(
    xmin = bb_talence["x","min"],
    ymin = bb_talence["y","min"],
    xmax = bb_talence["x","max"],
    ymax = bb_talence["y","max"]
  ), crs = st_crs(4326)))
}

#2) Réseau routier (contexte visuel)
talence_roads <- opq(bbox = bb_talence) |>
  add_osm_feature(key = "highway",
                  value = c("primary","secondary","tertiary","residential")) |>
  osmdata_sf()

roads_sf <- talence_roads$osm_lines
if (!is.null(roads_sf) && nrow(roads_sf) > 0) {
  roads_sf <- st_make_valid(roads_sf)
}

#3) Points de quartiers 

if (!all(c("lon","lat") %in% names(carte_all))) {
  # Renommer s'ils existent sous 'longitude' / 'latitude'
  if (all(c("longitude","latitude") %in% names(carte_all))) {
    carte_all <- carte_all |>
      rename(lon = longitude, lat = latitude)
  }
}

#Vérification minimale
stopifnot(all(c("Quartier","prix_moyen","lon","lat") %in% names(carte_all)))

quartiers_sf <- st_as_sf(carte_all, coords = c("lon","lat"), crs = 4326)

#4) Fenêtre de carte et tracé ggplot

#BBox pour fixer la vue (à partir du polygone/bbox Talence)
if (inherits(talence_bound, "sf")) {
  bbox_tal <- st_bbox(talence_bound)
} else {
  bbox_tal <- st_bbox(talence_bound)  # sfc
}

p_talence <- ggplot() +
  # Polygone/bbox de Talence
  { if (inherits(talence_bound, "sf") || inherits(talence_bound, "sfc")) 
    geom_sf(data = talence_bound, fill = "grey95", color = "grey70", linewidth = 0.5)
    else NULL } +
  # Routes
  { if (!is.null(roads_sf) && nrow(roads_sf) > 0) 
    geom_sf(data = roads_sf, color = "grey80", linewidth = 0.2)
    else NULL } +
  # Points de quartiers colorés par prix moyen
  geom_sf(data = quartiers_sf, aes(color = prix_moyen), size = 3) +
  scale_color_gradient(low = "yellow", high = "red", name = "Prix moyen (€)") +
  coord_sf(xlim = c(bbox_tal["xmin"], bbox_tal["xmax"]),
           ylim = c(bbox_tal["ymin"], bbox_tal["ymax"])) +
  annotation_scale(location = "bl", text_cex = 0.7) +
  annotation_north_arrow(location = "tl", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  labs(title = "Carte prédictive des prix immobiliers — Talence",
       subtitle = "Couleur = prix moyen prédit par quartier",
       x = "Longitude", y = "Latitude") +
  theme_minimal()

print(p_talence)

#Export PNG pour votre rapport LaTeX
ggsave("fig_carte_talence.png", p_talence, width = 7, height = 5, dpi = 300)

#5) Carte : tous les logements colorés par Quartier
#Cette carte permet de visualiser la situation géographiques des logements

# 5.1 Points logements -> sf (on retire les coordonnées manquantes)
logements_sf <- data_talence %>%
  dplyr::filter(!is.na(longitude), !is.na(latitude)) %>%
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# 5.2 Même fenêtre spatiale que précédemment
if (inherits(talence_bound, "sf")) {
  bbox_tal <- sf::st_bbox(talence_bound)
} else {
  bbox_tal <- sf::st_bbox(talence_bound)  # sfc
}

# 5.3 Carte : fond OSM + routes + points logements colorés par Quartier
p_talence_logements <- ggplot() +
  # Polygone/bbox de Talence
  { if (inherits(talence_bound, "sf") || inherits(talence_bound, "sfc"))
    geom_sf(data = talence_bound, fill = "grey95", color = "grey70", linewidth = 0.5)
    else NULL } +
  # Routes
  { if (!is.null(roads_sf) && nrow(roads_sf) > 0)
    geom_sf(data = roads_sf, color = "grey85", linewidth = 0.2)
    else NULL } +
  # Tous les logements
  geom_sf(data = logements_sf, aes(color = Quartier), size = 1.2, alpha = 0.85) +
  coord_sf(xlim = c(bbox_tal["xmin"], bbox_tal["xmax"]),
           ylim = c(bbox_tal["ymin"], bbox_tal["ymax"])) +
  labs(title = "Talence — tous les logements (DVF) colorés par quartier",
       subtitle = "Points = logements ; couleurs = quartiers (formes visibles par grappes de points)",
       x = "Longitude", y = "Latitude", color = "Quartier") +
  # Palette par défaut (hue) adaptée à de nombreuses catégories + légende compacte
  scale_color_discrete(guide = guide_legend(ncol = 3, override.aes = list(size = 4, alpha = 1))) +
  theme_minimal() +
  theme(legend.key.height = unit(0.5, "cm"),
        legend.key.width  = unit(0.6, "cm"),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 9))

print(p_talence_logements)

# 5.4 Export PNG pour le rapport
ggsave("fig_carte_talence_points.png", p_talence_logements, width = 7.5, height = 6, dpi = 300)