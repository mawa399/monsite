# Charger les librairies nécessaires
library(ncdf4)
library(fields)
library(maps)

# Ouvrir les fichiers NetCDF pour les températures minimales et maximales
TN <- nc_open("tn_ens_mean_0.25deg_reg_v29.0e.nc")
TX <- nc_open("tx_ens_mean_0.25deg_reg_v29.0e.nc")

# Récupérer les données de latitude et longitude
lat <- ncvar_get(TN, "latitude")
lon <- ncvar_get(TN, "longitude")

# Sélectionner les indices pour l'Europe
lat_rng <- c(34, 72)
lon_rng <- c(-12, 42)
lat_ind <- which(lat >= lat_rng[1] & lat <= lat_rng[2])
lon_ind <- which(lon >= lon_rng[1] & lon <= lon_rng[2])
lat_ts <- lat[lat_ind]
lon_ts <- lon[lon_ind]

# Extraire les données de température pour les indices sélectionnés
TN_ts <- ncvar_get(TN, "tn", start = c(lon_ind[1], lat_ind[1], 1), count = c(length(lon_ind), length(lat_ind), -1))
TX_ts <- ncvar_get(TX, "tx", start = c(lon_ind[1], lat_ind[1], 1), count = c(length(lon_ind), length(lat_ind), -1))

# Supprimer les 29 février
date <- seq(as.Date('1950-01-01'), as.Date('2023-12-31'), by = 'day')
bi <- which(format(date, "%m-%d") == "02-29")
date_ok <- date[-bi]
TN_ok <- TN_ts[, ,-bi]
TX_ok <- TX_ts[, ,-bi]

# Réarranger les données
TN_AA <- aperm(TN_ok, c(3, 1, 2))
TX_AA <- aperm(TX_ok, c(3, 1, 2))

# Charger le fichier des points à exclure
load("pt_exclure")

# Appliquer le masque
TN_AA[pt_exclure] <- NaN
TX_AA[pt_exclure] <- NaN

# Calculer la température moyenne quotidienne
temp_moy_AA <- (TN_AA + TX_AA) / 2

# Indiquer les périodes
period1_ind <- which(date_ok >= as.Date('1950-01-01') & date_ok <= as.Date('1986-12-31'))
period2_ind <- which(date_ok >= as.Date('1987-01-01') & date_ok <= as.Date('2023-12-31'))

# Moyennes pour chaque période
mean_temp_period1 <- apply(temp_moy_AA[period1_ind, , ], c(2, 3), mean, na.rm = TRUE)
mean_temp_period2 <- apply(temp_moy_AA[period2_ind, , ], c(2, 3), mean, na.rm = TRUE)

# Calcul des moyennes pour chaque période en appliquant le masque
mean_temp_period1[pt_exclure] <- NaN
mean_temp_period2[pt_exclure] <- NaN

# # Calcul des bornes pour les températures moyennes et l'évolution
# cat("Températures Moyennes 1950-1986 :\n")
# cat("Min :", min(mean_temp_period1, na.rm = TRUE), "Max :", max(mean_temp_period1, na.rm = TRUE), "\n")

# cat("Températures Moyennes 1987-2023 :\n")
# cat("Min :", min(mean_temp_period2, na.rm = TRUE), "Max :", max(mean_temp_period2, na.rm = TRUE), "\n")

# cat("Évolution des Températures :\n")
# cat("Min :", min(evolution_temp, na.rm = TRUE), "Max :", max(evolution_temp, na.rm = TRUE), "\n")

# Définir une légende commune pour les températures moyennes
legend_min <- -5
legend_max <- 23

# Créer une palette de couleurs avec un dégradé bleu-blanc-rouge
create_color_palette_bwr <- function(n_neg, n_pos) {
  blues <- colorRampPalette(c("blue", "white"))(n_neg)  # Dégradé bleu
  reds <- colorRampPalette(c("white", "red"))(n_pos)   # Dégradé rouge
  palette <- c(blues, reds)
  return(palette)
}

# Calculer le nombre de couleurs à attribuer à chaque côté de 0
n_colors <- 100
n_neg <- round(n_colors * abs(legend_min) / (abs(legend_min) + legend_max))  # Proportion de couleurs pour les valeurs négatives
n_pos <- n_colors - n_neg  # Proportion de couleurs pour les valeurs positives

# Générer la palette et les bornes pour les températures moyennes
color_palette_bwr <- create_color_palette_bwr(n_neg, n_pos)
breaks_periods <- seq(legend_min, legend_max, length.out = length(color_palette_bwr) + 1)

# Calcul de l'évolution des températures
evolution_temp <- mean_temp_period2 - mean_temp_period1
evolution_temp[pt_exclure] <- NaN

# Définir les bornes spécifiques pour l'évolution
evolution_legend_min <- -0.6
evolution_legend_max <- 2.4

# Calculer les proportions pour les parties négatives et positives pour l'évolution
n_neg_evolution <- round(n_colors * abs(evolution_legend_min) / (abs(evolution_legend_min) + evolution_legend_max))
n_pos_evolution <- n_colors - n_neg_evolution

# Générer la palette et les bornes pour l'évolution
color_palette_evolution <- create_color_palette_bwr(n_neg_evolution, n_pos_evolution)
breaks_evolution <- seq(evolution_legend_min, evolution_legend_max, length.out = length(color_palette_evolution) + 1)

# Fonction pour générer les cartes
generate_map <- function(data, file_name, title, breaks, palette, legend_label = "Valeur") {
  png(file_name, width = 800, height = 600)
  image.plot(
    lon_ts, lat_ts, data, 
    main = title, 
    xlab = "Longitude (°Est)", 
    ylab = "Latitude (°Nord)", 
    col = palette, 
    breaks = breaks, 
    legend.lab = legend_label, # Titre de la légende
    legend.line = 2.5          # Décalage du titre de la légende par rapport à la barre
  )
  map(add = TRUE)
  dev.off()
}

# Générer les cartes pour les deux périodes avec une légende commune
generate_map(mean_temp_period1, "last_temperature_1950_1986.png", "Températures Moyennes en Europe (1950-1986)", breaks_periods, color_palette_bwr, "Température (°C)")
generate_map(mean_temp_period2, "last_temperature_1987_2023.png", "Températures Moyennes en Europe (1987-2023)", breaks_periods, color_palette_bwr, "Température (°C)")

# Générer la carte pour l'évolution avec des bornes et une palette adaptées
generate_map(evolution_temp, "last_evolution_temperature.png", "Évolution des Températures Moyennes (1987-2023 vs 1950-1986)", breaks_evolution, color_palette_evolution, "Température (°C)")
