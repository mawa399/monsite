# Charger les librairies
library(ncdf4)
library(fields)
library(maps)
library(RColorBrewer)
library(magick)
library(ggplot2)
library(cowplot)

# Ouvrir les fichiers NetCDF pour les températures minimales
TN <- nc_open("tn_ens_mean_0.25deg_reg_v29.0e.nc")  # Températures minimales

# Récupérer les données de latitude et longitude
lat <- ncvar_get(TN, "latitude")
lon <- ncvar_get(TN, "longitude")

# Sélectionner les longitudes/latitudes à conserver (Europe)
lat_rng <- c(34, 72)
lon_rng <- c(-12, 42)

lat_ind <- which(lat >= lat_rng[1] & lat <= lat_rng[2])
lon_ind <- which(lon >= lon_rng[1] & lon <= lon_rng[2])

lat_ts <- lat[lat_ind]
lon_ts <- lon[lon_ind]

# Extraire les données de température minimale
TN_ts <- ncvar_get(TN, "tn", start = c(lon_ind[1], lat_ind[1], 1), count = c(length(lon_ind), length(lat_ind), -1))

# Supprimer les 29 février
date <- seq.Date(from = as.Date('1950-01-01'), to = as.Date('2023-12-31'), by = 'days')
bi <- which(format(date, format = "%m") == "02" & format(date, format = "%d") == "29")
date_ok <- date[-bi]
TN_ok <- TN_ts[, ,-bi]

# Réarranger les données
TN_AA <- aperm(TN_ok, c(3, 1, 2))

# Charger le fichier pt_exclure qui contient les points de grille sans données valides
load("pt_exclure")

# Calculer la moyenne de référence (1991-2020)
reference_years <- 1991:2020
reference_freeze_days <- array(0, dim = c(length(lon_ts), length(lat_ts)))

for (year in reference_years) {
  year_indices <- which(format(date_ok, "%Y") == as.character(year))
  freeze_days <- apply(TN_AA[year_indices, , ], c(2, 3), function(x) sum(x < 0, na.rm = TRUE))
  reference_freeze_days <- reference_freeze_days + freeze_days
}

reference_freeze_days <- reference_freeze_days / length(reference_years)
reference_freeze_days[pt_exclure] <- NaN

# Trouver les valeurs min et max des différences pour définir une échelle fixe
global_min <- 0
global_max <- 0

for (year in 1950:2023) {
  year_indices <- which(format(date_ok, "%Y") == as.character(year))
  freeze_days <- apply(TN_AA[year_indices, , ], c(2, 3), function(x) sum(x < 0, na.rm = TRUE))
  freeze_days[pt_exclure] <- NaN
  
  diff_days <- freeze_days - reference_freeze_days
  
  global_min <- min(global_min, min(diff_days, na.rm = TRUE))
  global_max <- max(global_max, max(diff_days, na.rm = TRUE))
}

# Créer une palette de couleurs divergente
diff_palette <- colorRampPalette(c("red", "white", "blue"))(100)

# Créer un dataframe pour les données du diagramme en barres
bar_data <- data.frame(
  Year = 1950:2023,
  Ecart = numeric(74)
)

# Créer une liste pour stocker les images combinées
combined_image_list <- list()

# Itérer sur les années pour calculer et tracer les différences de jours de gel
for (year in 1950:2023) {
  year_indices <- which(format(date_ok, "%Y") == as.character(year))
  freeze_days <- apply(TN_AA[year_indices, , ], c(2, 3), function(x) sum(x < 0, na.rm = TRUE))
  freeze_days[pt_exclure] <- NaN
  
  diff_days <- freeze_days - reference_freeze_days
  
  # Calculer l'écart moyen pour l'année
  bar_data$Ecart[year - 1949] <- mean(diff_days, na.rm = TRUE)
  
  # Créer l'image de la carte
  png(temp_file_map <- tempfile(fileext = ".png"), width = 800, height = 600)
  image.plot(lon_ts, lat_ts, diff_days, 
             main = paste("Différence de jours de gel en Europe pour l'année", year, 
                          "\npar rapport à la moyenne de la période de référence (1991-2020)"),
             xlab = "Longitude (°Est)", ylab = "Latitude (°Nord)", 
             zlim = c(global_min, global_max),
             col = diff_palette)
  map(add = TRUE)
  dev.off()
  
  # Créer le diagramme en barres
  p <- ggplot(bar_data[1:(year-1949),], aes(x = Year, y = Ecart, fill = Ecart > 0)) +
    geom_col() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_fill_manual(values = c("TRUE" = "blue", "FALSE" = "red"),
                      labels = c("TRUE" = "Plus de jours de gel", "FALSE" = "Moins de jours de gel")) +
    labs(title = "Diagramme en barre de l'écart des nombres de jours de gel de 1950 à 2023\npar rapport à la moyenne de la période de référence (1991-2020)",
         x = "Année", y = "Écart à la normale (jours)") +
    theme_minimal() +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(size = 7),
          plot.title = element_text(size = 10, face = "bold"),
          axis.title = element_text(size = 9),
          axis.text = element_text(size = 8)) +
    ylim(min(bar_data$Ecart, na.rm = TRUE), max(bar_data$Ecart, na.rm = TRUE)) +
    scale_x_continuous(breaks = seq(1950, 2020, by = 10))
  
  # Sauvegarder le diagramme en barres
  png(temp_file_bar <- tempfile(fileext = ".png"), width = 800, height = 400)
  print(p)
  dev.off()
  
  # Combiner les deux images verticalement
  combined_image <- image_append(c(image_read(temp_file_map), image_read(temp_file_bar)), stack = TRUE)
  
  # Ajouter l'image combinée à la liste
  combined_image_list[[year - 1949]] <- combined_image
}

# Créer le GIF animé avec les images combinées
animation <- image_animate(image_join(combined_image_list), fps = 2)

# Sauvegarder le GIF
image_write(animation, "Mes_diff_GIF/difference_jours_gel_avec_barres.gif")

# Fermer les fichiers NetCDF
nc_close(TN)
