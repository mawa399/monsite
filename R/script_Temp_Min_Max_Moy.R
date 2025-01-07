# Charger les librairies
library(ncdf4)
library(fields)
library(maps)

# Ouvrir les fichiers NetCDF pour les températures minimales et maximales
TN <- nc_open("tn_ens_mean_0.25deg_reg_v29.0e.nc")  # Températures minimales
TX <- nc_open("tx_ens_mean_0.25deg_reg_v29.0e.nc")  # Températures maximales

# Récupérer les données de latitude et longitude (identiques pour les deux fichiers)
lat <- ncvar_get(TN, "latitude")
lon <- ncvar_get(TN, "longitude")

# Sélectionner les longitudes/latitudes à conserver (Europe)
lat_rng <- c(34, 72)
lon_rng <- c(-12, 42)

lat_ind <- which(lat >= lat_rng[1] & lat <= lat_rng[2])
lon_ind <- which(lon >= lon_rng[1] & lon <= lon_rng[2])

lat_ts <- lat[lat_ind]
lon_ts <- lon[lon_ind]

# Extraire les données de température minimale et maximale
TN_ts <- ncvar_get(TN, "tn", start = c(lon_ind[1], lat_ind[1], 1), count = c(length(lon_ind), length(lat_ind), -1))
TX_ts <- ncvar_get(TX, "tx", start = c(lon_ind[1], lat_ind[1], 1), count = c(length(lon_ind), length(lat_ind), -1))

# Supprimer les 29 février
date <- seq.Date(from = as.Date('1950-01-01'), to = as.Date('2023-12-31'), by = 'days')
bi <- which(format(date, format = "%m") == "02" & format(date, format = "%d") == "29")
date_ok <- date[-bi]

TN_ok <- TN_ts[, ,-bi]
TX_ok <- TX_ts[, ,-bi]

# Réarranger les données
TN_AA <- aperm(TN_ok, c(3, 1, 2))
TX_AA <- aperm(TX_ok, c(3, 1, 2))

# Charger le fichier pt_exclure qui contient les points de grille sans données valides
load("pt_exclure")

# Calculer la moyenne des températures minimales et maximales sur 74 ans en excluant les points invalides
mean_TN <- apply(TN_AA, c(2, 3), mean, na.rm = TRUE)
mean_TX <- apply(TX_AA, c(2, 3), mean, na.rm = TRUE)

# Appliquer le masque pt_exclure pour éliminer les points sans données sur les 74 ans
mean_TN [pt_exclure] <- NaN
mean_TX [pt_exclure] <- NaN

# Afficher/exporter la carte des températures minimales moyennes
#PNG
png("mean_min_temperature_europe_V2.png", width = 800, height = 600)
par(bg = "grey", mar=c(5, 5, 4, 2) + 0.1, xpd=TRUE)
image.plot(lon_ts, lat_ts, mean_TN, main = "Moyenne des Températures journalières minimales en Europe (1950-2023)", xlab = "Longitude (°Est)", ylab = "Latitude (°Nord)")
map(add = TRUE)
dev.off()

#SVG
svg("mean_min_temperature_europe_V2.svg")
par(bg = "grey", mar=c(5, 5, 4, 2) + 0.1, xpd=TRUE)
image.plot(lon_ts, lat_ts, mean_TN, main = "Moyenne des Températures journalières minimales en Europe (1950-2023)", xlab = "Longitude (°Est)", ylab = "Latitude (°Nord)")
map(add = TRUE)
dev.off()

# Afficher/exporter la carte des températures maximales moyennes
#PNG
png("mean_max_temperature_europe_V2.png", width = 800, height = 600)
par(bg = "grey", mar=c(5, 5, 4, 2) + 0.1, xpd=TRUE)
image.plot(lon_ts, lat_ts, mean_TX, main = "Moyenne des Températures journalières maximales en Europe (1950-2023)", xlab = "Longitude (°Est)", ylab = "Latitude (°Nord)")
map(add = TRUE)
dev.off()

#SVG
svg("mean_max_temperature_europe_V2.svg")
par(bg = "grey", mar=c(5, 5, 4, 2) + 0.1, xpd=TRUE)
image.plot(lon_ts, lat_ts, mean_TX, main = "Moyenne des Températures journalières maximales en Europe (1950-2023)", xlab = "Longitude (°Est)", ylab = "Latitude (°Nord)")
map(add = TRUE)
dev.off()

# Fermer les fichiers NetCDF
nc_close(TN)
nc_close(TX)
