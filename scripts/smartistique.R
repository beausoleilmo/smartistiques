# Objectif
# Projet de pédagogie statistique avec des smarties.
# Application d'un exemple concrêt d'effet d'échantillonnage.
# Visualisation des données
library(vegan)
library(dplyr)
library(ggplot2)
library(paletteer)
library(readxl)
library(tidyr)


# Codes des couleurs en HEX
smartcolour <- paletteer::paletteer_d("RSkittleBrewer::smarties")
# Données de couleurs des Smarties
smart_coltab <- data.frame(
  nb = 1:8,
  col = as.vector(smartcolour),
  nom = c("Rose", "Violet", "Bleu", "Vert", "Jaune", "Orange", "Rouge", "Brun"),
  # noms des couleurs dans le fichier Excel
  nom_c = c(
    "rose",
    "mauve",
    "bleu",
    "vert",
    "jaune",
    "orange",
    "rouge",
    "brun"
  ),
  name = c(
    "Pink",
    "Violet",
    "Blue",
    "Green",
    "Yellow",
    "Orange",
    "Red",
    "Brown"
  )
)
# Lire les données des boîtes
smartboite <- readxl::read_xlsx(
  path = file.path("data", "experience_smarties.xlsx"),
  sheet = 2
)
# Lire les données expérimentales
smartdata <- readxl::read_xlsx(
  path = file.path("data", "experience_smarties.xlsx"),
  sheet = 1
) |>
  dplyr::left_join(
    y = smart_coltab,
    by = dplyr::join_by(couleur == nom_c)
  ) |>
  # complète avec les couleurs manquantes
  tidyr::complete(boite_ID, nesting(nom, col), fill = list(nombre = 0)) |>
  # ajout données des boîtes
  dplyr::left_join(
    y = smartboite,
    by = dplyr::join_by(boite_ID)
  )

# statisiques sur les boîtes
smartransf <- smartdata |>
  # sélectionne les éléments présent seulement
  dplyr::filter(nombre > 0) |>
  dplyr::group_by(boite_ID) |>
  dplyr::summarise(
    nb_total = sum(nombre),
    moy_poids_g = mean(pesee_g),
    nb_col = n_distinct(nom),
    # Supposition qu'il y a 8 couleurs : officiellement en 2026
    nb_col_absent = 8 - nb_col
  )

# Calcul poids moyen d'un smarties
poids_smarties_moy <- smartransf |>
  dplyr::summarise(
    som_nb = sum(nb_total),
    som_pds_g = sum(moy_poids_g),
    moy = som_nb / som_pds_g
  )

message(sprintf(
  "Le poids (g) moyen d'un Smarties est : %s",
  poids_smarties_moy |> dplyr::pull(moy) |> round(3)
))

# Plot with facet_wrap  allowing free scales
smardataPlot <- smartdata |>
  # ajout du sommaire
  dplyr::left_join(
    y = smartransf,
    by = dplyr::join_by(boite_ID)
  ) |>
  # calcul pourcentage du total par boite
  dplyr::mutate(pourcent = nombre / nb_total * 100)

smartplot <- smardataPlot |>
  # graphique en colonne : nb smarties par couleur
  ggplot(aes(x = nom, y = pourcent, fill = col)) +
  # Afficher les colonnes
  geom_col() +
  # Hypothèse nulle
  geom_hline(aes(yintercept = 1 / 8 * 100)) +
  facet_wrap(poids_g ~ boite_ID, scales = "free_x") +
  scale_fill_identity()

ggplot2::ggsave(
  filename = file.path("output", "smart_plot_wrap.png"),
  plot = smartplot,
  width = 12,
  height = 6
)

## Calcul d'entropie
# Construit la matrice de commaunauté des smarties dans chaque boîte
community_matrix <- smartdata |>
  dplyr::group_by(boite_ID, nom) |>
  dplyr::summarise(nombre = sum(nombre), .groups = 'drop') |>
  tidyr::pivot_wider(
    names_from = nom,
    values_from = nombre,
    values_fill = list(nombre = 0)
  ) |>
  dplyr::select(-boite_ID)


# Calculate richness
richness <- specnumber(community_matrix)

# Calculate Shannon's Entropy
shannon_entropy <- diversity(community_matrix, index = "shannon")

# Calculate Simpson's Entropy
simpson_entropy <- diversity(community_matrix, index = "simpson")
# Équitabilité de Pielou
evenness <- shannon_entropy / log(richness)

# Tableau sommaire des statistiques d'entropie
tibble(
  richness,
  shannon_entropy,
  simpson_entropy,
  evenness
)
