# ============================================================================
# PROJET TITANIC – ANALYSE UNIVARIÉE COMPLÈTE (II-2.1 À II-2.5)
# ============================================================================

# --------------------------
# 0. INITIALISATION ET CHARGEMENT DES LIBRAIRIES
# --------------------------
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(moments)) install.packages("moments")
if (!require(gridExtra)) install.packages("gridExtra")

library(tidyverse)
library(moments)
library(gridExtra)

options(scipen = 999) # Désactive la notation scientifique

# --------------------------
# 1. CHARGEMENT ET PRÉPARATION DES DONNÉES BRUTES (df)
# --------------------------
# VÉRIFIEZ ET CORRIGEZ CE CHEMIN SI NÉCESSAIRE.
df <- read.csv("C:/Users/MANOUR/Downloads/titanic.csv")

# Renommage des colonnes (pour utiliser les noms standard SibSp et Parch)
df <- df %>%
  rename(
    SibSp = Siblings.Spouses.Aboard,
    Parch = Parents.Children.Aboard
  )

# Conversion des variables catégorielles (pour l'analyse statistique)
df$Survived <- as.factor(df$Survived)
df$Pclass <- as.factor(df$Pclass)
df$Sex <- as.factor(df$Sex)

# Variables explicatives à analyser
variables_explicatives <- c("Pclass", "Sex", "Age", "SibSp", "Parch", "Fare")

cat("✓ Données chargées et prêtes pour l'analyse.\n")

# --------------------------
# 2. DÉFINITION DE LA FONCTION STATISTIQUE ET D'INTERPRÉTATION
# --------------------------
#' Calcule les statistiques descriptives et interprète la distribution d'une variable.
calculate_stats_and_interpret <- function(data, var) {
  
  x <- data[[var]]
  n_missing <- sum(is.na(x))
  pct_missing <- round(n_missing / length(x) * 100, 2)
  
  result <- list(
    Variable = var,
    Type = "N/A",
    N_Manquants = n_missing,
    Pourc_Manquants = pct_missing,
    Mediane = NA,
    Skewness = NA,
    Mode = NA,
    Pourc_Mode = NA,
    Distribution = ""
  )
  
  # ---- Cas Numérique ----
  if (is.numeric(x)) {
    x_clean <- na.omit(x)
    if (length(x_clean) > 1) {
      result$Type <- "Numérique"
      result$Mediane <- round(median(x_clean), 2)
      result$Skewness <- round(moments::skewness(x_clean), 3)
      
      # Interprétation de la distribution (Symétrie)
      if (abs(result$Skewness) < 0.5)
        result$Distribution <- "Symétrique"
      else if (result$Skewness > 0)
        result$Distribution <- "Asymétrique droite"
      else
        result$Distribution <- "Asymétrique gauche"
    }
  }
  
  # ---- Cas Catégoriel (Facteur ou Caractère) ----
  else if (is.factor(x) || is.character(x)) {
    x_clean <- na.omit(x)
    result$Type <- "Catégorielle"
    
    # Mode et pourcentage
    freq_table <- table(x_clean)
    if (length(freq_table) > 0) {
      mode_val <- names(which.max(freq_table))
      result$Mode <- mode_val
      
      freq_mode <- max(freq_table) / length(x_clean) * 100
      result$Pourc_Mode <- round(freq_mode, 2)
      
      # Interprétation du déséquilibre
      if (freq_mode > 70)
        result$Distribution <- "Très déséquilibrée"
      else
        result$Distribution <- "Déséquilibrée"
    }
  }
  
  return(as.data.frame(result))
}

# --------------------------
# 3. EXÉCUTION STATISTIQUE (II-2.1 & II-2.2)
# --------------------------
stats_summary <- map_df(variables_explicatives,
                        ~ calculate_stats_and_interpret(df, .x))

cat("\n=== II-2.1 & II-2.2 : RÉSUMÉ STATISTIQUE ET INTERPRÉTATION ===\n")
print(stats_summary %>% 
        select(Variable, Type, Pourc_Manquants, Mediane, Skewness, Mode, Pourc_Mode, Distribution))

# --------------------------
# 4. TRAITEMENT DES MANQUANTS ET OUTLIERS (II-2.3 & II-2.4)
# --------------------------
train_clean <- df # Copie de travail pour le nettoyage

cat("\n\n=== II-2.3 & II-2.4 : GESTION DES MANQUANTS ET OUTLIERS ===\n")

# II-2.3 : Gestion des valeurs manquantes (Age)
median_age <- median(train_clean$Age, na.rm = TRUE)
train_clean$Age[is.na(train_clean$Age)] <- median_age
cat("✓ Age : Manquants imputés par la Médiane.\n")

# II-2.4 : Gestion des valeurs aberrantes (Outliers)
# Transformation Log de Fare
train_clean$Fare_log <- log1p(train_clean$Fare)
# Capping SibSp / Parch
train_clean$SibSp_capped <- ifelse(train_clean$SibSp > 5, 5, train_clean$SibSp)
train_clean$Parch_capped <- ifelse(train_clean$Parch > 4, 4, train_clean$Parch)
cat("✓ Outliers traités (Logarithme de Fare, Capping de SibSp/Parch).\n")

# Sauvegarde du fichier nettoyé
write.csv(train_clean, "train_clean.csv", row.names = FALSE)
cat("✓ Jeu de données nettoyé sauvegardé dans 'train_clean.csv'.\n")


# --------------------------
# 5. VISUALISATION (II-2.5)
# --------------------------
cat("\n\n=== II-2.5 : VISUALISATION (Plots générés) ===\n")

# CRÉATION DU DOSSIER DE SORTIE
if (!dir.exists("outputs/plots")) dir.create("outputs/plots", recursive = TRUE)

# --------------------------
# 5.1. PLOTS DE COMPARAISON AVANT/APRÈS TRAITEMENT (OUTLIERS)
# --------------------------
cat("• Génération des plots de comparaison pour l'effet du traitement des Outliers.\n")

# Préparation des données pour Fare (Original vs Log)
fare_compare_df <- data.frame(
  Value = c(train_clean$Fare, train_clean$Fare_log),
  Variable = factor(rep(c("Fare_Original", "Fare_Log_Traité"), each = nrow(train_clean)))
)
# Préparation des données pour SibSp (Original vs Capped)
sibsp_compare_df <- data.frame(
  Value = c(df$SibSp, train_clean$SibSp_capped),
  Variable = factor(rep(c("SibSp_Original", "SibSp_Capped"), each = nrow(train_clean)))
)

# Plot 1: Boxplot de comparaison de Fare
p_fare_box <- ggplot(fare_compare_df, aes(x = Variable, y = Value, fill = Variable)) +
  geom_boxplot() + scale_fill_manual(values = c("Fare_Original" = "lightcoral", "Fare_Log_Traité" = "lightskyblue")) +
  ggtitle("Comparaison : Boxplot de Fare (Original vs Log)") + theme_minimal() + theme(legend.position = "none")

# Plot 2: Histogramme de comparaison de Fare
p_fare_hist <- ggplot(fare_compare_df, aes(x = Value, fill = Variable)) +
  geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
  ggtitle("Comparaison : Distribution de Fare") + theme_minimal() + labs(x="Valeur", y="Count")

# Plot 3: Boxplot de comparaison de SibSp
p_sibsp_box <- ggplot(sibsp_compare_df, aes(x = Variable, y = Value, fill = Variable)) +
  geom_boxplot() + scale_fill_manual(values = c("SibSp_Original" = "lightcoral", "SibSp_Capped" = "lightskyblue")) +
  ggtitle("Comparaison : Boxplot de SibSp (Original vs Capped)") + theme_minimal() + theme(legend.position = "none")

# Combinaison et sauvegarde des plots de comparaison
g_comparison <- grid.arrange(p_fare_box, p_fare_hist, p_sibsp_box, ncol = 3)
ggsave("outputs/plots/univariate_comparison_outliers.png", g_comparison, width = 18, height = 6)
cat("  • Plot de comparaison des traitements généré.\n")

# --------------------------
# 5.2. PLOTS DES VARIABLES NETTOYÉES (II-2.5) - Numériques
# --------------------------
numeric_vars <- c("Age", "Fare_log", "SibSp_capped", "Parch_capped")

for (var in numeric_vars) {
  
  # a. Courbes/Histogramme (Distribution des valeurs)
  p1 <- ggplot(train_clean, aes(x = .data[[var]])) +
    geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "#007bff", alpha = 0.7) +
    geom_density(color = "black", linewidth = 1) +
    theme_minimal() + ggtitle(paste("Distribution de", var))
  
  # Boxplot (Valeurs aberrantes et Quartiles)
  p2 <- ggplot(train_clean, aes(y = .data[[var]], x = 1)) +
    geom_boxplot(fill = "#dc3545") + theme_minimal() +
    ggtitle(paste("Boxplot de", var)) + theme(axis.title.x = element_blank(), axis.text.x = element_blank())

  # QQ-Plot (Test de normalité)
  p3 <- ggplot(train_clean, aes(sample = .data[[var]])) +
    stat_qq() + stat_qq_line() + theme_minimal() + ggtitle(paste("QQ-Plot de", var))
  
  # Combinaison et sauvegarde du graphique 3-en-1
  g_numeric <- grid.arrange(p1, p2, p3, ncol = 3)
  ggsave(paste0("outputs/plots/univariate_plot_", var, ".png"), g_numeric, width = 15, height = 5)
  cat(sprintf("  • Plot 3-en-1 généré pour %s.\n", var))
}

# --------------------------
# 5.3. PLOTS DES VARIABLES NETTOYÉES (II-2.5) - Catégorielles
# --------------------------
cat_vars <- c("Pclass", "Sex", "Survived")

for (var in cat_vars) {
  
  # Diagramme en Barres (Distribution des fréquences)
  p <- ggplot(train_clean, aes(x = .data[[var]])) +
    geom_bar(aes(y = after_stat(prop * 100), fill = .data[[var]]), 
             stat = "count", show.legend = FALSE) +
    geom_text(aes(y = after_stat(prop * 100), label = paste0(round(after_stat(prop * 100), 1), "%")),
              stat = "count", vjust = -0.5) +
    theme_minimal() + ylab("Pourcentage (%)") + ggtitle(paste("Distribution des fréquences de", var))
  
  # Sauvegarde
  ggsave(paste0("outputs/plots/univariate_barplot_", var, ".png"), p, width = 8, height = 6)
  cat(sprintf("  • Barplot de fréquences généré pour %s.\n", var))
}

cat("\n✅ ANALYSE UNIVARIÉE TERMINÉE : L'exécution complète du script générera toutes les statistiques et plots.\n")




# Matrice de corrélation de Pearson
numeric_vars_final <- c("Age", "Fare_log", "SibSp_capped", "Parch_capped")
correlation_matrix <- cor(train_clean[numeric_vars_final])

print("1. Matrice de corrélation de Pearson entre variables numériques:")
print(round(correlation_matrix, 3))
  

# =======================================================
# II-3-1: VISUALISATION ET CORRÉLATION (X vs X)
# =======================================================

# -------------------------------------------------------
# 1. Définition des Fonctions de Panneau (Pour la Matrice)
#    (CES FONCTIONS DOIVENT ÊTRE DÉFINIES EN PREMIER)
# -------------------------------------------------------

# Fonction pour ajouter les corrélations sur les plots (Personnalisée)
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y, use = "complete.obs"))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  
  # La taille du texte dépend de la force de la corrélation (Multiplication par 'r')
  text(0.5, 0.5, txt, cex = cex.cor * r) 
}

# Fonction pour tracer une ligne de lissage (Panel d'en bas)
panel.smooth <- function (x, y, col = "blue", bg = NA, pch = par("pch"), 
                          cex = 1, col.smooth = "red", span = 2/3, iter = 3, ...) {
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok)) 
    lines(stats::lowess(x[ok], y[ok], f = span, iter = iter), 
          col = col.smooth, ...)
}

# -------------------------------------------------------
# 2. Exécution de la Matrice de Nuages de Points
# -------------------------------------------------------

# Note: Remplacez 'train_clean' par votre data frame nettoyé réel
# et 'numeric_vars_final' par la liste des colonnes numériques
# (Ex: c("Age", "Fare_log", "SibSp_capped", "Parch_capped"))

# Exemple d'appel (vous devez avoir train_clean et numeric_vars_final définis)
numeric_vars_final <- c("Age", "Fare_log", "SibSp_capped", "Parch_capped")
pairs(train_clean[numeric_vars_final], lower.panel = panel.smooth, upper.panel = panel.cor, 
    main = "Matrice de Nuages de Points et Corrélation")

print("2. Les fonctions 'panel.cor' et 'panel.smooth' sont maintenant définies.")
print("Vous pouvez maintenant appeler la fonction 'pairs' sans erreur.")


# ============================================================
# CORRECTION DE L'ERREUR ET EXÉCUTION DU II-3-2 (Y vs X)
# ============================================================

# --------------------------
# 0. Vérification du jeu de données (Assurez-vous que ces variables existent)
# Supposons que train_clean et numeric_vars_final existent:
numeric_vars_final <- c("Age", "Fare_log", "SibSp_capped", "Parch_capped")
# --------------------------

# --------------------------
# 1. Préparation de la Variable Cible Survived (Pour garantir qu'elle est 0 ou 1)
# --------------------------

# Si Survived est un facteur (0, 1), as.numeric() le convertirait en (1, 2).
# Cette méthode assure une conversion en 0 et 1
train_clean$Survived_numeric <- as.integer(as.character(train_clean$Survived))
# OU si Survived est déjà 0 et 1 comme caractère:
# train_clean$Survived_numeric <- as.numeric(train_clean$Survived)

# --------------------------
# 2. Corrélation de Pearson entre Survived et X numériques
# --------------------------

# IMPORTANT : Utiliser la syntaxe as.matrix pour s'assurer que cor() accepte la combinaison
correlations_Y_X_numeric <- cor(train_clean$Survived_numeric, as.matrix(train_clean[numeric_vars_final]))

print("1. Corrélation de Pearson entre Survived et X numériques:")
print(round(correlations_Y_X_numeric, 3))


# --------------------------
# 3. Tests T de Student (Y vs X numériques)
# --------------------------
numeric_vars_final <- c("Age", "Fare_log", "SibSp_capped", "Parch_capped")

print("\nTests T de Student (Impact de X sur la moyenne de Survived):")
for (var in numeric_vars_final) {
  # Assurez-vous que Survived est un facteur pour t.test
  if (!is.factor(train_clean$Survived)) {
    train_clean$Survived <- as.factor(train_clean$Survived)
  }
  t_test_result <- t.test(train_clean[[var]] ~ train_clean$Survived, data = train_clean)
  print(paste("   T-Test pour", var, ": p-value =", round(t_test_result$p.value, 5)))
}

# --------------------------
# 4. Test du Chi-Carré (Y vs X catégorielles)
# --------------------------
cat_vars_final <- c("Pclass", "Sex")

print("\nTests du Chi-Carré (Relation entre Survived et X catégoriel):")
for (var in cat_vars_final) {
  chi_sq_result <- chisq.test(table(train_clean$Survived, train_clean[[var]]))
  print(paste("   Chi-Carré pour", var, ": p-value =", round(chi_sq_result$p.value, 5)))
}

# --------------------------
# 5. Interprétation des sous-groupes (Multicolinéarité, si vous avez la matrice de corrélation X vs X)
# --------------------------
print("\n4. Analyse des sous-groupes (Multicolinéarité):")
# Si vous avez la matrice X vs X :
# print(round(correlation_matrix, 3)) 
print("Aucune analyse de sous-groupes n'est requise car la corrélation la plus élevée est de 0.45 (SibSp vs Parch), ce qui est inférieur au seuil critique de 0.7.")


# Assurez-vous que Survived est un vecteur numérique (0 ou 1) pour la corrélation
# Si Survived était un facteur (0, 1) ou (No, Yes), cette étape est cruciale:
train_clean$Survived_numeric <- as.integer(as.character(train_clean$Survived))
# Si elle est déjà 0 et 1 comme facteur, utilisez simplement: as.numeric(train_clean$Survived) - 1

numeric_vars_final <- c("Age", "Fare_log", "SibSp_capped", "Parch_capped")

# Calcul de la corrélation
correlations_Y_X_numeric <- cor(train_clean$Survived_numeric, train_clean[numeric_vars_final])

print("1. Corrélation de Pearson entre Survived (Y) et X numériques:")
print(round(correlations_Y_X_numeric, 4))




# ==========================================================
# ÉTAPE II-3-2 : RELATION Y (Survived) vs X Numériques
# ==========================================================

# ==========================================================
# ÉTAPE II-3-2 : RELATION Y (Survived) vs X Numériques
# ==========================================================

library(ggplot2)
library(gridExtra)
library(corrplot) # Nouvelle librairie pour les heatmaps de corrélation
# Si vous n'avez pas corrplot, exécutez : install.packages("corrplot")


# --- Définition des variables ---
# Assurez-vous que Survived est un facteur pour les tests et plots
train_clean$Survived <- as.factor(train_clean$Survived)
# Assurez-vous que Survived_numeric est un vecteur numérique (0 ou 1) pour la corrélation
train_clean$Survived_numeric <- as.numeric(train_clean$Survived) - 1
numeric_vars_final <- c("Age", "Fare_log", "SibSp_capped", "Parch_capped")


### 1. CALCUL DU COEFFICIENT DE CORRÉLATION DE PEARSON (Y vs X)
print("1. Corrélation de Pearson entre Survived (Y) et X numériques:")
# Nous créons ici une matrice qui inclut Y (Survived) et tous les X
vars_for_corr_Y_X <- c("Survived_numeric", numeric_vars_final)
corr_matrix_Y_X <- cor(train_clean[vars_for_corr_Y_X])
print(round(corr_matrix_Y_X, 4))


### 2. TEST DE STUDENT (Significativité de la différence de moyenne)
print("\n2. Tests T de Student (Impact de X sur la moyenne de Survived):")
t_test_results <- list()
for (var in numeric_vars_final) {
  t_test_result <- t.test(train_clean[[var]] ~ train_clean$Survived, data = train_clean)
  t_test_results[[var]] <- t_test_result
  print(paste("   T-Test pour", var, ": p-value =", round(t_test_result$p.value, 8)))
}


### 3. VISUALISATION (PLOTS)

# 3.1. Boxplots (pour l'analyse de la distribution)
num_plots_Y_X <- list()
for (var in numeric_vars_final) {
  p <- ggplot(train_clean, aes_string(x = "Survived", y = var, fill = "Survived")) +
    geom_boxplot(alpha = 0.7, outlier.colour = "red") +
    labs(title = paste("Impact de", var, "sur la Survie"),
         x = "Survived (0=Décédé, 1=Survécu)",
         y = var) +
    scale_fill_manual(values = c("0" = "#F8766D", "1" = "#00BFC4")) +
    theme_minimal() +
    theme(legend.position = "none", plot.title = element_text(size = 10))
  
  num_plots_Y_X[[var]] <- p
}

print("\n3.1. Visualisation : Boxplots (Y vs X numériques) :")
grid.arrange(grobs = num_plots_Y_X, ncol = 2, 
             top = "Visualisation Bivariée : Impact des Variables Numériques sur la Survie")


# 3.2. Heatmap de corrélation (pour la vue synthétique des coefficients)
print("\n3.2. Heatmap de Corrélation (Y vs X):")
# Nous utilisons la matrice calculée en étape 1
corrplot(corr_matrix_Y_X, 
         method = "color", # Utilise la couleur pour représenter la corrélation
         type = "upper",   # Affiche seulement la partie supérieure de la matrice
         order = "hclust", # Réorganise les variables par groupement
         addCoef.col = "black", # Ajoute les coefficients sur les carrés
         tl.col = "black", # Couleur du texte des labels
         tl.srt = 45, # Rotation des labels
         diag = FALSE, # N'affiche pas la diagonale (car corrélation avec soi-même)
         title = "Heatmap de Corrélation : Survie (Y) et Variables Numériques (X)")

# ==========================================================
# ÉTAPE II-3-2 : TEST ANOVA (Équivalent du T-Test)
# ==========================================================

print("1. Tests ANOVA (Impact de X sur la moyenne de Survived) :")

# Assurez-vous que Survived est un facteur
train_clean$Survived <- as.factor(train_clean$Survived)
numeric_vars_final <- c("Age", "Fare_log", "SibSp_capped", "Parch_capped")

# Boucle pour exécuter l'ANOVA pour chaque variable numérique
anova_results <- list()
for (var in numeric_vars_final) {
  # Formule de l'ANOVA : variable numérique ~ variable catégorielle
  aov_formula <- as.formula(paste(var, "~ Survived"))
  aov_model <- aov(aov_formula, data = train_clean)
  
  # Extraction de la p-value
  summary_aov <- summary(aov_model)
  p_value <- summary_aov[[1]][["Pr(>F)"]][1]
  
  anova_results[[var]] <- summary_aov
  print(paste("   ANOVA pour", var, ": p-value =", round(p_value, 8)))
}

# Les p-values de cette ANOVA devraient être identiques ou extrêmement proches 
# des p-values obtenues lors de votre Test T de Student précédent.



# ==========================================================
# ÉTAPE II-3-2 : RELATION Y (Survived) vs X Numériques
# ==========================================================

# ==========================================================
# ÉTAPE II-3-2 : VISUALISATION Y vs X (Histograms de Densité)
# ==========================================================

# Chargement des librairies nécessaires
library(ggplot2)
library(gridExtra)

# --- Définition des variables ---
# Convertir 'Survived' en facteur pour les couleurs de remplissage
train_clean$Survived <- as.factor(train_clean$Survived) 

# Liste des variables numériques à visualiser
numeric_vars_final <- c("Age", "Fare_log", "SibSp_capped", "Parch_capped")

# 1. Créer une liste pour stocker chaque Histogramme
density_plots_Y_X <- list()

# 2. Boucle pour générer l'Histogramme de densité pour chaque variable
for (var in numeric_vars_final) {
  p <- ggplot(train_clean, aes_string(x = var, fill = "Survived", colour = "Survived")) +
    # Dessiner l'histogramme de densité
    # aes(y = ..density..) normalise la hauteur totale des barres à 1 pour la comparaison
    geom_histogram(aes(y = after_stat(density)), bins = 30, position = "identity", alpha = 0.5) +
    # Superposer la courbe de densité (plus lisse pour la tendance)
    geom_density(alpha = 0.2, linewidth = 1) + 
    
    # Définir les titres et labels
    labs(title = paste("Distribution de", var, "par Survie"),
         x = var,
         y = "Densité") +
    # Définir les couleurs personnalisées
    scale_fill_manual(values = c("0" = "tomato", "1" = "skyblue"), name = "Survived", labels = c("Décédé (0)", "Survécu (1)")) +
    scale_colour_manual(values = c("0" = "red", "1" = "blue"), name = "Survived", labels = c("Décédé (0)", "Survécu (1)")) +
    theme_minimal() +
    theme(plot.title = element_text(size = 12, face = "bold"))
  
  density_plots_Y_X[[var]] <- p
}

# 3. Afficher les Histograms dans une grille de 2 colonnes
print("Affichage des Histograms de Densité pour Y vs X :")
grid.arrange(grobs = density_plots_Y_X, ncol = 2, 
             top = "Analyse Bivariée : Distributions de X par Statut de Survie")

# ==========================================================
# ÉTAPE II-3-2 - 3 : TEST T DE STUDENT
# ==========================================================

# Définition des variables
# Assurez-vous que Survived est un facteur pour le test
train_clean$Survived <- as.factor(train_clean$Survived)
numeric_vars_final <- c("Age", "Fare_log", "SibSp_capped", "Parch_capped")


print("Résultats du Test T de Student (Comparaison des moyennes entre les groupes Survived=0 et Survived=1):")
t_test_results <- list()

# Boucle pour exécuter le Test T de Student pour chaque variable
for (var in numeric_vars_final) {
  # La formule : variable numérique ~ variable de groupe
  t_test_result <- t.test(train_clean[[var]] ~ train_clean$Survived, data = train_clean)
  t_test_results[[var]] <- t_test_result
  
  # Affichage de la p-value
  print(paste("   Test T pour", var, ": p-value =", round(t_test_result$p.value, 8)))
}

# La p-value est le critère de décision : si p-value < 0.05, la différence est significative.



# ==========================================================
# PRÉPARATION DES DONNÉES POUR L'ACP
# ==========================================================
library(FactoMineR) # Pour l'ACP
library(factoextra) # Pour la visualisation

# Définir le jeu de données pour l'ACP (X numériques)
# Assurez-vous d'utiliser les variables numériques nettoyées
# Si vous avez créé FamilySize, incluez-la ici
numeric_vars_for_pca <- train_clean[, c("Age", "Fare_log", "SibSp_capped", "Parch_capped")]

# Si vous avez des valeurs manquantes (NA) après le nettoyage, l'ACP ne fonctionnera pas.
# Assurez-vous que le data.frame est complet (NA doit être géré).

# ==========================================================
# 1. VÉRIFICATION DES CONDITIONS D'ACP
# ==========================================================
library("psych")
print("1. Tests d'adéquation pour l'ACP (Bartlett et KMO):")

# Test de Sphéricité de Bartlett
# Si p-value < 0.05, les variables sont suffisamment corrélées.
bartlett_test <- cortest.bartlett(cor(numeric_vars_for_pca), n = nrow(numeric_vars_for_pca))
print("Test de Bartlett:")
print(bartlett_test)

# Mesure KMO
# Si KMO > 0.6, l'ACP est appropriée.
kmo_result <- KMO(numeric_vars_for_pca)
print("Mesure KMO:")
print(kmo_result)

# Conclusion : Si KMO > 0.6 et Bartlett p-value < 0.05, on peut continuer l'ACP.


# ==========================================================
# MISE À JOUR DU JEU DE DONNÉES POUR L'ACP (Variable Supplémentaire)
# ==========================================================
library(FactoMineR) 
library(factoextra)

# Définir le jeu de données pour l'ACP (X numériques)
# Le jeu de données doit inclure Survived pour l'illustrer
# Assurez-vous que Survived est un facteur
train_clean$Survived <- as.factor(train_clean$Survived) 

# Liste des variables X qui participent à l'ACP (colonnes 1 à 4)
numeric_vars_for_pca <- train_clean[, c("Age", "Fare_log", "SibSp_capped", "Parch_capped")]

# L'indice de la colonne qui doit être illustrée (si nous la mettions dans le même df)
# Cependant, PCA de FactoMineR permet de spécifier la variable Y séparément:

# Appliquer l'ACP en spécifiant la variable catégorielle illustrative (Y)
# Ici, nous n'incluons pas Y dans le jeu de données "X", mais nous le gardons séparé
res.pca <- PCA(numeric_vars_for_pca, 
               scale.unit = TRUE, 
               ncp = 4,           
               graph = FALSE)

# L'étape 4b. de l'interprétation utilisera la variable Survived pour colorer la projection.



# ==========================================================
# 3. APPLICATION DE L'ACP ET ANALYSE DE LA VARIANCE
# ==========================================================
library(FactoMineR) 
library(factoextra)

# Définition des variables participantes (X numériques)
numeric_vars_for_pca <- train_clean[, c("Age", "Fare_log", "SibSp_capped", "Parch_capped")]

# Appliquer l'ACP (Standardisation cruciale avec scale.unit = TRUE)
res.pca <- PCA(numeric_vars_for_pca, 
               scale.unit = TRUE, 
               ncp = 4,           
               graph = FALSE)

# Afficher les valeurs propres (Eigenvalues) et la variance expliquée
print("3. Tableau des Valeurs Propres et Variance Expliquée :")
eig.val <- get_eigenvalue(res.pca)
print(eig.val)

# Graphique de l'éboulis (Scree Plot) pour déterminer le nombre de composantes
print("Graphique de l'éboulis (Scree Plot) :")
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 100))
# 

# ==========================================================
# 4. INTERPRÉTATION DES RÉSULTATS
# ==========================================================

# 4.a. Interprétation des Variables (Cercle des Corrélations)
print("\n4.a. Cercle des Corrélations (Projection des Variables) :")
fviz_pca_var(res.pca,
             col.var = "contrib", # Couleur selon la contribution à l'axe
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,    
             title = "Cercle des Corrélations (Variables Numériques)"
)
# 

# 4.b. Projection des Individus (Colorée par la variable Y - Survived)
# Assurez-vous que Survived est un facteur
train_clean$Survived_cat <- as.factor(train_clean$Survived) 

print("\n4.b. Projection des Individus (Colorée par Survie) :")
fviz_pca_ind(res.pca,
             geom.ind = "point", 
             col.ind = train_clean$Survived_cat, # Colorer par Survie
             palette = c("0" = "#F8766D", "1" = "#00BFC4"), # Couleurs Décédé/Survécu
             addEllipses = TRUE, # Ajouter des ellipses de confiance
             legend.title = "Survived",
             title = "Projection des Individus sur PC1 et PC2"
)
# # 1. Liste des variables catégorielles
cat_vars <- c("Pclass", "Sex", "Survived")

# 2. Boucle de génération des graphiques
for (var in cat_vars) {
  p <- ggplot(train_clean, aes(x = .data[[var]], fill = .data[[var]])) +
    geom_bar(aes(y = after_stat(count / sum(count) * 100))) +
    geom_text(aes(y = after_stat(count / sum(count) * 100), 
                  label = paste0(round(after_stat(count / sum(count) * 100), 1), "%")),
              stat = "count", vjust = -0.5) +
    labs(title = paste("Distribution de", var), y = "Pourcentage (%)", x = var) +
    theme_minimal() + theme(legend.position = "none")
  
  print(p)
}
# ==========================================================
# APPLICATION DE L'ACM (Variables Catégorielles)
# ==========================================================
library(FactoMineR)
library(factoextra)

# 1. Préparation des données (uniquement les facteurs)
cat_data_acm <- train_clean[, c("Pclass", "Sex", "Survived")]

# 2. Exécution de l'ACM
res.mca <- MCA(cat_data_acm, graph = FALSE)

# 3. Visualisation du nuage des modalités
fviz_mca_var(res.mca, 
             repel = TRUE, 
             ggtheme = theme_minimal(),
             title = "ACM : Relation entre Classe, Sexe et Survie")


# Test d'association entre le Sexe et la Survie
chi2_sex <- chisq.test(table(train_clean$Sex, train_clean$Survived))
print(chi2_sex)

# Test d'association entre la Classe et la Survie
chi2_pclass <- chisq.test(table(train_clean$Pclass, train_clean$Survived))
print(chi2_pclass)

if (!require(vcd)) install.packages("vcd") # Installe si absent
library(vcd) # Charge la librairie

# Relancer le graphique
mosaic(~ Sex + Survived, data = train_clean, 
       main = "Mosaïque : Sexe vs Survie", 
       shade = TRUE, legend = TRUE)

# 3. Mosaic Plot pour la Classe vs Survie
mosaic(~ Pclass + Survived, data = train_clean, 
       main = "Mosaïque : Classe vs Survie", 
       shade = TRUE, legend = TRUE)


# 1. Création de la variable 'Family_Size'
# SibSp (frères/conjoints) + Parch (parents/enfants) + 1 (le passager)
train_clean$Family_Size <- train_clean$SibSp + train_clean$Parch + 1

# 2. Création de la variable 'Is_Alone'
# 1 = seul, 0 = en famille
train_clean$Is_Alone <- ifelse(train_clean$Family_Size == 1, 1, 0)

# 3. Extraction du Titre (Title) à partir de la colonne Name
# Exemple : "Braund, Mr. Owen Harris" → "Mr"
train_clean$Title <- gsub("^.*, ([A-Za-z]+)\\..*$", "\\1", train_clean$Name)

# 4. Regroupement des titres similaires
train_clean$Title <- ifelse(train_clean$Title %in% c("Mlle", "Ms", "Mme"), 
                            "Miss", train_clean$Title)

# 5. Regroupement des titres rares
train_clean$Title <- ifelse(train_clean$Title %in% c("Mr", "Mrs", "Miss", "Master"),
                            train_clean$Title, "Rare")

# 6. Conversion en facteur
train_clean$Title <- as.factor(train_clean$Title)


# 1. Encodage Binaire Simple pour le Sexe (0 ou 1)
# Si c'est "male", on met 1. Sinon (female), on met 0.
train_clean$Sex_Code <- ifelse(train_clean$Sex == "male", 1, 0)

# 2. Encodage de la Classe (On garde Pclass en numérique 1, 2, 3 ou on utilise des dummies)
# Pour une régression logistique, il vaut mieux encoder Pclass séparément, 
# mais pour simplifier ici, utilisons model.matrix juste pour Pclass si besoin, 
# ou convertissons simplement en numérique :
train_clean$Pclass_Code <- as.numeric(train_clean$Pclass)

# 3. Mise à l'Échelle (Scaling) des variables continues
# (Age, Fare, Family_Size)
vars_scaled <- scale(train_clean[, c("Age", "Fare_log", "Family_Size")])

# 4. Création du DataFrame final pour le Machine Learning
train_ml <- data.frame(
  Survived = train_clean$Survived,   # La cible
  Sex = train_clean$Sex_Code,        # Sexe (0 ou 1)
  Pclass = train_clean$Pclass_Code,  # Classe (1, 2, 3)
  vars_scaled                        # Variables centrées réduites
)

# 5. Vérification
print("✅ Données prêtes avec Sex en une seule colonne :")
head(train_ml)


# --- 0. Chargement des librairies ---
if(!require(randomForest)) install.packages("randomForest")
if(!require(e1071)) install.packages("e1071") # Pour SVM
if(!require(ggplot2)) install.packages("ggplot2")
library(randomForest)
library(e1071)
library(ggplot2)

# --- 1. Préparation et Split (70% Train / 30% Test) ---
set.seed(42) # Pour des résultats reproductibles
# Important : Survived doit être un facteur pour la classification
train_ml$Survived <- as.factor(train_ml$Survived)

index <- sample(1:nrow(train_ml), 0.7 * nrow(train_ml))
train_set <- train_ml[index, ]
test_set  <- train_ml[-index, ]

print("✅ Données divisées : Apprentissage et Test prêts.")

# --- 2. Entraînement des Modèles ---

# A. Régression Logistique
model_log <- glm(Survived ~ ., data = train_set, family = "binomial")
prob_log <- predict(model_log, newdata = test_set, type = "response")
pred_log <- ifelse(prob_log > 0.5, 1, 0)
acc_log <- mean(pred_log == test_set$Survived)

# B. Random Forest
model_rf <- randomForest(Survived ~ ., data = train_set, ntree = 500)
pred_rf <- predict(model_rf, newdata = test_set)
acc_rf <- mean(pred_rf == test_set$Survived)

# C. SVM (Support Vector Machine)
model_svm <- svm(Survived ~ ., data = train_set, kernel = "radial")
pred_svm <- predict(model_svm, newdata = test_set)
acc_svm <- mean(pred_svm == test_set$Survived)

# --- 3. Résumé des Performances ---
results <- data.frame(
  Modele = c("Regression Logistique", "Random Forest", "SVM"),
  Accuracy = c(acc_log, acc_rf, acc_svm)
)

print("--- 🏆 RÉSULTATS FINAUX (ACCURACY) ---")
print(results)

# --- 4. Visualisation (Histogramme Comparatif) ---
ggplot(results, aes(x = reorder(Modele, -Accuracy), y = Accuracy, fill = Modele)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = paste0(round(Accuracy * 100, 1), "%")), vjust = -0.5, size = 5) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(title = "Comparaison des Modèles de Machine Learning",
       subtitle = "Qui est le meilleur pour prédire la survie ?",
       x = "Algorithme",
       y = "Précision (Accuracy)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.position = "none")


# Installation des librairies d'évaluation avancée
if(!require(caret)) install.packages("caret")
if(!require(pROC)) install.packages("pROC")
library(caret)
library(pROC)

# --- PRÉPARATION ---
# Caret préfère des noms de classes explicites (pas juste 0 et 1)
# On renomme 0 -> "Decede", 1 -> "Survecu"
train_ml$Survived <- factor(train_ml$Survived, 
                            levels = c(0, 1), 
                            labels = c("Decede", "Survecu"))

# --- 1. VALIDATION CROISÉE K-FOLD (10-Fold) ---
# On configure le "Control" : 10 découpages (folds)
fitControl <- trainControl(method = "cv", 
                           number = 10, 
                           classProbs = TRUE, # Important pour ROC
                           summaryFunction = twoClassSummary) # Pour avoir l'AUC

print("⏳ Entraînement avec Cross-Validation (cela peut prendre quelques secondes)...")
# On entraîne le Random Forest via Caret
model_cv <- train(Survived ~ ., data = train_ml, 
                  method = "rf", 
                  trControl = fitControl, 
                  metric = "ROC") # On optimise pour l'AUC-ROC

# Résultat de la stabilité
print("--- Résultats de la Validation Croisée (Moyenne sur 10 tests) ---")
print(model_cv)

# --- 2. MATRICE DE CONFUSION DÉTAILLÉE ---
# On fait des prédictions sur le jeu de test (qu'on renomme aussi pour correspondre)
test_set$Survived <- factor(test_set$Survived, levels = c(0, 1), labels = c("Decede", "Survecu"))
preds_class <- predict(model_cv, newdata = test_set)

# Génération de la matrice riche
conf_mat <- confusionMatrix(preds_class, test_set$Survived)
print("--- Matrice de Confusion Détaillée ---")
print(conf_mat$table)
print(paste("Sensibilité (Capacité à trouver les Survivants) :", round(conf_mat$byClass['Sensitivity'], 2)))

# --- 3. COURBE ROC et AUC ---
# On calcule les probabilités au lieu des classes brutes
preds_prob <- predict(model_cv, newdata = test_set, type = "prob")

# Calcul de la courbe ROC (Sur la classe "Survecu")
roc_obj <- roc(test_set$Survived, preds_prob$Survecu)
auc_score <- auc(roc_obj)

# Affichage Graphique
plot(roc_obj, main = paste("Courbe ROC (AUC =", round(auc_score, 3), ")"), 
     col = "#007bff", lwd = 3, legacy.axes = TRUE)
legend("bottomright", legend = paste("AUC =", round(auc_score, 3)), fill = "#007bff")

# 1. On entraîne un modèle "Demo" sur les données brutes (faciles à comprendre)
library(randomForest)
rf_demo <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + Fare_log + Family_Size + Is_Alone, 
                        data = train_clean, 
                        ntree = 500)

# 2. Fonction de prédiction adaptée
test_destin <- function(pclass, sex, age, fare, sibsp, parch) {
  
  # Calculs intermédiaires
  fare_log_val <- log(fare + 1)
  f_size <- sibsp + parch + 1
  alone <- ifelse(f_size == 1, 1, 0)
  
  # Création du passager
  passager <- data.frame(
    Pclass = factor(pclass, levels = levels(train_clean$Pclass)),
    Sex = factor(sex, levels = levels(train_clean$Sex)),
    Age = age,
    Fare_log = fare_log_val,
    Family_Size = f_size,
    Is_Alone = alone
  )
  
  # Prédiction
  proba <- predict(rf_demo, newdata = passager, type = "prob")
  chance <- proba[, 2] * 100 # On prend la colonne "1" (Survie)
  
  # Résultat
  if(chance < 50) {
    print(paste("💀 DÉCÉDÉ... (Chance de survie :", round(chance, 1), "%)"))
    print(paste("   Profil :", sex, "de classe", pclass, "avec", f_size-1, "proches."))
  } else {
    print(paste("🍀 SURVÉCU ! (Chance de survie :", round(chance, 1), "%)"))
  }
}

print("✅ Modèle de démonstration prêt.")

# Test 1 : Le "Monsieur Malchance" (Homme, 3ème classe, Famille de 8 personnes)
test_destin(pclass = 3, sex = "male", age = 40, fare = 60, sibsp = 1, parch = 6)

# Test 2 : Jack Dawson (Devrait mourir cette fois)
test_destin(pclass = 3, sex = "male", age = 20, fare = 7.5, sibsp = 0, parch = 0)

test_destin(pclass = 1, sex = "female", age = 20, fare = 1.5, sibsp = 0, parch = 0)


