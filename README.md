# 📊 Analyse Exploratoire et Prédictive - Dataset Titanic

> Pipeline complet de Data Science en R : de l'analyse exploratoire (EDA) à la modélisation prédictive (Machine Learning) pour prédire la survie des passagers du Titanic.

## 🎯 Objectif du Projet
Ce projet vise à analyser les données historiques du naufrage du Titanic afin d'identifier les facteurs de survie (âge, sexe, classe, etc.) et de construire des modèles de Machine Learning capables de prédire si un passager aurait survécu.

## 📁 Structure du projet
- `titanicAnalyse.R` — Script R complet contenant tout le pipeline (nettoyage, ACP, modélisation).
- `data/` — Dossier contenant les jeux de données bruts (`train.csv`, `test.csv`).
- `train_clean.csv` — Jeu de données nettoyé et enrichi, prêt pour la modélisation.

## 🚀 Librairies requises
Pour exécuter ce script, vous aurez besoin des packages R suivants :
```R
install.packages(c(
  "tidyverse", "moments", "gridExtra", "corrplot", 
  "FactoMineR", "factoextra", "psych", "vcd", 
  "randomForest", "e1071", "caret", "pROC"
))
