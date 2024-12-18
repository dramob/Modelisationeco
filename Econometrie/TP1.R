# Créer un dossier pour sauvegarder les outputs
dir.create("outputs", showWarnings = FALSE)

# ---- 1. Charger et préparer les données ----
lines <- readLines("varicelle.dat.txt")
lines <- lines[-1]  # Ignorer la première ligne descriptive
varicelle_data <- unlist(lapply(lines, function(line) {
  as.numeric(strsplit(trimws(line), "\\s+")[[1]])
}))

# Créer la série temporelle
varicelle_ts <- ts(varicelle_data, start = c(1931, 1), frequency = 12)

# ---- 2. Graphique de la série temporelle ----
png("outputs/serie_temporelle_varicelle.png", width = 800, height = 600)
plot(varicelle_ts, main = "Série temporelle des cas mensuels de varicelle",
     xlab = "Année", ylab = "Nombre de cas", col = "blue", lwd = 2)
grid()
dev.off()

# ---- 3. Décomposition de la série temporelle ----
decomposition <- decompose(varicelle_ts)

# Sauvegarder la décomposition
png("outputs/decomposition_varicelle.png", width = 800, height = 600)
plot(decomposition)
dev.off()

# ---- 4. Moyenne mensuelle ----
moyenne_mensuelle <- mean(varicelle_ts, na.rm = TRUE)

# Sauvegarder la moyenne dans un fichier texte
writeLines(paste("Nombre moyen de cas mensuel :", moyenne_mensuelle), 
           con = "outputs/moyenne_mensuelle.txt")

# ---- 5. Auto-corrélation ----
png("outputs/autocorrelations_varicelle.png", width = 800, height = 600)
acf(varicelle_ts, lag.max = 25, main = "Auto-corrélations des cas de varicelle")
dev.off()

# ---- 6. Évolutions mensuelles par année ----
png("outputs/evolutions_mensuelles_annees.png", width = 800, height = 600)
plot(varicelle_ts, type = "n", main = "Évolutions mensuelles par année",
     xlab = "Mois", ylab = "Nombre de cas")
for (year in 1931:1972) {
  lines(window(varicelle_ts, start = c(year, 1), end = c(year, 12)), col = "gray")
}
grid()
dev.off()

# ---- 7. Évolution annuelle ----
varicelle_annuelle <- aggregate(varicelle_ts, FUN = sum)

# Sauvegarder le graphique annuel
png("outputs/evolution_annuelle_varicelle.png", width = 800, height = 600)
plot(varicelle_annuelle, type = "b", col = "red", lwd = 2,
     main = "Évolution annuelle des cas de varicelle",
     xlab = "Année", ylab = "Nombre total de cas")
grid()
dev.off()

# Sauvegarder les données annuelles dans un CSV
write.csv(data.frame(Annee = time(varicelle_annuelle), Cas = varicelle_annuelle),
          "outputs/evolution_annuelle_varicelle.csv", row.names = FALSE)
