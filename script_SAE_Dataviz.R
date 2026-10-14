# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# SAE 2.06 - Analyse de données, reporting et datavisualisation
# Thématique: Analyse du traffic aérien au départ des aéroports de New York en 2023
# Auteurs: MOUELE Drecy | KINSIKLOUNON Ariane | BENNANI Selma | POUIT Caroline 
# Date de création: 02/03/2025
# Source: nycflights2023.txt | https://www.bts.gov/
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #



# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
#                                 Phase préliminaire
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #


# --------------------------------------------------------------------------------- #
## --- Chargement des données ---

# --- Information sur la structure numérique du fichier 
readLines(con = "../Data/nycflights2023.txt",
          n = 10)

# Importation des données 
dataset = read.table(file = "../Data/nycflights2023.txt", 
                     sep = "\t", 
                     header = TRUE, 
                     encoding = "UTF-8")

# Visualistaion d'un extrait des données 
head(x = dataset, 
     n = 10) 

# Visuation de la structure de l'objet "dataset" 
str(object = dataset)

summary(dataset) 

# ---------------------------------------------------------------------------- #
## --- Préparation des données ---

# Question 5: Transformation des variables quantitatives concernées 
within(dataset,{
  distance <- distance * 1.609;       # distance en km
  temp <- (temp - 32) * 5/9;          # temperature en °C
  dewp <- (dewp - 32) * 5/9;          # dewpoint en °C
  wind_speed <- wind_speed * 1.609;   # Wind_speed en km/h
  wind_gust <- wind_gust * 1.609;     # wind_gust en km/h
  precip <- precip * 25.4;            # precipitation en mm
  visib <- visib * 1.609;             # visibilité en km
  })  -> data1

head(x = data1, 
     n = 10) 


# Question 6: Création de variables d'intérêts 
# -- delay_dep -- 
data1$delay_dep <- ifelse(data1$dep_delay > 0, "Yes", "No")

# -- delay_arr -- 
data1$delay_arr <- ifelse(data1$arr_delay > 0, "Yes", "No")

# -- canceled -- 
data1$canceled <- ifelse(
  is.na(data1$dep_time) & 
    is.na(data1$arr_time) &
    is.na(data1$air_time), "Yes", "No")

# -- distance_class -- 
summary(data1$distance)
data1$distance_class <- cut(data1$distance,
                            breaks = quantile(data1$distance, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1)),
                            #labels = c("[0%-20%]", "]20%-40%]", "]40%-60%]", "]60%-80%]", "]80%-100%]"),
                            ordered = TRUE,
                            include.lowest = TRUE,
                            right = FALSE)
data1$distance_class <- factor(
  data1$distance_class)
  #labels = c("[0%-20%]", "]20%-40%]", "]40%-60%]", "]60%-80%]", "]80%-100%]"), (Pas utile)
  #ordered = TRUE), 

# -- dewp_class -- 
summary(data1$dewp)
data1$dewp_class <- cut(data1$dewp,
                        breaks = quantile(data1$dewp, probs = c(0, 0.25, 0.50, 0.75, 1), na.rm = TRUE), # Valeurs manquantes ignorées
                        #labels = c("[0%-25%]", "]25%-50%]", "]50%-75%]", "]75%-100%]"),
                        ordered = TRUE,
                        include.lowest = TRUE,
                        right = FALSE)
data1$dewp_class <- factor(
  data1$dewp_class)
  #labels = c("[0%-25%]", "]25%-50%]", "]50%-75%]", "]75%-100%]"),
  #ordered = TRUE)

str(data1)



# Question 7: Traitement du type des variables 
within(data1, {
  canceled <- as.factor(canceled)
  delay_dep <- as.factor(delay_dep)
  delay_arr <- as.factor(delay_arr)
  name <- as.factor(name)
  carrier <- as.factor(carrier)
  origin <- as.factor(origin)
  dest <- as.factor(dest)
  wind_dir <- as.numeric(wind_dir)
  dep_delay <- as.numeric(dep_delay)
  arr_delay <- as.numeric(arr_delay)
  air_time <- as.numeric(air_time)
}) -> data1
str(data1)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
#                                 Thématique 1 : retard des avions
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# --------------------------------------------------------------------------------- #
## --- Analyse des données ---

# Question 1 : Quel aéroport a connu le plus de vols retardés en 2023 ? 

# Filtre pour récupérer les vols retardés (décollage ou arrivée)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
#                                 Thématique 1 : retard des avions
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# --------------------------------------------------------------------------------- #
## --- Analyse des données ---

# Question 1 : Quel aéroport a connu le plus de vols retardés en 2023 ? 
# Filtre pour récupérer les vols retardés (décollage ou arrivée)
data_retard = subset(data1, 
                     (data1$delay_dep == "Yes")|(data1$delay_arr == "Yes"))
# Table pour les retards par aéroport
tab_retard = round(100*prop.table(table(data_retard$origin)), 2)
tab_retard
# Diagramme de Pareto
graph1 = barplot(sort(tab_retard),
                 las = 1,
                 col = c("blue", "blue", "red"),
                 ylim = c(0,40),
                 main = "Répartition des vols retardés par aéroport",
                 xlab = "",
                 ylab = "Pourcentage")

# ajout des labels au-dessus des barres
text(x = graph1,
     y = sort(tab_retard) + 2,
     labels = paste0(sort(tab_retard), "%"))

par(mar = c(5.1,10,4.1,2.1))

# Question 2 : La proportion de vols présentant un retard au décollage est-elle similaire sur les trois aéroports durant l’année 2023 ?

tab1 = table(data1$origin,data1$delay_dep)
print(tab1)

# Proportions par aéroport (profil ligne)
tab2 = round(100*prop.table(tab1,1),2)
print(tab2)

# Graphique a barres empilées 
par(mar= c(5, 6, 6, 4))
barplot(t(tab2),   # transposition pour avoir les aéroports en x
        beside = FALSE,
        col = c("skyblue","salmon"),
        ylim = c(0,100), 
        main = "Proportion de retards selon les aéroports", 
        ylab = "Proportion",
        xlab = "Aéroport",
        las = 1) -> bp
legend(
  x = mean(bp),          
  y = max(t(tab2)) * 1.7,
  legend = c("Présence de retard au décollage", "Absence de retard au décollage"),
  fill = c("salmon", "skyblue"),
  bty = "n",
  xjust  = 0.5,
  cex = 0.9,
  ncol = 2)

text(bp,
     t(tab2)[1,]/2,
     labels = paste0(t(tab2)[1,], "%"))

text(bp,
     t(tab2)[1,] + t(tab2)[2,]/2,
     labels = paste0(t(tab2)[2,], "%"))

# calcul du V de Cramer 
chisq = chisq.test(tab2)
cat("valeur du khi-deux :",chisq$statistic)
VCramer = sqrt(chisq$statistic/(sum(tab2)*min(dim(tab2)-1)))
names(VCramer) = "V-Cramer"
print(VCramer)

# Question 3 : Le temps de retard au décollage est-il différent d’un aéroport à l’autre ? 

subset(data1,
       subset = dep_delay > 0) -> retards 

summary(retards)

par(mar= c(5, 6, 6, 4))
with(retards,
     boxplot(log(dep_delay) ~ origin,
             main = "Distribution des retards au décollage selon l'aéroport ", 
             ylab = "Retards de décollage",
             xlab = "Aéroport",
             las = 1,
             col = c("red", "lightblue", "green")
     )) 



# importation de la librairie lsr pour utiliser la fonction etaSquared 
install.packages("lsr")
library(lsr)

# Calcul du Rapport de Correlation
with(retards, 
     aov(log(dep_delay) ~ origin)) -> anova
summary(anova)
etaSquared(anova)

# Question 4 :  Un retard au décollage est-il systématiquement synonyme d’un retard à l’atterrissage ?

tabl1 = table(data1$delay_dep,data1$delay_arr)
print(tabl1)

# Proportions par retard de départ (profil ligne)
tabl2 = round(100*prop.table(tabl1,1),2)
print(tabl2)

rownames(tabl2) <- c("Absence de retard au décollage ",
                     "Présence de retard au décollage")

# Graphique a barres empilées
par(mar= c(5, 6, 6, 4))
barplot(t(tabl2), 
        beside = FALSE,
        col = c("skyblue","salmon"),
        ylim = c(0,100),
        main = "Retard à l'atterrissage selon le retard au décollage",
        ylab = "Proportion",
        xlab = "",
        las = 1) -> bplot

legend(
  x = mean(bplot),          
  y = max(t(tabl2)) * 1.4,
  legend = c("Présence de retard à l'atterissage", "Absence de retard à l'atterissage"),
  fill = c("salmon", "skyblue"),
  bty = "n",
  xjust  = 0.5,
  cex = 0.9,
  ncol = 2)

text(bplot,
     t(tabl2)[1,]/2,
     labels = paste0(t(tabl2)[1,], "%"))

text(bplot,
     t(tabl2)[1,] + t(tabl2)[2,]/2,
     labels = paste0(t(tabl2)[2,], "%"))

# calcule du V de Cramer 
chisq = chisq.test(tabl2)
cat("valeur du khi-deux :",chisq$statistic)
V_Cramer = sqrt(chisq$statistic/(sum(tabl2)*min(dim(tabl2)-1)))
names(V_Cramer) = "V-Cramer"
print(V_Cramer)

# Question 5 : Le délais de retard à l’attérissage s’explique-t-il par le délais de retard au décollage ?

par(mar= c(5, 6, 6, 4))
with(data1,
     plot(arr_delay ~ dep_delay,
     pch = 20,
     col = "blue",
     cex = 1,
     main = "Retard à l'atterrissage en fonction du retard au décollage",
     xlab = "Retards décollage",
     ylab = "Retards d'attérissage"))

abline(lm(data1$arr_delay ~ data1$dep_delay),
            col = "red",
            lwd = 2) 
     

# calcule du coeficient de correlation de Pearson
cor(data1$arr_delay,data1$dep_delay,method = "pearson",
    use = "complete.obs") 

#modèle de régression linéaire simple
lm(arr_delay ~ dep_delay, data = data1) -> modele 
summary(modele)

# Question 6 : Quelles sont les compagnies les plus fiables au décollage pour l’aéroport JFK ? 

# Filtrer les vols fiables (retard = 0) à JFK
JFK_fiable <- dataset[dataset$origin == "JFK" &
                        data1$dep_delay == 0, ] 

# Comptage des compagnies
carrier <- table(JFK_fiable$carrier)
print(carrier)

# Tri décroissant (du plus fiable au moins fiable)
carrier_ordre <- sort(carrier, decreasing = TRUE)
print(carrier_ordre)

# Top 5 des compagnies les plus fiables 
top5 <- head(carrier_ordre, 5)
print(top5)

top5_plot <- top5

par(mar = c(5, 6, 4, 2))

par(mfrow=c(1,1))
graph1 = barplot(top5_plot,
                 las = 1,
                 col = c("red", "orange", "orange", "blue", "blue"),
                 ylim = c(0, max(top5_plot) + 200),
                 main="Top 5 des compagnies les plus fiables (JFK)",
                 xlab="Type de Carrier",
                 ylab="Nombre de vols sans retard")
text(x = graph1,
     y = top5_plot + 100,
     labels = top5_plot)


#Question7:À quelle période de la journée observe-t-on le plus de retards au décollage à JFK ?

# Filtrer les vols retardés à JFK
JFK_retard <- dataset[dataset$origin == "JFK" & dataset$dep_delay > 0, ]

# Création des périodes de la journée

JFK_retard$periode <- cut(JFK_retard$sched_dep_time,                         
                          breaks = c(0, 600, 1200, 1800, 2400),                          
                          labels = c("Nuit", "Matin", "Après-midi", "Soir"),                          
                          include.lowest = TRUE)

# Comptage des retards par période

tab_periode <- table(JFK_retard$periode)
print(tab_periode)

# Calcul des pourcentages
tab_periode <- round(prop.table(table(JFK_retard$periode)) * 100, 2)

# Affichage du tableau
print(tab_periode)

# Graphique
par(mfrow=c(1,1))

graph1 <- barplot(tab_periode,                  
                  col = c("darkblue", "orange", "red", "purple"),                  
                  las = 1,                  
                  main = "Retards des vols selon la période de la journée à JFK",                  
                  xlab = "Période de la journée",                 
                  ylab = "Nombre de vols retardés")

# Ajout Des valeurs sur les barres
text(x = graph1,
     y = tab_periode + 1.5,
     labels = paste0(tab_periode, "%"))


# Question 8 : Existe-t-il une incidence de l’horaire de départ sur la présence d’un retard au décollage ?

data_retard$tranche_horaire <- cut(
  data_retard$dep_time,
  breaks = c(0,600,1200,1800,2400),
  labels = c("Nuit","Matin","Après-midi","Soir")
)

table1 = table(data_retard$tranche_horaire,data_retard$delay_dep)
print(table1)

# Proportions par aéroport (profil ligne)
table2 = round(100*prop.table(table1,1),2)
print(table2)

# Graphique a barres empilées

par(mar= c(5, 6, 6, 3))

barplot(t(table2),   
        beside = FALSE,
        col = c("skyblue","salmon"),
        ylim = c(0,100),
        main = "Présence d’un retard selon l’horaire de départ",
        ylab = "Proportion",
        xlab = "Horaire de départ",
        las = 1) -> barp

legend(
  x = mean(barp),          
  y = max(t(table2)) * 1.35,
  legend = c("Absence de retard au décollage ", "Présence de retard au décollage"),
  fill = c("salmon", "skyblue"),
  bty = "n",
  xjust  = 0.5,
  cex = 0.9,
  ncol = 2)

text(x = barp,
     y = table2[,1]/2,
     labels = paste0(table2[,1], "%"))

text(x = barp,
     y = table2[,1] + table2[,2]/2,
     labels = paste0(table2[,2], "%"))

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
#                                 Thématique 2 : Annulation des vols
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #


# --------------------------------------------------------------------------------- #
## ---- Analyse des données ----

# Question 1 : Quel aéroport a connu le plus d’annulation de vols en 2023 ?
subset(data1,
       subset = canceled == "Yes") -> vol_annules 

summary(object = vol_annules)

table1 <- sort(x = table(vol_annules$origin), 
               decreasing = TRUE)

print(x = table1)

aeroports <- c(
  JFK = "John Fitzgerald Kennedy International \nAirport (JFK)", 
  LGA = "LaGardia Airport (LGA)",
  EWR = "Newark Liberty International Airport (EWR)"
)

par(mar = c(5, 4, 4, 10), 
    xpd = TRUE)

with(data = vol_annules,
     exp = barplot(height = table1,
                   space = 0.5,
                   col = c("red", rep("pink", 2)),
                   ylim = c(0, 5000),
                   las = 1,
                   names.arg = names(table1),
                   main = "Répartition des vols annulés selon l'aéroport d'origine",
                   xlab = "Aéroport",
                   ylab = "Nombre de vols annulés"
     )) -> Graph1

with(data = vol_annules,
     exp = text(x = Graph1,
                y = table1+200,
                labels = paste0(table1),
                cex = 0.8,
                font = 2,
                col = "black"))

legend(x = "topright",
       legend = aeroports[names(table1)],
       fill = c("red", rep("pink", 2)),
       inset = c(-0.01,-0.07),
       cex = 0.6,
       xpd = FALSE,
       bty = "n")


# Question 2 : La proportion de vols annulés est-elle similaire d’un aéroport à l’autre ?
tab_contingence2 <- table(data1$origin, data1$canceled)

print(x = tab_contingence2)

table2 <- round(x = 100*prop.table(tab_contingence2, margin = 1), 
                digits = 2) # Profil ligne

print(x = table2)

par(mar = c(5, 6, 6, 2),
    mgp = c(3.5, 1, 0),
    las = 1,
    xpd = TRUE           
)

barplot(
  height = t(table2),           
  beside = FALSE,          
  col = c("#EED8AE", "#FF8C00"),
  main = "Distribution de la variable canceled selon l'aéroport",
  xlab = "Aéroport",
  ylab = "Pourcentage",
  ylim = c(0, 100),
  las = 1,
  legend.text = FALSE,     
  args.legend = NULL,
  names.arg   = colnames(t(table2)),
  cex.names   = 1.0,
  cex.axis    = 0.9
) -> Graph2

percent <- apply(X = t(table2), 
                 MARGIN = 2, 
                 FUN = cumsum) - t(table2) / 2

text(
  x = rep(Graph2, each = nrow(t(table2))),
  y = c(percent),
  labels = paste0(c(t(table2)), "%"),
  cex = 0.80,
  font = 2,
  col = "black")

legend(
  x = mean(Graph2),          
  y = max(t(table2)) * 1.23,
  legend = c("Vols non annulés", "Vols annulés"),
  fill = c("#EED8AE", "#FF8C00"),
  bty = "n",
  xjust  = 0.5,
  cex = 0.9,
  ncol = 2)

# Indicateurs
cat("\n=== Test du khi-deux ===\n")

chisq = chisq.test(x = tab_contingence2)

print(x = chisq)

cat("Valeur du Khi-deux : ", chisq$statistic) 


cat("\n=== V de Cramer ===\n")

V_cramer <- sqrt(chisq$statistic / (sum(tab_contingence2) * (min(nrow(tab_contingence2), 
                                                                 ncol(tab_contingence2)) - 1)))
names(x = V_cramer) = "V-Cramer"

print(x = round(x = V_cramer,
                digits = 4))


# Question 3 : Quelles sont les compagnies aériennes qui ont subi 
#             le plus d’annulation de vols en 2023 tout aéroport confondu ?

summary(object = vol_annules)

table3 <- sort(x = table(vol_annules$name), decreasing = TRUE)[1:10]

print(x = table3)

par(
  mar = c(5, 12, 5, 2), 
  mgp = c(3, 0.5, 0),
  cex.axis = 0.75)

with(data = vol_annules,
     exp = barplot(height = rev(table3),
                   horiz = TRUE,
                   col = ifelse(seq_along(table3) == length(table3), "red",
                                ifelse(seq_along(table3) >= length(table3) - 2, 
                                       "orange", "#EED8AE")),
                   xlim = c(0, 3500),
                   las = 1,
                   names.arg = rev(names(table3)),
                   main = "Top 10  des compagnies aériennes avec \n le plus d’annulation de vols en 2023",
                   xlab = "Effectif",
                   ylab = ""
     )) -> Graph3 

with(data = vol_annules,
     exp = text(
       x = rev(table3 + 50),
       y = Graph3,
       labels = rev(table3),
       cex = 0.85,
       font = 2,
       adj = 0))

mtext("Compagnie aérienne", side = 2, 
      line = 8, cex = 1, las = 0)


# Question 4 : L’annulation d’un vol peut-elle s’expliquer 
#             par la distance à parcourir pour ce vol ?

tab_contingence4 <- table(data1$distance_class, data1$canceled)

print(x = tab_contingence4)

table4 <- round(x = 100*prop.table(tab_contingence4, margin = 1), 
                digits = 2)

print(x = table4)

par(
  mar = c(6, 5, 4, 2),
  mgp = c(3, 0.8, 0),
  xpd = FALSE
)

barplot(
  height = t(table4),
  beside = FALSE,
  col = c("#EED8AE", "#ff7f0e"),
  main = "Distribution de la variable canceled par classe de distance",
  xlab        = "Classe de distance (km)",
  ylab        = "Pourcentage",
  ylim        = c(0, 115),
  names.arg   = names(table4),
  las         = 1,
  cex.names   = 0.78,
) -> Graph4

percent <- apply(X = t(table4), 
                 MARGIN = 2, 
                 FUN = cumsum) - t(table4) / 2

text(
  x = rep(Graph4, each = nrow(t(table4))),
  y = c(percent),
  labels = paste0(c(t(table4)), "%"),
  cex = 0.80,
  font = 2,
  col = "black"
)

legend(
  x = mean(Graph4),          
  y = max(t(table4)) * 1.22,
  legend = c("Vols non annulés", "Vols annulés"),
  fill = c("#EED8AE", "#ff7f0e"),
  bty = "n",
  xjust  = 0.5,
  cex = 0.9,
  ncol = 2)

#Indicateurs 
cat("\n=== Test du khi-deux ===\n")

khi2 <- chisq.test(tab_contingence4)

print(x = khi2)

cat("Valeur du Khi-deux : ", khi2$statistic) 

cat("\n=== V de Cramer ===\n")

V_Cramer <- sqrt(chisq$statistic / (sum(tab_contingence4) * (min(nrow(tab_contingence4), 
                                                                 ncol(tab_contingence4)) - 1)))
cat("V de Cramer : ", round(x = V_Cramer, 
                            digits = 4), "\n")


# Question 5 : L’annulation d’un vol peut-elle s’expliquer par la valeur 
#              du point de rosée sur le tarmac ?

tab_contingence5 <- table(data1$dewp_class, data1$canceled)

print(x = tab_contingence5)

table5 <- round(x = 100*prop.table(tab_contingence5, margin = 1), 
                digits = 2)

print(x = table5)

par(
  mar = c(6, 5, 3, 2),
  mgp = c(3, 0.8, 0),
  xpd = FALSE
)

barplot(
  height  = t(table5),
  col = c("#EED8AE", "tomato"),
  main = "Distribution de la variable canceled par classe de point de rosée",
  xlab        = "Classe de point de rosée (°C)",
  ylab        = "Proportion",
  ylim        = c(0, 115),
  names.arg   = names(table5),
  las         = 1,
  cex.names   = 0.78,
) -> Graph5

percent <- apply(X = t(table5), 
                 MARGIN = 2, 
                 FUN = cumsum) - t(table5) / 2

text(
  x = rep(Graph5, each = nrow(t(table5))),
  y = c(percent),
  labels = paste0(c(t(table5)), "%"),
  cex = 0.80,
  font = 2,
  col = "black"
)

legend(
  x = mean(Graph5),          
  y = max(t(table5)) * 1.2,
  legend = c("Vols non annulés", "Vols annulés"),
  fill = c("#EED8AE", "tomato"),
  bty = "n",
  xjust  = 0.5,
  cex = 0.9,
  ncol = 2)

#Indicateurs 
cat("\n=== Test du khi-deux ===\n")

khi2 <- chisq.test(tab_contingence5)

print(x = khi2)

cat("Valeur du Khi-deux : ", khi2$statistic) 

cat("\n=== V de Cramer ===\n")

Vdecramer <- sqrt(chisq$statistic / (sum(tab_contingence5) * (min(nrow(tab_contingence5), 
                                                                  ncol(tab_contingence5)) - 1)))

cat("V de Cramer : ", round(x = Vdecramer, digits = 4), "\n")

