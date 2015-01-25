#################################################################################
#
#  Skript zum Paper 
#  Sebek P, Altman J, Platek M, Cizek L (2013) Is active management the key to the conservation of saproxylic biodiversity? Pollarding promotes the formation of tree hollows. 
#  PLoS ONE 8(3): e60456. http://dx.doi.org/10.1371/journal.pone.0060456
# 
#################################################################################

# Wie hängt das Alter der Bäume und Management mit der Höhlenbildung zusammen?
# Hypothese: Je älter der Bau desto höherer Prozentsatz mit Höhlen

# Daten einlesen und vorprozessieren: DHB und hollow
  tree_hollow_incidents <- read.table("Sebek_et_al_Pollarding - Hollow Incidence.csv", header = TRUE, sep = ";", dec = ".", na.strings = "NA")
  
  # Umbenennen von DHB..cm. weil Typo
  colnames(tree_hollow_incidents)[3] <- "DBHcm"
  
  # DBHclass 0 ausschliessen, Error
  tree_hollow_incidents <- subset(tree_hollow_incidents, DBHclass != 0)
  
  # Übersicht zu den Daten
  summary(tree_hollow_incidents)
  
  # Spalte tree.count Anhängen, um später die Anzahl pro Gruppe auszurechen
  # TODO: elegantere Methode finden
  tree_hollow_incidents$tree.count <- 1

  # Klassifizierung nach Umfang in 30cm Schritten
  diameter <- c(0:29,30:59,60:89,90:900)
  tree_hollow_incidents$diameter <- cut(tree_hollow_incidents$DBHcm, diameter)

# Subsets zur Kontrolle
hi_managed <- tree_hollow_incidents[tree_hollow_incidents$management == "pollard",]
hi_unmanaged <- tree_hollow_incidents[tree_hollow_incidents$management == "unman",]

## Funktion: treehollows

treehollows <- function(data = tree_hollow_incidents, management){
  data <- data[data$management ==  management,]
  tree_hollow_sums <- aggregate(data$hollow, by=list(DBHclass = data$DBHclass), FUN=sum)
  colnames(tree_hollow_sums)[2] <- "hollow"
  
  # Anzahl Bäume in jeder Klasse
  tree_hollow_count.totals <- aggregate(data$tree.count, by=list(DBHclass = data$DBHclass), FUN=sum)
  tree_hollow_sums$count <- tree_hollow_count.totals[,2]
  tree_hollow_sums$percentage.hollow <- (tree_hollow_sums$hollow * 100) / tree_hollow_sums$count
  tree_hollow_sums$percentage.nothollow <- 100 - tree_hollow_sums$percentage.hollow
  tree_hollow_results <<- tree_hollow_sums
  return( tree_hollow_sums )
}

# Funktion aufrufen: pollard
treehollows(tree_hollow_incidents, "pollard")

# plot
plot(tree_hollow_results$DBHclass, tree_hollow_results$percentage.hollow, type="l", main="Percentage of hollow in DBH Class")

# Funktion aufrufen
treehollows(tree_hollow_incidents, "unman")

# plot
counts <- table(tree_hollow_results$DBHclass, tree_hollow_results$percentage)
lines(tree_hollow_results$DBHclass, tree_hollow_results$percentage.nothollow, type="l")

# barplot transposed
# View( t(tree_hollow_results) )
tree_hollow_barplot <- as.matrix(t(tree_hollow_results))
tree_hollow_barplot <- tree_hollow_barplot[-c(1:3),]
barplot(tree_hollow_barplot, main="Hohl pro DBH Klasse", ylab="Prozentsatz", xlab="DBH Klasse", axes=TRUE, legend=c("hohl","nicht hohl") ) 
