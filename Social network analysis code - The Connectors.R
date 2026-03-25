setwd("~/R/R-Network-Analysis/Netwerkanalyse")
install.packages("readxl")
install.packages("igraph")
install.packages("ggraph")
install.packages("tidygraph")
install.packages("tidyverse")
install.packages("writexl") 


library(readxl)
library(igraph)
library(ggraph)
library(tidygraph)
library(tidyverse)

#------
#__T1__
#-----

#-------------------------
# Netwerkmatrix inlezen
#-------------------------
adj_mat_raw <- read_excel("input/Totaal_netwerk_overzicht_t1.xlsx", col_names = FALSE)

# Zet tibble om naar gewone data.frame (handiger voor row/colnames)
adj_df <- as.data.frame(adj_mat_raw)

# Rij- en kolomnamen ophalen
row_ids <- adj_df[-1, 1]              # eerste kolom (zonder header)
col_ids <- unlist(adj_df[1, -1])      # eerste rij (zonder eerste kolom)

# Verwijder eerste rij en kolom (die waren labels)
adj_df <- adj_df[-1, -1]

# Zet naar numerieke matrix
adj_mat <- as.matrix(sapply(adj_df, as.numeric))

# Voeg namen toe
rownames(adj_mat) <- row_ids
colnames(adj_mat) <- col_ids

# Check of het vierkant is
if (nrow(adj_mat) != ncol(adj_mat)) {
  stop("⚠️ Je matrix is niet vierkant. Controleer of je Excel bestand klopt.")
}

#-------------------------
# Graph object maken
#-------------------------
g <- graph_from_adjacency_matrix(adj_mat, mode = "undirected", diag = FALSE)

# Vertex namen controleren
print(V(g)$name[1:10])

vertex_attributes <- read_excel("input/totaal_netwerk_kenmerken_t1.xlsx")

# Zorg dat de ID-kolom character is, net als de rownames van je matrix
vertex_attributes$ID <- as.character(vertex_attributes$ID)


# igraph maken
g <- graph_from_adjacency_matrix(adj_mat, mode = "undirected", diag = FALSE)

# Zet ID’s als vertex names (deze komen uit de matrix!)
V(g)$name <- rownames(adj_mat)

# Zet om naar tidygraph
g_tbl <- as_tbl_graph(g)

# Maak 1 variabele 'sector' uit de binaire kolommen.
sector_cols<- c("Financieel",	"Cultuur",	"GGZ",	"Huisvesting",	"Bewonersinitiatief",	
                "Jeugd",	"Onderwijs",	"Participatie",	"Sport",	"Veiligheid",	
                "Zorg",	"Gemeente algemeen",	"Welzijn",	"Onderzoek")

vertex_attributes <- vertex_attributes %>%
  mutate(sector = apply(select(., all_of(sector_cols)), 1, function(x) sector_cols[which(x == 1)]))

vertex_attributes <- vertex_attributes %>% filter(!is.na(sector))
vertex_attributes <- vertex_attributes %>% filter(!is.na(Kernrespondent))

# Voeg attributen toe
g_tbl <- g_tbl %>%
  activate(nodes) %>%
  left_join(vertex_attributes, by = c("name" = "ID"))

print(g_tbl)

# Kijk ook of de kolommen zijn toegevoegd
g_tbl %>% activate(nodes) %>% as_tibble() %>% glimpse()


# Zet tidygraph terug naar igraph om te kunnen plotten
network_graph_T1 <- igraph::as.igraph(g_tbl)


sector_palette<- c("Financieel" = "#31a354",
                   "Cultuur" = "#bae4b3",
                   "GGZ" = "#67001f",
                   "Huisvesting" = "#74add1",
                   "Bewonersinitiatief" = "#f46d43",
                   "Jeugd" = "#fdae61",
                   "Onderwijs"= "#74c476",
                   "Participatie" = "#006d2c",
                   "Sport" = "#fee090",
                   "Veiligheid" = "#e0f3f8",	
                   "Zorg" = "#abd9e9",
                   "Gemeente algemeen" = "#d73027",	
                   "Welzijn" = "#4575b4",
                   "Onderzoek" = "#313695")



unique(V(network_graph_T1)$sector)
names(sector_palette)

setdiff(unique(V(network_graph_T1)$sector), names(sector_palette))


# --- Sectoren automatisch uit palette halen ---
sector_legends <- names(sector_palette)
sector_colors  <- unname(sector_palette)



# --- Alles combineren ---
all_legends <- c(paste(sector_legends, "(sector)"))
all_colors  <- c(sector_colors)


V(network_graph_T1)$color <- sector_palette[V(network_graph_T1)$sector]
V(network_graph_T1)$label <- NA
V(network_graph_T1)$size <- scales::rescale(degree(network_graph_T1, mode="in"), c(1,7))

# Layout
layout_T1 <- layout_with_fr(network_graph_T1, niter = 1000)

# Plot opslaan als afbeelding
png("Output/network_plot_T1_sectors_V2.png", width = 6000, height = 6000, res = 300)
par(mar = c(1, 1, 1, 8))
plot(network_graph_T1,
     layout = layout_T1,
     vertex.size = V(network_graph_T1)$size,
     vertex.color = V(network_graph_T1)$color,
     vertex.label = NA)

legend("bottomright",
       legend = all_legends,
       col    = all_colors,
       pch    = 19,
       pt.cex = 2,
       bty    = "n",
       title  = "Legend")

dev.off()

#nu met grootte node adhv aantal edges (kan ook zonder)

#------
#__T2__
#-----

#-------------------------
# Netwerkmatrix inlezen
#-------------------------
adj_mat_raw <- read_excel("input/Totaal_netwerk_overzicht_t2.xlsx", col_names = FALSE)

# Zet tibble om naar gewone data.frame (handiger voor row/colnames)
adj_df <- as.data.frame(adj_mat_raw)

# Rij- en kolomnamen ophalen
row_ids <- adj_df[-1, 1]              # eerste kolom (zonder header)
col_ids <- unlist(adj_df[1, -1])      # eerste rij (zonder eerste kolom)

# Verwijder eerste rij en kolom (die waren labels)
adj_df <- adj_df[-1, -1]

# Zet naar numerieke matrix
adj_mat <- as.matrix(sapply(adj_df, as.numeric))

# Voeg namen toe
rownames(adj_mat) <- row_ids
colnames(adj_mat) <- col_ids

# Check of het vierkant is
if (nrow(adj_mat) != ncol(adj_mat)) {
  stop("⚠️ Je matrix is niet vierkant. Controleer of je Excel bestand klopt.")
}

#-------------------------
# Graph object maken
#-------------------------
g <- graph_from_adjacency_matrix(adj_mat, mode = "undirected", diag = FALSE)

# Vertex namen controleren
print(V(g)$name[1:10])

vertex_attributes <- read_excel("input/totaal_netwerk_kenmerken_t2.xlsx")

# Zorg dat de ID-kolom character is, net als de rownames van je matrix
vertex_attributes$ID <- as.character(vertex_attributes$ID)


# igraph maken
g <- graph_from_adjacency_matrix(adj_mat, mode = "undirected", diag = FALSE)

# Zet ID’s als vertex names (deze komen uit de matrix!)
V(g)$name <- rownames(adj_mat)

# Zet om naar tidygraph
g_tbl <- as_tbl_graph(g)

# Maak 1 variabele 'sector' uit de binaire kolommen.
sector_cols<- c("Financieel",	"Cultuur",	"GGZ",	"Huisvesting",	"Bewonersinitiatief",	
                "Jeugd",	"Onderwijs",	"Participatie",	"Sport",	"Veiligheid",	
                "Zorg",	"Gemeente algemeen",	"Welzijn",	"Onderzoek")

vertex_attributes <- vertex_attributes %>%
  mutate(sector = apply(select(., all_of(sector_cols)), 1, function(x) sector_cols[which(x == 1)]))

vertex_attributes <- vertex_attributes %>% filter(!is.na(sector))
vertex_attributes <- vertex_attributes %>% filter(!is.na(Kernrespondent))

# Voeg attributen toe
g_tbl <- g_tbl %>%
  activate(nodes) %>%
  left_join(vertex_attributes, by = c("name" = "ID"))

print(g_tbl)

# Kijk ook of de kolommen zijn toegevoegd
g_tbl %>% activate(nodes) %>% as_tibble() %>% glimpse()


# Zet tidygraph terug naar igraph om te kunnen plotten
network_graph_T2 <- igraph::as.igraph(g_tbl)


sector_palette<- c("Financieel" = "#31a354",
                   "Cultuur" = "#bae4b3",
                   "GGZ" = "#67001f",
                   "Huisvesting" = "#74add1",
                   "Bewonersinitiatief" = "#f46d43",
                   "Jeugd" = "#fdae61",
                   "Onderwijs"= "#74c476",
                   "Participatie" = "#006d2c",
                   "Sport" = "#fee090",
                   "Veiligheid" = "#e0f3f8",	
                   "Zorg" = "#abd9e9",
                   "Gemeente algemeen" = "#d73027",	
                   "Welzijn" = "#4575b4",
                   "Onderzoek" = "#313695")



unique(V(network_graph_T2)$sector)
names(sector_palette)

setdiff(unique(V(network_graph_T2)$sector), names(sector_palette))


# --- Sectoren automatisch uit palette halen ---
sector_legends <- names(sector_palette)
sector_colors  <- unname(sector_palette)



# --- Alles combineren ---
all_legends <- c(paste(sector_legends, "(sector)"))
all_colors  <- c(sector_colors)


V(network_graph_T2)$color <- sector_palette[V(network_graph_T2)$sector]
V(network_graph_T2)$label <- NA
V(network_graph_T1)$size <- scales::rescale(degree(network_graph_T1, mode="in"), c(1,7))
# Layout
layout_T2 <- layout_with_fr(network_graph_T2, niter = 1000)

# Plot opslaan als afbeelding
png("Output/network_plot_T2_sectors_V2.png", width = 6000, height = 6000, res = 300)
par(mar = c(1, 1, 1, 8))
plot(network_graph_T2,
     layout = layout_T2,
     vertex.size = V(network_graph_T1)$size,
     vertex.color = V(network_graph_T2)$color,
     vertex.label = NA)

legend("bottomright",
       legend = all_legends,
       col    = all_colors,
       pch    = 19,
       pt.cex = 2,
       bty    = "n",
       title  = "Legend")

dev.off()


#----------------------------
#STATISTIEKEN
#----------------------------

# === NETWERKSTATISTIEKEN T1 ===
cat("T1 - nodes:", gorder(network_graph_T1), " | edges:", gsize(network_graph_T1), "\n")
cat("T1 - density:", edge_density(network_graph_T1), "\n")
cat("T1 - avg degree:", mean(degree(network_graph_T1)), "\n")
cat("T1 - avg path length:", average.path.length(network_graph_T1), "\n")
cat("T1 - clustering coefficient:", transitivity(network_graph_T1, type = "average"), "\n")
cat("T1 - geslacht:\n")
print(table(V(network_graph_T1)$"Man(1)/Vrouw(0)"))

cat("\nT1 - sectoren:\n")
print(table(V(network_graph_T1)$"sector"))

cat("T1 - formeel:\n")
print(table(V(network_graph_T1)$"Formeel(1)/Informeel(0)"))

cat("T1 - Dubbel:\n")
print(table(V(network_graph_T1)$"Dubbele rol"))


#Voorkomen van artefact in conclusies
# Bereken lokale clustering voor alle nodes
clust_all<- transitivity(network_graph_T1, type = "local", isolates = "zero")

#selecteer alleen nodes met degree>1
deg_T1<- degree(network_graph_T1)
clust_deg1<- clust_all[deg_T1>1]

mean_clust_deg1 <- mean(clust_deg1, na.rm = TRUE)

mean_clust_deg1



# === NETWERKSTATISTIEKEN T2 ===
cat("T2 - nodes:", gorder(network_graph_T2), " | edges:", gsize(network_graph_T2), "\n")
cat("T2 - density:", edge_density(network_graph_T2), "\n")
cat("T2 - avg degree:", mean(degree(network_graph_T2)), "\n")
cat("T2 - avg path length:", average.path.length(network_graph_T2), "\n")
cat("T2 - clustering coefficient:", transitivity(network_graph_T2, type = "average"), "\n")
cat("T2 - geslacht:\n")
print(table(V(network_graph_T2)$"Man(1)/Vrouw(0)"))

cat("\nT2 - sectoren:\n")
print(table(V(network_graph_T2)$"sector"))

cat("T2 - formeel:\n")
print(table(V(network_graph_T2)$"Formeel(1)/Informeel(0)"))

cat("T2 - Dubbel:\n")
print(table(V(network_graph_T2)$"Dubbele rol"))




#Voorkomen van artefact in conclusies
# Bereken lokale clustering voor alle nodes
clust_all<- transitivity(network_graph_T2, type = "local", isolates = "zero")

#selecteer alleen nodes met degree>1
deg_T2<- degree(network_graph_T2)
clust_deg1_T2<- clust_all[deg_T2>1]

mean_clust_deg1_T2 <- mean(clust_deg1_T2, na.rm = TRUE)

mean_clust_deg1_T2

#============= Overlap T1 & T2 ==============

# JACCARD INDEX (overlap)
common <- intersect(V(network_graph_T1)$name, V(network_graph_T2)$name)
g1_sub <- induced_subgraph(network_graph_T1, common)
g2_sub <- induced_subgraph(network_graph_T2, common)
m1 <- as.matrix(as_adjacency_matrix(g1_sub))
m2 <- as.matrix(as_adjacency_matrix(g2_sub))
jaccard <- sum(m1 & m2) / sum(m1 | m2)
cat("Jaccard index:", round(jaccard, 3), "\n")

#extra stappen ter onderbouwing/contextualiseren jaccard index
#groei van het netwerk
n_actors_T1 <- gorder(network_graph_T1)
n_actors_T2 <- gorder(network_graph_T2)
n_edges_T1  <- gsize(network_graph_T1)
n_edges_T2  <- gsize(network_graph_T2)

growth_nodes <- n_actors_T2 / n_actors_T1
growth_edges <- n_edges_T2 / n_edges_T1

#overlap actoren
actor_overlap <- length(common) / length(union(V(network_graph_T1)$name, V(network_graph_T2)$name))


#Meet in hoeverre het netwerk nog draait om een paar centrale actoren.
centralization.degree(network_graph_T1, normalized = TRUE)
centralization.degree(network_graph_T2, normalized = TRUE)

#Top vijf actoren met hoogste degree
sort(degree(network_graph_T1), decreasing = TRUE)[1:5]
sort(degree(network_graph_T2), decreasing = TRUE)[1:5]

# meet wie de "bruggen" zijn tussen clusters in het netwerk
betweenness(network_graph_T1, normalized = TRUE)
betweenness(network_graph_T2, normalized = TRUE)



# ---Functie om intra/intersectoral edges te tellen ---
count_sector_ties <- function(graph, sector_cols) {
  g <- as.undirected(graph, mode = "collapse")
  edges_df <- as.data.frame(get.edgelist(g))
  colnames(edges_df) <- c("from", "to")
  
  edges_df$sector_from <- V(g)$sector[match(edges_df$from, V(g)$name)]
  edges_df$sector_to   <- V(g)$sector[match(edges_df$to, V(g)$name)]
  edges_df$type <- ifelse(edges_df$sector_from == edges_df$sector_to, "intra", "inter")
  
  table_edges <- table(edges_df$type)
  prop_edges  <- prop.table(table_edges)
  
  return(list(counts = table_edges, proportions = round(prop_edges, 3), edges_df = edges_df))
}

# Pas toe op beide netwerken
sector_ties_T1 <- count_sector_ties(network_graph_T1)
sector_ties_T2 <- count_sector_ties(network_graph_T2)

# Maak edges_df beschikbaar
edges_df_T1 <- sector_ties_T1$edges_df
edges_df_T2 <- sector_ties_T2$edges_df

# Welke sectoren verbinden met elkaar? T1
edges_df_T1$pair <- paste(pmin(edges_df_T1$sector_from, edges_df_T1$sector_to),
                          pmax(edges_df_T1$sector_from, edges_df_T1$sector_to), sep = " - ")

sort(table(edges_df_T1$pair), decreasing = TRUE)

# Welke sectoren verbinden met elkaar? T2
edges_df_T2$pair <- paste(pmin(edges_df_T2$sector_from, edges_df_T2$sector_to),
                          pmax(edges_df_T2$sector_from, edges_df_T2$sector_to), sep = " - ")

sort(table(edges_df_T2$pair), decreasing = TRUE)


#sectoren en actoren overlap tussen T1 en T2 berekening
network_graph_T1
network_graph_T2

actors_T1 <- V(network_graph_T1)$name
actors_T2 <- V(network_graph_T2)$name

actors_both <- intersect(actors_T1, actors_T2)
length(actors_both)


sector_df <- data.frame(
  actor = actors_both,
  lapply(sector_cols, function(s)
    vertex_attr(network_graph_T1, s, index = actors_both))
)
names(sector_df)[-1] <- sector_cols

sector_verdeling_overlap <- colSums(sector_df[, -1], na.rm = TRUE)

sector_verdeling_overlap

round(100 * sector_verdeling_overlap / length(actors_both), 1)




#kern vs periferie per sector
#V(network_graph_T1)$Financieel
#V(network_graph_T1)$Cultuur
#V(network_graph_T1)$GGZ
#V(network_graph_T1)$Huisvesting
#V(network_graph_T1)$Bewonersinitiatief
#V(network_graph_T1)$Jeugd
#V(network_graph_T1)$Onderwijs
#V(network_graph_T1)$Participatie
#V(network_graph_T1)$Sport
#V(network_graph_T1)$Veiligheid
#V(network_graph_T1)$Zorg
#V(network_graph_T1)$"Gemeente algemeen"
#V(network_graph_T1)$Welzijn
#V(network_graph_T1)$Onderzoek


V(network_graph_T1)$positie <- ifelse(deg_T1 <= 1, "periferie", "kern")
V(network_graph_T2)$positie <- ifelse(deg_T2 <= 1, "periferie", "kern")

#t1
df_T1 <- data.frame(
  actor     = V(network_graph_T1)$name,
  positie   = V(network_graph_T1)$positie,
  Financieel = V(network_graph_T1)$Financieel,
  Cultuur = V(network_graph_T1)$Cultuur,
  GGZ = V(network_graph_T1)$GGZ,
  Huisvesting = V(network_graph_T1)$Huisvesting,
  Bewonersinitiatief = V(network_graph_T1)$Bewonersinitiatief,
  Jeugd = V(network_graph_T1)$Jeugd,
  Onderwijs = V(network_graph_T1)$Onderwijs,
  Sport = V(network_graph_T1)$Sport,
  Veiligheid = V(network_graph_T1)$Veiligheid,
  Zorg = V(network_graph_T1)$Zorg,
  Welzijn  = V(network_graph_T1)$Welzijn,
  Gemeente = V(network_graph_T1)$"Gemeente algemeen",
  Onderzoek = V(network_graph_T1)$Onderzoek
)

#van breed naar lang format
library(tidyr)
library(dplyr)

df_long_T1 <- df_T1 %>%
  pivot_longer(
    cols = Financieel:Onderzoek,
    names_to = "sector",
    values_to = "in_sector"
  )

#selecteer actoren die in de specifieke sector zitten
df_long_T1 <- df_long_T1 %>%
 filter(in_sector == 1)

#per sector tellen (in kern/periferie) en percentages berekenen
sector_kern_perif_T1 <- df_long_T1 %>%
  group_by(sector, positie) %>%
  summarise(aantal = n(), .groups = "drop")

sector_percentages_T1 <- sector_kern_perif_T1 %>%
  group_by(sector) %>%
  mutate(
    percentage = round(100 * aantal / sum(aantal), 1)
  )

print(sector_percentages_T1)

#zelfde voor T2
df_T2 <- data.frame(
  actor     = V(network_graph_T2)$name,
  positie   = V(network_graph_T2)$positie,
  Financieel = V(network_graph_T2)$Financieel,
  Cultuur = V(network_graph_T2)$Cultuur,
  GGZ = V(network_graph_T2)$GGZ,
  Huisvesting = V(network_graph_T2)$Huisvesting,
  Bewonersinitiatief = V(network_graph_T2)$Bewonersinitiatief,
  Jeugd = V(network_graph_T2)$Jeugd,
  Onderwijs = V(network_graph_T2)$Onderwijs,
  Sport = V(network_graph_T2)$Sport,
  Veiligheid = V(network_graph_T2)$Veiligheid,
  Zorg = V(network_graph_T2)$Zorg,
  Welzijn  = V(network_graph_T2)$Welzijn,
  Gemeente = V(network_graph_T2)$"Gemeente algemeen",
  Onderzoek = V(network_graph_T2)$Onderzoek,
  Participatie = V(network_graph_T2)$Participatie
)

#van breed naar lang format

df_long_T2 <- df_T2 %>%
  pivot_longer(
    cols = Financieel:Participatie,
    names_to = "sector",
    values_to = "in_sector"
  )

#selecteer actoren die in de specifieke sector zitten
df_long_T2 <- df_long_T2 %>%
 filter(in_sector == 1)

#per sector tellen (in kern/periferie) en percentages berekenen
sector_kern_perif_T2 <- df_long_T2 %>%
  group_by(sector, positie) %>%
  summarise(aantal = n(), .groups = "drop")

sector_percentages_T2 <- sector_kern_perif_T2 %>%
  group_by(sector) %>%
  mutate(
    percentage = round(100 * aantal / sum(aantal), 1)
  )

print(n=26, sector_percentages_T2)

# ======================
# VERANDERINGEN VISUALISEREN incl. knopen
# ======================

# 1. Zet beide netwerken om naar edge lists
edges_T1 <- igraph::as_data_frame(network_graph_T1, what = "edges")
edges_T2 <- igraph::as_data_frame(network_graph_T2, what = "edges")

# Voeg een kolom toe die de bron en doel combineert, zodat we makkelijk kunnen vergelijken
edges_T1$pair <- apply(edges_T1[,1:2], 1, function(x) paste(sort(x), collapse = "_"))
edges_T2$pair <- apply(edges_T2[,1:2], 1, function(x) paste(sort(x), collapse = "_"))

# 2. Classificeer de type veranderingen (edges)
edges_T1$type <- ifelse(edges_T1$pair %in% edges_T2$pair, "behouden", "verdwenen")
edges_T2$type <- ifelse(edges_T2$pair %in% edges_T1$pair, "behouden", "nieuw")

# 3. Combineer edge lists
all_edges <- rbind(
  edges_T1[, c("from", "to", "type")],
  edges_T2[edges_T2$type == "nieuw", c("from", "to", "type")]
)

# 4. Bepaal alle unieke knopen
nodes_T1 <- V(network_graph_T1)$name
nodes_T2 <- V(network_graph_T2)$name
all_nodes <- union(nodes_T1, nodes_T2)

# Classificeer knopen
node_status <- data.frame(
  name = all_nodes,
  node_type = ifelse(all_nodes %in% nodes_T1 & all_nodes %in% nodes_T2, "behouden",
                     ifelse(all_nodes %in% nodes_T1, "verdwenen", "nieuw"))
)

# 5. Maak gecombineerde grafiek
combined_graph <- graph_from_data_frame(all_edges, directed = FALSE, vertices = node_status)

# 6. Styling edges
E(combined_graph)$color <- ifelse(E(combined_graph)$type == "behouden", "gray50",
                                  ifelse(E(combined_graph)$type == "nieuw", "#31a354",
                                         ifelse(E(combined_graph)$type == "verdwenen", "#d73027", "black")))
E(combined_graph)$lty <- ifelse(E(combined_graph)$type == "behouden", 1, 2)

# 7. Styling nodes
V(combined_graph)$color <- ifelse(V(combined_graph)$node_type == "behouden", "white",
                                  ifelse(V(combined_graph)$node_type == "nieuw", "#4575b4", "#fdae61"))
V(combined_graph)$size <- 6
V(combined_graph)$label.cex <- NA


# 8. Layout (zelfde type als eerder voor consistentie)
layout_combined <- layout_with_fr(combined_graph, niter = 1000)

# 9. Plot
png("Output/network_changes_T1_T2_with_nodes.png", width = 6000, height = 6000, res = 300)
plot(combined_graph,
     layout = layout_combined,
     vertex.label = NA,
     vertex.label.cex = NA,
     vertex.color = V(combined_graph)$color,
     vertex.size = V(combined_graph)$size,
     edge.color = E(combined_graph)$color,
     edge.lty = E(combined_graph)$lty,
     main = "Netwerkveranderingen van T1 naar T2\nEdges: Grijs = behouden, Groen = nieuw, Rood = verdwenen\nNodes: Wit = behouden, Paars = nieuw, Blauw = verdwenen")
dev.off()



#---------------------------------------------------
#Andere optie figuur voor visualisatie veranderingen (nu bovenstaande gebruikt, niet deze)
#---------------------------------------------------
# --- 1. Bepaal welke nodes nieuw zijn in T2
new_nodes <- setdiff(V(network_graph_T2)$name, V(network_graph_T1)$name)

# --- 2. Node kleuren
V(network_graph_T2)$color <- ifelse(V(network_graph_T2)$name %in% new_nodes, "lightgreen", "grey80")

# --- 3. Edge-lijsten maken ---
edges_T1 <- igraph::as_data_frame(network_graph_T1, what = "edges")[,1:2]
edges_T2 <- igraph::as_data_frame(network_graph_T2, what = "edges")[,1:2]

# Zorg dat alle edges in dezelfde orde staan (onbelangrijk of A-B of B-A)
edges_T1 <- t(apply(edges_T1, 1, sort)) %>% as.data.frame(stringsAsFactors = FALSE)
edges_T2 <- t(apply(edges_T2, 1, sort)) %>% as.data.frame(stringsAsFactors = FALSE)

colnames(edges_T1) <- c("from","to")
colnames(edges_T2) <- c("from","to")

# --- 4. Nieuwe edges in T2 tov T1 ---
new_edges <- anti_join(edges_T2, edges_T1, by = c("from","to"))

# Maak een vector met alle edges in T2 als "from_to"
edges_all <- apply(edges_T2, 1, paste, collapse = "_")
edges_new <- apply(new_edges, 1, paste, collapse = "_")

E(network_graph_T2)$color <- ifelse(edges_all %in% edges_new, "darkgreen", "grey80")


# --- 5. Plotten
layout_T2 <- layout_with_fr(network_graph_T2, niter = 1000)

png("Output/network_plot_T2_new_vs_old.png", width = 6000, height = 6000, res = 300)
plot(network_graph_T2,
     layout = layout_T2,
     vertex.size = 5,
     vertex.color = V(network_graph_T2)$color,
     vertex.label = NA,
     edge.color = E(network_graph_T2)$color,
     edge.width = E(network_graph_T2)$width)
legend("bottomright",
       legend = c("Nieuwe node", "Bestaande node", "Nieuwe relatie", "Bestaande relatie"),
       col    = c("lightgreen", "grey80", "darkgreen", "grey80"),
       pch    = c(19, 19, NA, NA),
       lty    = c(NA, NA, 1, 1),
       pt.cex = 2,
       bty    = "n",
       title  = "Legenda")
dev.off()



