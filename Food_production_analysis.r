# Initialisation ----

# Chargemeent des données
donnees <- fread("Food_Production.csv")
donnees <- as.data.frame(donnees)

# Nomme les colonnes par le nom du produit alimentaire correspondant
names <- donnees$`Food product`
rownames(donnees) <- names

# Elimination des produits qui ont une valeur négative pour la variable Lande use change
donnees <- donnees %>% filter(`Land use change`>=0)
donnees <- donnees[,c("Land use change", "Animal Feed","Farm","Processing", "Transport","Packging","Retail","Total_emissions")]

# Satistiques descriptives ----
set.caption('Statistiques descriptives') #Légende du tableau
panderOptions('table.split.table', 300) # Pour éviter de spliter les tableaux
pander(apply(donnees,2,stat.desc), digits = 2, cex = 0.7)

boxplot(donnees[,1:7], ylab = "Kg d'équivalent CO2 par kg")

pairs(donnees) 

# Corrélations entre les variables
set.caption('Matrice des corrélations')
pander(cor(donnees), digits = 2, cex = 0.1)
panderOptions('table.split.table', 300)

Data_ACP <- subset(donnees, select = -c(Total_emissions))

# ACP ----

# Ajout d'une variable catégorielle 
x=c()
for (i in 1:29) {
  x[i] <- "No"
  
} 
y= c()
for (i in 1:10) {
  y[i] <- "Yes"
}



Product_type <- c(x,y)

Data_ACP <- cbind(Data_ACP,Product_type)
res <- PCA(Data_ACP[,-8], graph=FALSE)

# Choix composantes principales
set.caption('Valeurs propres de la matrice de corrélation')
eig <- res$eig
pander(eig)

## Variables ---- 

# Coordonnées factorielles des variables 
coord <- round(res$var$coord[,1:3],3)
set.caption('Coordonnées factorielles des variables')
pander(coord)

# Qualité de représentation des variables
cos2 <- round(res$var$cos2[,1:3],3)
set.caption('Qualité de représentation des variables (Cos^2)')
pander(cos2)

# Contribution des variables
contrib <- round(res$var$contrib[,1:3],3)
set.caption("Contribution des variables à l'élaboration des composantes principales")
pander(contrib)


# Cercles des corrélations
options(ggrepel.max.overlaps = Inf)
fviz_pca_var(res,axes = c(1,2), col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE )


options(ggrepel.max.overlaps = Inf)
fviz_pca_var(res, axes = c(1,3), col.var = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE )

options(ggrepel.max.overlaps = Inf)
fviz_pca_var(res, axes = c(2,3), col.var = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE )

## Individus ----

# Coordonnées factorielles des individus
coord_ind <- round(res$ind$coord[,1:3],3)
set.caption('Coordonnées factorielles des individus')
pander(coord_ind)

# Qualité de représentation des observations
coord_ind <- round(res$ind$cos2[,1:3],3)
set.caption('Qualité de représentation des observations')
pander(coord_ind)

# Contributions des observations
contrib_ind <- round(res$ind$contrib[,1:3],3)
set.caption('Contribution des observations')
pander(contrib_ind)

# Contributions des observations
dist_ind <- round(res$ind$dist,3)
set.caption("Distance à l'origine des observations")
pander(dist_ind)


# Distance à l'origine
dist_ind <- round(res$ind$dist,3)
set.caption("Distance à l'origine des observations")
pander(dist_ind)

# Cartes des individus

options(ggrepel.max.overlaps = Inf)
fviz_pca_ind(res, axes = c(1,2), col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Évite le chevauchement de texte
)

options(ggrepel.max.overlaps = Inf)
fviz_pca_ind(res, axes = c(1,3), col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Évite le chevauchement de texte
)

options(ggrepel.max.overlaps = Inf)
fviz_pca_ind(res, axes = c(2,3), col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Évite le chevauchement de texte
)

options(ggrepel.max.overlaps = Inf)
fviz_pca_ind(res,
             geom.ind = "point",
             col.ind = Data_ACP$Product_type, # colorer by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE,
             mean.point = TRUE,
             legend.title = "Product type"
)

# Clustering ----

# Standardisation des données
donnees <- scale(donnees)
donnees <- donnees[,1:7]

# Distances euclidiennes
dist_donnees <- dist(donnees, method = "euclidean")

# Classification hiérarchique algorithme de Ward
res_clust <- hclust(dist_donnees, method = "ward.D")

plot(res_clust,
     xlab = "",
     ylab = "Niveau d'agrégation",
     cex = 0.7)

rect.hclust(res_clust, k=6, border="red")
rect.hclust(res_clust, k=3, border="green")
rect.hclust(res_clust, k=2, border="blue")

# Barplot
barplot(res_clust$height,
        xlab = "Nombre de classes",
        names.arg = (nrow(donnees)-1):1,
        ylab = "Niveau d'agrégation")

#qualité partition - 6 classes
BSS_W <- sum(tail(res_clust$height,n=(6-1)))
TSS_W <- sum(res_clust$height)
BSS_W/TSS_W*100

#qualité partition - 3 classes
BSS_W <- sum(tail(res_clust$height,n=(3-1)))
TSS_W <- sum(res_clust$height)
BSS_W/TSS_W*100

#qualité partition - 2 classes
BSS_W <- sum(tail(res_clust$height,n=(2-1)))
TSS_W <- sum(res_clust$height)
BSS_W/TSS_W*100