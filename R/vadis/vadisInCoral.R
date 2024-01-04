
rm(list=ls())

library(knitr)
knitr::opts_chunk$set(
  collapse = TRUE,
  error = FALSE,
  message = FALSE,
  comment = "#>"
)

knitr::opts_knit$set(
  root.dir = normalizePath("../"), # set working directory as root
  cache.path = "vignettes/basic_vadis_cache/"
  )

library(glue)
library(tidyverse) # for data wrangling
library(magrittr) # for data wrangling
library(tidytext) # for data wrangling
library(lme4) # for regression models
library(ranger) # for random forests
library(permimp) # for conditional permutation importance
library(phangorn) # for neighborNets
library(tuneRanger)
library(mlr)
library(ranger)
library(plotly)

#installing VADIS
#devtools::install_github("jasongraf1/VADIS")
library(VADIS)

# call the dataset 
datasetComplete <- read.csv("C:/Users/joaop/Desktop/Corpora/C-Oral-Brasil/dissertation_codes/text_output_files/metadataWithTranscription_csv_2023-05-21.csv")

# replacing M by A in the age column
datasetComplete$age <- gsub("M", "A", datasetComplete$age)

# replacing D by c in the age column
datasetComplete$age <- gsub("D", "C", datasetComplete$age)

# replacing female by 0 in the sex column
datasetComplete$sex <- gsub("female", "0", datasetComplete$sex)

# replacing male by 1 in the sex column
datasetComplete$sex <- gsub("male", "1", datasetComplete$sex)


#converting social variables as factors
datasetComplete$sex = as.factor(datasetComplete$sex)
datasetComplete$schooling <- as.factor(datasetComplete$schooling)
datasetComplete$age = as.factor(datasetComplete$age)

#Convert column1 column type to numeric # class()
datasetComplete$count_interjections = as.numeric(datasetComplete$count_interjections)
datasetComplete$count_pronouns = as.numeric(datasetComplete$count_pronouns)
datasetComplete$total_nonstandard_verb_agreement = as.numeric(datasetComplete$total_nonstandard_verb_agreement)
datasetComplete$total_nonstandard_verb_conjugation = as.numeric(datasetComplete$total_nonstandard_verb_conjugation)
datasetComplete$total_non_standard_plural = as.numeric(datasetComplete$total_non_standard_plural)
datasetComplete$total_negation_ratio = as.numeric(datasetComplete$total_negation_ratio)
datasetComplete$count_apheresis = as.numeric(datasetComplete$count_apheresis)
datasetComplete$count_rhotacism = as.numeric(datasetComplete$count_rhotacism)
datasetComplete$count_diminutive_ratio = as.numeric(datasetComplete$count_diminutive_ratio)
datasetComplete$count_foreign_words = as.numeric(datasetComplete$count_foreign_words)
datasetComplete$count_mister_madam = as.numeric(datasetComplete$count_mister_madam)
datasetComplete$count_prepositions = as.numeric(datasetComplete$count_prepositions)

# converting to factor
datasetComplete$mo_intensifier = as.factor(datasetComplete$mo_intensifier)



# Selecting specific columns using the subset() function
selected_dataset <- subset(datasetComplete, select = c("age", "sex", "schooling", "total_nonstandard_verb_agreement", "total_nonstandard_verb_conjugation","total_negation_ratio", "total_non_standard_plural", "count_apheresis", "mo_intensifier", "count_rhotacism","count_mister_madam", "count_diminutive_ratio", "count_foreign_words", "count_prepositions", "count_pronouns", "count_interjections"))


# converting the column values to binary values
# if above the median = 1; if lower than the median = 0
selected_dataset$total_nonstandard_verb_agreement = ifelse(selected_dataset$total_nonstandard_verb_agreement>median(selected_dataset$total_nonstandard_verb_agreement),1,0)

selected_dataset$total_nonstandard_verb_conjugation = ifelse(selected_dataset$total_nonstandard_verb_conjugation>median(selected_dataset$total_nonstandard_verb_conjugation),1,0)

selected_dataset$total_negation_ratio = ifelse(selected_dataset$total_negation_ratio>median(selected_dataset$total_negation_ratio),1,0)

selected_dataset$total_non_standard_plural = ifelse(selected_dataset$total_non_standard_plural>median(selected_dataset$total_non_standard_plural),1,0)

selected_dataset$count_apheresis = ifelse(selected_dataset$count_apheresis>median(selected_dataset$count_apheresis),1,0)

selected_dataset$count_mister_madam = ifelse(selected_dataset$count_mister_madam>median(selected_dataset$count_mister_madam),1,0)

selected_dataset$count_diminutive_ratio = ifelse(selected_dataset$count_diminutive_ratio>median(selected_dataset$count_diminutive_ratio),1,0)

selected_dataset$count_foreign_words = ifelse(selected_dataset$count_foreign_words>median(selected_dataset$count_foreign_words),1,0)

selected_dataset$count_prepositions = ifelse(selected_dataset$count_prepositions>median(selected_dataset$count_prepositions),1,0)


selected_dataset$count_pronouns = ifelse(selected_dataset$count_pronouns>median(selected_dataset$count_pronouns),1,0)

selected_dataset$count_interjections = ifelse(selected_dataset$count_interjections>median(selected_dataset$count_interjections),1,0)



# create list of dataframes based on the age variable
data_list <- split(selected_dataset, selected_dataset$age, drop = TRUE) # drop unused levels
names(data_list)

# These are the predictors we'll include in our variable grammar models. 
# We'll define our model formula using this set.
# total_nonstandard_verb_conjugation + total_nonstandard_verb_agreement + count_interjections + count_pronouns +  total_non_standard_plural+ total_negation_ratio + count_apheresis +count_rhotacism +count_mister_madam +count_diminutive_ratio + count_foreign_words +count_prepositions

# ====================== 
### NON STANDARD VERB CONJUGATION

f1 <-  total_nonstandard_verb_conjugation ~ sex + schooling

glm_list <- vector("list") # empty list to store our models 

for (i in seq_along(data_list)){
  # i = 5
  d <- data_list[[i]]
  # now standardize t]he model fixed effects inputs before fitting.
  d_std <- stand(d, cols = f1) # use the fitting function for convenience
  # fit the model
  glm_list[[i]] <- glm(f1, data = d_std,
                       family = binomial, x = TRUE) # note the x = TRUE
}

names(glm_list) <- names(data_list) # add names to the list of models


#for (i in seq_along(glm_list)){
#  m <- glm_list[[i]]
#  print(summary(m))
#  print("\n========\n\n")
#}


# check fitting metrics
summary_line1 <- summary_stats(glm_list, data_list) %>%
  round(3)

# VADIS 1st line of evidence (constraint significance)
signif_line <- vadis_line1(glm_list)
signif_line$signif.table
signif_line$distance.matrix
signif_line$similarity.scores


# VADIS 2nd line of evidence (magnitude of effects)
coef_line <- vadis_line2(glm_list)
coef_line$coef.table %>% 
  round(3)
coef_line$distance.matrix %>% 
  round(3)
coef_line$similarity.scores %>% 
  arrange(desc(Similarity))


# creating imaginary opposite variants
d <- data.frame(
  A = rep(c(-1,1), each = 5),
  B = rep(c(1,-1), each = 5),
  row.names = paste0("constraint.", 1:10))

# creating tuning dataframe
tune_df <- data.frame(matrix(NA, ncol = 5, nrow = length(data_list)))
names(tune_df) <- c("mtry", "min.node.size", "sample.fraction", "auc", "exec.time")

# tuning
for (i in seq_along(data_list)){
  d <- data_list[[i]][, all.vars(f1)]
  d$total_nonstandard_verb_conjugation <- as.factor(d$total_nonstandard_verb_conjugation)
  pv_task <- makeClassifTask(data = d, target = "total_nonstandard_verb_conjugation")
  
  # Tuning process (takes around 1 minute); Tuning measure is the Area Under the Curve
  result <- tuneRanger(pv_task, measure = list(auc), num.trees = 1000, 
                       num.threads = 4, iters = 80, show.info = F) 
  
  tune_df[i,] <- result$recommended.pars
}

rownames(tune_df) <- names(data_list) # assigning name to the tuning dataframe

# creating the "forest"
rf_list <- vector("list")
for (i in seq_along(data_list)){
  d <- data_list[[i]]
  # fit the random forest and add it to the list
  rf_list[[i]] <- ranger(
    f1, 
    data = d,
    seed = 1234,
    num.trees = 1000,
    mtry = tune_df[i, "mtry"],
    min.node.size = tune_df[i, "min.node.size"],
    sample.fraction = tune_df[i, "sample.fraction"],
    probability = TRUE,
    importance = "permutation"
  )
}
names(rf_list) <- names(data_list) #assigning names to the forests

# checking fitting metrics
summary_stats(rf_list, data_list, response = "total_nonstandard_verb_conjugation") %>% 
  round(3)

# VADIS 3rd line of evidence (variable importance ranking)
varimp_line <- vadis_line3(rf_list, conditional = FALSE)
varimp_line$varimp.table %>% 
  round(3)
varimp_line$rank.table
varimp_line$distance.matrix %>%
  round(2)

varimp_line$similarity.scores %>% 
  arrange(desc(Similarity))

# merge the similarity scores across the three lines of evidence, and arrive at mean scores for each sociolect.
mean_sims <- data.frame(
  line1 = signif_line$similarity.scores[,1], # get only the values in the 2nd column
  line2 = coef_line$similarity.scores[,1],
  line3 = varimp_line$similarity.scores[,1],
  row.names = names(data_list)
)

# calculate a mean of the mean similarity scores
mean_sims$mean <- rowMeans(mean_sims)
round(mean_sims, 3)

# core grammar coefficient!
mean(mean_sims$mean)

# create a combined distance matrix from the three lines of evidence
fused_dist <- analogue::fuse(signif_line$distance.matrix, 
                             coef_line$distance.matrix, 
                             varimp_line$distance.matrix)
round(fused_dist, 3)


# hierarchical clustering
hclust(fused_dist, method = "ward.D2") %>% 
  plot(main = "Hierarchical clustering of fused distances")

# divisive/unrooter clustering
par(mfrow = c(1,2))
cluster::diana(fused_dist) %>% 
  cluster::pltree(main = "Divisive clustering")
ape::nj(fused_dist) %>% 
  plot(type = "u", main = "Unrooted clustering")

# Distance matrices can also be fed into a multidimensional scaling analysis, which maps distances onto a 2 or 3 dimensional space. The closer the points are in this space, the more similar the sociolects' grammars.
line2_mds <- cmdscale(coef_line$distance.matrix, k = 2, eig = T)
line2_mds[[1]] %>%
  as.data.frame() %>% 
  mutate(genres = rownames(.)) %>% 
  ggplot(aes(V1, V2, label = genres)) +
  geom_point() +
  geom_text(nudge_y = .01, size = 4)

# creating 3D plots (box-like image)
dd <- line2_mds[[1]] %>%
  as.data.frame() 
library(scatterplot3d)
with(dd, {
  scttr <- scatterplot3d(x = V1, y = V2, type = "h", pch = 18)
  scttr_coords <- scttr$xyz.convert(V1, V2)
  text(scttr_coords$x, scttr_coords$y, labels = rownames(dd), pos = 3)
})


# run an MDS based on the fused distance matrix.
fused_mds <- cmdscale(fused_dist,k = 2, eig = T)

fused_mds[[1]] %>% # extract the coordinates
  as.data.frame() %>% 
  rownames_to_column("Variety") %>%
  plot_ly() %>%
  add_trace(x = ~V1, y = ~V2, 
            type = "scatter3d", inherit = F,
            marker = list(size = 4),
            mode = "markers") %>%
  add_text(x = ~V1, y = ~V2,
           text = ~ Variety,
          type = "scatter3d",
           mode = "markers",
           showlegend = FALSE)





# NeighborNet is a technique for constructing and visualizing phylogenetic relationships - using the 2nd line of evidence
line2_NNet <- phangorn::neighborNet(coef_line$distance.matrix)
plot(line2_NNet, "phylogram")


# now with the fused matrix
nets_fused_matrix <- phangorn::neighborNet(fused_dist)
plot(nets_fused_matrix, "phylogram")


# ====================== 
### NON STANDARD VERB AGREEMENT

f2 <-  total_nonstandard_verb_agreement ~ sex + schooling

glm_list <- vector("list") # empty list to store our models 

for (i in seq_along(data_list)){
  # i = 5
  d <- data_list[[i]]
  # now standardize t]he model fixed effects inputs before fitting.
  d_std <- stand(d, cols = f2) # use the fitting function for convenience
  # fit the model
  glm_list[[i]] <- glm(f2, data = d_std,
                       family = binomial, x = TRUE) # note the x = TRUE
}

names(glm_list) <- names(data_list) # add names to the list of models


# check fitting metrics
summary_line1 <- summary_stats(glm_list, data_list) %>%
  round(3)

# VADIS 1st line of evidence (constraint significance)
signif_line <- vadis_line1(glm_list)
signif_line$signif.table
signif_line$distance.matrix
signif_line$similarity.scores


# VADIS 2nd line of evidence (magnitude of effects)
coef_line <- vadis_line2(glm_list)
coef_line$coef.table %>% 
  round(3)
coef_line$distance.matrix %>% 
  round(3)
coef_line$similarity.scores %>% 
  arrange(desc(Similarity))


# creating imaginary opposite variants
d <- data.frame(
  A = rep(c(-1,1), each = 5),
  B = rep(c(1,-1), each = 5),
  row.names = paste0("constraint.", 1:10))

# creating tuning dataframe
tune_df <- data.frame(matrix(NA, ncol = 5, nrow = length(data_list)))
names(tune_df) <- c("mtry", "min.node.size", "sample.fraction", "auc", "exec.time")

# tuning
for (i in seq_along(data_list)){
  d <- data_list[[i]][, all.vars(f2)]
  d$total_nonstandard_verb_agreement <- as.factor(d$total_nonstandard_verb_agreement)
  pv_task <- makeClassifTask(data = d, target = "total_nonstandard_verb_agreement")
  
  # Tuning process (takes around 1 minute); Tuning measure is the Area Under the Curve
  result <- tuneRanger(pv_task, measure = list(auc), num.trees = 1000, 
                       num.threads = 4, iters = 80, show.info = F) 
  
  tune_df[i,] <- result$recommended.pars
}

rownames(tune_df) <- names(data_list) # assigning name to the tuning dataframe

# creating the "forest"
rf_list <- vector("list")
for (i in seq_along(data_list)){
  d <- data_list[[i]]
  # fit the random forest and add it to the list
  rf_list[[i]] <- ranger(
    f2, 
    data = d,
    seed = 1234,
    num.trees = 1000,
    mtry = tune_df[i, "mtry"],
    min.node.size = tune_df[i, "min.node.size"],
    sample.fraction = tune_df[i, "sample.fraction"],
    probability = TRUE,
    importance = "permutation"
  )
}
names(rf_list) <- names(data_list) #assigning names to the forests

# checking fitting metrics
summary_stats(rf_list, data_list, response = "total_nonstandard_verb_agreement") %>% 
  round(3)

# VADIS 3rd line of evidence (variable importance ranking)
varimp_line <- vadis_line3(rf_list, conditional = FALSE)
varimp_line$varimp.table %>% 
  round(3)
varimp_line$rank.table
varimp_line$distance.matrix %>% round(5)

varimp_line$similarity.scores %>% 
  arrange(desc(Similarity))

# merge the similarity scores across the three lines of evidence, and arrive at mean scores for each sociolect.
mean_sims <- data.frame(
  line1 = signif_line$similarity.scores[,1], # get only the values in the 2nd column
  line2 = coef_line$similarity.scores[,1],
  line3 = varimp_line$similarity.scores[,1],
  row.names = names(data_list)
)

# calculate a mean of the mean similarity scores
mean_sims$mean <- rowMeans(mean_sims)
round(mean_sims, 3)

# core grammar coefficient!
mean(mean_sims$mean)

# create a combined distance matrix from the three lines of evidence
fused_dist <- analogue::fuse(signif_line$distance.matrix, 
                             coef_line$distance.matrix, 
                             varimp_line$distance.matrix)
round(fused_dist, 3)


# hierarchical clustering
hclust(fused_dist, method = "ward.D2") %>% 
  plot(main = "Hierarchical clustering of fused distances")

# divisive/unrooter clustering
par(mfrow = c(1,2))
cluster::diana(fused_dist) %>% 
  cluster::pltree(main = "Divisive clustering")
ape::nj(fused_dist) %>% 
  plot(type = "u", main = "Unrooted clustering")

# Distance matrices can also be fed into a multidimensional scaling analysis, which maps distances onto a 2 or 3 dimensional space. The closer the points are in this space, the more similar the sociolects' grammars.
line2_mds <- cmdscale(coef_line$distance.matrix, k = 2, eig = T)
line2_mds[[1]] %>%
  as.data.frame() %>% 
  mutate(genres = rownames(.)) %>% 
  ggplot(aes(V1, V2, label = genres)) +
  geom_point() +
  geom_text(nudge_y = .01, size = 4)

# creating 3D plots (box-like image)
dd <- line2_mds[[1]] %>%
  as.data.frame() 
library(scatterplot3d)
with(dd, {
  scttr <- scatterplot3d(x = V1, y = V2, type = "h", pch = 18)
  scttr_coords <- scttr$xyz.convert(V1, V2)
  text(scttr_coords$x, scttr_coords$y, labels = rownames(dd), pos = 3)
})


# run an MDS based on the fused distance matrix.
fused_mds <- cmdscale(fused_dist,k = 2, eig = T)

fused_mds[[1]] %>% # extract the coordinates
  as.data.frame() %>% 
  rownames_to_column("Variety") %>%
  plot_ly() %>%
  add_trace(x = ~V1, y = ~V2, 
            type = "scatter3d", inherit = F,
            marker = list(size = 4),
            mode = "markers") %>%
  add_text(x = ~V1, y = ~V2,
           text = ~ Variety,
           type = "scatter3d",
           mode = "markers",
           showlegend = FALSE)





# NeighborNet is a technique for constructing and visualizing phylogenetic relationships - using the 2nd line of evidence
line2_NNet <- phangorn::neighborNet(coef_line$distance.matrix)
plot(line2_NNet, "phylogram")


# now with the fused matrix
nets_fused_matrix <- phangorn::neighborNet(fused_dist)
plot(nets_fused_matrix, "phylogram")


# ====================== 
### RATIO OF NEGATION PARTICLES

f3 <-  total_negation_ratio ~ sex + schooling

glm_list <- vector("list") # empty list to store our models 

for (i in seq_along(data_list)){
  # i = 5
  d <- data_list[[i]]
  # now standardize t]he model fixed effects inputs before fitting.
  d_std <- stand(d, cols = f3) # use the fitting function for convenience
  # fit the model
  glm_list[[i]] <- glm(f3, data = d_std,
                       family = binomial, x = TRUE) # note the x = TRUE
}

names(glm_list) <- names(data_list) # add names to the list of models


# check fitting metrics
summary_line1 <- summary_stats(glm_list, data_list) %>%
  round(3)

# VADIS 1st line of evidence (constraint significance)
signif_line <- vadis_line1(glm_list)
signif_line$signif.table
signif_line$distance.matrix
signif_line$similarity.scores


# VADIS 2nd line of evidence (magnitude of effects)
coef_line <- vadis_line2(glm_list)
coef_line$coef.table %>% 
  round(3)
coef_line$distance.matrix %>% 
  round(3)
coef_line$similarity.scores %>% 
  arrange(desc(Similarity))


# creating imaginary opposite variants
d <- data.frame(
  A = rep(c(-1,1), each = 5),
  B = rep(c(1,-1), each = 5),
  row.names = paste0("constraint.", 1:10))

# creating tuning dataframe
tune_df <- data.frame(matrix(NA, ncol = 5, nrow = length(data_list)))
names(tune_df) <- c("mtry", "min.node.size", "sample.fraction", "auc", "exec.time")

# tuning
for (i in seq_along(data_list)){
  d <- data_list[[i]][, all.vars(f3)]
  d$total_negation_ratio <- as.factor(d$total_negation_ratio)
  pv_task <- makeClassifTask(data = d, target = "total_negation_ratio")
  
  # Tuning process (takes around 1 minute); Tuning measure is the Area Under the Curve
  result <- tuneRanger(pv_task, measure = list(auc), num.trees = 1000, 
                       num.threads = 4, iters = 80, show.info = F) 
  
  tune_df[i,] <- result$recommended.pars
}

rownames(tune_df) <- names(data_list) # assigning name to the tuning dataframe

# creating the "forest"
rf_list <- vector("list")
for (i in seq_along(data_list)){
  d <- data_list[[i]]
  # fit the random forest and add it to the list
  rf_list[[i]] <- ranger(
    f3, 
    data = d,
    seed = 1234,
    num.trees = 1000,
    mtry = tune_df[i, "mtry"],
    min.node.size = tune_df[i, "min.node.size"],
    sample.fraction = tune_df[i, "sample.fraction"],
    probability = TRUE,
    importance = "permutation"
  )
}
names(rf_list) <- names(data_list) #assigning names to the forests

# checking fitting metrics
summary_stats(rf_list, data_list, response = "total_nonstandard_verb_agreement") %>% 
  round(3)

# VADIS 3rd line of evidence (variable importance ranking)
varimp_line <- vadis_line3(rf_list, conditional = FALSE)
varimp_line$varimp.table %>% 
  round(3)
varimp_line$rank.table
varimp_line$distance.matrix %>% round(5)

varimp_line$similarity.scores %>% 
  arrange(desc(Similarity))

# merge the similarity scores across the three lines of evidence, and arrive at mean scores for each sociolect.
mean_sims <- data.frame(
  line1 = signif_line$similarity.scores[,1], # get only the values in the 2nd column
  line2 = coef_line$similarity.scores[,1],
  line3 = varimp_line$similarity.scores[,1],
  row.names = names(data_list)
)

# calculate a mean of the mean similarity scores
mean_sims$mean <- rowMeans(mean_sims)
round(mean_sims, 3)

# core grammar coefficient!
mean(mean_sims$mean)

# create a combined distance matrix from the three lines of evidence
fused_dist <- analogue::fuse(signif_line$distance.matrix, 
                             coef_line$distance.matrix, 
                             varimp_line$distance.matrix)
round(fused_dist, 3)


# hierarchical clustering
hclust(fused_dist, method = "ward.D2") %>% 
  plot(main = "Hierarchical clustering of fused distances")

# divisive/unrooter clustering
par(mfrow = c(1,2))
cluster::diana(fused_dist) %>% 
  cluster::pltree(main = "Divisive clustering")
ape::nj(fused_dist) %>% 
  plot(type = "u", main = "Unrooted clustering")

# Distance matrices can also be fed into a multidimensional scaling analysis, which maps distances onto a 2 or 3 dimensional space. The closer the points are in this space, the more similar the sociolects' grammars.
line2_mds <- cmdscale(coef_line$distance.matrix, k = 2, eig = T)
line2_mds[[1]] %>%
  as.data.frame() %>% 
  mutate(genres = rownames(.)) %>% 
  ggplot(aes(V1, V2, label = genres)) +
  geom_point() +
  geom_text(nudge_y = .01, size = 4)

# creating 3D plots (box-like image)
dd <- line2_mds[[1]] %>%
  as.data.frame() 
library(scatterplot3d)
with(dd, {
  scttr <- scatterplot3d(x = V1, y = V2, type = "h", pch = 18)
  scttr_coords <- scttr$xyz.convert(V1, V2)
  text(scttr_coords$x, scttr_coords$y, labels = rownames(dd), pos = 3)
})


# run an MDS based on the fused distance matrix.
fused_mds <- cmdscale(fused_dist,k = 2, eig = T)

fused_mds[[1]] %>% # extract the coordinates
  as.data.frame() %>% 
  rownames_to_column("Variety") %>%
  plot_ly() %>%
  add_trace(x = ~V1, y = ~V2, 
            type = "scatter3d", inherit = F,
            marker = list(size = 4),
            mode = "markers") %>%
  add_text(x = ~V1, y = ~V2,
           text = ~ Variety,
           type = "scatter3d",
           mode = "markers",
           showlegend = FALSE)





# NeighborNet is a technique for constructing and visualizing phylogenetic relationships - using the 2nd line of evidence
line2_NNet <- phangorn::neighborNet(coef_line$distance.matrix)
plot(line2_NNet, "phylogram")


# now with the fused matrix
nets_fused_matrix <- phangorn::neighborNet(fused_dist)
plot(nets_fused_matrix, "phylogram")


