# Import the data, filter, and log transform ####
Import <- read.csv("training_pool.csv")

# Select only the elemental data used for training the ANN
data_in <- Import[Import$is_ok == 1, 4:16]

# Log transform elements that look like they should be log transformed
log_cols <- c("TIO2.WT..", "K2O.WT..", "P2O5.WT..", "NB.PPM.", "CE.PPM.", "LA.PPM.")
data_in[,log_cols] <- log10(data_in[,log_cols])

# Remove all rows containing NA or inf
if(TRUE){
  for(i in 1:nrow(data_in)){
    for(j in 1:ncol(data_in)){
      if(is.infinite(data_in[i,j])){data_in[i,j] <- NA}
    }
  }
  data_in <- data_in[complete.cases(data_in),]
}

#View(data_in)

# Cluster analysis ####

# Normalize dataset
z <- data_in
m <- apply(z, 2, mean)
s <- apply(z, 2, sd)
z <- scale(z, m, s)

# Scree plot
wss <- numeric(0)
for(i in 2:20) wss[i] <- sum(kmeans(z, centers = i)$withinss)
plot(1:20, wss, type = "b", xlab = "Number of clusters", ylab = "Within group SS")

# calculate clusters and assign colors
k.c <- kmeans(z, 5)
k.c$size

clusters <- as.numeric(k.c$cluster)
col_clust <- rep(NA, length(clusters))
col_clust[clusters == 1] <- "red"
col_clust[clusters == 2] <- "blue"
col_clust[clusters == 3] <- "green"
col_clust[clusters == 4] <- "orange"
col_clust[clusters == 5] <- "purple"

# Plot histograms and scatter plots ####

# Histograms
pdf("C:/Users/Lowell Moore/Desktop/inclusions_hist.pdf", width = 12, height = 8)
par(mfrow = c(3, 5))
for(i in 1:ncol(data_in)){
  hist(data_in[,i], main = NULL, xlab = colnames(data_in)[i], ylab = "")
}
par(mfrow = c(1, 1))
dev.off()

# Scatter plots
pdf("C:/Users/Lowell Moore/Desktop/inclusions_scatter.pdf", width = 12, height = 8)
par(mfrow = c(3, 5))
for(i in 1:ncol(data_in)){
  plot(data_in[,i], data_in$CO2.PPM., main = NULL, xlab = colnames(data_in)[i], ylab = ""
       , pch = 21, cex = 0.7, col = col_clust)
}
par(mfrow = c(1, 1))
dev.off()

pdf("C:/Users/Lowell Moore/Desktop/scatter_matrix.pdf", width = 16, height = 16)
pairs(data_in, col = col_clust)
dev.off()

# Linear model ####

training <- sample(x = 1:nrow(data_in), size = round(0.66*nrow(data_in), 0))
testing <- which(!(1:nrow(data_in) %in% training))

training <- data_in[training,]
testing <- data_in[testing,]

linear <- lm(formula = CO2.PPM. ~ SIO2.WT..
             #+ TIO2.WT..
             + AL2O3.WT..
             + FeOT_calc
             #+ MGO.WT..
             #+ CAO.WT..
             + NA2O.WT..
             #+ K2O.WT..
             + P2O5.WT..
             + NB.PPM.
             #+ CE.PPM.
             #+ LA.PPM.
             , data = training)

prediction <- predict(linear, newdata = testing[, 2:ncol(testing)], level = 0.95, type = "response", interval = "p", na.action = na.exclude)

pdf("linear_prediction.pdf", width = 5, height = 5)
plot(prediction[,1], testing[,1])
abline(0, 1, col = "red")
dev.off()

summary(linear)

# Regression tree : rpart ####
library(rpart)

training <- sample(x = 1:nrow(data_in), size = round(0.66*nrow(data_in), 0))
testing <- which(!(1:nrow(data_in) %in% training))

training <- data_in[training,]
testing <- data_in[testing,]

tree1 <- rpart(formula = CO2.PPM. ~ SIO2.WT.. + TIO2.WT.. + AL2O3.WT.. + FeOT_calc + MGO.WT.. + CAO.WT.. + NA2O.WT.. + K2O.WT.. + P2O5.WT.. + NB.PPM. + CE.PPM. + LA.PPM.
              , data = training, method = "anova")
plot(tree1)
text(tree1)

prediction <- predict(tree1, newdata = testing[, 2:ncol(testing)])

plot(prediction, testing[,1])
abline(0, 1, col = "red")

# Regression tree : party ####
library(party)

training <- sample(x = 1:nrow(data_in), size = round(0.66*nrow(data_in), 0))
testing <- which(!(1:nrow(data_in) %in% training))

training <- data_in[training,]
testing <- data_in[testing,]

tree2 <- ctree(formula = CO2.PPM. ~ SIO2.WT.. + TIO2.WT.. + AL2O3.WT.. + FeOT_calc + MGO.WT.. + CAO.WT.. + NA2O.WT.. + K2O.WT.. + P2O5.WT.. + NB.PPM. + CE.PPM. + LA.PPM.
               , data = training)

# Save .pdf figure
pdf("tree2.pdf", width = 20, height = 8)
plot(tree2)
dev.off()

prediction <- predict(tree2, newdata = testing[, 2:ncol(testing)])

# Save .pdf figure
pdf("tree2_prediction.pdf", width = 5, height = 5)
plot(prediction, testing[,1])
abline(0, 1, col = "red")
dev.off()

# Artificial Nueral Network ####

library(neuralnet)
library(beepr)

# Attach cluster information to input dataframe
data_in.c <- cbind.data.frame(data_in, clusters)
data_in.c[1:10,]

training <- sample(x = 1:nrow(data_in.c), size = round(0.66*nrow(data_in.c), 0))
testing <- which(!(1:nrow(data_in.c) %in% training))

training <- data_in.c[training,]
testing <- data_in.c[testing,]

# Train neural network
nn <- neuralnet(formula = CO2.PPM. ~ SIO2.WT..
             + TIO2.WT..
             + AL2O3.WT..
             + FeOT_calc
             + MGO.WT..
             + CAO.WT..
             + NA2O.WT..
             + K2O.WT..
             + P2O5.WT..
             + NB.PPM.
             + CE.PPM.
             + LA.PPM.
             + clusters
             , data = training, hidden = c(15), threshold = 0.1, stepmax = 10^6, rep = 5, lifesign = "full", lifesign.step = 10^4)
beep(sound = 1)

# Check results
pdf("C:/Users/Lowell Moore/Desktop/net results 1.pdf", width = 8, height = 6)
par(mfrow = c(2, 3))
for(i in 1:5){
  results <- compute(nn, testing[,2:ncol(testing)], rep = i)$net
  correct <- testing[,1]
  plot(results, correct)
  abline(0, 1, col = "red")
  abline(0.5, 1, col = "red", lty = 2)
  abline(-0.5, 1, col = "red", lty = 2)
}
par(mfrow = c(1, 1))
dev.off()

# Colorful summary plot
ys_all <- numeric(0)
cols <- c("red", "orange", "yellow", "green", "blue")
plot(0, 0, cex = 0, xlab = "Correct", ylab = "Computed"
     , xlim = c(0, 6), ylim = c(0, 6))
for(i in -1:1){abline(i, 1)}
xs <- correct

for(i in 1:5){
  ys <-compute(nn, testing[,2:ncol(testing)], rep = i)$net
  ys_all <- cbind(ys_all, ys)
  points(xs, ys, col = cols[i])
}
ys <- numeric(0); for(i in 1:nrow(ys_all)){ys[i] <- mean(ys_all[i,])}
points(xs, ys, pch = 21, bg = "purple")
print(cor(xs, ys))

# General weight plots
pdf("standard gwplot.pdf", width = 12, height = 9)
par(mfrow = c(3, 5))
for(i in 1:13){
  gwplot(nn, rep = 1,selected.covariate = i)
}
par(mfrow = c(1, 1))
dev.off()

# Colorful GW plot
ys_all <- numeric(0)
var_i <- 2; print(colnames(testing))
cols <- c("red", "orange", "yellow", "green", "blue")
xs <- testing[,var_i]
plot(0, 0, cex = 0, xlab = colnames(testing)[var_i], ylab = "GW"
     , xlim = c(min(xs), max(xs))
     , ylim = c(-2, 2)
)
for(i in 1:5){
  ys <- nn$generalized.weights[[i]][,var_i]
  points(xs, ys, col = cols[i])
  ys_all <- cbind(ys_all, ys)
  print(length(which(ys == 0)))
}
ys <- numeric(0); for(i in 1:nrow(ys_all)){ys[i] <- mean(ys_all[i,])}
points(xs, ys, pch = 21, bg = "purple")

var_i <- var_i + 1; ys_all <- numeric(0)
