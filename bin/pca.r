# Load/run the main Python script
source_python("src/rfe.py")
support <- support("C:\\DM2\\r-binary-classification")



Churn <- normalizedDataset$isChurn
drops <- c("isChurn")


#PCA doesn't help the company to understand what variables are more important
pca<- prcomp( normalizedDataset[ ,!(names(normalizedDataset) %in% drops)], 
              center = TRUE, 
              scale. = TRUE)



# Plot of the variances (y-axis)
# associated with the PCs (x-axis).
plot(pca, type = "l")


summary(pca)


comp3 <- pca$x[,1:4]
comp3[,4] <- normalizedDataset$isChurn
colnames(comp3)[4] <- 'isChurn'
comp3 <- as.data.frame(comp3)
