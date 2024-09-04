# import libraries
install.packages("ggplot2")
library(ggplot2)

# import files
df_large = read.csv("pythonAUC_large_datasets.csv")
df_small = read.csv("pythonAUC_small_datasets.csv")

df_balanced = read.csv("pythonAUC_balanced_datasets.csv")
df_imbalanced = read.csv("pythonAUC_imbalanced_datasets.csv")

df_feature_75 = read.csv("pythonAUC_features_ds75.csv")
df_feature_367 = read.csv("pythonAUC_features_ds367.csv")
df_feature_722 = read.csv("pythonAUC_features_ds722.csv")

df_binary = read.csv("pythonAUC_binary_datasets.csv")
df_ordinal = read.csv("pythonAUC_ordinal_datasets.csv")
df_categorical = read.csv("pythonAUC_categorical_datasets.csv")

# append a new column to store feature numbers
feature_num_75 = c(135, 110, 95, 75, 60, 45, 30, 15, 5)
df_feature_75$feature_number = feature_num_75
df_feature_75
feature_num_367 = c(100, 88, 76, 64, 52, 40, 28, 16, 4)
df_feature_367$feature_number = feature_num_367
df_feature_367
feature_num_722 = c(78, 69, 60, 51, 42, 33, 24, 15, 6)
df_feature_722$feature_number = feature_num_722
df_feature_722


# assign column names
names = c('LR', 'RF', 'KNN', 'SVM')

# Boxplot of auc scores in large datasets and small datasets
par(mfrow = c(1,2))
boxplot(df_large, col = c('red','blue','green','yellow'),
        names = c('LR', 'RF', 'KNN', 'SVM'),
        xlab = 'classifiers', ylab = 'AUC score', 
        main = 'boxplot of performance in large datasets')
boxplot(df_small, col = c('red','blue','green','yellow'),
        names = c('LR', 'RF', 'KNN', 'SVM'),
        xlab = 'classifiers', ylab = 'AUC score', 
        main = 'boxplot of performance in small datasets')

# Boxplot of auc scores in balanced and imbalanced datasets
par(mfrow = c(1,2))
boxplot(df_balanced, col = c('red','blue','green','yellow'),
        names = c('LR', 'RF', 'KNN', 'SVM'),
        xlab = 'classifiers', ylab = 'AUC score', 
        main = 'boxplot of performance in balanced datasets')
boxplot(df_imbalanced, col = c('red','blue','green','yellow'),
        names = c('LR', 'RF', 'KNN', 'SVM'),
        xlab = 'classifiers', ylab = 'AUC score', 
        main = 'boxplot of performance in imbalanced datasets')


# feature 75: plot scatterplots and trends for more and less features datasets
p <- ggplot(data = df_feature_75, aes(x = feature_number)) +
  geom_point(aes(y = Logistic.Regression, colour = "LR")) + 
  geom_smooth(aes(y = Logistic.Regression, colour = "LR"), method = "loess") +
  geom_point(aes(y = Random.Forest, colour = "RF")) +  
  geom_smooth(aes(y = Random.Forest, colour = "RF"), method = "loess") +
  geom_point(aes(y = K.Nearest.Neighbors, colour = "KNN")) +
  geom_smooth(aes(y = K.Nearest.Neighbors, colour = "KNN"), method = "loess") + 
  geom_point(aes(y = Support.Vector.Machine, colour = "SVM")) +
  geom_smooth(aes(y = Support.Vector.Machine, colour = "SVM"), method = "loess") + 
  labs(title = "Scatterpoints and trends of four classifiers in dataset 75",
       x = "Feature Numbers",
       y = "AUC Score",
       colour = "Classifier") +
  theme_minimal() 

print(p)
  
# feature 367: plot scatterplots and trends for more and less features datasets
p <- ggplot(data = df_feature_367, aes(x = feature_number)) +
  geom_point(aes(y = Logistic.Regression, colour = "LR")) + 
  geom_smooth(aes(y = Logistic.Regression, colour = "LR"), method = "loess") +
  geom_point(aes(y = Random.Forest, colour = "RF")) +  
  geom_smooth(aes(y = Random.Forest, colour = "RF"), method = "loess") +
  geom_point(aes(y = K.Nearest.Neighbors, colour = "KNN")) +
  geom_smooth(aes(y = K.Nearest.Neighbors, colour = "KNN"), method = "loess") + 
  geom_point(aes(y = Support.Vector.Machine, colour = "SVM")) +
  geom_smooth(aes(y = Support.Vector.Machine, colour = "SVM"), method = "loess") + 
  labs(title = "Scatterpoints and trends of four classifiers in dataset 367",
       x = "Feature Numbers",
       y = "AUC Score",
       colour = "Classifier") +
  theme_minimal() 

print(p)

# feature 722: plot scatterplots and trends for more and less features datasets
p <- ggplot(data = df_feature_722, aes(x = feature_number)) +
  geom_point(aes(y = Logistic.Regression, colour = "LR")) + 
  geom_smooth(aes(y = Logistic.Regression, colour = "LR"), method = "loess") +
  geom_point(aes(y = Random.Forest, colour = "RF")) +  
  geom_smooth(aes(y = Random.Forest, colour = "RF"), method = "loess") +
  geom_point(aes(y = K.Nearest.Neighbors, colour = "KNN")) +
  geom_smooth(aes(y = K.Nearest.Neighbors, colour = "KNN"), method = "loess") + 
  geom_point(aes(y = Support.Vector.Machine, colour = "SVM")) +
  geom_smooth(aes(y = Support.Vector.Machine, colour = "SVM"), method = "loess") + 
  labs(title = "Scatterpoints and trends of four classifiers in dataset 722",
       x = "Feature Numbers",
       y = "AUC Score",
       colour = "Classifier") +
  theme_minimal() 

print(p)


# Boxplot of auc scores in binary, ordinal and categorical datasets
par(mfrow = c(1,3))
boxplot(df_binary, col = c('red','blue','green','yellow'),
        names = c('LR', 'RF', 'KNN', 'SVM'),
        xlab = 'classifiers', ylab = 'AUC score', 
        main = 'boxplot of performance in binary datasets')
boxplot(df_ordinal, col = c('red','blue','green','yellow'),
        names = c('LR', 'RF', 'KNN', 'SVM'),
        xlab = 'classifiers', ylab = 'AUC score', 
        main = 'boxplot of performance in ordinal datasets')
boxplot(df_categorical, col = c('red','blue','green','yellow'),
        names = c('LR', 'RF', 'KNN', 'SVM'),
        xlab = 'classifiers', ylab = 'AUC score', 
        main = 'boxplot of performance in categorical datasets')
