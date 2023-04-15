# Paso 1: cargar los datos
datos <- read.csv('train_data.csv',header = TRUE)
str(datos)

# Paso 2: Eliminamos todos los datos que no sean numericos


datos <- datos[, -which(names(datos) == "num_access_files")]
datos <- datos[, -which(names(datos) == "is_host_login")]
datos <- datos[, -which(names(datos) == "is_guest_login")]
datos <- datos[, -which(names(datos) == "num_outbound_cmds")]
datos <- datos[, -which(names(datos) == "duration")]
datos <- datos[, -which(names(datos) == "protocol_type")]
datos <- datos[, -which(names(datos) == "service")]
datos <- datos[, -which(names(datos) == "flag")]
datos$class <- ifelse(datos$class == "normal", 0, 1)

head(datos,5)
str(datos)

# Cargar la librería necesaria para realizar PCA
library(stats)



# Realizar PCA
pca <- prcomp(datos[,1:33], center = TRUE, scale. = TRUE)


# Seleccionar el número de componentes principales que se desean utilizar
num_comp <- 10
pca_data <- data.frame(pca$x[,1:num_comp], class = datos$class)


# Separar la base de datos en un conjunto de entrenamiento y otro de prueba
library(caTools)
set.seed(123)
spl <- sample.split(pca_data$class, SplitRatio = 0.7)
train <- subset(pca_data, spl == TRUE)
test <- subset(pca_data, spl == FALSE)


# Crear el modelo de regresión logística
model <- glm(class ~ ., data = train, family = binomial)


# Hacer predicciones con el modelo
pred <- predict(model, newdata = test, type = "response")

# Evaluar el modelo
library(pROC)
roc(test$class, pred)

library(caret)
# Convertir las predicciones en clases (1 o 0) en lugar de probabilidades
predicted_classes <- ifelse(pred > 0.5, 1, 0)

# Crear la matriz de confusión
conf_mat <- confusionMatrix(factor(predicted_classes), factor(test$class))

accuracy <- conf_mat$overall['Accuracy']
precision <- conf_mat$byClass['Precision'][1]
recall <- conf_mat$byClass['Recall'][1]


accuracy
