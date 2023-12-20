library(readr)
library(dplyr)
library(randomForest)
library(scales)

# Чтение данных
data1 <- read_csv("Hepatotoxicity ECFP2 fingerprints.csv")
data2 <- read_csv("Hepatotoxicity PaDEL 2D.csv")
data3 <- read_csv("Hepatotoxicity QNA.csv")

# Удаление дублирующихся колонок
data2 <- data2[, -which(names(data2) %in% c("Generic.name", "DrugBankID", "class", "Dosage"))]
data3 <- data3[, -which(names(data3) %in% c("Generic.name", "DrugBankID", "class", "Dosage"))]

# Объединение данных
combined_data <- cbind(data1, data2, data3)

# Тренировка модели Random Forest для определения важности признаков
set.seed(123)  # Для воспроизводимости
rf_model <- randomForest(x = combined_data[, -c(1:4)], y = factor(combined_data$class), importance = TRUE)

# Получение имен важных признаков и их значений важности
importance_values <- importance(rf_model)
feature_names <- rownames(importance_values)

# Выбор признаков, суммируя до 90% важности
total_importance <- sum(importance_values[,1])
cumulative_importance <- 0
selected_features <- c()

for (feature in feature_names[order(importance_values[,1], decreasing = TRUE)]) {
  if (cumulative_importance < 0.9 * total_importance) {
    selected_features <- c(selected_features, feature)
    cumulative_importance <- cumulative_importance + importance_values[feature,1]
  } else {
    break
  }
}

# Создание нового датасета с выбранными признаками
final_data <- cbind(combined_data[, 1:4], combined_data[, selected_features])
library(DT)
datatable(final_data)
# Дальнейший анализ или моделирование с использованием final_data


library(caret)
library(e1071)
library(ggplot2)

  # Нормализация признаков
preProcValues <- preProcess(final_data[, -c(1:2)], method = c("center", "scale"))
normalized_data <- predict(preProcValues, final_data[, -c(1:2)])
normalized_data <- cbind(final_data[, 1:2], normalized_data)

# Разбиение на обучающую и тестовую выборки
set.seed(123)
splitIndex <- createDataPartition(normalized_data$class, p = .8, list = FALSE)
train_data <- normalized_data[splitIndex,]
test_data <- normalized_data[-splitIndex,]

# Удаление первых двух столбцов (DrugBankID и Generic.name)
train_data <- train_data[, -c(1,2)]
test_data <- test_data[, -c(1,2)]

# Загрузка необходимых библиотек
library(e1071)
library(caret)
library(ggplot2)
library(Metrics)

# Загрузка данных (предполагаем, что данные загружены и предобработаны, как в вашем коде)

# Удаление константных столбцов
train_data <- train_data[, sapply(train_data, function(x) length(unique(x)) > 1)]

# Удаление столбцов с NA или NaN
train_data <- na.omit(train_data)

# Проверка и преобразование 'class' в фактор
train_data$class <- as.factor(train_data$class)

# Убедитесь, что тестовые данные также обработаны аналогичным образом

# Обучение модели SVM
svm_model_class <- svm(class ~ ., data = train_data[, -which(names(train_data) == "Dosage")], 
                       type = "C-classification", kernel = "radial")
predictions_class <- predict(svm_model_class, test_data[, -which(names(test_data) == "Dosage")])
# Преобразование предсказаний и истинных значений в факторы
predictions_class_factor <- factor(predictions_class, levels = levels(factor(train_data$class)))
test_class_factor <- factor(test_data$class, levels = levels(factor(train_data$class)))

# Считаем матрицу путаницы
confusion_matrix_class <- confusionMatrix(predictions_class_factor, test_class_factor)

# Для регрессии Dosage
svm_model_dosage <- svm(Dosage ~ ., data = train_data[, -which(names(train_data) == "class")], 
                        type = "eps-regression", kernel = "radial")
predictions_dosage <- predict(svm_model_dosage, test_data[, -which(names(test_data) == "class")])
mse_dosage <- mse(test_data$Dosage, predictions_dosage)

# Вывод метрик и графиков
print(confusion_matrix_class)
print(paste("Mean Squared Error for Dosage: ", mse_dosage))



# График матрицы ошибок для class
confusion_matrix_data <- as.data.frame(confusion_matrix_class$table)
colnames(confusion_matrix_data) <- c("Reference", "Prediction", "Frequency")

ggplot(confusion_matrix_data, aes(x = Reference, y = Prediction)) +
  geom_tile(aes(fill = Frequency), color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  theme_minimal() +
  ggtitle("Confusion Matrix for Class Prediction") +
  xlab("Actual Class") + ylab("Predicted Class")

# График сравнения фактического и предсказанного Dosage
compare_dosage <- data.frame(Actual = test_data$Dosage, Predicted = predictions_dosage)

ggplot(compare_dosage, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.4) +
  geom_abline(color = "red", linetype = "dashed") +
  theme_minimal() +
  ggtitle("Actual vs Predicted Dosage") +
  xlab("Actual Dosage") + ylab("Predicted Dosage")

