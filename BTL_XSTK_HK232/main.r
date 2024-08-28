library(dplyr)
library(stringr)
library("tidyverse")
library(corrplot)
library(datarium)
library(car)
library(MASS)
library(DescTools)
library(ggpubr)
library(ggplot2)
library(agricolae)


# Pre process
all_gpus <- read.csv("All_GPUs.csv")
head(all_gpus, 6)

all_gpus <- all_gpus %>%
  replace(., . == "", NA) %>%
  replace(., . == "\n- ", NA) %>%
  replace(., . == "None ", NA)

missing_data <- data.frame(
  Column = colnames(all_gpus),
  Percentage_Missing = sapply(all_gpus, function(x) sum(is.na(x)) / nrow(all_gpus) * 100)
)

ggplot(
  missing_data,
  aes(x = Column, y = Percentage_Missing)
) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(
    aes(label = paste0(round(Percentage_Missing, 2), "%")),
    vjust = -0.5,
    size = 3
  ) +
  labs(
    title = "Missing Data Percentage by Column",
    x = "Column",
    y = "Percentage Missing"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

selected_column <- c(
  "Manufacturer",
  "Process",
  "Max_Power",
  "Memory",
  "Memory_Bandwidth",
  "Memory_Bus",
  "Memory_Speed",
  "Release_Date"
)


new_GPUs <- all_gpus[, c(selected_column)]
head(new_GPUs$Memory_Bandwidth[10:20])
new_GPUs$Memory_Bandwidth <- ifelse(
  is.na(new_GPUs$Memory_Bandwidth),
  NA,
  ifelse(
    str_detect(new_GPUs$Memory_Bandwidth, "MB/sec"),
    as.numeric(gsub("MB/sec", "", new_GPUs$Memory_Bandwidth)) / 1024,
    as.numeric(gsub("GB/sec", "", new_GPUs$Memory_Bandwidth))
  )
)
print(new_GPUs$Memory_Bandwidth)
replaceMonth <- function(x) {
  x <- gsub("Jan", "01", x)
  x <- gsub("Feb", "02", x)
  x <- gsub("Mar", "03", x)
  x <- gsub("Apr", "04", x)
  x <- gsub("May", "05", x)
  x <- gsub("Jun", "06", x)
  x <- gsub("Jul", "07", x)
  x <- gsub("Aug", "08", x)
  x <- gsub("Sep", "09", x)
  x <- gsub("Oct", "10", x)
  x <- gsub("Nov", "11", x)
  x <- gsub("Dec", "12", x)
  return(x)
}
#head(new_GPUs)
new_GPUs$Memory_Speed <- gsub(" MHz", "", new_GPUs$Memory_Speed)
new_GPUs$Memory_Speed <- as.numeric(new_GPUs$Memory_Speed)

new_GPUs$Memory_Bus <- gsub(" Bit", "", new_GPUs$Memory_Bus)
new_GPUs$Memory_Bus <- as.numeric(new_GPUs$Memory_Bus)

new_GPUs$Memory <- gsub(" MB", "", new_GPUs$Memory)
new_GPUs$Memory <- as.numeric(new_GPUs$Memory)

head(new_GPUs)
boxplot(Memory_Speed ~ Manufacturer, data = new_GPUs, 
        main = "Boxplot of Memory by Manufacturer", 
        xlab = "Manufacturer", ylab = "Memory_Speed")
new_GPUs$Release_Date <- trimws(new_GPUs$Release_Date)
unknown_date <- new_GPUs$Release_Date == "Unknown Release Date"
new_GPUs$Release_Date[unknown_date] <- NA
new_GPUs$Release_Date <- replaceMonth(new_GPUs$Release_Date)
new_GPUs$Release_Date <- (
  as.numeric(
    format(as.Date(new_GPUs$Release_Date, format = "%d-%m-%Y"), "%Y")
  ) +
    as.numeric(
      format(as.Date(new_GPUs$Release_Date, format = "%d-%m-%Y"), "%m")
    ) / 12
)


missing_counts <- sapply(new_GPUs, function(x) sum(is.na(x)))
print(missing_counts)
for (c in colnames(new_GPUs)) {
  new_GPUs[[c]] <- ifelse(
    is.na(new_GPUs[[c]]),
    median(new_GPUs[[c]], na.rm = TRUE),
    new_GPUs[[c]]
  )
}
missing_counts <- sapply(new_GPUs, function(x) sum(is.na(x)))
print(missing_counts)
boxplot(Memory_Speed ~ Manufacturer, data = new_GPUs, 
        main = "Boxplot of Memory by Manufacturer", 
        xlab = "Manufacturer", ylab = "Memory_Speed")
missing_counts <- sapply(new_GPUs, function(x) sum(is.na(x)))
print(missing_counts)
#GPU_number <- new_GPUs[, !colnames(new_GPUs) %in% "Manufacturer"]
head(GPU_number, 6)
summary(GPU_number)
corrplot(
  cor(GPU_number),
  method = "color",
  type = "full",
  number.cex = 1,
  tl.cex = 0.8,
  addCoef.col = "orange"
)


# boxplot and histogram, xlab is the name of the column
for (c in colnames(GPU_number)) {
  boxplot(GPU_number[[c]], main = c, xlab = c)
  hist(GPU_number[[c]], main = c, xlab = c)
}
#boxplot(GPU_number$, main = c, xlab = c)
# Tạo một danh sách để lưu trữ các đối tượng biểu đồ
qq_plots <- list()

for (c in colnames(GPU_number)) {
  qq_plots[[c]] <- ggqqplot(GPU_number[[c]], ylab = c) +
    labs(x = "Theoretical") # Chỉ định nhãn cho trục x
}

# Hiển thị các biểu đồ trong danh sách
for (plot in qq_plots) {
  print(plot)
}

# Inferential statistic

# 1. shapiro test
shapiro.test(new_GPUs$Memory_Bandwidth)



# 2.Kiem dinh 1 mau
t.test(new_GPUs$Memory_Bandwidth, conf.level = 0.95)

# 3. Kiem dinh 2 mau
before_2016 <- subset(new_GPUs, Release_Date < 2016)
after_2016 <- subset(new_GPUs, Release_Date >= 2016)

min_rows <- min(nrow(before_2016), nrow(after_2016))

before_2016 <- before_2016[1:min_rows, ]
after_2016 <- after_2016[1:min_rows, ]

df <- data.frame(
  "Before_2016" = before_2016$Memory_Bandwidth,
  "After_2016" = after_2016$Memory_Bandwidth
)

shapiro.test(df$Before_2016)
shapiro.test(df$After_2016)
var.test(df$Before_2016, df$After_2016)
t.test(df$Before_2016, df$After_2016,
  alternative =
    c("less"), conf.level = 0.95
)

# 4 Phan tich phuong sai
print(Manufacture_table <- table(new_GPUs$Manufacturer))

Manufacturer_filter <- new_GPUs[new_GPUs$Manufacturer %in% c("AMD", "Nvidia"), ]
# table(Manufacturer_filter)


bartlett.test(Manufacturer_filter$Memory_Bandwidth ~ Manufacturer, data = Manufacturer_filter)

# # Kiem dinh ANOVA
anova_Result <- aov(Memory_Bandwidth ~ Manufacturer, data = Manufacturer_filter)
summary(anova_Result)

# # Phan tich sau anova
print(LSD.test(anova_Result, "Manufacturer", alpha = 0.05))
pairwise.t.test(Manufacturer_filter$Memory_Bandwidth, Manufacturer_filter$Manufacturer, p.adjust.method = "none")



# Hoi quy tuyen tinh don
model_single <- lm(formula = Memory_Bandwidth ~ Max_Power, data = new_GPUs)
summary(model_single)


# Hoi quy tuyen tinh boi
model <- lm(formula = Memory_Bandwidth ~ . - Manufacturer, data = new_GPUs)
summary(model)

# # check VIF
vif(model)
print(model)


# # Thiết lập vị trí các biểu đồ trên cửa sổ
par(mfrow = c(2, 2))
# # Dùng hàm plot() cho kết quả mô hình đã xây dựng
plot(model, pch = 20)


# # predict
set.seed(50)
sample <- sample(c(T, F), nrow(new_GPUs), replace = T, prob = c(0.8, 0.2))
train <- new_GPUs[sample, ]
test <- new_GPUs[!sample, ]


model1 <- lm(Memory_Bandwidth ~ ., data = train)
summary(model1)
coef(model1)
predict(model1, newdata = test)
pred <- data.frame(predict(model1, newdata = test))
compare <- cbind(test$Memory_Bandwidth, pred)
colnames(compare) <- c("Test_set", "Prediction")
head(compare, 10)

SSE <- sum((test$Memory_Bandwidth - pred)^2)
SST <- sum((test$Memory_Bandwidth - mean(test$Memory_Bandwidth))^2)
cat("The accuracy of the model on test set: ", round((1 - SSE / SST) * 100, 2), "%")


# # Poly regression
poly_model <- lm(formula = Memory_Bandwidth ~ . - Manufacturer, data = new_GPUs)

summary(poly_model)
max(coef(poly_model))

# # predict
set.seed(50)
sample <- sample(c(T, F), nrow(new_GPUs), replace = T, prob = c(0.8, 0.2))
train <- new_GPUs[sample, ]
test <- new_GPUs[!sample, ]

# List of predictor variables
model_predictor <- c("Max_Power", "Memory", "Memory_Bus", "Memory_Speed", "Release_Date")

buildFormula <- function(d) {
  fm <- ""
  for (c in model_predictor) {
    fm <- paste(fm, "+", paste("poly(", c, ",", d, ")"))
  }
  fm <- paste("Memory_Bandwidth ~", substr(fm, 3, nchar(fm)))
  formula <- as.formula(fm)
  return(formula)
}


for (d in 1:6) {
  print(paste("[+] Degree:", d))
  modelPoly10 <- lm(
    buildFormula(d),
    data = train
  )
  print(summary(modelPoly10))
}

# # Apply the model to the test data4
degree <- 1
linear_formula <- as.formula(paste("Memory_Bandwidth ~ poly(Max_Power, ", degree, ") + poly(Memory, ", degree, ") + poly(Memory_Bus, ", degree, ") + poly(Memory_Speed, ", degree, ") + poly(Release_Date, ", degree, ")", sep = ""))
linear <- lm(formula = linear_formula, data = train)

degree <- 6
poly_formula <- as.formula(paste("Memory_Bandwidth ~ poly(Max_Power, ", degree, ") + poly(Memory, ", degree, ") + poly(Memory_Bus, ", degree, ") + poly(Memory_Speed, ", degree, ") + poly(Release_Date, ", degree, ")", sep = ""))
poly6 <- lm(formula = poly_formula, data = train)

test_predictors <- subset(test, select = -c(Manufacturer))
predictions_poly6 <- predict(poly6, newdata = test_predictors)
predictions_linear <- predict(linear, newdata = test_predictors)
error_poly6 <- abs(predictions_poly6 - test$Memory_Bandwidth)
error_linear <- abs(predictions_linear - test$Memory_Bandwidth)
model_type <- ifelse(error_poly6 < error_linear, "Poly6", "Linear")

# # Create a data frame with the requested information
result_df <- data.frame(
  testing_Memory_Bandwidth = test$Memory_Bandwidth, predictedbyPoly6 = predictions_poly6, predictedbyLinear = predictions_linear,
  DifferencePoly6 = error_poly6, DifferenceLinear = error_linear, model_type
)
print(result_df)

# # Create a table of counts for each 'model_type'
model_type_table <- table(model_type)

# # Calculate the percentages
percentage_poly6 <- (model_type_table["Poly6"] / sum(model_type_table)) * 100
percentage_linear <- (model_type_table["Linear"] / sum(model_type_table)) * 100

# # Print the results
cat(
  "\nPercentage of observations classified as Poly6:", percentage_poly6, "%",
  "\nPercentage of observations classified as Linear:", percentage_linear, "%\n"
)

