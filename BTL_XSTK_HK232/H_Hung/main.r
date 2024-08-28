library(dplyr)
library(stringr)
library("tidyverse")
library(corrplot)
library(datarium)
library(car)
library(MASS)
library(DescTools)


all_gpus <- read.csv("D:/WORKSPACE/Data Science/Data_PreProcessing/Rcode/H_Hung/All_GPUs.csv")

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
  "Max_Power",
  "Memory",
  "Memory_Bandwidth",
  "Memory_Bus",
  "Memory_Speed",
  "Release_Date"
)

new_GPUs <- all_gpus[, c(selected_column)]

new_GPUs$Memory_Bandwidth <- ifelse(
  is.na(new_GPUs$Memory_Bandwidth),
  NA,
  ifelse(
    str_detect(new_GPUs$Memory_Bandwidth, "MB/sec"),
    as.numeric(gsub("MB/sec", "", new_GPUs$Memory_Bandwidth)) / 1024,
    as.numeric(gsub("GB/sec", "", new_GPUs$Memory_Bandwidth))
  )
)

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

head(new_GPUs)

missing_counts <- colSums(is.na(new_GPUs))

# In số lượng dữ liệu khuyết của từng cột
print(missing_counts)
for (c in colnames(new_GPUs)) {
  new_GPUs[[c]] <- ifelse(
    is.na(new_GPUs[[c]]),
    NA,
    as.numeric(gsub("[^0-9.]", "", new_GPUs[[c]]))
  )
}

for (c in colnames(new_GPUs)) {
  ifelse(
    missing_data$Percentage_Missing[missing_data$Column == c] < 3,
    new_GPUs <- new_GPUs[!is.na(new_GPUs[[c]]), ],
    new_GPUs[[c]] <- ifelse(
      is.na(new_GPUs[[c]]),
      median(new_GPUs[[c]], na.rm = TRUE),
      new_GPUs[[c]]
    )
  )
}


hist(new_GPUs$Max_Power,xlab="Max_Power",ylab="Frequency",main="Histogram of Max_Power",xlim = c(0,2000))


corrplot(
  cor(new_GPUs),
  method = "color",
  type = "full",
  number.cex = 1,
  tl.cex = 0.8,
  addCoef.col = "orange"
)

head(new_GPUs)

shapiro.test(new_GPUs$Memory_Bandwidth)

model <- lm(
  Memory_Bandwidth ~ .,
  data = new_GPUs
)
summary(model)


calculate_mode <- function(x) {
  return (Mode(x))
}

for(c in colnames(new_GPUs)){
  print(paste("Mode of ", c, " ", calculate_mode(new_GPUs[[c]])))
}
# Thong ke mo ta

summary(new_GPUs, digits = 10)
model <- lm(Memory_Bandwidth ~ Max_Power + Memory + Memory_Bus + Memory_Speed + Release_Date, data = new_GPUs)
vif(model)

stepwise_model <- stepAIC(lm(Memory_Bandwidth ~ Max_Power + Memory + Memory_Bus + Memory_Speed + Release_Date, data = new_GPUs) , direction = "both")
summary(stepwise_model)

