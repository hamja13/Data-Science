dataSet<-read.csv("D:/data.csv",header=TRUE,sep=",")
dataSet
summary(dataSet)
str(dataSet)


incorrect_values<-dataSet$age < 0 | dataSet$impluse < 0 | dataSet$pressurehight <0 | dataSet$pressurelow < 0 | dataSet$glucose < 0
num_incorrect_values <- sum(incorrect_values, na.rm = TRUE)
print(paste("Number of incorrect values:", num_incorrect_values))

dataSet[dataSet < 0] <- NA
dataSet


is.na(dataSet)
missing_values <- is.na(dataSet)
num_missing_values <- sum(missing_values)
print(paste("Number of missing values:", num_missing_values))


cleaned_data<- na.omit(dataSet)
cleaned_data

mean_age_value <- mean(dataSet$age, na.rm = TRUE) 
print(mean_age_value)


mean_impluse_value <- mean(dataSet$impluse, na.rm = TRUE)
print(mean_impluse_value)


mean_ph_value <- mean(dataSet$pressurehight, na.rm = TRUE)
print(mean_ph_value)


mean_pl_value <- mean(dataSet$pressurelow, na.rm = TRUE)
print(mean_pl_value)


mean_glucose_value <- mean(dataSet$glucose, na.rm = TRUE)
print(mean_glucose_value)



calculate_mode <- function(x) {
  unique_values <- unique(x)
  frequencies <- table(x)
  mode_values <- unique_values[frequencies == max(frequencies)]
  return(mode_values)
}

mode_gender <- calculate_mode(dataSet$gender)
print(mode_gender)

mode_class <- calculate_mode(dataSet$class)
print(mode_class)


dataSet$age <- round(dataSet$age, 0)
dataSet[is.na(dataSet$age),"age"] <- mean_age_value
print(dataSet)

dataSet$impluse <- round(dataSet$impluse, 2)
dataSet[is.na(dataSet$impluse),"impluse"] <- mean_impluse_value
print(dataSet)

dataSet$pressurehight <- round(dataSet$pressurehight, 2)
dataSet[is.na(dataSet$pressurehight),"pressurehight"] <- mean_ph_value
print(dataSet)

dataSet$pressurelow <- round(dataSet$pressurelow, 1)
dataSet[is.na(dataSet$pressurelow),"pressurelow"] <- mean_pl_value
print(dataSet)

dataSet$glucose <- round(dataSet$glucose, 1)
dataSet[is.na(dataSet$glucose),"glucose"] <- mean_glucose_value
print(dataSet)


dataSet[is.na(dataSet$gender),"gender"] <- mode_gender
print(dataSet)

dataSet[is.na(dataSet$class),"class"] <- mode_class
print(dataSet)


age_bp <- boxplot(Information$Age)

outlier <- agebp$out

print(outlier)


par(mfrow = c(2, 2))

boxplot(dataSet$age, main = "Age")

boxplot(dataSet$impluse, main = "Impluse")

boxplot(dataSet$pressurehight, main = "Pressure Hight")

boxplot(dataSet$pressurelow, main = "Pressure Low")

boxplot(dataSet$glucose, main = "Glucose")

barplot(table(dataSet$gender), main = "Gender Distribution", xlab = "Gender", ylab = "Count")
barplot(table(dataSet$class), main = "Class Distribution", xlab = "Class", ylab = "Count")



dataSet$gender<-factor(dataSet$gender,
                       levels = c("male","female"),
                       labels = c(1,2))
dataSet

dataSet$class<-factor(dataSet$class,
                      levels = c("positive","negative"),
                      labels = c(3,4))
dataSet


min_age <- min(dataSet[["age"]]) 
max_age <- max(dataSet[["age"]]) 
age_range <- c(min_age, max_age) 
age_range <- range(dataSet[["age"]]) 
print(age_range)

min_impluse <- min(dataSet[["impluse"]])
max_impluse <- max(dataSet[["impluse"]])
impulse_range <- c(min_impluse, max_impluse)
impulse_range <- range(dataSet[["impluse"]])
print(impulse_range)


age_variance <- var(dataSet[["age"]])
print(age_variance)

age_std_dev <- sd(dataSet[["age"]])
print(age_std_dev)


hist(dataSet[["pressurehight"]],
     main= paste("Histogram of", "pressurehight"),
     xlab = "pressurehight",
     ylab = "Frequency",
     col = "blue",
     border = "coral")


hist(dataSet[["pressurelow"]],
     main= paste("Histogram of", "pressurelow"),
     xlab = "pressurelow",
     ylab = "Frequency",
     col = "yellow",
     border = "green")



