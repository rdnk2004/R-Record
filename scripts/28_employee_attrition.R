library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
employee <- read.csv("data/Employee Attrition.csv",
                     stringsAsFactors = FALSE,
                     check.names = FALSE)
print(head(employee, 10))
print(tail(employee, 10))
str(employee)
dim(employee)
summary(employee)
sapply(employee, class)
names(employee) <- make.names(names(employee), unique = TRUE)
names(employee) <- tolower(names(employee))
names(employee) <- gsub("\\.", "_", names(employee))
missing_values <- colSums(is.na(employee))
print(missing_values)
employee <- employee %>%
  mutate(across(where(is.character), str_trim))
if ("Attrition" %in% names(employee)) {
  employee$Attrition <- str_to_title(employee$Attrition)
}
if ("Gender" %in% names(employee)) {
  employee$Gender <- str_to_title(employee$Gender)
}
if ("Department" %in% names(employee)) {
  employee$Department <- str_to_title(employee$Department)
}
if ("JobRole" %in% names(employee)) {
  employee$JobRole <- str_to_title(employee$JobRole)
}
if ("MonthlyIncome" %in% names(employee)) {
  employee$MonthlyIncome <- as.numeric(employee$MonthlyIncome)
}
if ("Age" %in% names(employee)) {
  employee$Age <- as.numeric(employee$Age)
}
if ("TotalWorkingYears" %in% names(employee)) {
  employee$TotalWorkingYears <- as.numeric(employee$TotalWorkingYears)
}
if ("YearsAtCompany" %in% names(employee)) {
  employee$YearsAtCompany <- as.numeric(employee$YearsAtCompany)
}
if ("Age" %in% names(employee)) {
  employee$Age[employee$Age < 18 | employee$Age > 70] <- NA
}
if ("MonthlyIncome" %in% names(employee)) {
  employee$MonthlyIncome[employee$MonthlyIncome <= 0] <- NA
}
if ("TotalWorkingYears" %in% names(employee)) {
  employee$TotalWorkingYears[employee$TotalWorkingYears < 0] <- NA
}
if ("YearsAtCompany" %in% names(employee)) {
  employee$YearsAtCompany[employee$YearsAtCompany < 0] <- NA
}
employee <- employee %>%
  distinct()
employee <- employee %>%
  mutate(across(where(is.numeric),
                ~ ifelse(is.na(.),
                         median(., na.rm = TRUE),
                         .)))
if ("Age" %in% names(employee)) {
  employee$Age <- as.numeric(employee$Age)
}
if ("MonthlyIncome" %in% names(employee)) {
  employee$MonthlyIncome <- as.numeric(employee$MonthlyIncome)
}
if ("TotalWorkingYears" %in% names(employee)) {
  employee$TotalWorkingYears <- as.numeric(employee$TotalWorkingYears)
}

if ("YearsAtCompany" %in% names(employee)) {
  employee$YearsAtCompany <- as.numeric(employee$YearsAtCompany)
}

numeric_columns <- names(employee)[sapply(employee, is.numeric)]

outlier_summary <- data.frame(
  Variable = character(),
  Lower = numeric(),
  Upper = numeric(),
  Outliers = integer()
)

for (col in numeric_columns) {
  q1 <- quantile(employee[[col]], 0.25, na.rm = TRUE)
  q3 <- quantile(employee[[col]], 0.75, na.rm = TRUE)
  iqr_value <- q3 - q1
  lower <- q1 - 1.5 * iqr_value
  upper <- q3 + 1.5 * iqr_value
  count <- sum(employee[[col]] < lower | employee[[col]] > upper, na.rm = TRUE)
  outlier_summary <- rbind(
    outlier_summary,
    data.frame(
      Variable = col,
      Lower = lower,
      Upper = upper,
      Outliers = count
    )
  )
}

print(outlier_summary)

for (col in numeric_columns) {
  q1 <- quantile(employee[[col]], 0.25, na.rm = TRUE)
  q3 <- quantile(employee[[col]], 0.75, na.rm = TRUE)
  iqr_value <- q3 - q1
  lower <- q1 - 1.5 * iqr_value
  upper <- q3 + 1.5 * iqr_value
  employee[[col]] <- pmin(pmax(employee[[col]], lower), upper)
}

if ("Age" %in% names(employee)) {
  employee$Age_Group <- cut(
    employee$Age,
    breaks = c(0, 25, 35, 45, 55, Inf),
    labels = c("18-25", "26-35", "36-45", "46-55", "56+"),
    include.lowest = TRUE
  )
}

if ("MonthlyIncome" %in% names(employee)) {
  employee$Salary_Category <- cut(
    employee$MonthlyIncome,
    breaks = c(-Inf, 3000, 6000, 10000, Inf),
    labels = c("Low", "Medium", "High", "Very High")
  )
}

if ("TotalWorkingYears" %in% names(employee)) {
  employee$Experience_Level <- cut(
    employee$TotalWorkingYears,
    breaks = c(-Inf, 2, 5, 10, Inf),
    labels = c("Fresher", "Junior", "Mid-Level", "Senior")
  )
}

if ("YearsAtCompany" %in% names(employee)) {
  employee$Years_of_Service <- employee$YearsAtCompany
}

if ("Age" %in% names(employee)) {
  employee$Age_Standardized <- as.numeric(scale(employee$Age))
}

if ("MonthlyIncome" %in% names(employee)) {
  employee$Salary_Standardized <- as.numeric(scale(employee$MonthlyIncome))
}

if ("TotalWorkingYears" %in% names(employee)) {
  employee$Experience_Standardized <- as.numeric(scale(employee$TotalWorkingYears))
}

if ("YearsAtCompany" %in% names(employee)) {
  employee$Service_Standardized <- as.numeric(scale(employee$YearsAtCompany))
}

date_columns <- names(employee)[grepl("date|Date", names(employee))]

if (length(date_columns) > 0) {
  for (col in date_columns) {
    employee[[col]] <- suppressWarnings(
      parse_date_time(
        employee[[col]],
        orders = c("ymd", "dmy", "mdy", "Y-m-d", "d/m/Y", "m/d/Y")
      )
    )
  }
}

if ("Hire_Date" %in% names(employee)) {
  employee$Hire_Date <- as.Date(employee$Hire_Date)
}

for (col in names(employee)) {
  if (is.character(employee[[col]])) {
    employee[[col]] <- str_trim(employee[[col]])
  }
}

if ("Attrition" %in% names(employee)) {
  employee$Attrition <- as.factor(employee$Attrition)
}

if ("Gender" %in% names(employee)) {
  employee$Gender <- as.factor(employee$Gender)
}

if ("Department" %in% names(employee)) {
  employee$Department <- as.factor(employee$Department)
}

if ("JobRole" %in% names(employee)) {
  employee$JobRole <- as.factor(employee$JobRole)
}

print(sum(duplicated(employee)))

print(colSums(is.na(employee)))

if ("Age" %in% names(employee)) {
  print(sum(employee$Age < 0, na.rm = TRUE))
}

if ("MonthlyIncome" %in% names(employee)) {
  print(sum(employee$MonthlyIncome < 0, na.rm = TRUE))
}

if ("TotalWorkingYears" %in% names(employee)) {
  print(sum(employee$TotalWorkingYears < 0, na.rm = TRUE))
}

if ("YearsAtCompany" %in% names(employee)) {
  print(sum(employee$YearsAtCompany < 0, na.rm = TRUE))
}

if ("MonthlyIncome" %in% names(employee)) {
  boxplot(employee$MonthlyIncome,
          main = "Monthly Income Outliers",
          ylab = "Monthly Income")
}

if ("Age" %in% names(employee)) {
  boxplot(employee$Age,
          main = "Age Outliers",
          ylab = "Age")
}

if ("Age" %in% names(employee) &&
    "MonthlyIncome" %in% names(employee)) {
  plot(employee$Age,
       employee$MonthlyIncome,
       main = "Age vs Monthly Income",
       xlab = "Age",
       ylab = "Monthly Income")
}

employee <- employee %>%
  distinct()

employee <- employee %>%
  mutate(across(where(is.numeric),
                ~ ifelse(is.na(.),
                         median(., na.rm = TRUE),
                         .)))

print(str(employee))
print(summary(employee))
print(dim(employee))
print(colSums(is.na(employee)))
print(sum(duplicated(employee)))

write.csv(employee,
          "output/Employee_Cleaned.csv",
          row.names = FALSE)

print(head(employee, 10))
