
# PROJECT: STUDENT PERFORMANCE & ACADEMIC EDA
# LANGUAGE: R
# DATASET: StudentsPerformance.csv



# 1. IMPORT DATASET

students <- read.csv("StudentsPerformance.csv")

library(ggplot2)


# 2. DATASET CHECK

print(head(students))
print(str(students))
print(dim(students))
print(summary(students))


# 3. AVERAGE MARKS ANALYSIS

cat("\n--- Average Marks Analysis ---\n")

cat("Math Average:",
    mean(students$math.score, na.rm = TRUE), "\n")

cat("Reading Average:",
    mean(students$reading.score, na.rm = TRUE), "\n")

cat("Writing Average:",
    mean(students$writing.score, na.rm = TRUE), "\n")


# 4. HIGHEST AND LOWEST SCORES

cat("\n--- Highest and Lowest Scores ---\n")

cat("Highest Math Score:",
    max(students$math.score, na.rm = TRUE), "\n")

cat("Lowest Math Score:",
    min(students$math.score, na.rm = TRUE), "\n")

cat("Highest Reading Score:",
    max(students$reading.score, na.rm = TRUE), "\n")

cat("Lowest Reading Score:",
    min(students$reading.score, na.rm = TRUE), "\n")


# 5. SUBJECT-WISE SUMMARY

cat("\n--- Subject-wise Summary ---\n")

print(summary(students[, c(
  "math.score",
  "reading.score",
  "writing.score"
)]))


# 6. OVERALL AVERAGE SCORE

students$average_score <- rowMeans(
  students[, c(
    "math.score",
    "reading.score",
    "writing.score"
  )],
  na.rm = TRUE
)

# If all three subject scores are missing, mark average as NA
all_scores_missing <- rowSums(
  !is.na(students[, c(
    "math.score",
    "reading.score",
    "writing.score"
  )])
) == 0

students$average_score[all_scores_missing] <- NA_real_

print(head(students[, c(
  "math.score",
  "reading.score",
  "writing.score",
  "average_score"
)]))


# 7. TOP 10 STUDENTS

top_students <- students[
  order(-students$average_score, na.last = TRUE),
]

cat("\n--- Top 10 Students ---\n")
print(head(top_students, 10))


# 8. GENDER-WISE AVERAGE MARKS

cat("\n--- Gender-wise Average Marks ---\n")

gender_analysis <- aggregate(
  cbind(math.score, reading.score, writing.score) ~ gender,
  data = students,
  FUN = function(x) mean(x, na.rm = TRUE)
)

print(gender_analysis)


# 9. TEST PREPARATION ANALYSIS

cat("\n--- Test Preparation Analysis ---\n")

test_prep_analysis <- aggregate(
  cbind(math.score, reading.score, writing.score) ~
    test.preparation.course,
  data = students,
  FUN = function(x) mean(x, na.rm = TRUE)
)

print(test_prep_analysis)


# 10. GENDER-WISE MATH PERFORMANCE

ggplot(
  students,
  aes(x = gender, y = math.score, fill = gender)
) +
  geom_boxplot(na.rm = TRUE) +
  labs(
    title = "Math Performance by Gender",
    x = "Gender",
    y = "Math Score"
  ) +
  theme_minimal()


# 11. DISTRIBUTION OF MATH SCORES

hist(
  students$math.score,
  main = "Distribution of Math Scores",
  xlab = "Math Score",
  ylab = "Number of Students",
  col = "skyblue",
  border = "white"
)


# 12. AVERAGE SCORE BY SUBJECT

subject_means <- colMeans(
  students[, c(
    "math.score",
    "reading.score",
    "writing.score"
  )],
  na.rm = TRUE
)

print(subject_means)

barplot(
  subject_means,
  main = "Average Score by Subject",
  xlab = "Subject",
  ylab = "Average Marks",
  col = c("skyblue", "lightgreen", "salmon"),
  ylim = c(0, 100)
)


# 13. MATH VS READING RELATIONSHIP

ggplot(
  students,
  aes(x = math.score, y = reading.score)
) +
  geom_point(alpha = 0.6, na.rm = TRUE) +
  geom_smooth(method = "lm", se = FALSE, na.rm = TRUE) +
  labs(
    title = "Math Score vs Reading Score",
    x = "Math Score",
    y = "Reading Score"
  ) +
  theme_minimal()


# 14. CORRELATION ANALYSIS

score_correlation <- cor(
  students[, c(
    "math.score",
    "reading.score",
    "writing.score"
  )],
  use = "complete.obs"
)

cat("\n--- Correlation Matrix ---\n")
print(round(score_correlation, 3))


# 15. STUDENT PERFORMANCE CATEGORIES

students$performance_category <- cut(
  students$average_score,
  breaks = c(-Inf, 40, 60, 80, Inf),
  labels = c(
    "Needs Improvement",
    "Average",
    "Good",
    "Excellent"
  ),
  right = FALSE
)

cat("\n--- Performance Category Counts ---\n")
print(table(students$performance_category, useNA = "ifany"))


# 16. PERFORMANCE CATEGORY VISUALIZATION

ggplot(
  students,
  aes(x = performance_category, fill = performance_category)
) +
  geom_bar(na.rm = TRUE) +
  labs(
    title = "Student Performance Categories",
    x = "Performance Category",
    y = "Number of Students"
  ) +
  theme_minimal()


# 17. PARENT EDUCATION ANALYSIS

parent_education <- aggregate(
  cbind(math.score, reading.score, writing.score) ~
    parental.level.of.education,
  data = students,
  FUN = function(x) mean(x, na.rm = TRUE)
)

cat("\n--- Parent Education Analysis ---\n")
print(parent_education)


# 18. LUNCH ANALYSIS

lunch_analysis <- aggregate(
  cbind(math.score, reading.score, writing.score) ~ lunch,
  data = students,
  FUN = function(x) mean(x, na.rm = TRUE)
)

cat("\n--- Lunch Analysis ---\n")
print(lunch_analysis)


# 19. FINAL KEY INSIGHTS

cat("\n========== FINAL PROJECT INSIGHTS ==========\n")

cat(
  "Total Students:",
  nrow(students), "\n"
)

cat(
  "Average Math Score:",
  round(mean(students$math.score, na.rm = TRUE), 2), "\n"
)

cat(
  "Average Reading Score:",
  round(mean(students$reading.score, na.rm = TRUE), 2), "\n"
)

cat(
  "Average Writing Score:",
  round(mean(students$writing.score, na.rm = TRUE), 2), "\n"
)

cat(
  "Average Overall Score:",
  round(mean(students$average_score, na.rm = TRUE), 2), "\n"
)

cat(
  "Highest Overall Average:",
  round(max(students$average_score, na.rm = TRUE), 2), "\n"
)

cat(
  "Lowest Overall Average:",
  round(min(students$average_score, na.rm = TRUE), 2), "\n"
)

cat(
  "Average Math-Reading Correlation:",
  round(score_correlation["math.score", "reading.score"], 3),
  "\n"
)


# 20. SAVE ANALYZED DATASET

write.csv(
  students,
  "student_performance_analyzed.csv",
  row.names = FALSE
)


# 21. EXPORT SUMMARY REPORTS

write.csv(
  gender_analysis,
  "gender_analysis.csv",
  row.names = FALSE
)

write.csv(
  test_prep_analysis,
  "test_preparation_analysis.csv",
  row.names = FALSE
)

write.csv(
  parent_education,
  "parent_education_analysis.csv",
  row.names = FALSE
)

write.csv(
  lunch_analysis,
  "lunch_analysis.csv",
  row.names = FALSE
)

write.csv(
  as.data.frame(score_correlation),
  "score_correlation.csv",
  row.names = TRUE
)


# 22. PROJECT COMPLETED

cat(" STUDENT PERFORMANCE & ACADEMIC EDA\n")
cat(" PROJECT COMPLETED SUCCESSFULLY!\n")
