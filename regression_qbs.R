nfl_data <- read.csv('nfl_final.csv')
college_data <- read.csv('college_final.csv')
nfl_new <- read.csv('nfl_new.csv')
coll_new <- read.csv('coll_new(1).csv')
colnames(nfl_new)
colnames(coll_new)
library(dplyr)

#using new nfl scores and new college scores
merged <- merge(nfl_new, coll_new, by = "Name") 
model3 <- lm(Final_score ~ Score.y, data = merged)
summary(model3)  # View model details
plot(merged$Score.y, merged$Final_score, main="A", xlab="X Variable", ylab="Y Variable", pch=16)
abline(model3, col="red", lwd=2)


boxplot(merged$Final_score, main = "Boxplot of Final Scores", horizontal = TRUE)

model <- lm(Final_score ~ Score.y, data = merged)
cooksD <- cooks.distance(model)
plot(cooksD, type = "h", main = "Cook's Distance")


merged_final <- merged[abs(scale(merged$Final_score)) < 3, ]
merged_final <- merged[abs(scale(merged$Final_score)) > -3, ]


model_updated <- lm(Final_score ~ Score.y, data = merged_final)
summary(model_updated)

plot(merged_final$Score.y, merged_final$Final_score, main="B", xlab="X Variable", ylab="Y Variable", pch=16)
abline(model_updated, col="red", lwd=2)

colnames(incoming_qbs)[colnames(incoming_qbs) == "TotalPoints"] <- "Score.y"

incoming_qbs$Projected_NFL_Score <- predict(model, newdata = incoming_qbs)

library(ggplot2)

ggplot(incoming_qbs, aes(x = Score.y, y = Projected_NFL_Score)) +
  geom_point(color = "blue", alpha = 0.6) +  # Scatter plot
  geom_smooth(method = "lm", color = "red", se = TRUE) +  # Regression line
  labs(title = "Regression: College Score vs. NFL Score",
       x = "College Score (Score.y)",
       y = "NFL Score (Projected_NFL_Score)") +
  theme_minimal()

incoming_qbs <- incoming_qbs[order(-incoming_qbs$Projected_NFL_Score), ]
head(incoming_qbs, 10)  # Top 10 draftees
ggplot(incoming_qbs, aes(x = Score.y, y = Projected_NFL_Score)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Projected NFL Success vs. College Score", x = "College Score", y = "Predicted NFL Score")


print(incoming_qbs$Name)
draftees <- subset(incoming_qbs, !Name %in% c("Cade Klubnik", "John Mateer", "Chandler Morris"))

print(draftees)
draftees <- draftees[order(-draftees$Projected_NFL_Score), ]
head(draftees, 20)  # Top 10 draftees

install.packages("writexl")  # Install package (only needed once)
library(writexl)

# Save dataframe to an Excel file
write_xlsx(merged_final, "merged_data.xlsx")

