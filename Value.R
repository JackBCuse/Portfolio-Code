#Predicting Player Stats
columns <- c("Age","G","MPG","PaintPts","FTP","PRREff","PutbacksEff","OREB","TwoPP","Prime Lineup")


library(dplyr)

dunkers_Vdata <- DunkersValue %>%
  group_by(Year) %>%  # Group by year to rank within each year
  mutate(across(
    c("G", "PaintPts", "FTP", "PRREff", "PutbacksEff", "OREB", "TwoPP"),
    ~ rank(-., ties.method = "min"), # Rank in descending order (1 = best rank)
    .names = "rank_{.col}"
  )) %>%
  rowwise() %>%
  mutate(
    Value = sum(c_across(starts_with("rank_"))) # Sum all ranks for each player
  ) %>%
  ungroup() %>%
  group_by(Year) %>%
  mutate(
    # Normalize the Value between 0 and 1, then invert so 1 is best and 0 is worst
    Normalized_Value = 1 - (Value - min(Value)) / (max(Value) - min(Value))
  ) %>%
  ungroup()

# View the result
head(dunkers_Vdata)


PPVersatileValue <- PPVersatileValue %>%
  group_by(Year) %>%  # Group by year to rank within each year
  mutate(across(
    c("G","PtsPaint","FT%","PRRPPP","PutbackPPP","OREB","TPP",
      "Ast","ADT","IsoPPP","PostUpPPP"),
    ~ rank(-., ties.method = "min"), # Rank in descending order (1 = best rank)
    .names = "rank_{.col}"
  )) %>%
  rowwise() %>%
  mutate(
    Value = sum(c_across(starts_with("rank_"))) # Sum all ranks for each player
  ) %>%
  ungroup() %>%
  group_by(Year) %>%
  mutate(
    # Normalize the Value between 0 and 1, then invert so 1 is best and 0 is worst
    Normalized_Value = 1 - (Value - min(Value)) / (max(Value) - min(Value))
  ) %>%
  ungroup()



PPSUValue <- PPSUV %>%
  group_by(Year) %>%  # Group by year to rank within each year
  mutate(across(
    c("G","PaintPts","FTP","PRRPPP","PBPPP","OREB","TwoPPP",
      "SUPPP","PUPPP","ThreePP","CornerPts"),
    ~ rank(-., ties.method = "min"), # Rank in descending order (1 = best rank)
    .names = "rank_{.col}"
  )) %>%
  rowwise() %>%
  mutate(
    Value = sum(c_across(starts_with("rank_"))) # Sum all ranks for each player
  ) %>%
  ungroup() %>%
  group_by(Year) %>%
  mutate(
    # Normalize the Value between 0 and 1, then invert so 1 is best and 0 is worst
    Normalized_Value = 1 - (Value - min(Value)) / (max(Value) - min(Value))
  ) %>%
  ungroup()


BallDominantValue <- BallDominantValue %>%
  group_by(Year) %>%  # Group by year to rank within each year
  mutate(across(
    c("G","ISOPPP","Pts","Ast","ADT","MidRangeFGP","Pupts",
      "DrivesPts","PRBHPPP","FTP"),
    ~ rank(-., ties.method = "min"), # Rank in descending order (1 = best rank)
    .names = "rank_{.col}"
  )) %>%
  rowwise() %>%
  mutate(
    Value = sum(c_across(starts_with("rank_"))) # Sum all ranks for each player
  ) %>%
  ungroup() %>%
  group_by(Year) %>%
  mutate(
    # Normalize the Value between 0 and 1, then invert so 1 is best and 0 is worst
    Normalized_Value = 1 - (Value - min(Value)) / (max(Value) - min(Value))
  ) %>%
  ungroup()



SecondaryValue <- SecondaryPlayMakerV %>%
  group_by(Year) %>%  # Group by year to rank within each year
  mutate(across(
    c("G","ISOPPP","Pts...15","Ast","ADT","MidRangeFGP","Pupts",
      "DrivesPts","PRBHPPP","SpotUpPPP","PtsCorner","ThreePPP"),
    ~ rank(-., ties.method = "min"), # Rank in descending order (1 = best rank)
    .names = "rank_{.col}"
  )) %>%
  rowwise() %>%
  mutate(
    Value = sum(c_across(starts_with("rank_"))) # Sum all ranks for each player
  ) %>%
  ungroup() %>%
  group_by(Year) %>%
  mutate(
    # Normalize the Value between 0 and 1, then invert so 1 is best and 0 is worst
    Normalized_Value = 1 - (Value - min(Value)) / (max(Value) - min(Value))
  ) %>%
  ungroup()


VersatilePMValue <- VersatilePlaymakerValue %>%
  group_by(Year) %>%  # Group by year to rank within each year
  mutate(across(
    c("G","ISOPPP","Pts","Ast","ADT","MidRangeFGP","Pupts",
      "DrivesPts","OREB","PostUpPPP"),
    ~ rank(-., ties.method = "min"), # Rank in descending order (1 = best rank)
    .names = "rank_{.col}"
  )) %>%
  rowwise() %>%
  mutate(
    Value = sum(c_across(starts_with("rank_"))) # Sum all ranks for each player
  ) %>%
  ungroup() %>%
  group_by(Year) %>%
  mutate(
    # Normalize the Value between 0 and 1, then invert so 1 is best and 0 is worst
    Normalized_Value = 1 - (Value - min(Value)) / (max(Value) - min(Value))
  ) %>%
  ungroup()


CatchAndShootV <- CatchAndShootValue %>%
  group_by(Year) %>%  # Group by year to rank within each year
  mutate(across(
    c("G","PtsThree","SpotUpPPP","CatchShootPPP","ThreePP"),
    ~ rank(-., ties.method = "min"), # Rank in descending order (1 = best rank)
    .names = "rank_{.col}"
  )) %>%
  rowwise() %>%
  mutate(
    Value = sum(c_across(starts_with("rank_"))) # Sum all ranks for each player
  ) %>%
  ungroup() %>%
  group_by(Year) %>%
  mutate(
    # Normalize the Value between 0 and 1, then invert so 1 is best and 0 is worst
    Normalized_Value = 1 - (Value - min(Value)) / (max(Value) - min(Value))
  ) %>%
  ungroup()


OffTheDribbleValue <- OffTheDribble %>%
  group_by(Year) %>%  # Group by year to rank within each year
  mutate(across(
    c("G","PtsThree","SpotUpPPP","CatchShootPPP","ThreePP","PRBHPPP"),
    ~ rank(-., ties.method = "min"), # Rank in descending order (1 = best rank)
    .names = "rank_{.col}"
  )) %>%
  rowwise() %>%
  mutate(
    Value = sum(c_across(starts_with("rank_"))) # Sum all ranks for each player
  ) %>%
  ungroup() %>%
  group_by(Year) %>%
  mutate(
    # Normalize the Value between 0 and 1, then invert so 1 is best and 0 is worst
    Normalized_Value = 1 - (Value - min(Value)) / (max(Value) - min(Value))
  ) %>%
  ungroup()

write.csv(AllTeamData, file = "C:/Users/Jackson Bayuk/OneDrive/Documents/R/teamlineup.csv", row.names = FALSE)


library(dplyr)

datasets <- list(
  BallDominantValue = mutate(BallDominantValue, offensive_role = "Playmaker-Ball Dominant"),
  CatchAndShootV = mutate(CatchAndShootV, offensive_role = "Shooter-Catch and Shoot"),
  dunkers_Vdata = mutate(dunkers_Vdata, offensive_role = "Paint Player-Dunker"),
  OffTheDribbleValue = mutate(OffTheDribbleValue, offensive_role = "Shooter-Off the Dribble"),
  PPSUValue = mutate(PPSUValue, offensive_role = "Paint Player-Spot Up"),
  SecondaryValue = mutate(SecondaryValue, offensive_role = "Playmaker-Secondary"),
  VersatilePMValue = mutate(VersatilePMValue, offensive_role = "Playmaker-Versatile"),
  PPVersatileValue = mutate(PPVersatileValue, offensive_role = "Paint Player-Versatile")
)

combined_data <- bind_rows(
  lapply(datasets, function(df) select(df, Player, 
                                       NewPLayer2, NewTeam, Normalized_Value,
                                       offensive_role, Year))
)

library(dplyr)

BallDominantValueProjects <- BallDominantValue %>%
  arrange(Player,Year) %>%
  group_by(Player) %>%
  mutate(lagpts = dplyr::lag(Pts, default = NULL)) %>%
  ungroup()

BallDominantValueProjects <- na.omit(BallDominantValueProjects)

model1 <- lm(Pts ~ I(Age^2)  + lagpts, data = BallDominantValueProjects_no_outliers)
model2 <- lm(Pts ~ Age + lagpts, data = BallDominantValueProjects_no_outliers)

summary(model1)
summary(model2)

ggplot(BallDominantValueProjects_no_outliers, aes(Age, Pts)) +
  geom_point()+
  geom_smooth(formula = y ~ poly(x, 2), method = "lm", col = "blue")

boxplot(BallDominantValueProjects_no_outliers$Pts)
maxpts <- max(BallDominantValueProjects$Pts)
minpts <- min(BallDominantValueProjects$Pts)
BallDominantValueProjects_no_outliers <- 
  BallDominantValueProjects[BallDominantValueProjects$Pts != maxpts &
                              BallDominantValueProjects$Pts != minpts, ]

names(DriveAndKick) <- gsub("[-[:space:]]", "_", names(DriveAndKick))

library(randomForest)
lrmodel <- lm(
  ORtg ~ 
    Paint_Player_Spot_Up + 
    Paint_Player_Versatile + 
    Shooter_Catch_and_Shoot + 
    Shooter_Off_the_Dribble + 
    Playmaker_Versatile + 
    Playmaker_Secondary + 
    Playmaker_Ball_Dominant + 
    Paint_Player_Spot_Up:Shooter_Catch_and_Shoot + 
    Paint_Player_Spot_Up:Shooter_Off_the_Dribble + 
    Paint_Player_Versatile:Shooter_Catch_and_Shoot + 
    Paint_Player_Versatile:Shooter_Off_the_Dribble + 
    Playmaker_Versatile:Shooter_Catch_and_Shoot + 
    Playmaker_Versatile:Shooter_Off_the_Dribble + 
    Playmaker_Secondary:Shooter_Catch_and_Shoot + 
    Playmaker_Secondary:Shooter_Off_the_Dribble + 
    Playmaker_Ball_Dominant:Shooter_Catch_and_Shoot + 
    Playmaker_Ball_Dominant:Shooter_Off_the_Dribble,
  data = DriveAndKick
)

# Step 3: Print the model results and variable importance
print(rf_model)
print(importance(rf_model))


summary(lrmodel)



anova_results <- aov(ORtg ~ LineupType, data = SideProjectData)

summary(anova_results)



if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# Install nba_hoopR from GitHub
devtools::install_github("sportsdataverse/nba_hoopR")
library(dplyr)
df <- Bench1 %>%
  select(6,29,20)
colnames(df) <- c("NORtg","PLValue","BenchValue")

# Replace spaces and dashes with underscores
colnames(df) <- gsub(" |-", "_", colnames(df))

X <- df[, c("PLValue","BenchValue")]


y <- df$NORtg


library(randomForest)
model <- randomForest(x = X, y = y, importance = TRUE)

# Feature importance
importance <- importance(model)
print(importance)


importance_df <- data.frame(
  Feature = rownames(importance(model)),
  Importance = importance(model)[, "IncNodePurity"]
)

# Sort by importance
importance_df <- importance_df[order(importance_df$Importance, decreasing = TRUE), ]

# Plot importance
ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Feature Importance (Random Forest)",
       x = "Offensive Role",
       y = "Importance (IncNodePurity)") +
  theme_minimal()

weights <- importance[, 1] / sum(importance[, 1])
names(weights) <- colnames(X)
print(weights)

ranked_data <- DunkersValue %>%
  group_by(Year) %>%
  mutate(across(c("G", "PaintPts", "FTP", "PRREff", "OREB", "TwoPP", "OWS"), ~ rank(.x, ties.method = "average"))) %>%
  ungroup()
library(dplyr)
modelData <- ranked_data %>%
  select("NewPLayer2", "G", "PaintPts", "FTP", "PRREff", "OREB", "TwoPP", "OWS")



# Step 2: Fit a regression model to determine weights (not by year, across entire data)
model <- lm(OWS ~ G + PaintPts + FTP + PRREff +
              OREB + TwoPP, data = ranked_data)
model2 <- lm(OWS ~ G + PaintPts + FTP + PRREff +
              OREB + TwoPP + PutbacksEff, data = ranked_data)

weights <- coef(model)[-1]  # Exclude intercept

# Step 3: Apply weights to calculate weighted sums
ranked_data <- ranked_data %>%
  rowwise() %>%
  mutate(
    Weighted_Sum = sum(c_across(c("G", "PaintPts", "FTP", "PRREff", "PutbacksEff", "OREB", "TwoPP")) * weights)
  ) %>%
  ungroup()

# Step 4: Normalize values within each year to get final offensive value scores
ranked_data <- ranked_data %>%
  group_by(Year) %>%
  mutate(Offensive_Value_Score = (Weighted_Sum - min(Weighted_Sum)) / 
           (max(Weighted_Sum) - min(Weighted_Sum))) %>%
  ungroup()


summary(model)

weights
