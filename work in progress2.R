library(readr)
Data <- SideProjectData
library(tidyr)
library(dplyr)
library(ggplot2)
df_mean <- Data %>%
  group_by(Age) %>%
  summarise(
    At_Rim = mean(At_Rim),
    Paint = mean(Paint),
    MidRange = mean(MidRange),
    Three = mean(Three)
  )
df_mean2 <- df_mean %>%
  pivot_longer(cols = c(At_Rim, Paint, MidRange, Three), 
               names_to = "Shot_Type", 
               values_to = "Percentage")
plot1 <- ggplot(df_mean2, aes(x = Age, y = Percentage, fill = Shot_Type)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Shot Type Distribution by Age",
       x = "Age",
       y = "Percentage of Total Shots") +
  scale_fill_manual(values = c("At_Rim" = "red", "Paint" = "blue", "MidRange" = "green", "Three" = "purple")) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top")




new_df <- data.frame(
  aggregate(At_Rim ~ Year, Data, FUN = mean),
  aggregate(Paint ~ Year, Data, FUN = mean),
  aggregate(MidRange ~ Year, Data, FUN = mean),
  aggregate(Three ~ Year, Data, FUN = mean)
)
df3 <- new_df %>%
  select(1,2,4,6,8)
df_long <- df3 %>%
  pivot_longer(cols = c(At_Rim, Paint, MidRange, Three),
               names_to = "Shot_Type",
               values_to = "Percentage")
ggplot(df_long, aes(x = Year, y = Percentage, color = Shot_Type, group = Shot_Type)) +
  geom_line(size = 1.2) +               
  geom_point(size = 3) +                
  labs(x = "Year", y = "Percentage (%)", title = "Shot Distribution by Year") +
  theme_minimal() +                     
  theme(legend.title = element_blank())



df_long2 <- Data %>%
  select(Year, Three) %>%
  pivot_longer(cols = Three, names_to = "Shot_Type", values_to = "Percentage")

ggplot(df_long2, aes(x = factor(Year), y = Percentage, fill = factor(Year))) +
  geom_violin(trim = FALSE) +  
  labs(x = "Year", y = "% Of Shots from Three", title = "Three-Point Shot Distribution by Year") +
  theme_minimal() +  
  theme(legend.title = element_blank())




Al_Horford <- Data[Data$Player == "Al Horford",]

# Create the dot plot
ggplot(Al_Horford, aes(x = Three, y = MPG, color = Year)) +
  geom_point(size = 3) +  # Add dots
  labs(y = "Minutes Per Game", x = "Percent of Shots from 3", title = "MPG vs Percent of 3 Point Shots for Al Horford") +
  scale_color_gradient(low = "lightblue", high = "darkblue") +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "black") + 
  theme_minimal() +
  theme(legend.title = element_blank())


brook_lopez_data <- Data[Data$Player == "Brook Lopez",]

# Create the dot plot
ggplot(brook_lopez_data, aes(x = Three, y = MPG, color = Year)) +
  geom_point(size = 3) +  # Add dots
  labs(y = "Minutes Per Game", x = "Percent of Shots from 3", title = "MPG vs Percent of 3 Point Shots for Brook Lopez") +
  scale_color_gradient(low = "lightgreen", high = "darkgreen") +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "black") + 
  theme_minimal() +
  theme(legend.title = element_blank())

dhData <- Data %>%
  filter(Player == "Dwight Howard")


djData <- Data %>%
  filter(Player == "DeAndre Jordan")



ggplot(djData, aes(x = Three, y = MPG, color = Year)) +
  geom_point(size = 3) +  # Add dots
  xlim(0,.5) +
  labs(y = "Minutes Per Game", x = "Percent of Shots from 3", title = "MPG vs Percent of 3 Point Shots for DeAndre Jordan") +
  scale_color_gradient(low = "yellow", high = "darkorange") +
  theme_minimal() +
  theme(legend.title = element_blank())



ggplot(Data, aes(x = Age)) +
  geom_bar(fill = "steelblue", color = "black") +
  labs(
    title = "Frequency of Players by Age",
    x = "Age",
    y = "Number of Players"
  ) +
  theme_minimal()

dunkers <- Data %>%
  filter(`Offensive Role...33` == "Paint Player-Dunker")

ggplot(dunkers, aes(x = Age)) +
  geom_bar(position = "stack", color = "black") +
  labs(
    title = "Frequency of Players by Age and Player Type",
    x = "Age",
    y = "Number of Players",
    fill = "Player Type"  # Legend title
  ) +
  theme_minimal()
