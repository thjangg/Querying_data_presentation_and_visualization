economics
A. Data Understanding
a <- economics
a
str(a)
summary(a)

colSums(is.na(a))

C.
library(ggplot2)
library(dplyr)
install.packages("plotly")
library(plotly)
install.packages("reshape2")
library(reshape2)


ggplot(a)
ggplot(data = economics, aes(x = date, y = pce)) +
  geom_line(color = "steelblue") + 
  labs(title="Personal Consumption Expenditure in US", x="Year", y="PCE")

ggplot(data = economics, aes(x = date, y = psavert)) +
  geom_line(color = "orange") + 
  labs(title="Personal Saving Rate in US", x="Year", y="PSAvert")

ggplot(data = economics, aes(x = date, y = unemploy)) +
  geom_line(color = "darkgreen") + 
  labs(title = "Numbers of unemployed in US", 
       x = "Year", 
       y = "Number of Unemployed") +
  annotate(
    geom = "text", 
    x = as.Date("2009-10-01"), 
    y = 15352, size=2.5,
    label = "Number of unemployed\nreached over 15K in 2009",
    color = "black",
    hjust = 1.2) +  
    annotate(geom="point", x=as.Date("2009-10-01"), color = "red",
            y=15352, size=9, shape=21, fill="transparent")

ggplot(data = economics, aes(x = date, y = uempmed)) +
  geom_line(color = "darkred") + 
  labs(title="Median duration of unemployment in US", x="Year", y="uempmed") +
  annotate(
  geom = "text", 
  x = as.Date("2010-06-01"), 
  y = 25.2, size=2.5,
  label = "Median duration of unemployment \nreached 25.2 in 2010",
  color = "black",
  hjust = 1.2) +  
  annotate(geom="point", x=as.Date("2010-06-01"), color = "red",
           y=25.2, size=9, shape=21, fill="transparent")

___


ggplot(data = economics, aes(x = uempmed)) +
  geom_histogram(binwidth = 1, fill = "darkred", color = "white") +
  labs(title = "Histogram of Median Unemployment Duration", x = "Median Unemployment Duration", y = "Frequency")

ggplot(data = economics, aes(x = pce)) +
  geom_histogram(binwidth = 1000, fill = "darkblue", color = "white") +
  labs(title = "Histogram of Personal Consumption Expenditures", 
       x = "Personal Consumption Expenditures (PCE)", y = "Frequency")

ggplot(data = economics, aes(x = psavert)) +
  geom_histogram(binwidth = 1, fill = "orange", color = "white") +
  labs(title = "Histogram of Personal Savings Rate", 
       x = "Personal Savings Rate", y = "Frequency")

ggplot(data = economics, aes(x = unemploy)) +
  geom_histogram(binwidth = 500, fill = "darkgreen", color = "white") +
  labs(title = "Histogram of Number of Unemployed", 
       x = "Number of Unemployed", y = "Frequency")

_____
Bubble

ggplot(economics, aes(x = pce, y = pop, size = unemploy)) +
  geom_point(alpha = 0.6, color = "darkblue") +
  labs(title = "Bubble Chart: Pce vs Pop (Size: Unemploy)",
       x = "Personal Consumption Expenditures (Billion $)",
       y = "Population (Thousands)",
       size = "Unemployed (Thousands)") +
  theme_minimal()


ggplot(economics, aes(x = psavert, y = uempmed, size = pop)) +
  geom_point(alpha = 0.6, color = "darkred") +
  labs(title = "Bubble Chart: Psavert vs Uempmed (Size: Pop)",
       x = "Personal Savings Rate (%)",
       y = "Median Duration of Unemployment (Weeks)",
       size = "Population (Thousands)") +
  theme_minimal()


ggplot(economics, aes(x = pce, y = uempmed, size = psavert)) +
  geom_point(alpha = 0.6, color = "darkgreen") +
  labs(title = "Bubble Chart: Pce vs Uempmed (Size: Psavert)",
       x = "Personal Consumption Expenditures (Billion $)",
       y = "Median Duration of Unemployment (Weeks)",
       size = "Personal Savings Rate (%)") +
  theme_minimal()


