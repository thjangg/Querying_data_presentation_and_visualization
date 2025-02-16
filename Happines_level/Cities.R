city <- read.csv("/Users/maycuagiang/Downloads/MASTER PROGRAM/Querrying/Final Project
                 /healthy_lifestyle_city_2021.csv")
View(city)                   

A. Data Understanding
str(city)

tail(city)
summary(city)
dim(city)

##missing data
sum(is.na(city))
colSums(is.na(city))

sum(duplicated(city$City))

B. Data Prerocessing
## các dữ liệu bị thiếu được để là ký hiệu -
sapply(city, function(x) sum(x == "-"))
##changing missing data '-' to NA
city[city == "-"] <- NA
colSums(is.na(city))


a. 
str(city$Sunshine.hours.City.)
city$Sunshine.hours.City. <- as.numeric(gsub("[^0-9]", "", city$Sunshine.hours.City.))
sum(is.na(city$Sunshine.hours.City.))

b. 
str(city$Pollution.Index.score...City.)
city$Pollution.Index.score...City. <- as.numeric(gsub("[^0-9]", "", city$Pollution.Index.score...City.))
sum(is.na(city$Pollution.Index.score...City.))
city$Pollution.Index.score...City.[is.na(city$Pollution.Index.score...City.)] <- mean(city$Pollution.Index.score...City., na.rm = TRUE)

c.
str(city$Annual.avg..hours.worked)
city$Annual.avg..hours.worked <- as.numeric(gsub("[^0-9]", "", city$Annual.avg..hours.worked))
city$Annual.avg..hours.worked[is.na(city$Annual.avg..hours.worked)] <- mean(city$Annual.avg..hours.worked, na.rm = TRUE)

colSums(is.na(city))

# Loại bỏ ký hiệu '%' và chuyển sang numeric
city$Obesity.levels.Country. <- as.numeric(gsub("%", "", city$Obesity.levels.Country.))

# Loại bỏ ký hiệu '£' và chuyển sang numeric
city$Cost.of.a.bottle.of.water.City. <- as.numeric(gsub("£", "", city$Cost.of.a.bottle.of.water.City.))
city$Cost.of.a.monthly.gym.membership.City. <- as.numeric(gsub("£", "", city$Cost.of.a.monthly.gym.membership.City.))


colnames(city) <- c("City", "Rank", "SunHr", "BottleWaterCost", "ObesityLev", 
                    "LifeExp", "Pollution", "WorkHours", "HappinessLev", 
                    "OutdoorAct", "TakeOutPlaces", "GymCost")

C. Visualize

ggplot(city, aes(x = HappinessLev)) +
  geom_density( fill = "goldenrod1", color = "goldenrod") +
  labs(title = "Histogram of Happiness Level Between Cities",
       x = "Happiness Level",
       y = "Number of Cities") +
  theme_minimal() +
  theme(
    plot.title = element_text(family = "Times", size = 14, face = "bold"),
    axis.title.x = element_text(family = "Arial", size = 10, face = "italic"),
    axis.title.y = element_text(family = "Arial", size = 10, face = "italic"),
    axis.text = element_text(family = "Arial", size = 10)
  )
#Divide into groups
city$HappinessGroup <- cut(city$HappinessLev,
                           breaks = c(0, 5.5, 7.5, Inf),
                           labels = c("Low Happiness", "Average Happiness", "High Happiness"))

ggplot(city, aes(x = HappinessGroup, fill = HappinessGroup)) +
  geom_bar() +
  labs(title = "Happiness Group Distribution",
       x = "Happiness Group",
       y = "Number of Cities") +
  scale_fill_brewer(palette = "Paired") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  theme_minimal() +
    theme(
      plot.title = element_text(family = "Times", size = 14, face = "bold"),
      axis.title.x = element_text(family = "Arial", size = 10, face = "italic"),
      axis.title.y = element_text(family = "Arial", size = 10, face = "italic"),
      axis.text = element_text(family = "Arial", size = 10)
    )

  table(city$HappinessGroup)
  
  ggplot(city, aes(x = HappinessGroup, fill = HappinessGroup)) +
    geom_bar() +
    labs(
      title = "Happiness Group Distribution",
      x = "Happiness Group",
      y = "Number of Cities"
    ) +
    scale_fill_brewer(palette = "Paired") +
    theme_minimal() +  # Đặt theme cơ bản trước
    theme(
      plot.title = element_text(family = "Times", size = 14, face = "bold"),
      axis.title.x = element_text(family = "Arial", size = 10, face = "italic"),
      axis.title.y = element_text(family = "Arial", size = 10, face = "italic"),
      axis.text = element_text(family = "Arial", size = 8),
      axis.text.x = element_text(angle = 45, hjust = 1)  # Giữ xoay nhãn trục x
    )
  
  
  ----
    library(tidyr)
  city_long <- city %>%
    pivot_longer(cols = c(SunHr, Pollution, OutdoorAct, LifeExp),
                 names_to = "Variable",
                 values_to = "Value")
  
  # Sử dụng facet_wrap để vẽ Boxplot
  ggplot(city_long, aes(x = HappinessGroup, y = Value, fill = HappinessGroup)) +
    geom_boxplot() +
    facet_wrap(~ Variable, scales = "free_y") +
    labs(
      title = "Compare factors between groups Happiness Group",
      x = "Happiness Group",
      y = "Values"
    ) +  scale_fill_brewer(palette = "Paired") +
    theme_minimal() +
    theme(
      plot.title = element_text(family = "Times", size = 14, face = "bold"),
      axis.title.x = element_text(family = "Arial", size = 10, face = "italic"),
      axis.title.y = element_text(family = "Arial", size = 10, face = "italic"),
      axis.text = element_text(family = "Arial", size = 8),
      axis.text.x = element_text(angle = 45, hjust = 1)  # Giữ xoay nhãn trục x
    )
  
  lm(HappinessLev ~ SunHr, data = city)
  
  
  ggplot(city, aes(x = Pollution, y = HappinessLev, color = HappinessGroup)) +
    geom_point(size = 3, alpha = 0.7) +
    labs(title = "Mối quan hệ giữa Pollution Index và Happiness Level",
         x = "Pollution Index",
         y = "Happiness Level") +
    theme_minimal()
  
  library(reshape2)
  
  # Tính toán ma trận tương quan
  numeric_vars <- city[, c("HappinessLev", "SunHr",
                           "Pollution", "OutdoorAct",
                           "LifeExp", "WorkHours")]
  cor_matrix <- cor(numeric_vars, use = "complete.obs")
  cor_data <- melt(cor_matrix)
  

  ggplot(cor_data, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile() +
    scale_fill_distiller(palette = "RdYlBu", direction = 1) +
    labs(
      title = "Matrix Correlation between features and Happiness Level",
      fill = "Correlation"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(family = "Times", size = 14, face = "bold"),
      axis.title.x = element_blank(), # Ẩn tiêu đề trục X
      axis.title.y = element_blank(), # Ẩn tiêu đề trục Y
      panel.grid = element_blank(),   # Loại bỏ các đường lưới
      panel.background = element_blank(), # Loại bỏ nền xám
      plot.background = element_blank(),  # Loại bỏ nền của toàn bộ biểu đồ
      axis.text.y = element_text(vjust = 0.5) # Căn chỉnh nhãn trục Y
    )
  
  __
  str(city)
  library(dplyr)
  
  levels(city$HappinessGroup)
  filtered_data <- city %>%
    filter(HappinessGroup %in% c("Average Happiness", "High Happiness"))
  library(ggplot2)
  
  ggplot(filtered_data, aes(x = Pollution, fill = HappinessGroup)) +
    geom_density(alpha = 0.5) +  # Độ trong suốt của đường cong
    labs(
      title = "Pollution Density Plot between Average & High Happiness",
      x = "Pollution Index",
      y = "Density",
      fill = "Happiness Group"
    ) +
    scale_fill_manual(values = c("Average Happiness" = "blue", "High Happiness" = "green")) +
    theme_minimal() +
    theme(
      plot.title = element_text(family = "Times", size = 14, face = "bold"),
      axis.title.x = element_text(family = "Arial", size = 10, face = "italic"),
      axis.title.y = element_text(family = "Arial", size = 10, face = "italic"),
      axis.text = element_text(family = "Arial", size = 8),
    )
  
---
    ggplot(city, aes(x = OutdoorAct, y = HappinessLev, color = Rank)) +
    geom_point(size = 3, alpha = 0.7) +
    scale_color_gradient(low = "blue", high = "red") +
    labs(
      title = "Outdoor Activities vs Happiness Level",
      x = "Outdoor Activities",
      y = "Happiness Level"
    ) +
    theme_minimal()
  
  ggplot(city, aes(x = SunHr, y = HappinessLev, size = WorkHours, color = Rank)) +
    geom_point(alpha = 0.7) +
    scale_color_gradient(low = "blue", high = "red") +
    labs(
      title = "Bubble Chart: SunHr, WorkHours \n vs Happiness Level",
      x = "Sunshine Hours",
      y = "Happiness Level",
      size = "WorkHours"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(family = "Times", size = 14, face = "bold"),
      axis.title.x = element_text(family = "Arial", size = 10, face = "italic"),
      axis.title.y = element_text(family = "Arial", size = 10, face = "italic"),
      axis.text = element_text(family = "Arial", size = 8),
      legend.text = element_text(size = 8),    
      legend.title = element_text(size = 8, face = "bold") )
  
  
  ggplot(city, aes(x = SunHr, y = HappinessLev)) +
    geom_point(color = "orange3", size = 3, alpha = 0.6) +
    geom_smooth(method = "lm", color = "darkgreen", se = TRUE) +  # Thêm đường hồi quy tuyến tính
    labs(
      title = "Relationship Between SunHr & Happiness Level",
      x = "Sunshine Hours (Annual Average)",
      y = "Happiness Level"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(family = "Times", size = 14, face = "bold"),
      axis.title.x = element_text(family = "Arial", size = 10, face = "italic"),
      axis.title.y = element_text(family = "Arial", size = 10, face = "italic"),
      axis.text = element_text(family = "Arial", size = 8)  # Giữ xoay nhãn trục x
    )

  ggplot(city, aes(x = OutdoorAct, y = HappinessLev)) +
    geom_point(color = "darkgreen", size = 3, alpha = 0.6) +
    geom_smooth(method = "lm", color = "red", se = TRUE) +  # Thêm đường hồi quy tuyến tính
    labs(
      title = "Relationship Between Outdoor Activites 
      \n & Happiness Level",
      x = "Amount of Outdoor Activities",
      y = "Happiness Level"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(family = "Times", size = 14, face = "bold"),
      axis.title.x = element_text(family = "Arial", size = 10, face = "italic"),
      axis.title.y = element_text(family = "Arial", size = 10, face = "italic"),
      axis.text = element_text(family = "Arial", size = 8)  # Giữ xoay nhãn trục x
    )
  
  ggplot(city, aes(x = WorkHours, y = HappinessLev)) +
    geom_point(color = "darkblue", size = 3, alpha = 0.6) +
    geom_smooth(method = "lm", color = "red", se = TRUE) +  # Thêm đường hồi quy tuyến tính
    labs(
      title = "Relationship Between Working Hours & Happiness Level",
      x = "Working Hours",
      y = "Happiness Level"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(family = "Times", size = 12, face = "bold"),
      axis.title.x = element_text(family = "Arial", size = 10, face = "italic"),
      axis.title.y = element_text(family = "Arial", size = 10, face = "italic"),
      axis.text = element_text(family = "Arial", size = 8)  # Giữ xoay nhãn trục x
    )
  
  

  ___

  install.packages("ggcorrplot")
  library(ggcorrplot)
  
  # Tính ma trận tương quan
  cor_data <- city %>%
    select(GymCost, BottleWaterCost, ObesityLev)
  
  cor_matrix <- cor(cor_data, use = "complete.obs")
  
  # Vẽ correlogram
  ggcorrplot(cor_matrix, 
             method = "circle", 
             type = "upper", 
             lab = TRUE, 
             lab_col = "black", 
             colors = c("blue", "white", "red"),
             title = "Correlogram of Selected Variables") +
    theme_minimal()
 ___ 
  install.packages("corrgram")
  library(corrgram)
  
  selected_data <- city %>% 
    select(GymCost, BottleWaterCost, ObesityLev, Rank)
  
# Vẽ correlogram cho 3 cột đã chọn
  corrgram(selected_data, 
           lower.panel = panel.shade,   # Tô màu cho lower panel
           upper.panel = panel.pie,     # Hiển thị tương quan dạng pie chart ở upper panel
           diag.panel = panel.minmax,   # Hiển thị giá trị min và max trên đường chéo
           col.regions = colorRampPalette(c("blue", "white", "red")), # Màu tương quan
           main = "Correlogram of Selected Variables")
_____

#create healthiest cities according to analyzed variables
    city <- city %>%
    mutate(
      HappinessGroup = cut(HappinessLev, breaks = c(0, 5.5, 7.5, Inf), 
                           labels = c("Low", "Average", "High")),
      LifeExpGroup = ifelse(LifeExp >= median(LifeExp, na.rm = TRUE), "High", "Low"),
      PollutionGroup = ifelse(Pollution < median(Pollution, na.rm = TRUE), "Low", "High"),
      WorkHoursGroup = ifelse(WorkHours < median(WorkHours, na.rm = TRUE), "Balanced", "Long"),
      SunHrGroup = cut(SunHr, breaks = quantile(SunHr, probs = c(0, 0.33, 0.67, 1), na.rm = TRUE),
                       labels = c("Low", "Medium", "High"))
    )
  
# Healthy cities 
    filter(HappinessGroup == "High",
           LifeExpGroup == "High",
           PollutionGroup == "Low",
           WorkHoursGroup == "Balanced")
  
  ggplot(healthy_cities, aes(x = reorder(City, Rank), y = Rank, fill = City)) +
    geom_bar(stat = "identity", width = 0.7) +
    geom_label(aes(label = round(HappinessLev, 2)), 
              hjust = 1.2, 
              size = 4,
              fontface = "bold",
              color = "black") +
    coord_flip() +  # Lật trục để dễ đọc tên thành phố
    labs(
      title = "Ranking of Top 3 Highest-happiness-level Cities",
      x = "City",
      y = "Rank"
    ) +
    scale_fill_brewer(palette = "Paired") +
    theme_minimal() +
    theme(legend.position = "none") + 
    theme(
      plot.title = element_text(family = "Times", size = 14, face = "bold"),
      axis.title.x = element_text(family = "Arial", size = 12, face = "italic"),
      axis.title.y = element_text(family = "Arial", size = 12, face = "italic"),
      axis.text = element_text(family = "Arial", size = 8)  # Giữ xoay nhãn trục x
    )

  
    