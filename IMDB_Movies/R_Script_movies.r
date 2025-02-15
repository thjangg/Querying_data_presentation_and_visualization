# Exercise.

install.packages("ggplot2movies")
library(ggplot2movies)
library(ggplot)
library(dplyr)
?movies

data(movies, package="ggplot2movies")
movies <- as.data.frame(movies)
##Understanding data
View(movies)

head(movies)

summary(movies)
sum(is.na(movies))
colSums(is.na(movies))
movies_clean <- na.omit(movies)
nrow(movies)

##visulization

movies_by_year <- movies %>%
  group_by(year) %>%
  summarise(Movie_Count = n())

# Vẽ biểu đồ số lượng phim qua các năm
ggplot(movies_by_year, aes(x = year, y = Movie_Count)) +
  geom_line(color = "darkblue", size = 1) +  
  labs(
    title = "Number of movies produced through years",
    x = "Year",
    y = "Movie counts"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold")
  )

____
ggplot(movies, aes(x = rating)) +
  geom_histogram(binwidth = 0.5, fill = "darkblue", color = "black", alpha = 0.7) +
  labs(
    title = "Rating Distribution",
    x = "Rating",
    y = "Frequency"
  ) +
  theme_minimal()
___
top10_movies <- movies %>%
  arrange(desc(rating)) %>%  
  slice(1:10)  

# Hiển thị danh sách top 10 phim
top10_movies %>%
  select(title, year, rating, votes)


# Visualization cho Top 10 phim
ggplot(top10_movies, aes(x = reorder(title, rating), y = rating, fill = rating)) +
  geom_bar(stat = "identity", color = "black") +
  coord_flip() +  # Xoay ngang biểu đồ
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(
    title = "Top 10 highest rating movies",
    x = "Movie title",
    y = "Rating"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10),  
    plot.title = element_text(size = 14, hjust = 0.5) 
  )

____
top10_movies <- movies %>%
  arrange(desc(votes)) %>%
  slice(1:10)

ggplot(top10_movies, aes(x = reorder(title, votes), y = votes, fill = votes)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Top 10 most voted movies",
    x = "Movie title",
    y = "Votes"
  ) +
  theme_minimal()
___
# Tạo dataset mới với tổng số phim theo từng thể loại
genre_count <- movies %>%
  select(Action, Animation, Comedy, Drama, Documentary, Romance, Short) %>%
  summarise_all(sum)

# Reshape dữ liệu để phù hợp với ggplot2
genre_count_long <- tidyr::pivot_longer(genre_count, cols = everything(), names_to = "Genre", values_to = "Count")

# Vẽ bar chart
ggplot(genre_count_long, aes(x = Genre, y = Count, fill = Genre)) +
  geom_bar(stat = "identity") + coord_flip() +
  scale_fill_brewer(palette = "Paired") +
  labs(title = "Movie counts by Genre", x = "Genre", y = "Count") +
  theme_minimal()


# Tổng hợp số lượng phim theo thể loại
genre_count <- movies %>%
  select(Action, Comedy, Drama, Documentary, Romance, Short) %>%
  summarise_all(sum) %>%
  pivot_longer(cols = everything(), names_to = "Genre", values_to = "Count")

# Vẽ Pie Chart
ggplot(genre_count, aes(x = "", y = Count, fill = Genre)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Film distribution by genre") +
  theme_void() +
  theme(legend.position = "right")

_________
# Tính điểm trung bình theo từng thể loại
genre_ratings <- movies %>%
  select(Action, Comedy, Drama, Documentary, Romance, Short, rating) %>%
  pivot_longer(cols = Action:Short, names_to = "Genre", values_to = "Is_Genre") %>%
  filter(Is_Genre == 1) %>%
  group_by(Genre) %>%
  summarise(Average_Rating = mean(rating, na.rm = TRUE))

# Vẽ bar chart
ggplot(genre_ratings, aes(x = reorder(Genre, Average_Rating), y = Average_Rating, fill = Genre)) +
  geom_bar(stat = "identity") +
  coord_flip() +  scale_fill_brewer(palette = "Paired") +
  labs(title = "Average score by genres", 
       x = "Genre", 
       y = "Average score") +
  theme_minimal()

___
ggplot(movies, aes(x = year, y = length)) +
  geom_point(aes(group = year)) +
  scale_y_continuous(limits=c(0, 900))
  labs(title = "Movie length pattern through years", 
       x = "Year", 
       y = "Movie length (minute)") +
  theme_minimal()
  
  ggplot(movies, aes(x=year, y=length) ) +
    geom_hex(aes(group = year), bins = 70) +

    scale_fill_continuous(type = "viridis") +
    labs(title = "Movie length pattern through years") +
     theme_bw()
  
  ____
  
  
  # Tổng hợp số lượng phim theo thể loại
  genre_count <- movies %>%
    select(Action, Comedy, Drama, Documentary, Romance, Short) %>%
    summarise_all(sum) %>%
    pivot_longer(cols = everything(), names_to = "Genre", values_to = "Count")
  
  # Tính tỷ lệ phần trăm cho từng thể loại
  genre_count <- genre_count %>%
    mutate(Percentage = Count / sum(Count) * 100)
  
  # Vẽ Pie Cha
  ggplot(genre_count, aes(x = "", y = Count, fill = Genre)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar(theta = "y") +
    scale_fill_brewer(palette = "Paired") +
    geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
              position = position_stack(vjust = 0.5)) +
    labs(title = "Movie distribution by genres") +
    theme_void()
  
____
  # Chuyển dữ liệu từ binary sang định dạng "long" để dễ thao tác
  movies_long <- movies %>%
    select(year, Action, Comedy, Drama, Documentary, Romance, Short) %>%
    pivot_longer(cols = -year, names_to = "Genre", values_to = "Is_Produced") %>%
    filter(Is_Produced == 1) %>%  
    group_by(year, Genre) %>%
    summarise(Count = n(), .groups = "drop")  
  
  # Vẽ biểu đồ với các panel
  ggplot(movies_long, aes(x = year, y = Count)) +
    geom_line(color = "darkblue", size = 1) +
    facet_wrap(~ Genre, scales = "free_y") +  
    labs(
      title = "Movies released through years by categories",
      x = "Year",
      y = "Number of movies released"
    ) +
    theme_minimal() +
    theme(
      strip.text = element_text(size = 12, face = "bold"),  
      axis.text.x = element_text(angle = 45, hjust = 1)   
    )
  
  
  ___
  movies_long <- movies %>%
    select(votes, rating, Action, Comedy, Drama, Documentary, Romance, Short) %>%
    pivot_longer(cols = Action:Short, names_to = "Genre", values_to = "Is_Produced") %>%
    filter(Is_Produced == 1)
  ggplot(movies_long, aes(x = votes, y = rating)) +
    geom_point(alpha = 0.6, color = "darkblue") +
    facet_wrap(~ Genre, scales = "free") +  
    labs(
      title = "Votes and Rating relationship by Genre",
      x = " Votes",
      y = "Rating"
    ) +
    theme_minimal() +
    theme(
      strip.text = element_text(size = 12, face = "bold"),  
      axis.text.x = element_text(angle = 45, hjust = 1)   
    )
  
  ggplot(movies, aes(x = length, y = votes)) +
    geom_point(alpha = 0.6, color = "steelblue") +
    scale_x_continuous(limits=c(0, 1000)) +
    labs(title = "Votes and Length Realationship",
      x = "Movie length (minute)",
      y = "Votes"
    ) +
    theme_minimal()
  ____

  