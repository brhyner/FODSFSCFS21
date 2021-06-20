# Set Up #################

source("Scripts/Init.R")


# import Data set ########

nyt <- readRDS("trump_politics_nyt.Rda")

# recode date variable
nyt$pub.date <- as_date(nyt$response.docs.pub_date) 

# overview ###############

colnames(nyt)

# type of material 

df_type <- nyt %>% 
  
  group_by(response.docs.type_of_material) %>%
  
  summarise(freq = n())

df_type

# average word count by type of material

avg_wc <- nyt %>%
  
  filter(response.docs.word_count > 0) %>%
   
  group_by(response.docs.type_of_material) %>%
  
  summarise(freq = n(), 
            avg_words = mean(response.docs.word_count, na.rm = T))

avg_wc

# published articles by date

gg1 <- ggplot(nyt, 
              aes(x = pub.date)) + 
  
  # geom_area(stat = "bin")
  
  geom_bar(stat = "bin", binwidth = 20)

gg1

# dates with most published articles

most_art <- nyt %>%
  
  group_by(pub.date) %>%
  
  summarise(n.art = n()) %>%
  
  arrange(desc(n.art)) %>%
  
  head(n = 10)

most_art

# Analysis ##################

# Trump mentioned in the Headline? 
# create new variable: if Trump is mentioned in the Headline --> 1, 0 otherwies
nyt$Trump <- ifelse(grepl("Trump", nyt$response.docs.headline.main), 1, 0)

# relative freq of Trump mentionings
sum(nyt$Trump)/nrow(nyt) * 100
# Trump was mentioned in 25% of all headlines!

# sample of headlines mentioning Trump
sample(nyt$response.docs.headline.main[nyt$Trump == 1], 10)

# Trump mentionings over time

trump_time <- nyt %>%
  
  group_by(pub.date) %>%
  
  summarise(n.articles = n(),
            trump.articles = sum(Trump),
            freq = sum(Trump) / n() * 100)

gg2 <- ggplot(trump_time[trump_time$trump.articles > 0,], aes(x = pub.date)) + 
  
  geom_bar(stat = "bin", binwidth = 15)

gg2  

# alternatively: only makes sense for months 

trump_time_2 <- nyt %>%
  
  mutate(yyyy.mm = format_ISO8601(pub.date, precision = "ym")) %>%
  
  group_by(yyyy.mm, Trump) %>%
  
  summarise(n.articles = n())

gg3 <- ggplot(trump_time_2, aes(x = yyyy.mm, y = n.articles, fill = factor(Trump))) + 
  
  geom_bar(stat = "identity") + 
  
  theme_minimal() +
  
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 10),
        legend.position = "bottom") + 
  
  ggtitle("Trump Mentionings over time") + 
  
  scale_fill_manual(name = "Was Trump mentioned?", labels = c("No", "Yes"), values = c("darkred", "darkblue"))


gg3
 