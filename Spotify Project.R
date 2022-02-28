library(tidyverse)
library(moderndive)
load(file = "../Project/Spotify_SP22.RData")
View(Spotify_SP22)

## Contributed by Monica Min
## Find the average and standard deviation of valence and energy to determine how positive 
## and intense, respectively, a popular song tends to be.
pop_val_en <- Spotify_SP22 %>% 
  filter(popularity > 50) %>% 
  summarize(mean_val = mean(valence, na.rm = TRUE), 
            sd_val = sd(valence, na.rm = TRUE),
            mean_en = mean(energy, na.rm = TRUE), 
            sd_en = sd(energy, na.rm = TRUE))
pop_val_en
## Shows how songs that have popularity over 50 tend to have a valence mean of 0.511 and an energy mean of 0.624. 
## Based on this data, popular songs tend to have a balance between being very musically positive
## and negative. This observation is relatively reliable because of the standard 
## deviation value of 0.251 and 0.224 for valence and energy, respectively, demonstrating the data is not very widespread. This
## data helps show that companies should focus on songs that are not too positive 
## nor too negative and not too intense, as that will attract the most amount of traction from the public.


## Contributed by Monica Min
# [QUESTION] Which variables significantly affect the popularity of a song?
# [PLAN] Solve correlation between popularity and variables such as valence, energy, and year.

round(cor(Spotify_SP22$valence, Spotify_SP22$popularity), digits = 2) 
# Demonstrates how there is a weakly positive 0.01 correlation between valence and popularity of a song.
# This data shows that valence is not a significant factor into the popularity of a song and not much 
# emphasis should be placed on it than other variables. While the previous data shows that popular songs 
# (with popularity value over 50) have a valence mean of 0.511, the songs' popularity is most likely not
# severely influenced by a song's valence.

round(cor(Spotify_SP22$energy, Spotify_SP22$popularity), digits = 2)
# There is a substantially strong positive correlation between energy and popularity of 0.5, demonstrating 
# how the energy of a song possibly plays an important role in how popular a song will be. This should be 
# analyzed more for the business to understand if energy is an important factor that should be considered
# when developing a song or predicting a song's success.

round(cor(Spotify_SP22$year, Spotify_SP22$popularity), digits = 2)
# There is a strong positive correlation between year and popularity (0.88). This data shows that
# songs' release date have a strong impact on the success of a song. However, we
# can assume that a song's age is not something the company can control.



## Contributed by Monica Min
## [QUESTION] What is the probability of a popular song having a slower beat (valence + energy)?
## [PLAN] Create variables that filter out songs that have a high popularity, valence, and energy
Spotify_SP22 <- Spotify_SP22 %>% 
  mutate(pop_type = ifelse(popularity > 50, "high popularity", "low popularity"), 
         val_type = ifelse(valence > 0.5, "high valence", "low valence"),
         ener_type = ifelse(energy > 0.5, "high energy", "low energy")) #create new variable for songs that have popularity of greater than 50

proportions(table(Spotify_SP22$val_type, Spotify_SP22$pop_type), margin = 1)
proportions(table(Spotify_SP22$ener_type, Spotify_SP22$pop_type), margin = 1)
# 23.15% of low-valence songs and 11.22% of low-energy songs tend to have high popularity.
# Probability of a song being popular and having a low valence is 10.33%. However, this data also shows that 44.59% of songs 
# that have a high valence tend to not be very popular, possibly hinting at how many positive songs tend to 
# sound generic or similar to one another.


## Contributed by Monica Min
# Create a histogram that shows the relationship between energy and popularity, 
# and how it has changed over the years.

Spotify_SP22 %>% 
  ggplot(aes(x = year, fill = ener_type)) +
  geom_histogram(position = "dodge") +
  facet_wrap(~pop_type) + 
  ggtitle("Relationship between Year and Energy by Popularity") +
  labs(x = "Year") +
  theme(plot.title = element_text(hjust = 0.5))

# This graph is left skewed. Songs that are popular have seen a drastic increase in the 
# amount of high energy songs starting from the 1960s. Songs that are not as popular 
# used to generally be low energy until the 1970s, where many high energy songs also
# tended to not be very popular.
