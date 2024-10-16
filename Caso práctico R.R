install.packages("readr")

library(tidyverse)

str(Titanicv2)
summary(Titanicv2)


Titanicv2 %>% 
  select(PassengerId, Survived, Pclass, Name, Sex, Age, Ticket, Fare, Embarked) %>%
  filter(Sex == "female") %>%
  summarise(mean_Age = mean(Age, na.rm = TRUE)) %>%
  ungroup() %>% 
  arrange(desc(mean_Age))

Titanicv2 %>% 
  select(PassengerId, Survived, Pclass, Name, Sex, Age, Ticket, Fare, Embarked) %>%
  filter(Sex == "female") %>%
  summarise(median_Age = median(Age, na.rm = TRUE)) %>%
  ungroup() %>% 
  arrange(desc(median_Age))

Titanicv2 %>% 
  select(PassengerId, Survived, Pclass, Name, Sex, Age, Ticket, Fare, Embarked) %>%
  filter(Sex == "female") %>%
  summarise(min_Fare = min(Fare, na.rm = TRUE)) %>%
  ungroup() %>% 
  arrange(desc(min_Fare))

Titanicv2 %>% 
  select(PassengerId, Survived, Pclass, Name, Sex, Age, Ticket, Fare, Embarked) %>%
  filter(Sex == "female") %>%
  summarise(max_Fare = max(Fare, na.rm = TRUE)) %>%
  arrange(desc(max_Fare))

Titanicv2 %>% 
  select(PassengerId, Survived, Pclass, Name, Sex, Age, Ticket, Fare, Embarked) %>%
  filter(Sex == "female") %>%
  summarise(sum_Fare = sum(Fare, na.rm = TRUE)) %>%
  ungroup() %>% 
  arrange(desc(mean_Age))


group_by(Pclass, Age) %>%
  summarise(median_Age = median(Age, na.rm = TRUE)) %>%
  summarise(max_Fare = max(Fare, na.rm = TRUE)) %>%
  summarise(min_Fare = min(Fare, na.rm = TRUE)) %>%
  summarise(sum_Fare = sum(Fare, na.rm = TRUE)) %>%
  
colSums(is.na(Titanicv2))
  
install.packages("ggplot2")
library(ggplot2)

str(Titanicv2$Age) 

Titanicv2$Age <- as.numeric(Titanicv2$Age)

ggplot(Titanicv2, aes(X = Age)) + 
  geom_histogram(fill = "blue", color = "black") +
  labs(title = "Histograma de frecuencia de edad", x = "Age", y = "Frecuencia")


ggplot(Titanicv2, aes(x=Age, y=Fare)) + geom_point()

ggplot(Titanicv2, aes(x = Sex, y = Fare)) +
  geom_col()

