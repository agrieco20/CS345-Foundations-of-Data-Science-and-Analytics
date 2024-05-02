#Author: Anthony Grieco
#Date: 2/29/2024

#1
library(tidyverse)
data("starwars")
names(starwars)

#2
eyebrown <- starwars %>% select(name, height, mass, eye_color, species) %>% filter(eye_color == "brown")
view(eyebrown)

#3
eyebrown %>% arrange(desc(height))

#4
starwars <- starwars %>% mutate(bmi = mass/ (height/100)^2)
view(starwars)

#5 (Unfinished)
speciesBMI <- starwars %>% group_by(Species = species) %>% summarize(n=n(), MeanBMI = mean(bmi, na.rm = TRUE)) %>% filter(n>1)
view(speciesBMI)

#6
starwars %>% ggplot(aes(x=height, y=mass)) + geom_point()

#7
sw_mass <- starwars %>% filter(!is.na(mass))

sw_mass %>% ggplot(aes(x=height, y=mass)) + geom_point()
view(sw_mass)
sw_mass <- sw_mass %>% filter(mass < 1357)
view(sw_mass)
sw_mass %>% ggplot(aes(x=height, y=mass)) + geom_point()
sw_massCo <- lm(sw_mass$mass ~ sw_mass$height)
sw_massCo

#8
masculine <- starwars %>% filter(!is.na(mass)) %>% filter(gender == "masculine")

masculine %>% ggplot(aes(x=height, y=mass)) + geom_point()
view(masculine)
masculine <- masculine %>% filter(mass < max(mass))
view(masculine)
masculine %>% ggplot(aes(x=height, y=mass)) + geom_point()
masculineCo <- lm(masculine$mass ~ masculine$height)
masculineCo