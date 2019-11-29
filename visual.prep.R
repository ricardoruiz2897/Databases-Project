library("tidyverse")

population.all <- read.csv("acs2015_county_data.csv")
rent <- read.csv("rent.csv")
states <- read.csv("States.csv")
Zip.Code <- read.csv("zip-code.csv")

tx.co.geometry <- readRDS("tx.co.geometry.rds") %>%
  rename("County" = county.name)

population <- population.all %>%
  filter(State == "Texas") %>%
  left_join(tx.co.geometry, by = "County")


population.zip.code <- population %>%
  left_join(Zip.Code, by = "County")


#To get means per county
pop.rent <- population.zip.code %>%
  left_join(rent, by = "Zip_Code") %>%
  group_by(County) %>%
  summarise(rent.means = mean(mean), rent.median = median(median)) %>%
  left_join(tx.co.geometry, by = "County")

#Texas map data
texas <- ggplot2::map_data("county") %>%
  filter(region == "texas") %>%
  rename(County = subregion)

#Highest rent average.
pop.rent.high <- pop.rent %>%
  filter(County == "Comal")

#Highest population.
population.high <- pop.rent %>%
  filter(County == "Tarrant")

#Lowest population.
population.low <- pop.rent %>%
  filter(County == "King")

#Lowest rent average.
pop.rent.low <- pop.rent %>%
  filter(County == "Jim Hogg")

pop.rent.low.high <- rbind(pop.rent.high, pop.rent.low)
pop.low.high <- rbind(population.high, population.low)


#Demographics data for counties Tn texas
population.comal <- population %>%
  filter(County == "Comal")

population.jim <- population %>%
  filter(County == "Jim Hogg")

jim.map <- texas %>%
  filter(County == "jim hogg")

jim.plot <- ggplot2::ggplot(jim.map) +
  ggplot2::geom_polygon(aes(long, lat, group = County)) +
  labs(title = "Jim Hogg") 

demographics.jim <- population.jim %>%
  select(County, Hispanic, White, Black, Native, Asian) %>% 
  pivot_longer(cols = -County, names_to = "Race", values_to = "Total")


demographics.comal <- population.comal %>%
  select(County, Hispanic, White, Black, Native, Asian) %>% 
  pivot_longer(cols = -County, names_to = "Race", values_to = "Total")


##########
# States #
##########

#Texas map data
texas <- ggplot2::map_data("county") %>%
  filter(region == "texas") %>%
  rename(County = subregion)

cal <- ggplot2::map_data("county") %>%
  filter(region == "california") %>%
  rename(County = subregion)

population.cal <- population.all %>%
  filter(State == "California")

pop.zip.code.cal <- population.cal %>%
  left_join(Zip.Code, by = "County")

#Rents per county in california.
avg.rents.cal <- pop.zip.code.cal %>% 
  left_join(rent, by = "Zip_Code") %>%
  group_by(County) %>%
  summarise(rent.avg = mean(mean))

avg.rents.tx <- population.zip.code %>% 
  left_join(rent, by = "Zip_Code") %>%
  group_by(County) %>%
  summarise(rent.avg = mean(mean))

#Average rents
mean(avg.rents.cal$rent.avg, na.rm = T)
mean(avg.rents.tx$rent.avg, na.rm = T)

#Demographics cal.
View(population.cal)

total.hispanics.cal <- sum(population.cal$Hispanic)
total.white.cal <- sum(population.cal$White)
total.black.cal <- sum(population.cal$Black)
total.native.cal <- sum(population.cal$Native)
total.asian.cal <- sum(population.cal$Asian)

demographics.cal <- data.frame(races = c("Asian", "Native", "Black", "White", "Hispanic"), 
                               totals = c(total.asian.cal, total.native.cal, total.black.cal, total.white.cal, total.hispanics.cal)) 

#Demographics texas.
total.hispanics.tx <- sum(population$Hispanic)
total.white.tx <- sum(population$White)
total.black.tx <- sum(population$Black)
total.native.tx <- sum(population$Native)
total.asian.tx <- sum(population$Asian)

demographics.tx <- data.frame(races = c("Asian", "Native", "Black", "White", "Hispanic"), 
                               totals = c(total.asian.tx, total.native.tx, total.black.tx, total.white.tx, total.hispanics.tx)) 

View(demographics.tx)
