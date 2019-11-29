#Rent mean per county.
map <- ggplot2::ggplot(texas) + 
  ggplot2::geom_polygon(aes(long, lat, group = County)) +
  geom_sf(data = pop.rent, aes(fill = rent.means, geometry = geometry)) + 
  geom_point(data = pop.rent.low.high, aes(latitude, longitude), color = alpha("red", 1)) +
  labs(title = "Rent By County")

plot(map)

#Jim Hogg vs Comal.

ggplot(data=demographics.jim, aes(x=Race, y=Total)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=Total), vjust=-0.3, size=3.5)+
  theme_minimal() + 
  ggtitle(label = "Jim Hogg Demographics ($187.3333)")

ggplot(data=demographics.comal, aes(x=Race, y=Total)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=Total), vjust=-0.3, size=3.5)+
  theme_minimal() + 
  ggtitle(label = "Comal Demographics ($1558)")


#Average Rent Maps.
ggplot2::ggplot(cal) + 
  ggplot2::geom_polygon(aes(long, lat, group = County)) + 
  ggtitle(label = "California Average Rent: $976.687")

ggplot2::ggplot(texas) + 
  ggplot2::geom_polygon(aes(long, lat, group = County)) + 
  ggtitle(label = "Texas Average Rent: $674.8337")


ggplot(data=demographics.tx, aes(x=races, y=totals)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=totals), vjust=-0.3, size=3.5)+
  theme_minimal() + 
  ggtitle(label = "Texas Demographics ($674.8337)")

ggplot(data=demographics.cal, aes(x=races, y=totals)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=totals), vjust=-0.3, size=3.5)+
  theme_minimal() + 
  ggtitle(label = "California Demographics ($976.687)")




