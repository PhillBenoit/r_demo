library(tidyverse)

beer <- read_csv("https://docs.google.com/spreadsheets/d/18Iux-10Ggj2qLNEgH5WJGGUNTKET9Tpy3HHl1gc6L9Y/gviz/tq?tqx=out:csv")

summary(beer)

glimpse(beer)

beer$number_of_reviews
beer$brewery_state
unique(beer$brewery_state)
unique(beer$brewery_name)
unique(beer$style)
unique(beer$abv)
unique(beer$year)

ggplot(beer,aes(x=year))+geom_histogram()
names(beer)

xxx = c(1,2,3,4,5,6,7,8,9,10)
ifelse(xxx>5,0,xxx)
ifelse(xxx>5,0,xxx)

#fix data
beer$abv = ifelse(beer$abv == 110.50, NA, beer$abv)
beer$brewery_state <- ifelse(beer$brewery_state == 'Ariz0na', 'Arizona', beer$brewery_state)
beer$brewery_state <- ifelse(beer$brewery_state == 'washington', 'Washington', beer$brewery_state)
beer$brewery_state <- ifelse(beer$brewery_state == 'Wershington', 'Washington', beer$brewery_state)
beer$brewery_state <- ifelse(beer$brewery_state == 'xyz', NA, beer$brewery_state)
beer$year <- ifelse(beer$year == 2209, 2009, beer$year)
beer$year <- ifelse(beer$year == 0, NA, beer$year)

beer_slim <- beer %>% select(number_of_reviews,rating,abv)

glimpse(beer_slim)
summary(beer_slim)

#get low rated beers from wisconsin
bad_beer <- beer %>% filter(rating < 3.0 & brewery_state == 'Wisconsin')
summary(bad_beer)
unique(bad_beer$brewery_state)

#pull with list
two_states <- beer %>% filter(brewery_state %in% c('Arizona','Wisconsin'))
unique(two_states$brewery_state)

#add a column
beer <- beer %>% mutate(years_on_market = 2018 - year)
glimpse(beer)

#add a column
beer <- beer %>% mutate(reviews_per_year = number_of_reviews/years_on_market)
glimpse(beer)


beers_az <- beer %>% filter(brewery_state == 'Arizona') %>% select(-brewery_state)

#format graph
ggplot(beer,aes(x=reviews_per_year))+
  geom_histogram() +
  labs(x = 'Number of reviews per year', y = 'Count', title = 'Graph Name') +
  theme_classic() +
  theme(text = element_text(size = 20))

#export to my documents, dimensions in inches, watch for font size
ggsave('test_plot.jpg', width = 5, height = 4)

#theme overrides font size
ggplot(na.omit(beer),aes(x = style, y = rating)) +
  geom_boxplot() +
  theme_linedraw() +
  theme(text = element_text(size = 20)) +
  coord_flip()

#color
ggplot(na.omit(beer), aes(x=reviews_per_year, y = rating,
                          color = style, size = abv)) +
  geom_point() +
  geom_smooth(method = 'lm')

#overlay
ggplot(na.omit(beer), aes(x=brewery_state, y=abv)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, aes(alpha=0.1))

