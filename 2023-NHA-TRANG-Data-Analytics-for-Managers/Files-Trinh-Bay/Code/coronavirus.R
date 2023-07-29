library(dplyr)
library(tidyr)
library(plotly)
library(coronavirus)


#data source
# -----https://covid19.who.int/info?openIndex=2
#----- https://github.com/RamiKrispin/coronavirus/tree/master/csv


#df <- coronavirus
df <- read.csv("../data/coronavirus/coronavirus.csv", header = TRUE, sep = ",")

data("coronavirus")

total_cases <- df %>% 
  group_by(type) %>%
  summarise(cases = sum(cases)) %>%
  mutate(type = factor(type, levels = c("confirmed", "death", "recovery")))

total_cases

head(df)


summary_df <- df %>% 
  filter(type == "confirmed") %>%
  group_by(country) %>%
  summarise(total_cases = sum(cases)) %>%
  arrange(-total_cases)


summary_df %>% head(20) 

df %>% 
  filter(date == max(date)) %>%
  select(country, type, cases) %>%
  group_by(country, type) %>%
  summarise(total_cases = sum(cases)) %>%
  pivot_wider(names_from = type,
              values_from = total_cases) %>%
  arrange(-confirmed)


df %>% 
  group_by(type, date) %>%
  summarise(total_cases = sum(cases)) %>%
  pivot_wider(names_from = type, values_from = total_cases) %>%
  arrange(date) %>%
  mutate(active = confirmed - death - recovery) %>%
  mutate(active_total = cumsum(active),
         recovered_total = cumsum(recovery),
         death_total = cumsum(death)) %>%
  plot_ly(x = ~ date,
          y = ~ active_total,
          name = 'Active', 
          fillcolor = '#1f77b4',
          type = 'scatter',
          mode = 'none', 
          stackgroup = 'one') %>%
  add_trace(y = ~ death_total, 
            name = "Death",
            fillcolor = '#E41317') %>%
  add_trace(y = ~recovered_total, 
            name = 'Recovered', 
            fillcolor = 'forestgreen') %>%
  layout(title = "Distribution of Covid19 Cases Worldwide",
         legend = list(x = 0.1, y = 0.9),
         yaxis = list(title = "Number of Cases"),
         xaxis = list(title = "Source: Johns Hopkins University Center for Systems Science and Engineering"))


conf_df <- df %>% 
  filter(type == "confirmed") %>%
  group_by(country) %>%
  summarise(total_cases = sum(cases)) %>%
  arrange(-total_cases) %>%
  mutate(parents = "Confirmed") %>%
  ungroup() 

plot_ly(data = conf_df,
        type= "treemap",
        values = ~total_cases,
        labels= ~ country,
        parents=  ~parents,
        domain = list(column=0),
        name = "Confirmed",
        textinfo="label+value+percent parent")






