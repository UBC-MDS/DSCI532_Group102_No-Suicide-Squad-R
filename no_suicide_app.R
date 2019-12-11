
#### IMPORT LIBRARIES
library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashTable)
library(tidyverse)
library(plotly)
library(tidyverse, quiet = TRUE)
library(cowplot)
library(gapminder)
library(ggridges) 
library(scales)
library(janitor)
#library(viridis)
library(plotly)
library(ggthemes)
theme_set(theme_cowplot())

app <- Dash$new(external_stylesheets = "https://codepen.io/chriddyp/pen/bWLwgP.css")

#### IMPORT DATA
final_df <- read_csv('data/final_df.csv')
plot_a_data <- read_csv('data/plot_a_data.csv')
general_data <- read_csv('data/general_data.csv')

#### CHARTS
# TAB 0 - PLOT 0A (MAP)

# TAB 1 - PLOT 1A (STATIC CONTINENTS)
plot_1a <- ggplot() +
    geom_line(data =plot_a_data, aes(x=year, y=suicides_per_100k_pop,color = continent),size = 1)+
    geom_line(data =general_data, aes(x=year, y=suicides_per_100k_pop,color = Label),size = 1,linetype = "dashed")+
    ggtitle("Suicide Rate by Continent") +
    ylab("Suicides per 100k pop") +
    xlab("Year") +
    guides(color=guide_legend(title="Continents"))+
    theme_bw()
plot_1a <- ggplotly(plot_1a)
plot_1a

# TAB 1 - PLOT 1B
make_plot1b <- function(selected_regions = list('Central America','Western Europe')) {
    regions <- c(selected_regions)
    a <- c('Central America','Western Europe')
    
    plot_b_data <- final_df %>%
    filter(sub_region %in% a) %>%
    group_by(year,sub_region) %>% 
    summarise(suicides_per_100k_pop = mean(suicides_per_100k_pop)) %>%
    modify_if( ~is.numeric(.), ~round(., 2))
    
    options(repr.plot.width = 7, repr.plot.height = 5)
    plot_1_b <- ggplot() +
    geom_line(data =general_data, aes(x=year, y=suicides_per_100k_pop,color = Label),size = 1,linetype = "dashed")+
    geom_line(data =plot_b_data, aes(x=year, y=suicides_per_100k_pop,color = sub_region),size = 1)+
    ggtitle("Suicide Rate by Region") +
    ylab("Suicides per 100k pop") +
    xlab("Year") +
    guides(color=guide_legend(title="Regions"))+
    theme_bw()
    
    plot_1b <- ggplotly(plot_1_b)
    return(plot_1b)
}

# TAB 1 - PLOT 1C
make_plot1c <- function(selected_countries_c = list("Canada", "United States")) {
    countries_c <- c(selected_countries_c)

    country_df <- final_df %>%
    mutate(suicides_by_pop = suicides_no/(population/100000)) %>%
    filter(country %in% countries_c) %>%
    group_by(country, year) %>%
    summarize(suicide_rate = mean(suicides_by_pop)) 

    options(repr.plot.width = 7, repr.plot.height = 5)
    country_plot_gg <- ggplot(data = country_df, aes(x = year, y = suicide_rate, fill = country, color = country)) + 
    geom_line() +
    labs(x = "Year", 
         y = "Suicides per 100k pop", 
         title = "Suicide Rate per Country",
         color = "Country")

    country_plot <- ggplotly(country_plot_gg)
    return(country_plot)
}

# TAB 1 - PLOT 1D
make_plot1d <- function(selected_countries_d = list("Canada", "United States")) {
    countries_d <- c(selected_countries_d)

    demographic_df <- final_df %>%
    mutate(suicides_by_pop = suicides_no/(population/100000)) %>%
    filter(country %in% countries_d) %>%
    group_by(sex, year) %>%
    summarize(suicide_rate = mean(suicides_by_pop, na.rm = TRUE))

    options(repr.plot.width = 7, repr.plot.height = 5)
    demographic_plot <- ggplot(data = demographic_df, aes(x = year, y = suicide_rate, fill = sex, color = sex)) + 
    geom_line() + 
    labs(x = "Year", 
         y = "Suicides per 100 k pop", 
         title = "Average Suicide Rate by Sex in Selected Country (Countries)",
         color = "Sex")

    dem_plot <- ggplotly(demographic_plot)
    return(dem_plot)
}

# TAB 2 - PLOT 2A 
make_plot2a <- function(country_a = 'Any Country', country_b = 'Any Country', year_list = list(1, 1)) {
    # Sets default values
    year_start <- year_list[[1]]
    year_end <- year_list[[2]]
    
    a = country_a
    b = country_b
    
    fill_colors = c("#699FA1", "#DD8627")
    
    data_2a <- final_df %>%
    filter(suicides_per_100k_pop > 0.1) %>%
    filter(year >= year_start & year <= year_end) %>%
    filter(country == a | country == b) %>%
    group_by(country) %>%
    summarize(mean_suicides = mean(suicides_per_100k_pop, na.rm = TRUE))

    chart_2a <- ggplot(data_2a) +
        theme_bw() +
        geom_bar(aes(x = fct_rev(country), y = mean_suicides, fill = country), stat = 'identity') +
        scale_fill_manual(values = fill_colors) +
        labs(x = 'Country', y = 'Average Suicide Rate (per 100k pop)', title = 'Suicide Rate by Country') +
        coord_flip() +
        theme(legend.position = "none") 
    chart_2a <- ggplotly(chart_2a, tooltip = c("suicide_rate")) %>% config(displayModeBar = FALSE)
    return(chart_2a)
}

# TAB 2 - PLOT 2B 
make_plot2b <- function(country_a = 'Any Country', country_b = 'Any Country', year_list = list(1,1), demo_selection = list('female : 25-34 years')) {
    # Sets default values
    year_start <- year_list[[1]]
    year_end <- year_list[[2]]
    demos <- c(demo_selection)
    
    a = country_a
    b = country_b
    
    fill_colors = c("#699FA1", "#DD8627")
    
    data_2b <- final_df %>%
    filter(suicides_per_100k_pop > 0.1) %>%
    filter(year >= year_start & year <= year_end) %>%
    filter(country == a | country == b) %>%
    filter(demo_group %in% demos) %>%
    group_by(country, demo_group) %>%
    summarize(mean_suicides = mean(suicides_per_100k_pop, na.rm = TRUE))
    
    chart_2b <- ggplot(data_2b) +
        theme_bw() +
        geom_bar(aes(x = fct_rev(demo_group), y = mean_suicides, fill = country), stat = 'identity', position = position_dodge(width = 0.9)) +
        scale_fill_manual(values = fill_colors) +    
        labs(x = 'Country', y = 'Average Suicide Rate (per 100k pop)', title = 'Suicide Rate by Demographic Group')

    chart_2b <- ggplotly(chart_2b, tooltip = c("mean_suicides")) %>% config(displayModeBar = FALSE)
    return(chart_2b)
}
#### PLOTS
graph_1b <- dccGraph(
  id = 'graph_1b',
  figure = make_plot1b(list('Central America','Western Europe'))
)

graph_1c <- dccGraph(
  id = 'graph_1c',
  figure = make_plot1c(list("Canada", "United States")) 
)

graph_1d <- dccGraph(
  id = 'graph_1d',
  figure = make_plot1d(list("Canada", "United States"))
)

graph_2a <- dccGraph(
  id = 'graph_2a',
  figure = make_plot2a('Canada', 'United States', list(2010, 2015))
)

graph_2b <- dccGraph(
  id = 'graph_2b',
  figure = make_plot2b('Canada', 'United States', list(2010,2012), list('male : 15-24 years', 'female : 15-24 years'))
)

#### SET UP LAYOUT
app$layout(htmlDiv(list(
  htmlDiv(className = 'app_body', children = list(
    dccTabs(id = 'control-tabs', value = 'tabs', children = list(
      dccTab(
        label = 'Tab One',
        value = 'tab one',
        children = htmlDiv(className = 'control-tab', children = list(
          htmlP('Written instructions go here.', id = 'text1'),
          graph_1b
          #### charts go here
        ))
      ),

      dccTab(
        label = 'Tab Two',
        value = 'tab two',
        children = htmlDiv(className = 'control-tab', children = list(
          htmlP('Written instructions go here.', id = 'text2')
          #### charts go here
        ))
      ),
      
      dccTab(
        label = 'Tab Three',
        value = 'tab three',
        children = htmlDiv(className = 'control-tab', children = list(
          htmlP('Written instructions go here.', id = 'text3')
          #### charts go here
        ))
      )
    ))
  ))
)))

# Selection components

#We can get the years from the dataset to make ticks on the slider
yearMarks <- map(unique(final_df$year), as.character)
names(yearMarks) <- unique(final_df$year)
yearSlider <- dccRangeSlider(
  id = 'year_slider',
  marks = yearMarks,
  min = 1986,
  max = 2014,
  step=1,
  value = list(1986, 2014)
)

continentDropdown <- dccDropdown(
  id = 'continent_dd',
  options = map(
    levels(final_df$continent), function(x){
      list(label=x, value=x)
    }),
  value = levels(final_df$continent), 
  multi = TRUE
)

# # Storing the labels/values as a tibble means we can use this both 
# # to create the dropdown and convert colnames -> labels when plotting
# yaxisKey <- tibble(label = c("GDP Per Capita", "Life Expectancy", "Population"),
#                    value = c("gdpPercap", "lifeExp", "pop"))

# yaxisDropdown <- dccDropdown(
#   id = "filter_dd",
#   options = map(
#     1:nrow(yaxisKey), function(i){
#       list(label=yaxisKey$label[i], value=yaxisKey$value[i])
#     }),
#   value = "gdpPercap"
# )


app$run_server()
