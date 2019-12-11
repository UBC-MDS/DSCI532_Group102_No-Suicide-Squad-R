
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
library(plotly)
library(ggthemes)
theme_set(theme_cowplot())

app <- Dash$new(external_stylesheets = "https://codepen.io/chriddyp/pen/bWLwgP.css")

#### IMPORT DATA
final_df <- read_csv('data/final_df.csv')
plot_a_data <- read_csv('data/plot_a_data.csv')
general_data <- read_csv('data/general_data.csv')

#### CHART FUNCTIONS
# TAB (WORLDWIDE OVERVIEW)

# TAB 1 - PLOT 1a (STATIC CONTINENTS)


# TAB 1 - PLOT 1b
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

# TAB 1 - PLOT 1c
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

# TAB 1 - PLOT 1d
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

# TAB 2 - PLOT 2a 
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

# TAB 2 - PLOT 2Bb
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
#### DCC GRAPHS
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

#### DROPDOWNS
regionDD <- dccDropdown(
  id = 'region_dd',
  options = map(
    levels(final_df$sub_region), function(x){
      list(label=x, value=x)
    }),
    value = levels(final_df$sub_region),
    multi = TRUE
)

countryDD1 <- dccDropdown(
  id = 'country_dd_1',
  options = map(
    levels(final_df$country), function(x){
      list(label=x, value=x)
    }),
    value = levels(final_df$country),
    multi = TRUE
)

countryDD2a <- dccDropdown(
  id = 'country_dd_2a',
  options = map(
    levels(final_df$country), function(x){
      list(label=x, value=x)
    }),
    value = levels(final_df$country),
    multi = TRUE
)

countryDD2b <- dccDropdown(
  id = 'country_dd_2b',
  options = map(
    levels(final_df$country), function(x){
      list(label=x, value=x)
    }),
    value = levels(final_df$country),
    multi = TRUE
)

yearMarks2 <- map(unique(final_df$year), as.character)
names(yearMarks2) <- unique(final_df$year)
yearSlider <- dccRangeSlider(
  id = 'year_slider',
  marks = yearMarks2,
  min = 1986,
  max = 2014,
  step=1,
  value = list(1986, 2014)
)

demoCC <- dccDropdown(
  id = 'demo_cc',
  value = levels(final_df$demo_group),
  multi = TRUE
)

#### SET UP LAYOUT
app$layout(htmlDiv(list(
  htmlH2('Understanding Suicide Rates'),
  dccMarkdown("
                The purpose of this app is to help you visualize suicide rates in different locations over time, and see how a variety of different factors (i.e. age, gender, and year) affect these rates.  
                
                We have 2 main questions we are trying to answer: 

                **Tab 1**: How does the suicide rate change over time, and what effect does continent, region, country, age, and gender have on this?  
                **Tab 2**: How does the suicide rate of one country compare against the suicide rate of another country?   

                Click on the 'Worldwide Overview' tab to begin exploring, and then move on to Tabs 1 and 2 to dive deeper.  

                **If you have thoughts of suicide, please reach out to your local Crisis Centre or Suicide Prevention Hotline.**  
                **In BC, you can get help by visiting [www.crisiscentre.bc.ca](https://crisiscentre.bc.ca) or by calling 1-800-784-2433 from anywhere in the province.**
                
                Source: Suicide Rate Data used in this app is from [Kaggle](https://www.kaggle.com/russellyates88/suicide-rates-overview-1985-to-2016#master.csv)
                "),
  htmlDiv(className = 'app_body', children = list(
    dccTabs(id = 'control-tabs', value = 'tabs', children = list(
      dccTab(
        label = 'Overview',
        value = 'tab value',
        children = htmlDiv(className = 'control-tab', children = list(
          htmlP('Written instructions go here.', id = 'text0')

        ))
      ),

      dccTab(
        label = 'Tab 1: Continent-Region-Country Analysis',
        value = 'continent-region-country analysis',
        children = htmlDiv(className = 'control-tab', children = list(
          htmlH2('Continent-Region-Country Analysis'),

          # PLOT 1a
          htmlH3('Suicide Rate by Continent'),
          dccMarkdown('**Step 1:** This graph shows the average suicide rate over time, by continent.'),
          graph_1a,

          # PLOT 1b
          htmlH3('Suicide Rate by Region'),
          dccMarkdown('**Step 2:** Are there any sub-regions you are specifically interested in looking at?
                      Select one or more sub-regions (arranged by continent) to view the average suicide rate by year.'),
          regionDD,
          graph_1b,

          # PLOT 1c
          htmlH3('Suicide Rate by Country'),
          dccMarkdown('**Step 3:** Are there any specific countries you\'d like to look into?
                      Select one or more countries (arranged by continent) to view the average suicide rate by year.'),
          countryDD1,
          graph_1c,

          # PLOT 1d
          htmlH3('Suicide Rate by Demographic Groups'),
          dccMarkdown('**Step 4:** This graph will automatically show the average suicide rates for men and women based on the countries you selected previously.'),
          graph_1d
        ))
      ),
      
      dccTab(
        label = 'Tab 3: Two Country Comparison',
        value = 'two country comparison',
        children = htmlDiv(className = 'control-tab', children = list(
          htmlH2('Two Country Comparison'),

          # PLOT 2a
          dccMarkdown('**Step 1:** Select 2 countries that you would like to compare (1 country per dropdown)'),
          countryDD2a,
          countryDD2b,
          dccMarkdown('**Step 2:** Select a range of years to see the average suicide rate for this time frame.'),
          yearSlider,
          graph_2a,

          # PLOT 2b
          dccMarkdown('**Step 3:** Select one or more groups to see average suicide rates by demographics in each country.'),
          demoCC,
          graph_2b
        ))
      )
    ))
  ))
)))

#### CALLBACKS

app$run_server()
