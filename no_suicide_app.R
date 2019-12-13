#### IMPORT LIBRARIES
library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
#library(dashTable)
library(plotly)
library(tidyverse, quiet = TRUE)
library(ggthemes)
theme_set(theme_bw())

app <- Dash$new(external_stylesheets = "https://codepen.io/chriddyp/pen/bWLwgP.css")

#### IMPORT DATA
final_df <- read_csv('https://raw.githubusercontent.com/UBC-MDS/DSCI532_Group102_No-Suicide-Squad-R/master/data/final_df.csv') %>%
  modify_if( ~is.numeric(.), ~round(., 3))
plot_a_data <- read_csv('https://raw.githubusercontent.com/UBC-MDS/DSCI532_Group102_No-Suicide-Squad-R/master/data/plot_a_data.csv') %>%
  modify_if( ~is.numeric(.), ~round(., 3))
general_data <- read_csv('https://raw.githubusercontent.com/UBC-MDS/DSCI532_Group102_No-Suicide-Squad-R/master/data/general_data.csv') %>%
  modify_if( ~is.numeric(.), ~round(., 3))

#### DROPDOWNS
regionDD <- dccDropdown(
  id = 'region_dd',
  options = lapply(
    unique(final_df$sub_region), function(x){
      list(label=x, value=x)
    }),
    #value = levels(final_df$sub_region),
    multi = TRUE
)

countryDD1 <- dccDropdown(
  id = 'country_dd_1',
  options = lapply(
    unique(final_df$country), function(x){
      list(label=x, value=x)
    }),
    #value = 'Australia',
    multi = TRUE
)

countryDD2a <- dccDropdown(
  id = 'country_dd_2a',
  options = lapply(
    unique(final_df$country), function(x){
      list(label=x, value=x)
    }),
    value = 'Canada',
    multi = FALSE
)

countryDD2b <- dccDropdown(
  id = 'country_dd_2b',
  options = lapply(
    unique(final_df$country), function(x){
      list(label=x, value=x)
    }),
    value = 'United States',
    multi = FALSE
)

yearMarks_wm <- lapply(unique(final_df$year), as.character)
names(yearMarks_wm) <- unique(final_df$year)
yearSlider_wm <- dccRangeSlider(
  id = 'year_slider_wm',
  marks = yearMarks_wm,
  min = 1986,
  max = 2014,
  step = 1,
  value = list(1986, 2014)
)

yearMarks2 <- lapply(unique(final_df$year), as.character)
names(yearMarks2) <- unique(final_df$year)
yearSlider2 <- dccRangeSlider(
  id = 'year_slider',
  marks = yearMarks2,
  min = 1986,
  max = 2014,
  step = 1,
  value = list(1986, 2014)
)

demoCC <- dccDropdown(
  id = 'demo_cc',
  options = lapply(
    unique(final_df$demo_group), function(x){
      list(label=x, value=x)
    }),
    value = 'female : 25-34 years',
    multi = TRUE
)

#### CHART FUNCTIONS
# TAB (WORLDWIDE OVERVIEW)
make_world_plot <- function(year_list = list(1987, 2014)) {
  year_start <- year_list[[1]]
  year_end <- year_list[[2]]

  map_data <- final_df %>%
    filter(between(year, year_start, year_end)) %>%
    group_by(year,country_code_name,country) %>% 
    summarise(suicides_per_100k_pop = mean(suicides_per_100k_pop)) %>%
    modify_if( ~is.numeric(.), ~round(., 3))

  options(repr.plot.width = 7, repr.plot.height = 5)
  l <- list(color = toRGB("black"), width = 0.5)
  # specify map projection/options
  g <- list(
    #showframe = FALSE,
    #showcoastlines = FALSE,
    showocean = TRUE,
    oceancolor = toRGB("#DCEEFF"),
    projection = list(type = 'Natural earth')
  )
  map_plot <- plot_geo(map_data) %>%
    add_trace(
      z = ~suicides_per_100k_pop, color = ~suicides_per_100k_pop, colorscale='Magma', zmax = 10,
      text = ~country, locations = ~country_code_name, marker = list(line = l)
    ) %>%
    colorbar(title = 'Annual Suicides per 100k pop') %>%
    layout(
      title = 'Average Suicide Rate per Country',
      geo = g
    )
  return(map_plot)
}

# TAB 1 - PLOT 1A (STATIC CONTINENTS)
make_plot1a <- function() {
  #' Makes a plot showing suicide rate over time by continent
  #'
  #' @param None
  #' @return An interactive plotly graph of suicide rate by continent
  #' @examples
  #' make_plot1a()
  
    plot_1a <- ggplot() +
      geom_line(data =plot_a_data, aes(x = year, y = suicides_per_100k_pop,color = continent)) +
      geom_line(data =general_data, aes(x = year, y = suicides_per_100k_pop,color = Label),linetype = "dashed") +
      ggtitle("Suicide Rate by Continent") +
      ylab("Suicides per 100k pop") +
      xlab("Year") +
      guides(color=guide_legend(title="Continents"))+
      scale_x_continuous(breaks = c(1985, 1990, 1995, 2000, 2005, 2010, 2015), 
        labels = c(1985, 1990, 1995, 2000, 2005, 2010, 2015),
        limits = c(1985, 2016)) +
      theme_bw()

    plot_1a <- ggplotly(plot_1a)
    return(plot_1a)
}

# TAB 1 - PLOT 1b
make_plot1b <- function(selected_regions = list('Central America','Western Europe')) {
  #' Makes a plot showing suicide rate over time by region
  #'
  #' @param selected_regions A list of one or more regions
  #' @return An interactive plotly graph of suicide rate by selected regions
  #' @examples
  #' make_plot1b(selected_regions = list("Northern America"))
  #' make_plot1b(selected_regions = list("Central America", "Northern America"))
    regions <- c(selected_regions)
    #a <- c('Central America','Western Europe')
    
    plot_b_data <- final_df %>%
      filter(sub_region %in% selected_regions) %>%
      group_by(year,sub_region) %>% 
      summarise(suicides_per_100k_pop = mean(suicides_per_100k_pop)) %>%
      modify_if( ~is.numeric(.), ~round(., 3))
    
    options(repr.plot.width = 7, repr.plot.height = 5)
    plot_1_b <- ggplot() +
      geom_line(data =general_data, aes(x=year, y=suicides_per_100k_pop,color = Label), linetype = "dashed")+
      geom_line(data =plot_b_data, aes(x=year, y=suicides_per_100k_pop,color = sub_region))+
      ggtitle("Suicide Rate by Region") +
      ylab("Suicides per 100k pop") +
      xlab("Year") +
      guides(color=guide_legend(title="Regions"))+
      scale_x_continuous(breaks = c(1985, 1990, 1995, 2000, 2005, 2010, 2015), 
        labels = c(1985, 1990, 1995, 2000, 2005, 2010, 2015),
        limits = c(1985, 2016)) +
      theme_bw()
    
    plot_1b <- ggplotly(plot_1_b)
    return(plot_1b)
}

# TAB 1 - PLOT 1c
make_plot1c <- function(selected_countries_c = list("Canada", "United States")) {
  #' Makes a plot showing suicide rate over time by country
  #'
  #' @param selected_countries_c A list of one or more countries
  #' @return An interactive plotly graph of suicide rate by selected countries
  #' @examples
  #' make_plot1c(selected_countries_c = list("Canada"))
  #' make_plot1c(selected_countries_c = list("Canada", "United States"))
    countries_c <- c(selected_countries_c)

    country_df <- final_df %>%
      mutate(suicides_by_pop = suicides_no/(population/100000)) %>%
      filter(country %in% countries_c) %>%
      group_by(country, year) %>%
      summarize(suicide_rate = mean(suicides_by_pop)) %>%
      modify_if( ~is.numeric(.), ~round(., 3))

    options(repr.plot.width = 7, repr.plot.height = 5)
    country_plot_gg <- ggplot() + 
      geom_line(data = general_data, aes(x=year, y=suicides_per_100k_pop,color = Label),linetype = "dashed") +
      geom_line(data = country_df, aes(x = year, y = suicide_rate, color = country)) +
      labs(x = "Year", 
          y = "Suicides per 100k pop", 
          title = "Suicide Rate per Country",
          color = "Country") +
        scale_x_continuous(breaks = c(1985, 1990, 1995, 2000, 2005, 2010, 2015), 
          labels = c(1985, 1990, 1995, 2000, 2005, 2010, 2015),
          limits = c(1985, 2016))

    country_plot <- ggplotly(country_plot_gg)
    return(country_plot)
}

# TAB 1 - PLOT 1d
make_plot1d <- function(selected_countries_d = list("Canada", "United States")) {
  #' Makes a plot showing suicide rate over time by demographic and country
  #'
  #' @param selected_countries_d A list of one or more countries
  #' @return An interactive plotly graph of suicide rate by sex for selected countries
  #' @examples
  #' make_plot1d(selected_countries_d = list("Canada"))
  #' make_plot1d(selected_countries_d = list("Canada", "United States"))
    countries_d <- c(selected_countries_d)

    demographic_df <- final_df %>%
      mutate(suicides_by_pop = suicides_no/(population/100000)) %>%
      filter(country %in% countries_d) %>%
      group_by(sex, year) %>%
      summarize(suicide_rate = mean(suicides_by_pop, na.rm = TRUE)) %>%
      modify_if( ~is.numeric(.), ~round(., 3))

    options(repr.plot.width = 7, repr.plot.height = 5)
    demographic_plot <- ggplot() + 
      geom_line(data =general_data, aes(x=year, y=suicides_per_100k_pop,color = Label),linetype = "dashed") +
      geom_line(data = demographic_df, aes(x = year, y = suicide_rate, color = sex)) + 
      labs(x = "Year", 
          y = "Suicides per 100 k pop", 
          title = "Average Suicide Rate by Sex in Selected Country (Countries)",
          color = "Sex") +
      scale_x_continuous(breaks = c(1985, 1990, 1995, 2000, 2005, 2010, 2015), 
        labels = c(1985, 1990, 1995, 2000, 2005, 2010, 2015),
        limits = c(1985, 2016))

    dem_plot <- ggplotly(demographic_plot)
    return(dem_plot)
}

# TAB 2 - PLOT 2a 
make_plot2a <- function(country_a = 'Canada', country_b = 'United States', year_list = list(1, 1)) {
  #' Makes a plot showing suicide rate for 2 countries in a specified year range
  #'
  #' @param country_a A string of one country (default: "Canada")
  #' @param country_b A string of one country (default: "United States")
  #' @param year_list A list of year ranges (default: list(1986, 2014))
  #' @return An interactive plotly graph of suicide rate for selected countries and year range
  #' @examples
  #' make_plot2a(country_a = "Brazil", country_b = "Japan", year_list = list(1987, 1996))
  #' make_plot2a(country_a = "Ukraine", country_b = "Thailand", year_list = list(1993, 2010))
    
    # Sets default values
    year_start <- year_list[[1]]
    year_end <- year_list[[2]]
    
    data_2a <- final_df %>%
      filter(suicides_per_100k_pop > 0.1) %>%
      filter(year >= year_start & year <= year_end) %>%
      filter(country == country_a | country == country_b) %>%
      group_by(country) %>%
      summarize(mean_suicides = mean(suicides_per_100k_pop, na.rm = TRUE)) %>%
      modify_if( ~is.numeric(.), ~round(., 3))

    chart_2a <- ggplot(data_2a) +
        theme_bw() +
        geom_bar(aes(x = fct_rev(country), y = mean_suicides, fill = country), stat = 'identity') +
        labs(x = 'Country', y = 'Average Suicide Rate (per 100k pop)', title = 'Suicide Rate by Country') +
        coord_flip() +
        theme(legend.position = "none") 
    chart_2a <- ggplotly(chart_2a, tooltip = c("country", "mean_suicides")) %>% config(displayModeBar = FALSE)
    return(chart_2a)
}

# TAB 2 - PLOT 2b
make_plot2b <- function(country_a = 'Canada', country_b = 'United States', year_list = list(1,1), demo_selection = list('female : 25-34 years')) {
  #' Makes plot(s) showing suicide rate by demographic group(s) for specified countries and year range
  #'
  #' @param country_a A string of one country (default: "Canada")
  #' @param country_b A string of one country (default: "United States")
  #' @param year_list A list of year ranges (default: list(1986, 2014))
  #' @param demo_selection A list of one or more demographic groups (default: list('female: 25-34 years'))
  #' @return Interactive plotly graphs of suicide rates for selected countries and year ranges by demographic group
  #' @examples
  #' make_plot2b(country_a = "Brazil", country_b = "Japan", year_list = list(1987, 1996), demo_selection = list('female: 55-74 years', 'male: 55-74 years'))
  #' make_plot2b(country_a = "Ukraine", country_b = "Thailand", year_list = list(1993, 2010), demo_selection = list('female: 35-54 years', 'female: 15-24 years', 'male: 25-34 years'))
    
    # Sets default values
    year_start <- year_list[[1]]
    year_end <- year_list[[2]]
    demos <- c(demo_selection)

    data_2b <- final_df %>%
      filter(suicides_per_100k_pop > 0.1) %>%
      filter(year >= year_start & year <= year_end) %>%
      filter(country == country_a | country == country_b) %>%
      filter(demo_group %in% demos) %>%
      group_by(country, demo_group) %>%
      summarize(mean_suicides = mean(suicides_per_100k_pop, na.rm = TRUE)) %>% 
      modify_if( ~is.numeric(.), ~round(., 3))
    
    chart_2b <- ggplot(data_2b) +
      theme_bw() +
      geom_bar(aes(x = fct_rev(demo_group), y = mean_suicides, fill = country), stat = 'identity', position = position_dodge(width = 0.9)) + 
      labs(x = 'Demographic Groups', y = 'Average Suicide Rate (per 100k pop)', title = 'Suicide Rate by Demographic Group') +
      guides(color = guide_legend(title = "Country"))

    chart_2b <- ggplotly(chart_2b, tooltip = c("country", "mean_suicides")) %>% 
      config(displayModeBar = FALSE)
    return(chart_2b)
}

#### DCC GRAPHS
graph_wm <- dccGraph(
  id = 'graph_wm',
  figure = make_world_plot()
)

graph_1a <- dccGraph(
  id = 'graph_1a',
  figure = make_plot1a()
)

graph_1b <- dccGraph(
  id = 'graph_1b',
  figure = make_plot1b()
)

graph_1c <- dccGraph(
  id = 'graph_1c',
  figure = make_plot1c() 
)

graph_1d <- dccGraph(
  id = 'graph_1d',
  figure = make_plot1d()
)

graph_2a <- dccGraph(
  id = 'graph_2a',
  figure = make_plot2a()
)

graph_2b <- dccGraph(
  id = 'graph_2b',
  figure = make_plot2b()
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
          htmlH2('Worldwide Overview'),

          htmlP('The map below shows average suicide rates by country. 
                Try selecting a range of years and hover over the map to see what countries have interesting data.', 
                id = 'text0'),
          htmlIframe(height=15, width=5, style=list(borderWidth = 0)), #space
          yearSlider_wm,
          htmlIframe(height=30, width=5, style=list(borderWidth = 0)), #space
          graph_wm

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
        label = 'Tab 2: Two Country Comparison',
        value = 'two country comparison',
        children = htmlDiv(className = 'control-tab', children = list(
          htmlH2('Two Country Comparison'),

          # PLOT 2a
          dccMarkdown('**Step 1:** Select 2 countries that you would like to compare (1 country per dropdown)'),
          countryDD2a,
          countryDD2b,
          htmlIframe(height=15, width=5, style=list(borderWidth = 0)), #space
          dccMarkdown('**Step 2:** Select a range of years to see the average suicide rate for this time frame.'),
          yearSlider2,
          htmlIframe(height=20, width=5, style=list(borderWidth = 0)), #space
          graph_2a,
          htmlIframe(height=20, width=5, style=list(borderWidth = 0)), #space

          # PLOT 2b
          dccMarkdown('**Step 3:** Select one or more groups to see average suicide rates by demographics in each country.  
                      Note: If only one bar appears by default, there is no demographic data available for the other selected country.'),
          demoCC,
          htmlIframe(height=15, width=5, style=list(borderWidth = 0)), #space
          graph_2b
        ))
      )
    ))
  ))
)))

#### CALLBACKS

# CALLBACK FOR WORLDWIDE OVERVIEW PLOT
app$callback(
  output=list(id = 'graph_wm', property='figure'),
  params=list(input(id = 'year_slider_wm', property='value')),
  function(year_list) {
    make_world_plot(year_list)
  })

# CALLBACK FOR PLOT 1b
app$callback(
  output=list(id = 'graph_1b', property='figure'),
  params=list(input(id = 'region_dd', property='value')),
  function(region_value) {
    make_plot1b(region_value)
  })

# CALLBACK FOR PLOT 1c
app$callback(
  output=list(id = 'graph_1c', property='figure'),
  params=list(input(id = 'country_dd_1', property='value')),
  function(country_value) {
    make_plot1c(country_value)
  })

# CALLBACK FOR PLOT 1d
app$callback(
  output=list(id = 'graph_1d', property='figure'),
  params=list(input(id = 'country_dd_1', property='value')),
  function(country_value) {
    make_plot1d(country_value)
  })

# CALLBACK FOR PLOT 2a
app$callback(
  output=list(id = 'graph_2a', property='figure'),
  params=list(input(id = 'country_dd_2a', property='value'),
              input(id = 'country_dd_2b', property='value'),
              input(id = 'year_slider', property='value')),
  function(country_value1, country_value2, year_list) {
    make_plot2a(country_value1, country_value2, year_list)
  })

# CALLBACK FOR PLOT 2b
app$callback(
  output=list(id = 'graph_2b', property='figure'),
  params=list(input(id = 'country_dd_2a', property='value'),
              input(id = 'country_dd_2b', property='value'),
              input(id = 'year_slider', property='value'),
              input(id = 'demo_cc', property='value')),
  function(country_value1, country_value2, year_list, demo_list) {
    make_plot2b(country_value1, country_value2, year_list, demo_list)
  })

app$run_server(host = "0.0.0.0", port = Sys.getenv('PORT', 8050))
