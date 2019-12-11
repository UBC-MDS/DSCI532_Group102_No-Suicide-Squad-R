#### IMPORT LIBRARIES
library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashTable)
library(tidyverse)
library(plotly)

app <- Dash$new(external_stylesheets = "https://codepen.io/chriddyp/pen/bWLwgP.css")

#### IMPORT DATA
final_df <- read_csv('data/final_df.csv')
plot_a_data <- read_csv('data/plot_a_data.csv')
general_data <- read_csv('data/general_data.csv')

#### SET UP LAYOUT
app$layout(htmlDiv(list(
  htmlDiv(className = 'app_body', children = list(
    dccTabs(id = 'control-tabs', value = 'tabs', children = list(
      dccTab(
        label = 'Tab One',
        value = 'tab one',
        children = htmlDiv(className = 'control-tab', children = list(
          htmlP('Written instructions go here.', id = 'text1')
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

# Storing the labels/values as a tibble means we can use this both 
# to create the dropdown and convert colnames -> labels when plotting
yaxisKey <- tibble(label = c("GDP Per Capita", "Life Expectancy", "Population"),
                   value = c("gdpPercap", "lifeExp", "pop"))

yaxisDropdown <- dccDropdown(
  id = "filter_dd",
  options = map(
    1:nrow(yaxisKey), function(i){
      list(label=yaxisKey$label[i], value=yaxisKey$value[i])
    }),
  value = "gdpPercap"
)


app$run_server()
