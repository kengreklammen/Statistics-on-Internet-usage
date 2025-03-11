# R application that displays some basic statistics on Internet usage in Germany between 2000 and 2023

library(tidyverse)
library(shiny)
library(ggplot2)
library(bslib)
#library(shinythemes)
library(rio)
library(dplyr)
library(readr)

ds <- rio::import("data/internet_usage.csv")

View(ds)

df <- t(ds) %>% as.data.frame(df)

colnames(df) <- df[1, ]
df <- df[-1, ]
str(df)

df <- df %>%
  mutate_at(c('female','male'),funs(str_replace(., "..", "0")))
str(df)

df$year = parse_number(df$year, )

df$female = parse_number(df$female, )

df$male = parse_number(df$male, )

# df %>%
#   mutate(year = as.numeric(year))
# df %>%
#   mutate(female = as.numeric(female))
# df %>%
#   mutate(male = as.numeric(male))

View(df)
str(df)

# **************************** User Interface ****************************

ui <-
  page_fluid(
    theme = bs_theme(bootswatch = "minty"),
    layout_sidebar(
      sidebar = sidebar(
        sliderInput("year_interval", "Years:", min = 2000, max = 2023, sep = "", value = c(2000, 2023))
      ),
      layout_columns(
        col_widths = c(12),
        card(
          helpText(
            h2("Average internet usage in Germany between 2000 and 2023")
          )  
        )
      ),
      layout_columns(
        col_widths = c(6, 6),
        value_box("By women", textOutput("mean_usage_women")),
        value_box("By men", textOutput("mean_usage_men")),
      ),
      layout_columns(
        col_widths = c(12),
        card(
          card_header("Internet usage in Germany by women and men"),
          full_screen = T,
          card_body(plotOutput("lineplot"))
        )
      )
    )
  )


# *************************** Server logic *****************************

server <- function(input, output) {
  theme_choice <- reactive(theme_light())
  
  output$mean_usage_women <- renderText({
    paste(round(mean(df$female), 2), "percent")
  })

  output$mean_usage_men <- renderText({
    paste(round(median(df$male), 2), "percent")
  })

  output$lineplot <- renderPlot({
    df %>% filter(between(year, input$year_interval[1], input$year_interval[2])) %>%
      pivot_longer(cols = -year) %>%
    ggplot(aes(x = year, y = value, group = name, color = name)) + 
      geom_line(size=2, alpha=0.9, linetype=1) + labs(title = "Internet usage in Germany based on gender", x = "Year", y = "Percentage") +
      theme_choice()
  })
}



# ************************* Run the application *************************

 shinyApp(ui, server, options = list(display.mode = "showcase"))
 
 
 
 
 
 # df %>%
 #  pivot_longer(cols = -year) %>%
 #  ggplot(aes(x = year, y = value, group = name, color = name)) + geom_line() + labs(title = "Density Plot of Internet usage", x = "Year", y = "Percentage") +
 #   theme_choice()