#===============================================================================
# 2019-12-09 -- digital dem
# shiny app-- Popularity of the US names  
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#===============================================================================

# Data: https://www.ssa.gov/oact/babynames/limits.html
# Post: https://fivethirtyeight.com/features/how-to-tell-someones-age-when-all-you-know-is-her-name/
# Gist: https://gist.github.com/ikashnitsky/654965cb971f3a11928806c4d0a0ef23
# Tweet: https://twitter.com/ikashnitsky/status/1201770729282424832

library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(magrittr)
library(here)
library(hrbrthemes)
library(ggdark)


# data --------------------------------------------------------------------


load(here("data", "names-surv.rda"))
load(here("data", "median.rda"))

names <- df %>%
    pull(name) %>%
    unique() %>%
    sort()


# plotting function -------------------------------------------------------

plot_name_surivors <- function(choose_name = "Anna", choose_sex = "f") {
    
    choose_sex <- tolower(choose_sex)
    
    # filter out median age  for the chosen name
    median_age <- df_med %>% 
        filter(name %>% is_in(choose_name), sex %>% is_in(choose_sex)) %>% 
        pull(amed)
    
    # helper to align label
    max_y <- df %>% 
        filter(name %>% is_in(choose_name), sex %>% is_in(choose_sex)) %>%
        pull(value) %>% 
        max(na.rm = TRUE)
    
    # plot
    df %>% 
        filter(name %>% is_in(choose_name), sex %>% is_in(choose_sex)) %>% 
        ggplot(aes(year))+
        geom_col(aes(y = n_surv, fill = lx %>% multiply_by(100)), 
                 color = NA, width = .75,  position = position_nudge(.5))+
        geom_step(aes(y = value), color = "cyan")+
        geom_vline(xintercept = 2019 %>% subtract(median_age))+
        annotate(
            "text", 
            x = 2019 %>% subtract(median_age), 
            y = max_y,
            label = paste(
                "Median age of living\npersons named\n", 
                choose_name, ":", median_age %>% round(1)
            ),
            vjust = 1, hjust = 1.1, lineheight = .9, fontface = 2,
            family = font_rc
        )+
        scale_fill_viridis_c(
            option = "B",
            guide = guide_colorbar(
                barwidth = 15, barheight = .5
            ),
            limits = c(0, 100)
        )+
        coord_cartesian(xlim = c(1880, 2014))+
        dark_theme_minimal(base_family = font_rc)+
        theme(legend.position = "bottom",
              plot.title = element_text(family = "mono", size = 20),
              axis.title.y = element_text(color = "cyan"))+
        labs(title = "How old is a person named",
             x = NULL,
             fill = "Proportion\nliving, %",
             caption = "@ikashnitsky",
             y = paste0("Number of persons born each year") )+
        annotate(
            "text", x = 1881, y = max_y %>% multiply_by(.8), 
            label = choose_name, family = "mono", fontface = 2,
            size = 16, color = "white", alpha = .2, 
            hjust = 0, vjust = 1
        )
}


# ui and server -----------------------------------------------------------

# Define UI for application that draws a plot
ui <- fluidPage(theme = shinytheme("slate"),

    # Application title
    titlePanel(NULL),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(width = 3,
            # choose the name
            selectizeInput(
                "choose_name",
                label     = "Choose a name",
                choices   = names, 
                multiple = FALSE,
                selected  = "Anna",
                options = list(create = TRUE, maxItems = 2)
            )
            ,
            # choose sex
            radioButtons(inputId = "choose_sex",
                         label   = "Choose sex",
                         choices = list(Female = "f",
                                        Male   = "m"),
                         selected = "f")
        ),
        # Show plot 
        mainPanel(
           plotOutput("plot", height = "600px")
        )
    )
)

# server to draw the plot
server <- function(input, output) {

    output$plot <- renderPlot({
        plot_name_surivors(
            choose_name = input$choose_name, 
            choose_sex = input$choose_sex
        )
    }, width = 800, height = 600, res = 144)
}

# Run the application 
shinyApp(ui = ui, server = server)
