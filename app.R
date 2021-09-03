
library(shiny)
library(shinythemes)
library(tidyverse)

generate_christmas_tree <- function(.seed, .pocet_pater_strom, .velikost_kouli, .barva_kouli, .barvy_retezu){
    set.seed(.seed)
    
    df_strom <- bind_rows(
        tibble(x = 0, y = rep(1, min(ceiling(.pocet_pater_strom/7), 3) ), width = 1.5, col = "chocolate4", id = "kmen"),
        tibble(x = 0, y = rep(1, .pocet_pater_strom), width = .pocet_pater_strom:1, col = "darkgreen", id = "koruna")
    ) %>%
        mutate(y = cumsum(y))
    
    df_retezy <- df_strom %>%
        filter(id == "koruna") %>%
        rowwise() %>%
        mutate(id = "retezy",
               col = sample(.barvy_retezu, 1), # gold
               y = y - 0.5,
               x = seq(from = -width/2, to = width/2, by = 1) %>% list(),
               y = (y + cumsum(rep(1/length(x), length(x)))) %>% `-`(1/length(x)) %>% list(),
               width = NULL) %>% ungroup() %>%
        mutate(grp = row_number()) %>%
        unnest(cols = c(x, y))
    
    df_koule <- df_strom %>%
        filter(id == "koruna") %>%
        select(y, width) %>%
        rowwise() %>%
        mutate(x_min = -width/2 + 0.5,
               x_max = -x_min,
               pocet_kouli = length(x_min:x_max) %>% `/`(2) %>% floor(),
               x = seq(from = x_min + runif(1, min = 0, max = 1.5), to = x_max - runif(1, min = 0, max = 1.5), length.out = pocet_kouli) %>% list(),
               col = sample(.barva_kouli, replace = T, pocet_kouli) %>% list(),
               size = sample(c(8, .velikost_kouli*5:3), prob = c(0.4, 0.4, 0.1, 0.1),replace = T, size = pocet_kouli) %>% list()
        ) %>%
        select(x, y, col, size) %>%
        unnest(cols = c(x, col, size)) %>%
        rowwise() %>%
        mutate(y = y + runif(n = 1, -0.5, 0.5))
    
    
    p <- ggplot() +
        geom_tile(data = df_strom, aes(x, y, width = width, fill = col)) +
        geom_path(data = df_retezy, aes(x, y, color = col, group = grp)) +
        geom_point(data = df_koule, position = position_jitter(), aes(x, y, color = col, size = size)) +
        scale_fill_identity() + scale_color_identity() + scale_size_identity() +
        theme_bw() +
        scale_x_continuous(breaks = NULL) + 
        scale_y_continuous(breaks = NULL) +
        labs(x = "", y = "") +
        geom_tile(aes(x = -3, y = 0.5, width = 3)) + 
        geom_tile(aes(x = -3, y = 1.5, width = 3)) + 
        geom_segment(aes(x = -4.5, xend = -1.5, y = 1, yend = 1), colour = "blueviolet", size = 3) +
        geom_segment(aes(x = -3, xend = -3, y = 0, yend = 2), colour = "blueviolet", size = 2) +
        
        geom_tile(aes(x = 3, y = 0.5 + 0.2, width = 3)) + 
        geom_tile(aes(x = 3, y = 1.5 + 0.2, width = 3)) + 
        geom_segment(aes(x = 1.5, xend = 4.5, y = 1 + 0.2, yend = 1 + 0.2), colour = "blueviolet", size = 3) +
        geom_segment(aes(x = 3, xend = 3, y = 0 + 0.2, yend = 2 + 0.2), colour = "blueviolet", size = 2)
    
    
    tibble::lst(df_strom, df_retezy, df_koule, p)
    
}


ui <- fluidPage(
    theme = shinytheme("cyborg"),
    # shinythemes::themeSelector(),
    titlePanel("Merry Christmas"),
    sidebarLayout(
        sidebarPanel(
            sliderInput("seed", "Random number:", 3, min = 0, max = 100, step = 1),
            sliderInput("pocet_pater", "Pocet pater:", 12, min = 5, max = 15, step = 1),
            sliderInput("velikost_kouli", "Velikost banek:", 3, min = 1, max = 11, step = 0.1),
            selectInput("barvy_kouli", "Barvy banek:", choices = colors(), multiple = T, selected = c("firebrick2", "gold", "dodgerblue3")),
            selectInput("barvy_retezu", "Barvy retezu:", choices = colors(), multiple = T, selected = c("steelblue", "red3", "yellow")),
            width = 3
        ),
        mainPanel(
            plotOutput("plot", width = "400px", height = "800px")
        )
    )
)

server <- function(input, output) {
    

    output$plot <- renderPlot({
        req(input$seed, input$pocet_pater, input$velikost_kouli, input$barvy_kouli, input$barvy_retezu)
        generate_christmas_tree(input$seed, input$pocet_pater, input$velikost_kouli, input$barvy_kouli, input$barvy_retezu)$p
        # generate_christmas_tree(3, 12, 2, c("firebrick2", "gold", "dodgerblue3"), paste0("cornsilk", 1:4))$p
    })
}

shinyApp(ui = ui, server = server)

