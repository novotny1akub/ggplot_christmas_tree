if(rstudioapi::isAvailable()){
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

library(tidyverse)

generate_christmas_tree <- function(.seed){
  set.seed(.seed)
  
  df_strom <- bind_rows(
    tibble(x = 0, y = rep(1, 2), width = 1.5, col = "chocolate4", id = "kmen"),
    tibble(x = 0, y = rep(1, 10), width = 10:1, col = "darkgreen", id = "koruna")
  ) %>%
    mutate(y = cumsum(y))
  
  df_retezy <- df_strom %>%
    filter(id == "koruna") %>%
    rowwise() %>%
    mutate(id = "retezy",
           col = sample(paste0("cornsilk", 1:4), 1), # gold
           y = y - 0.5,
           x = seq(from = -width/2, to = width/2, by = 1) %>% list(),
           y = (y + cumsum(rep(1/length(x), length(x)))) %>% `-`(1/length(x)) %>% list(),
           width = NULL) %>% ungroup() %>%
    mutate(grp = row_number()) %>%
    unnest
  
  df_koule <- df_strom %>%
    filter(id == "koruna") %>%
    select(y, width) %>%
    rowwise() %>%
    mutate(x_min = -width/2 + 0.5,
           x_max = -x_min,
           pocet_kouli = length(x_min:x_max) %>% `/`(2) %>% floor(),
           x = seq(from = x_min + runif(1, min = 0, max = 1.5), to = x_max - runif(1, min = 0, max = 1.5), length.out = pocet_kouli) %>% list(),
           col = sample(c("firebrick2", "gold", "dodgerblue3"), replace = T, pocet_kouli) %>% list(),
           size = sample(c(8, 5:3), prob = c(0.4, 0.4, 0.1, 0.1),replace = T, size = pocet_kouli) %>% list()
    ) %>%
    select(x, y, col, size) %>%
    unnest %>%
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
      labs(x = "", y = "")
  
  p
  
}

(p <- generate_christmas_tree(3))

p_plotly <- plotly::ggplotly(p)

htmlwidgets::saveWidget(p_plotly, "happy_christmas.html", selfcontained = T)

read_file("happy_christmas.html") %>%
  str_replace_all(pattern = 'width = "100%"', replacement = 'width = "30%"') %>%
  write_file("happy_christmas.html")

browseURL("happy_christmas.html")