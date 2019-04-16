library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)
library(graphlayouts)
library(gganimate)
library(cowplot)

#leer datos 

datos <- read_delim(file = "20190410/partidos.txt", delim = "\t")
datos_save <- datos
#modificar 
datos <- datos %>% dplyr::select(equipo_1, equipo_2, dplyr::everything())

datos <- datos %>% mutate(diferencia = equipo_1_final - equipo_2_final) 
datos <- datos %>% mutate(ganador  = ifelse(diferencia > 0, equipo_1, equipo_2),
                          perdedor = ifelse(diferencia > 0, equipo_2, equipo_1),
                          empate   = ifelse(diferencia == 0, TRUE, FALSE))

datos <- datos %>% dplyr::select(ganador, perdedor, empate, anio, anfitrion, dplyr::everything())
datos <- datos %>% mutate(kolor = ifelse(empate == TRUE, "grey", "black"))
datos <- datos %>% mutate(anio = as.factor(anio))

#hacer plots por anio
my_levels <- levels(datos$anio)
names(my_levels) <- my_levels

my_graphs <-
  lapply(my_levels, FUN = function(i){
    dato_anual <- datos %>% filter(anio ==i)
    my_g <- graph_from_data_frame(d = dato_anual)
    my_g <- as_tbl_graph(my_g)
    
    p.dopest <- ggraph(my_g, layout = "centrality", cent = graph.strength(my_g)) +
      geom_node_point() + 
      geom_edge_link(aes(edge_colour = kolor, 
                         alpha = kolor),
                     arrow = arrow(length = unit(4, 'mm')),
                     end_cap = circle(3, 'mm'), 
                     show.legend = FALSE)+
      scale_edge_color_manual(values = c("lavenderblush4", "goldenrod1")) +
      scale_edge_alpha_manual(values = c(0.5, 0.25)) +
      geom_node_text(aes(label=name), repel=T)+
      ggtitle(i) +
      theme_graph() 
    
    p.dopest
  })

lapply(seq_along(my_graphs), FUN = function(i){
  ggsave(filename = paste0("20190410/plots/", my_levels[i], ".png"), 
         plot = my_graphs[[i]])
})

#convert convert -delay 100 -loop 0 *.png video.gif