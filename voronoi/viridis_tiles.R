library(dplyr)
library(binaryLogic)
library(ggplot2)
library(ggforce)

# Hat tip to Georgios Karamanis for the function used to create the data set:
primest <- function(n){
  p <- 2:n
  i <- 1
  while (p[i] <= sqrt(n)) {
    p <-  p[p %% p[i] != 0 | p==p[i]]
    i <- i+1
  }
  p
}

n = 82

prime_df2 <- data.frame(x = primest(n)) %>% 
  rowwise() %>% 
  mutate(
    bin = paste0(as.binary(x), collapse = ""),
    bin_rev = intToUtf8(rev(utf8ToInt(bin))),
    y = x*1.5 - strtoi(bin_rev, base = 2)
  ) %>% 
  ungroup() %>% 
  mutate(n = row_number(),
         state = rep(c('a', 'b'), 11)) 

ggplot(prime_df2) +
  geom_voronoi_tile(aes(x, y, fill = n), expand = unit(0.5, 'mm')) +
  scale_fill_gradientn(colours = viridis::mako(8)) +
  coord_flip() +
  scale_x_reverse() +
  theme_void() +
  theme(
    plot.margin = unit(c(0,0,0,0), "cm"),
    plot.background = element_rect(fill = "#ffffff", color = NA),
    legend.position = "none"
  ) 

ggsave(filename = "generative art/mako_tiles.png",
       width = 8, height = 11,
       dpi=700,
       type = "cairo-png") 

ggplot(prime_df2) +
  geom_voronoi_tile(aes(x, y, fill = n), expand = unit(0.5, 'mm')) +
  scale_fill_gradientn(colours = viridis::magma(8)) +
  coord_flip() +
  scale_x_reverse() +
  theme_void() +
  theme(
    plot.margin = unit(c(0,0,0,0), "cm"),
    plot.background = element_rect(fill = "#ffffff", color = NA),
    legend.position = "none"
  ) 

ggsave(filename = "generative art/magma_tiles.png",
       width = 8, height = 11,
       dpi=700,
       type = "cairo-png") 
