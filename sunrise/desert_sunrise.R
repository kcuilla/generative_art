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

n = 1100

prime_df2 <- data.frame(x = primest(n)) %>%
  rowwise() %>%
  mutate(
    bin = paste0(as.binary(x), collapse = ""),
    bin_rev = intToUtf8(rev(utf8ToInt(bin))),
    y = x * 3 - strtoi(bin_rev, base = 2)
  ) %>%
  ungroup() %>%
  mutate(n = row_number())

ggplot(prime_df2) +
  geom_voronoi_tile(aes(x, y, fill = n), normalize = TRUE, asp.ratio = 2.8) +
  scale_fill_gradientn(colours = c("#e69d25","#e23e64","#886593","#6f4683")) +
  coord_flip() +
  scale_x_reverse() +
  theme_void() +
  theme(
    legend.position = "none",
    panel.border = element_blank(),
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent", color = NA), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    legend.background = element_rect(fill = "transparent"),
    legend.box.background = element_rect(fill = "transparent"),  
    legend.key = element_rect(fill = "transparent", colour = NA), 
    axis.line = element_line(colour = "black")
    ) 
  
ggsave(filename = "generative art/desert_sunrise.png",
       width = 8, height = 11,
       dpi=700,
       type = "cairo-png") 

ggplot(prime_df2) +
  geom_voronoi_tile(aes(x, y, fill = n), normalize = TRUE, asp.ratio = 2.8) +
  scale_fill_gradientn(colours = c("#8d9ea5", "#316d92", "#2a465c", "#031019")) +
  coord_flip() +
  scale_x_reverse() +
  theme_void() +
  theme(
    legend.position = "none",
    panel.border = element_blank(),
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent", color = NA), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    legend.background = element_rect(fill = "transparent"),
    legend.box.background = element_rect(fill = "transparent"),  
    legend.key = element_rect(fill = "transparent", colour = NA), 
    axis.line = element_line(colour = "black")
    ) 

ggsave(filename = "generative art/blue_ridge.png",
       width = 8, height = 11,
       dpi=700,
       type = "cairo-png") 
