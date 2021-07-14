library(ggplot2)
library(dplyr)
library(binaryLogic)

# Original code from Georgios Karamanis:
# https://github.com/gkaramanis/aRtist/blob/main/genuary/2021/2021-13/2021-13.R

primest <- function(n){
  p <- 2:n
  i <- 1
  while (p[i] <= sqrt(n)) {
    p <-  p[p %% p[i] != 0 | p==p[i]]
    i <- i+1
  }
  p
}

nm = 30000

prime_df <- data.frame(x = primest(nm)) %>% 
  rowwise() %>% 
  mutate(
    bin = paste0(as.binary(x), collapse = ""),
    bin_rev = intToUtf8(rev(utf8ToInt(bin))),
    y = x - strtoi(bin_rev, base = 2)
  ) %>% 
  ungroup() %>% 
  mutate(n = row_number())

ggplot(prime_df) +
  geom_tile(aes(x, y, fill = n, height = n/3, width = n/3), color = "#222222", alpha = 0.9) +
  scale_fill_gradientn(colours = c("#e69d25","#e23e64","#886593","#6f4683")) +
  coord_flip() +
  scale_x_reverse() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#111111", color = NA),
    legend.position = "none"
  )

ggsave("tile_aRt.png", width = 6.5, height = 7, device = "png", dpi = 150)

animated_plot <- ggplot(prime_df) +
  geom_tile(aes(group = seq_along(n), x, y, fill = n, height = n/3, width = n/3), color = "#222222", alpha = 0.9) +
  scale_fill_gradientn(colours = c("#e69d25","#e23e64","#886593","#6f4683")) +
  coord_flip() +
  scale_x_reverse() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#111111", color = NA),
    legend.position = "none"
  )

animated_plot <- animated_plot +
  transition_reveal(n) +
  ease_aes('linear')

anim_save("tile_aRt.gif", animated_plot)
 