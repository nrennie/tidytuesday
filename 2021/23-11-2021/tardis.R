

base_plot <- ggplot() +
  annotate(
    geom = "rect",
    xmin = 0,
    ymin = -1,
    xmax = 5,
    ymax = 10,
    fill = highlight_col
  ) +
  annotate(
    geom = "rect",
    xmin = -0.2,
    ymin = -1,
    xmax = 5.2,
    ymax = 8.7,
    fill = highlight_col
  ) +
  annotate(
    geom = "rect",
    xmin = -0.5,
    ymin = -1,
    xmax = 5.5,
    ymax = 9.3,
    fill = highlight_col
  ) +
  annotate(
    geom = "rect",
    xmin = -0.6,
    ymin = 8.5,
    xmax = 5.6,
    ymax = 9.2,
    fill = highlight_col
  ) +
  annotate(
    geom = "rect",
    xmin = -0.6,
    ymin = -1,
    xmax = 5.6,
    ymax = 0,
    fill = highlight_col
  ) +
  annotate(
    geom = "rect",
    xmin = 0,
    ymin = -0.5,
    xmax = 5,
    ymax = 8.5,
    fill = highlight_col,
    colour = alpha("white", 0.3)
  ) +
  coord_fixed()
base_plot

windows_plot <- base_plot  +
  # bottom row
  annotate(
    geom = "rect",
    xmin = 0.2,
    ymin = -0.3,
    xmax = 2.2,
    ymax = 1.7,
    fill = alpha("white", 0.05),
    colour = alpha("white", 0.3)
  ) +
  annotate(
    geom = "rect",
    xmin = 2.8,
    ymin = -0.3,
    xmax = 4.8,
    ymax = 1.7,
    fill = alpha("white", 0.05),
    colour = alpha("white", 0.3)
  ) +
  # second bottom row
  annotate(
    geom = "rect",
    xmin = 0.2,
    ymin = 1.9,
    xmax = 2.2,
    ymax = 3.9,
    fill = alpha("white", 0.05),
    colour = alpha("white", 0.3)
  ) +
  annotate(
    geom = "rect",
    xmin = 2.8,
    ymin = 1.9,
    xmax = 4.8,
    ymax = 3.9,
    fill = alpha("white", 0.05),
    colour = alpha("white", 0.3)
  ) +
  # second row
  annotate(
    geom = "rect",
    xmin = 0.2,
    ymin = 4.1,
    xmax = 2.2,
    ymax = 6.1,
    fill = alpha("white", 0.05),
    colour = alpha("white", 0.3)
  ) +
  annotate(
    geom = "rect",
    xmin = 2.8,
    ymin = 4.1,
    xmax = 4.8,
    ymax = 6.1,
    fill = alpha("white", 0.05),
    colour = alpha("white", 0.3)
  ) +
  # top row
  annotate(
    geom = "rect",
    xmin = 0.2,
    ymin = 6.3,
    xmax = 2.2,
    ymax = 8.3,
    fill = alpha("white", 0.9),
    colour = alpha("white", 0.95)
  ) +
  annotate(
    geom = "rect",
    xmin = 2.8,
    ymin = 6.3,
    xmax = 4.8,
    ymax = 8.3,
    fill = alpha("white", 0.9),
    colour = alpha("white", 0.95)
  ) 

windows_plot +
  # lines on windows
  geom_segment(
    data = data.frame(
      x = c(0.2, 2.8, 0.86, 1.53, 3.46, 4.13), xend = c(2.2, 4.8, 0.86, 1.53, 3.46, 4.13),
      y = c(7.3, 7.3, 6.3, 6.3, 6.3, 6.3), yend = c(7.3, 7.3, 8.3, 8.3, 8.3, 8.3)
    ),
    mapping = aes(x = x, xend = xend, y = y, yend = yend),
    colour = highlight_col
  ) +
  annotate(
    geom = "rect",
    xmin = -0.3,
    ymin = 8.6,
    xmax = 5.3,
    ymax = 9.15,
    fill = "black",
    colour = "black"
  ) 

# fix alignment at top
# add key hole
# add police box text
# add circle thing
# add square with text
# add thing at top

