# Plot a TARDIS

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
    ymax = 9.7,
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

windows2_plot <- windows_plot +
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

details_plot <- windows2_plot +
  geom_text(
    data = data.frame(
      x = c(1, 4.2), y = c(8.85, 8.85), label = c("POLICE", "BOX")
    ),
    mapping = aes(x = x, y = y, label = label),
    colour = "white",
    size = 12,
    fontface = "bold"
  ) +
  geom_text(
    data = data.frame(
      x = 2.7, y = 8.85, label = c("PUBLIC\nCALL")
    ),
    mapping = aes(x = x, y = y, label = label),
    colour = "white",
    size = 6,
    lineheight = 0.3,
    fontface = "bold"
  ) +
  annotate(
    geom = "point",
    colour = "grey80",
    x = 2.7,
    y = 4
  ) +
  annotate(
    geom = "point",
    colour = "grey80",
    x = 3.8,
    y = 5.1,
    size = 6
  ) +
  annotate(
    geom = "point",
    colour = "grey20",
    x = 3.8,
    y = 5.1,
    size = 4,
    pch = 21
  ) +
  annotate(
    geom = "segment",
    colour = "grey30",
    x = 2.7,
    xend = 2.7,
    y = 5.3,
    yend = 4.7,
    linewidth = 1.0
  )

tardis <- details_plot +
  annotate(
    geom = "rect",
    xmin = 2.2,
    ymin = 10,
    xmax = 2.8,
    ymax = 10.8,
    fill = "grey90",
    colour = "grey60"
  ) +
  annotate(
    geom = "rect",
    xmin = 2.2,
    ymin = 10,
    xmax = 2.8,
    ymax = 10.2,
    fill = "grey10",
    colour = "grey10"
  ) +
  annotate(
    geom = "rect",
    xmin = 2.18,
    ymin = 10.7,
    xmax = 2.82,
    ymax = 10.85,
    fill = "grey10",
    colour = "grey10"
  ) +
  annotate(
    geom = "rect",
    xmin = 0.5,
    ymin = 4.3,
    xmax = 1.9,
    ymax = 5.9,
    fill = "grey80",
    colour = "grey80"
  ) +
  annotate(
    geom = "text",
    x = 1.2,
    y = 5.1,
    label = "POLICE TELEPHONE\n\nFREE\nFOR USE OF\nPUBLIC\nADVICE & ASSISTANCE\nOBTAINABLE IMMEDIATELY\nOFFICER & CARS\nRESPOND TO ALL CALLS\n\nPULL TO OPEN",
    lineheight = 0.3,
    size = 2
  ) +
  theme_void() 
  