christmas_cracker <- function(x = 0,
                              y = 0,
                              col = "red",
                              ribbon_col = "gold",
                              outline_col = "black",
                              ribbon_outline_col = "black") {
  # cracker body
  mid_pts <- sf::st_polygon(list(matrix(
    c(
      x, y + 0.3,
      x + 0.15, y + 0.4,
      x + 0.15, y + 0.6,
      x, y + 0.7,
      x - 0.15, y + 0.6,
      x - 0.15, y + 0.4,
      x, y + 0.3
    ),
    ncol = 2,
    byrow = TRUE
  )))
  top_pts <- sf::st_polygon(list(matrix(
    c(
      x, y + 0.7 - 0.05,
      x + 0.15, y + 0.8,
      x + 0.15, y + 1.0,
      x + 0.1, y +  0.95,
      x + 0.05, y + 1.0,
      x + 0, y + 0.95,
      x - 0.05, y + 1.0,
      x - 0.1, y + 0.95,
      x - 0.15, y + 1.0,
      x - 0.15, y + 0.8,
      x, y + 0.7 - 0.05
    ),
    ncol = 2,
    byrow = TRUE
  )))
  bottom_pts <- sf::st_polygon(list(matrix(
    c(
      x, y + 0.3 + 0.05,
      x + 0.15, y + 0.2,
      x + 0.15, y + 0.0,
      x + 0.1, y + 0.05,
      x + 0.05, y + 0.0,
      x + 0, y + 0.05,
      x - 0.05, y + 0.0,
      x - 0.1, y + 0.05,
      x - 0.15, y + 0.0,
      x - 0.15, y + 0.2,
      x, y + 0.3 + 0.05
    ),
    ncol = 2,
    byrow = TRUE
  )))
  cracker_body <- sf::st_union(sf::st_union(mid_pts, top_pts), bottom_pts)
  cracker_body <- sf::st_sfc(cracker_body)
  cracker_body <- sf::st_cast(cracker_body, "POLYGON")
  cracker_body <- sf::st_sf(cracker_body)
  cracker_body$fill <- col
  cracker_body$colour <- outline_col
  cracker_body <- cracker_body |>
    dplyr::rename(poly = cracker_body)

  # cracker top ribbon
  top_ribbon <- sf::st_polygon(list(matrix(
    c(
      x, y + 0.7 - 0.01,
      x + 0.18, y + 0.73 - 0.025,
      x + 0.15, y + 0.7 - 0.025,
      x + 0.18, y + 0.67 - 0.025,
      x, y + 0.7 - 0.02,
      x - 0.18, y + 0.67 - 0.025,
      x - 0.15, y + 0.7 - 0.025,
      x - 0.18, y + 0.73 - 0.025,
      x, y + 0.7 - 0.01
    ),
    ncol = 2,
    byrow = TRUE
  )))
  top_ribbon <- sf::st_sfc(top_ribbon)
  top_ribbon <- sf::st_cast(top_ribbon, "POLYGON")
  top_ribbon <- sf::st_sf(top_ribbon)
  top_ribbon$fill <- ribbon_col
  top_ribbon$colour <- ribbon_outline_col
  top_ribbon <- top_ribbon |>
    dplyr::rename(poly = top_ribbon)

  # cracker bottom ribbon
  bottom_ribbon <- sf::st_polygon(list(matrix(
    c(
      x, y + 0.3 + 0.02,
      x + 0.18, y + 0.33 + 0.025,
      x + 0.15, y + 0.3 + 0.025,
      x + 0.18, y + 0.27 + 0.025,
      x, y + 0.3 + 0.01,
      x - 0.18, y + 0.27 + 0.025,
      x - 0.15, y + 0.3 + 0.025,
      x - 0.18, y + 0.33 + 0.025,
      x, y + 0.3 + 0.02
    ),
    ncol = 2,
    byrow = TRUE
  )))
  bottom_ribbon <- sf::st_sfc(bottom_ribbon)
  bottom_ribbon <- sf::st_cast(bottom_ribbon, "POLYGON")
  bottom_ribbon <- sf::st_sf(bottom_ribbon)
  bottom_ribbon$fill <- ribbon_col
  bottom_ribbon$colour <- ribbon_outline_col
  bottom_ribbon <- bottom_ribbon |>
    dplyr::rename(poly = bottom_ribbon)

  # join
  cracker <- rbind(rbind(cracker_body, top_ribbon), bottom_ribbon)

  # create geom
  g <- ggplot2::geom_sf(
    data = cracker,
    mapping = ggplot2::aes(fill = fill, colour = colour)
  )
  return(g)
}
