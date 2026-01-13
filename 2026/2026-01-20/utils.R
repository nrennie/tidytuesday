#' Colour distance
#'
#' Distance between two colours
#'
#' @param col1 First hex code
#' @param col2 Second hex code
#' @return A numeric value
#' @noRd
colour_dist <- function(col1, col2) {
  col1_rgb <- grDevices::col2rgb(col1)
  col2_rgb <- grDevices::col2rgb(col2)

  r <- (col1_rgb[1, 1] - col2_rgb[1, 1])^2
  g <- (col1_rgb[2, 1] - col2_rgb[2, 1])^2
  b <- (col1_rgb[3, 1] - col2_rgb[3, 1])^2

  #  red 30%, green 59%, and blue 11%
  dist <- unname(0.3 * r + 0.59 * g + 0.11 * b)
  return(dist)
}

#' To Treemap
#'
#' Create a treemap of the colours used in an image
#'
#' @param img_path File path of image to read.
#' @param n Number of colours to use. Default 5.
#' @param rescale Rescaling factor. Default 400.
#' @param plot Whether or not to plot the treemap (TRUE) or return hex colour count (FALSE).
#' @return A ggplot2 object.
#' @examples
#' fpath <- system.file("images", "tree.jpg", package = "pixR")
#' to_treemap(fpath)
#' @export

to_treemap <- function(img_path,
                       n = 5,
                       rescale = 400,
                       plot = TRUE) {
  temp_file <- tempfile(fileext = ".png")
  curl::curl_download(img_path, temp_file)
  img <- imager::load.image(temp_file)
  img <- imager::resize(img, round(imager::width(img) / rescale), round(imager::height(img) / rescale))
  img_mat <- imager::channels(img)
  if (length(img_mat) == 1) {
    m <- matrix(as.matrix(img_mat$c.1), nrow = nrow(as.matrix(img_mat$c.1)))
    colnames(m) <- seq_len(ncol(m))
    rownames(m) <- seq_len(nrow(m))
  } else {
    hex_m <- grDevices::rgb(as.matrix(img_mat$c.1), as.matrix(img_mat$c.2), as.matrix(img_mat$c.3))
    m <- matrix(hex_m, nrow = nrow(as.matrix(img_mat$c.1)))
    colnames(m) <- seq_len(ncol(m))
    rownames(m) <- seq_len(nrow(m))
  }

  # Process image
  plot_df <- m |>
    tibble::as_tibble() |>
    dplyr::mutate(x = dplyr::row_number()) |>
    tidyr::pivot_longer(-.data$x, names_to = "y") |>
    dplyr::mutate(y = as.numeric(.data$y))

  # Plot
  if (length(img_mat) == 1) {
    p <- ggplot2::ggplot() +
      ggplot2::geom_raster(
        data = plot_df,
        mapping = ggplot2::aes(x = .data$x, y = .data$y, fill = .data$value),
      ) +
      ggplot2::scale_fill_gradient(low = "white", high = "black", limits = c(0, 1)) +
      ggplot2::scale_y_reverse() +
      ggplot2::coord_fixed(expand = FALSE) +
      ggplot2::theme_void() +
      ggplot2::theme(legend.position = "none")
  } else {
    p <- ggplot2::ggplot() +
      ggplot2::geom_raster(
        data = plot_df,
        mapping = ggplot2::aes(x = .data$x, y = .data$y, fill = .data$value)
      ) +
      ggplot2::scale_fill_identity() +
      ggplot2::scale_y_reverse() +
      ggplot2::coord_fixed(expand = FALSE) +
      ggplot2::theme_void()
  }
  

  tmp <- tempfile()
  ggplot2::ggsave(tmp, p,
    device = "png", width = rescale * imager::width(img),
    height = rescale * imager::height(img),
    units = "px"
  )

  pal <- suppressMessages(eyedroppeR::extract_pal(
    n = 5,
    img_path = tmp,
    plot_output = FALSE
  )$pal)
  
  unique_pixels <- plot_df |>
    dplyr::select(value) |>
    dplyr::distinct() |>
    dplyr::pull()
  
  if (is.numeric(unique_pixels[1])) {
    unique_pixels <- unique_pixels[unique_pixels > 0]
  }

  dist_mat <- matrix(NA, nrow = length(unique_pixels), ncol = length(pal))
  colnames(dist_mat) <- pal

  for (i in 1:nrow(dist_mat)) {
    for (j in 1:ncol(dist_mat)) {
      dist_mat[i, j] <- colour_dist(unique_pixels[i], pal[j])
    }
  }

  colour_count <- dist_mat |>
    tibble::as_tibble() |>
    dplyr::mutate(
      id = dplyr::row_number()
    ) |>
    tidyr::pivot_longer(-id, names_to = "hex", values_to = "dist") |>
    dplyr::group_by(id) |>
    dplyr::slice_min(dist) |>
    dplyr::ungroup() |>
    dplyr::count(hex)

  if (plot) {
    p <- ggplot2::ggplot(
      data = colour_count,
      mapping = ggplot2::aes(
        area = n, fill = hex,
        label = hex
      )
    ) +
      treemapify::geom_treemap() +
      treemapify::geom_treemap_text(
        colour = "white",
        place = "centre",
        size = 15
      ) +
      ggplot2::scale_fill_identity() +
      ggplot2::theme(legend.position = "none")
    return(p)
  } else {
    return(colour_count)
  }
}

extract_data <- function(i) {
  output <- to_treemap(img_path = imgs[i], n = 5, rescale = 50, plot = FALSE)
  output$id <- i
  print(i)
  return(output)
}
