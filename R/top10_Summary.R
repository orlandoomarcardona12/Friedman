top10_summary <- function(data, stat, name_col) {

  #standardizing stat input
  stat <- tolower(stat)

  #ERROR if name_col is missing
  if (missing(name_col)) {
    stop("You must provide a name_col (e.g., player or team column).")
  }

  #Checking if columns exist
  if (!stat %in% names(data)) {
    stop(paste("Column not found:", stat))
  }

  if (!name_col %in% names(data)) {
    stop(paste("Name column not found:", name_col))
  }

  #ensuring the stat is a numeric stat
  data[[stat]] <- as.numeric(data[[stat]])

  #top 10
  top10 <- dplyr::arrange(data, dplyr::desc(.data[[stat]])) |>
    dplyr::slice_head(n = 10)

  #plotting
  plot <- ggplot2::ggplot(
    top10,
    ggplot2::aes(
      x = stats::reorder(.data[[name_col]], .data[[stat]]),
      y = .data[[stat]]
    )
  ) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = paste("Top 10 by", stat),
      x = name_col,
      y = stat
    )

  print(plot)

  return(top10)
}
