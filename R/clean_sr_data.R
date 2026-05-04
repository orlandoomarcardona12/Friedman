clean_sr_data <- function(data, keep = NULL, output_name = "cleaned_data") {

  cleaned_data <- data

  #using the first row as a header
  names(cleaned_data) <- as.character(unlist(cleaned_data[1, ]))
  cleaned_data <- cleaned_data[-1, ]

  # clean column names just incase
  names(cleaned_data) <- stringr::str_to_lower(names(cleaned_data))
  names(cleaned_data) <- stringr::str_replace_all(names(cleaned_data), " ", "_")

  #fix duplicate column names to make it run more smoothly
  names(cleaned_data) <- make.unique(names(cleaned_data))

  #remove totals rows
  if ("team" %in% names(cleaned_data)) {
    cleaned_data <- dplyr::filter(cleaned_data, team != "TOT")
  }

  # convert character columns to numeric when it is possible
  cleaned_data <- dplyr::mutate(
    cleaned_data,
    dplyr::across(
      dplyr::where(is.character),
      ~ suppressWarnings(as.numeric(.))
    )
  )

  # replace NA with 0
  cleaned_data <- dplyr::mutate(
    cleaned_data,
    dplyr::across(
      dplyr::where(is.numeric),
      ~ tidyr::replace_na(., 0)
    )
  )

  #keeping the selected columns
  if (!is.null(keep)) {
    keep <- stringr::str_to_lower(keep)
    cleaned_data <- dplyr::select(cleaned_data, dplyr::any_of(keep))
  }

  #puts it in a new dataset and into the global environment
  assign(output_name, cleaned_data, envir = .GlobalEnv)

  message("Created dataset: ", output_name)

  return(cleaned_data)
}

