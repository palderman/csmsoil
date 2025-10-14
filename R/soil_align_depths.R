#' Align soil profile data to a new set of depths to base of layer
#'
#' @param soil_data a data frame containing soil profile data including a column
#'  for depth of base of layer
#'
#' @param new_depths a numeric vector with new values for depth to base of layer
#'   to which soil profile data should be aligned
#'
#' @param depth_name a character string containing the name for the column in
#'  \code{soil_data} that contains the depth of base of layer
#'
#' @param drop_deeper_layers a logical value indicating whether layers below the maximum
#'   value in \code{new_depths} should be dropped. If TRUE the values will be dropped (default),
#'   if FALSE layers will be kept.
#'
#' @export
#'
#' @examples
#'
#' soil_align_depths(soil_depth, new_depths)
#'
#' @importFrom dplyr tibble "%>%" full_join arrange rename_all group_by mutate mutate_at
#'   select
#' @importFrom stringr str_remove
#' @importFrom tidyr fill
#'
soil_align_depths <- function(soil_data, new_depths,
                              depth_name = "SLB",
                              drop_deeper_layers = TRUE){

  variables <- colnames(soil_data)

  old_soil_data <- soil_data

  old_depth_name <- paste0(depth_name, "_old")
  new_depth_name <- paste0(depth_name, "_new")

  # Set top of layer
  old_soil_data[["top"]] <-
    old_soil_data[[depth_name]] |>
    head(-1) |>
    c(0, x = _)

  # Rename columns for old soil data
  colnames(old_soil_data) <-
    colnames(old_soil_data) |>
    paste0("_old")

  # Set depth for merging
  old_soil_data[[depth_name]] <-
    old_soil_data[[old_depth_name]]

  new_soil_data <- data.frame(new_depths)
  names(new_soil_data) <- depth_name

  new_soil_data <-
    new_soil_data |>
    merge(old_soil_data, all = TRUE)

  new_soil_data <-
    # order rows by depth
    new_soil_data[order(new_soil_data[[depth_name]]), ]

  new_soil_data[[new_depth_name]] <-
    ifelse(new_soil_data[[depth_name]] %in% new_depths,
           new_soil_data[[depth_name]],
           NA)

  new_soil_data[["top_new"]] <-
    new_soil_data[[new_depth_name]] |>
    head(-1) |>
    c(0, x = _)

  fill_up_names <-
    paste0("(", new_depth_name, ")|(_old)") |>
    grep(names(new_soil_data),
         value = TRUE)

  fill_val <- NA
  for(col in fill_up_names){
    for(i in nrow(new_soil_data):1){
      if(!is.na(new_soil_data[[col]][i])){
        fill_val <- new_soil_data[[col]][i]
      }else{
        new_soil_data[[col]][i] <- fill_val
      }
    }
  }

  fill_val <- NA
  for(i in 1:nrow(new_soil_data)){
    if(!is.na(new_soil_data[["top_new"]][i])){
      fill_val <- new_soil_data[["top_new"]][i]
    }else{
      new_soil_data[["top_new"]][i] <- fill_val
    }
  }

  new_soil_data[["depth_wt"]] <-
    calc_depth_weight(depth_new = new_soil_data[[new_depth_name]],
                      depth_old = new_soil_data[[old_depth_name]],
                      top_new = new_soil_data[["top_new"]],
                      top_old = new_soil_data[["top_old"]])

  new_soil_data[[new_depth_name]] <-
    ifelse(
      is.na(new_soil_data[[new_depth_name]]),
      new_soil_data[[old_depth_name]],
      new_soil_data[[new_depth_name]]
    )

  new_soil_data[[old_depth_name]] <- NULL
  new_soil_data[[depth_name]] <- NULL
  new_soil_data[["top_old"]] <- NULL
  new_soil_data[["top_new"]] <- NULL

  char_cols <-
    new_soil_data |>
    lapply(is.character) |>
    unlist()

  char_df <-
    new_soil_data[, char_cols] |>
    aggregate(by = new_soil_data[new_depth_name],
              FUN = \(.x) paste0(unique(.x), collapse = "/"))

  other_cols <-
    new_soil_data[!char_cols] |>
    names() |>
    grep(paste0("(^depth_wt$)|(^", new_depth_name, "$)"),
         x = _,
         invert = TRUE,
         value = TRUE)

  for(.col in other_cols){
    new_soil_data[[.col]] <-
      new_soil_data[[.col]] * new_soil_data[["depth_wt"]]
  }

  other_df <-
    new_soil_data[, other_cols] |>
    aggregate(by = new_soil_data[new_depth_name],
              FUN = \(.x) sum(.x, na.rm = TRUE))

  out_df <-
    char_df |>
    merge(other_df, by = new_depth_name, all = TRUE)

  colnames(out_df) <-
    colnames(out_df) |>
    gsub("_old|_new", "", x = _)

  if(drop_deeper_layers){
    out_df <- out_df[out_df[[depth_name]] %in% new_depths, ]
  }

  return(out_df)
}

calc_depth_weight <- function(depth_new, depth_old, top_new, top_old){
  mapply(
    .dnew = depth_new,
    .dold = depth_old,
    .tnew = top_new,
    .told = top_old,
    \(.dnew, .dold, .tnew, .told){
      if(is.na(.dnew) |
         (.dnew <= .dold & .tnew >= .told)){
        weight <- 1
      }else if(.dold > .tnew & .dnew > .dold){
        weight <- (.dold - .tnew)/(.dnew - .tnew)
      }else{
        weight <- (.dnew - .told)/(.dnew - .tnew)
      }
      return(weight)
    })
}
