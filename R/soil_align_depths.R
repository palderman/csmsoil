#' Align soil profile data to a new set of depths to base of layer
#'
#' @param new_SLB a numeric vector with new values for depth to base of layer (SLB)
#'   to which soil profile data should be aligned
#'
#' @param profile a tibble containing soil profile data including a column for depth
#'   to base of layer (SLB)
#'
#' @param drop_deeper_layers a logical value indicating whether layers below the maximum
#'   value in `new_SLB` should be dropped. If TRUE the values will be dropped (default),
#'   if FALSE layers will be kept.
#'
#' @export
#'
#' @examples
#'
#' soil_align_depths(new_SLB, profile)
#'
#' @importFrom dplyr tibble "%>%" full_join arrange rename_all group_by mutate mutate_at
#'   select
#' @importFrom stringr str_remove
#' @importFrom tidyr fill
#'
soil_align_depths <- function(new_SLB, profile, drop_deeper_layers = TRUE){

  variables <- colnames(profile)

  old_profile <- profile %>%
    rename_all(~str_c(.,"_old")) %>%
    mutate(top_old = c(0,head(SLB_old,-1)),
           SLB = SLB_old)

  new_profile <- tibble(SLB = new_SLB) %>%
    full_join(old_profile) %>%
    arrange(SLB) %>%
    mutate(SLB_new = ifelse(SLB %in% new_SLB, SLB, NA),
           top_new = c(0,head(SLB_new,-1))) %>%
    fill(matches("(SLB_new)|(_old)"),.direction="up") %>%
    fill(top_new,.direction="down") %>%
    mutate(wt = ifelse((SLB_new <= SLB_old & top_new >= top_old) | is.na(SLB_new),
                       1,
                        ifelse(SLB_old > top_new & SLB_new > SLB_old,
                       (SLB_old - top_new)/(SLB_new-top_new),
                       (SLB_new - top_old)/(SLB_new-top_new))),
           SLB_new = ifelse(is.na(SLB_new), SLB_old, SLB_new)) %>%
    select(-SLB_old, -top_old, -SLB, -top_new) %>%
    group_by(SLB_new) %>%
    summarize_at(vars(matches("_old")),~if(is.character(.)){str_c(unique(.),collapse="/")}else{sum(.*wt)}) %>%
    rename_at(vars(matches("(_old)|(_new)")),~str_remove(.,"(_old)|(_new)"))

  if(drop_deeper_layers){
    new_profile <- new_profile %>%
      filter(SLB %in% new_SLB)
  }

  return(new_profile)
}
