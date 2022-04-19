#' An adapted [dplyr::bind_cols()] to handle differing row numbers
#' 
#' @desciption Column binding for equal and unequal row lengths.
#' If the number of rows is unequal, the shorter data frames will be padded 
#' with NAs. See [dplyr::bind_cols()] for more information.
#'
#' @param ... Data frames to combine.
#'
#'   Each argument can either be a data frame, a list that could be a data
#'   frame, or a list of data frames.
#'
#'   When row-binding, columns are matched by name, and any missing
#'   columns will be filled with NA.
#'
#'   When column-binding, rows are matched by position, so all data
#'   frames must have the same number of rows. To match by value, not
#'   position, see [mutate-joins].
#' @param .name_repair One of `"unique"`, `"universal"`, or
#'   `"check_unique"`. See [vctrs::vec_as_names()] for the meaning of these
#'   options.
#'   
#' @return `bind_cols_fill()` returns the same type as
#'   the first input, either a data frame, `tbl_df`, or `grouped_df`, with the 
#'   columns all binded and the shorter data frames padded with NAs.
#'   
#' @examples
#' df.1 <- tibble(a = 1:3)
#' df.2 <- tibble(b = 1:4)
#'
#' bind_cols_fill(df.1,df.2)
#' @output
#'      a     b
#'    <int> <int>
#'      1     1
#'      2     2
#'      3     3
#'     NA     4
#' 
#'


bind_cols_fill <- function (..., .name_repair = c("unique", "universal", 
                                                  "check_unique", "minimal")) {
  
  dots <- list2(...)
  dots <- squash_if(dots, vec_is_list)
  dots <- discard(dots, is.null)
  is_data_frame <- map_lgl(dots, is.data.frame)
  names(dots)[is_data_frame] <- ""
  
  padTemp <- map(dots,nrow) %>% unlist() %>% max()
  
  dots <- map(dots, 
              ~bind_rows(.,
                         ungroup(.) %>% slice(rep(1,padTemp - nrow(.))) %>% 
                                        mutate_all(~replace(.,!is.na(.),NA))
                         )
              )
  
  out <- vec_cbind(!!!dots, .name_repair = .name_repair)
  
  if (!any(map_lgl(dots, is.data.frame))) {
    out <- as_tibble(out)
  }
  if (length(dots) && is.data.frame(first <- dots[[1L]])) {
    out <- dplyr_reconstruct(out, first)
  }
  out
}