#'  Merge less blindly 
#'
#' @description
#' `stata_join()` allows a merge to have an output summarizing the success of a 
#'  a merge (similiar to stata), while maintaining the R flexibility of doing 
#'  merges in a chain.
#'
#' @param x, this is the first dataset to be merged. In a dplyr chain, this 
#'   argument is inputted automatically.
#' @param y, this is the second dataset to be included in the merge,
#' @param by, vector of common columns, or column mapping to be used in the merge.
#'   There is no default argument here. For the sake of being careful and 
#'   complete, the common columns must be specified.
#' @param type, this is by default a left join. This means that x is the master
#'   dataset, and y gets merged on to x. If its not type left, then its a full 
#'   join. See `dplyr::full_join` for more details.
#' @param quiet, this logical parameter specifies whether or not to display output. If 
#'  `TRUE`, merge summary will not be displayed. If `'FALSE' (default) the 
#'   summary will be displayed.
#'   Possible values are:
#'   
#' @output `stata_join` prints a summary of the merge. This includes:
#'  - The number and percentage of observations not merged from the master (x)
#'  - The number and percentage of observations not merged from the Using (y)
#'  - The number of observations merged
#'  - The number of new rows added if the merge is 1:m or m:m
#'  - The time the function took to complete the merge
#'
#' @returns
#' `stata_join` returns the tibble resulting from the merge, It also adds an
#'  additional column, `m_`. `m_` signifies the status of the merge for that 
#'  row. 
#'  - m_ == 1 means that the row in x didn't have a merge in y
#'  - m_ == 2 means that the row in y didn't have a merge in x (full joins only)
#'  - m_ == 3 means that the merge was successful for that row
#'
#' @example
#' # across() -----------------------------------------------------------------
#' df.1 <- tibble(a = 1:3,b = 2:4)
#' df.2 <- tibble(b = 3:5, c = 4:6)
#' 
#' merged.df <- stata_join(df.1,df.2,by = "b",type = "full")
#' @output
#' merge                        n        
#' <chr>                        <chr>    
#' Not merged: Master           1 (33.3%)
#' Not merged: Using            1 (33.3%)
#' Merged                       2        
#' Double Merged/New rows added 0        
#' Time difference of 0.05476093 secs
#' 
#' merged.df
#' @output
#' # A tibble: 4 x 4
#'  a     b     c    m_
#'<int> <int> <int> <dbl>
#'  1     2    NA     1
#'  2     3     4     3
#'  3     4     5     3
#' NA     5     6     2
#'
#' 

stata_join <- function(x,y,by, type = "left",quiet = F){
  
  start <- now()
  
  x %<>% mutate(n_ = 1)
  y %<>% mutate(o_ = 2)
  z = full_join(x,y,by = by)
  
  z %<>% mutate_at(vars(n_,o_), ~replace(.,is.na(.),0)) %>%
         mutate(m_ = n_ + o_) %>%
         select(-n_,-o_)
  
  template <- tribble(~m_,~merge,
                      1,"Not merged: Master",
                      2,"Not merged: Using",
                      3,"Merged")
  
  print_output <- template %>% left_join(z %>% group_by(m_) %>%
                                               summarise(n=n()),
                                         by = "m_") %>%
                               replace(is.na(.),0) %>%
                               select(merge,n,m_) %>%
                               add_row(merge = "Double Merged/New rows added", 
                                       n = nrow(z) - max(0,.$n[.$merge == "Not merged: Using"],na.rm=T) -nrow(x)) %>%
                               mutate(n = ifelse(row_number() == 3,n[3]-n[4],n))
  
 print_output %<>% mutate(p = case_when( is.na(m_) | m_ == 3~ -999,
                                          m_ == 1 ~ n/nrow(x),
                                          m_ == 2 ~ n/nrow(y))) %>%
                   mutate(n = case_when(p != -999 ~ str_c(n," (",round(100*p,1),"%)"),
                                        T~as.character(n))) %>%
                   select(merge,n)

  
  if(type == "left"){
    z %<>% filter(m_ != 2)
  }
  
  if(!quiet){
    print(print_output)
    print(now() - start)
  }
  
  return(z)
  
}
