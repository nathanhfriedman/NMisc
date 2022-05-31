#' A function to compare columns within a merged dataset
#' 
#' @desciption A function to compare columns within a merged data frame, that 
#' has suffixes `.x` and `.y`. These are the default suffixes when merging 
#' a dataset and there are duplicate columns not used in identifying the merge.
#' The results will be the same dataset, 
#'
#' @param .data, this is a dataframe that has the columns to be compared. 
#'  The columns that match in their root, but differ in their suffix with .x
#'  and .y will be compared. For example col.x and col.y. These columns result 
#'  naturally from merges.
#'  
#' @param threshold, a numeric threshold to be used when assessing whether 2
#'  numerical values are similar enough in absolute value.  
#'   
#' @return `xy_compare()` returns `.data` with 2 additional columns:
#'  1. matches: this column is a TRUE or FALSE column indicating whether or 
#'     not the columns that were compared match.
#'  2. deviant_cols: this column contains a string which names the columns that 
#'     did not match. If the columns match, then the string is empty.
#'   
#' @examples
#' df.1 <- tibble(id = 1:3, num = c(2,3,4), char = c("a","b","c"))
#' df.2 <- tibble(id = 1:3, num = c(2,3,5), char = c("aa","b","c"))
#' dfs.merged <- df.1 %>% left_join(df.2,by = "id")
#' dfs.merged
#' @output 
#'  id  num.x char.x num.y char.y
#' <int> <dbl> <chr>  <dbl> <chr> 
#'   1     2    a       2   aa    
#'   2     3    b       3   b     
#'   3     4    c       5   c    
#'
#' xy_compare(dfs_merged)
#' 
#' @output
#'   id  char.x char.y num.x num.y matches deviant_cols
#' <dbl> <chr>  <chr>  <dbl> <dbl> <lgl>   <chr>       
#'   1    a      aa         2     2 FALSE   "char"      
#'   2    b      b          3     3 TRUE    ""          
#'   3    c      c          4     5 FALSE   "num" 
#'

xy_compare <- function(.data, threshold = 0.1){

  .data = .data %>% ungroup() %>% 
                    mutate(row = 1:n()) %>%
                    mutate_if(is.character, ~replace(.,is.na(.),"")) %>%
                    mutate_if(is.integer, as.double)
  
  var.types <- sapply(.data,typeof) %>% enframe()
  
  comp.cols <- colnames(.data) %>% .[str_detect(.,"\\.x|\\.y")] %>% 
                                   sort() %>% 
                                   tibble(var = .) %>% 
                                   left_join(var.types,by = c("var" = "name"))
  
  comp.cols.unique <- comp.cols %>% mutate(var = str_remove(var,"\\.x|\\.y")) %>%
                                    distinct() 
  
  comp.cols.check <- comp.cols.unique %>% group_by(var) %>%
                                          summarise(n=n())
  
  if(max(comp.cols.check$n) >1){
    
    print(str_c("Variable has more than 1 type:",
                comp.cols.check$var[comp.cols.check$n>1],
                sep = "\n"))
    break()
    
  }
  
  results <- .data %>% mutate(matches = T,
                           deviant_cols = "")
  
  for(var in comp.cols.unique$var){
    
    if(comp.cols.unique$value[comp.cols.unique$var == var] != "double"){
      results %<>% mutate(!!var := get(str_c(var,".x")) == get(str_c(var,".y")),
                          matches = matches & get(var),
                          deviant_cols = case_when(!get(var) & deviant_cols != "" ~ str_c(deviant_cols,", ",var),
                                                   !get(var) & deviant_cols == "" ~ var,
                                                   T~deviant_cols))
    } else {
      results %<>% mutate(!!var := case_when(is.na(get(str_c(var,".x"))) & is.na(get(str_c(var,".y"))) ~ T,
                                             T ~ coalesce(abs(get(str_c(var,".x")) - get(str_c(var,".y"))) < threshold,
                                                          FALSE) ),
                          matches = matches & get(var),
                          deviant_cols = case_when(!get(var) & deviant_cols != ""~ str_c(deviant_cols,", ",var),
                                                   !get(var) & deviant_cols == ""~ var,
                                                   T~deviant_cols))
    }
  }
  
  results %<>% select(row,matches,deviant_cols) 
  
  .data %>% select(-matches("\\.x|\\.y"), comp.cols$var) %>% 
            left_join(results, by = "row") %>%
            select(-row) 
  
}
