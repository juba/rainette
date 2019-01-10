

# tidy_groups <- function(res) {
#   purrr::imap_dfr(res, function(l, i) {
#     line <- purrr::imap_dfr(l$groups, ~ list(level = i, group = .y, members = list(.x)))
#     return(line)
#   })
# }

rainette2 <- function(dtm, max_k = 5, uc_size1 = 10, uc_size2 = 15, ...) {

  res1 <- rainette(dtm, k = max_k, min_uc_size = uc_size1, verbose = TRUE,...)
  res2 <- rainette(dtm, k = max_k, min_uc_size = uc_size2, verbose = TRUE,...)
  
  groups1 <- data.frame(res1$uce_groups)
  colnames(groups1) <- 1:(max_k - 1)
  groups2 <- data.frame(res2$uce_groups)
  colnames(groups2) <- 1:(max_k - 1)

  cross_eff <- purrr::map2_dfr(groups1, groups2, function(g1, g2) {
    df <- data.frame(g1, g2)
    df %>% dplyr::count(g1)
   counts <- df %>% 
      dplyr::count(g1, g2) %>% 
      rename(n_both = n) %>% 
      dplyr::right_join(df %>% dplyr::count(g1), by = "g1") %>% 
      rename(n1 = n) %>% 
      dplyr::right_join(df %>% dplyr::count(g2), by = "g2") %>% 
      rename(n2 = n)
   ## TODO compute chisq value
  }, .id="level")
  
  
  return(cross_eff)
}

