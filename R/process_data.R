
#' @import dplyr
#' @export
process_data <- function(x, min_cases = 100){
  output = x %>%
    arrange(date) %>%
    ungroup() %>%
    filter(confirmed > min_cases) %>%
    mutate(A = confirmed - recovered - deaths) %>%
    mutate(S = pop - confirmed) %>%
    mutate(D = deaths) %>%
    mutate(R = recovered) %>%
    select(S, A, D, R) %>%
    as.matrix()

  return(output)
}
