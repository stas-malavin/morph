importOneCurveSM <- function(path, curve, scaled = T)
{
  require(magrittr)
  require(Momocs)
  coo <- list()
  xmltag <- paste0('<', curve)
  for (i in 1:length(list.files(path))) {
    parsed <- list.files(path, full.names = T)[i] %>% readLines
    begin <- (grep(xmltag, parsed))[as.numeric(scaled) + 2] + 1
    end <- (grep(sub('<', '</', xmltag), parsed))[as.numeric(scaled) + 2] - 1
    tryCatch(
      coo[[i]] <- parsed[begin:end] %>% 
        sub('^\\t*', '', .) %>%
        strsplit('\t') %>%
        unlist %>%
        as.numeric %>%
        matrix(length(.)/2, 2, byrow = T),
        error = function(e) coo[[i]] <<- matrix(0, 1, 2)
    )
    names(coo)[i] <- list.files(path)[i] %>% sub('.txt', '', .)
    names(coo)[i] %<>% sub('-.*$', '', .)
  }
  return(coo)
}