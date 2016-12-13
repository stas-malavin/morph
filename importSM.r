importSM <- function(path, class = c('Opn', 'Out', 'Ldk', 'Control'), scaled = T)
{
  require(magrittr)
  require(Momocs)
  class <- match.arg(class)
  coo <- list()
  # Out and Opn ---------------------------------------------------------------
  if (grepl('O', class)) {
    if (scaled) {
      xmltag <- '<curves.scaled'
    } else {
      xmltag <- '<curves.pixel'
    }
    for (i in 1:length(list.files(path))) {
      parsed <- list.files(path, full.names = T)[i] %>% readLines
      coo.begin <- parsed %>% grep(xmltag, .) + 1
      coo.end <- parsed %>% grep(sub('<', '</', xmltag), .) - 1
      tryCatch(
        coo[[i]] <-
          parsed[coo.begin:coo.end] %>%
          gsub('^.*<.*>.*$', '', .) %>% #Removing separate curves
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
  }
  # Curves control points -----------------------------------------------------
  if (class == 'Control') {
    xmltag <- '<curves.control'
    for (i in 1:length(list.files(path))) {
      parsed <- list.files(path, full.names = T)[i] %>% readLines
      coo.begin <- parsed %>% grep(xmltag, .) + 1
      coo.end <- parsed %>% grep(sub('<', '</', xmltag), .) - 1
      tryCatch(
        coo[[i]] <-
          parsed[coo.begin:coo.end] %>%
          sub('^\\t*', '', .) %>%
          strsplit('\t') %>%
          unlist %>%
          .[!grepl('<', .)] %>%
          as.numeric %>%
          matrix(length(.)/2, 2, byrow = T) %>%
          unique,
        error = function(e) coo[[i]] <<- matrix(0, 1, 2)
      )
      names(coo)[i] <- list.files(path)[i] %>% sub('.txt', '', .)
      names(coo)[i] %<>% sub('-.*$', '', .)
    }
  }
  # Ldk -----------------------------------------------------------------------
  if (class == 'Ldk') {
    if (scaled) {
      xmltag <- '<landmarks.scaled'
    } else {
      xmltag <- '<landmarks.pixel'
    }
    for (i in 1:length(list.files(path))) {
      parsed <- list.files(path, full.names = T)[i] %>% readLines
      coo.begin <- parsed %>% grep(xmltag, .) + 1
      coo.end <- parsed %>% grep(sub('<', '</', xmltag), .) - 1
      tryCatch(
        coo[[i]] <-
          parsed[coo.begin:coo.end] %>%
          sub('^\\t*', '', .) %>%
          strsplit('\t') %>%
          lapply(function(x) as.numeric(x[-1])) %>%
          unlist %>%
          matrix(length(.)/2, 2, byrow = T),
        error = function(e) coo[[i]] <<- matrix(0, 1, 2)
      )
      names(coo)[i] <- list.files(path)[i] %>% sub('.txt', '', .)
      names(coo)[i] %<>% sub('-.*$', '', .)
    }
  }
  return(coo)
}
