# sharer
#
# Remote data format in R
#   list(hash=data.frame(name=c('Biiko', 'Balou', 'Christian'),
#                        id=as.integer(c(1, 2, 3)), stringsAsFactors=F),
#        brix=list())

lapply(list('rstudioapi', 'jsonlite', 'httr'), function(p) {
  if (!p %in% .packages(T)) install.packages(p)
})

SHARER <- list()
SHARER$NAME <- 'Biiko'  # 'Balou', 'Christian'
SHARER$STORE_ID <- 'q1h39'
SHARER$ID <- sapply(list(SHARER$NAME), function(n) {
  SHARER$HASH <<- jsonlite::fromJSON(paste0('https://api.myjson.com/bins/', SHARER$STORE_ID))$hash
  return(SHARER$HASH[SHARER$HASH$name == n, 'id'])  # ur personal ID
})
SHARER$IN <- list()  # timestamped chr list of brix with
#                      4 char string as first vector item,
#                      code string as second vector item;
#                      4 char string:
#                        1st id indicating receiver
#                        2nd id indicating sender
#                        3rd 'T' 4 console, 'F' 4 file
#                        4th 'T' 4 read 'F' 4 not read

sharer_push <- function(code=NULL, to=NULL,
                        type=if (grepl('\\.r(md)?$', code, ignore.case=T)) 'file' else 'console',
                        name=SHARER$NAME, id=SHARER$ID, store_id=SHARER$STORE_ID) {
  stopifnot(is.character(code), length(code) == 1, to %in% SHARER$HASH$name,
            type %in% c('console', 'file'), nchar(name) > 0, is.integer(id),
            nchar(store_id) > 0)
  if (file.exists(code) && grepl('\\.r(md)?$', code, ignore.case=T)) {
    bric <- paste0(readLines(code, warn=F), sep='\n', collapse='')
  } else if (nchar(code) > 0) {
    bric <- code
  } else { stop('Invalid input!') }
  headr <- paste0(as.character(SHARER$HASH[SHARER$HASH$name == to, 'id']),  # receiver
                  as.character(id),  # sender
                  if (type == 'console') 'T' else 'F',  # is code meant 4 console or file?
                  'F')  # it has not been read
  shr <- jsonlite::fromJSON(paste0('https://api.myjson.com/bins/', store_id))
  shr$brix[[as.character(as.integer(Sys.time()))]] <- c(headr, bric)
  re <- httr::PUT(paste0('https://api.myjson.com/bins/', store_id), body=shr, encode='json')
  if (re$status_code == 200) {
    message('Upload complete.')
    return(invisible(0L))
  } else {
    stop('Upload error ', as.character(re$status_code), ' (-+_+)')
  }
}

sharer_show <- function(id=SHARER$ID, store_id=SHARER$STORE_ID) {
  stopifnot(rstudioapi::isAvailable(), is.integer(id), nchar(store_id) > 0)
  if (length(SHARER$IN) == 0 ||
      all(sapply(SHARER$IN, function(b) if (grepl('T$', b[1])) T else F))) {
    if (!readline('Pull from remote? [y/n] ') == 'y') return(invisible(NULL))
    shr <- jsonlite::fromJSON(paste0('https://api.myjson.com/bins/', store_id))
    SHARER$IN <<- shr$brix[sapply(shr$brix, function(b) {
      if (as.integer(unlist(strsplit(b[1], ''))[1]) == id) T else F
    })]
    if (length(SHARER$IN) == 0) return(message('No more code 4 u ... (-+_+)'))
    shr.flushd <- list(hash=shr$hash, brix=shr$brix[!shr$brix %in% SHARER$IN])
    re <- httr::PUT(paste0('https://api.myjson.com/bins/', store_id), body=shr.flushd, encode='json')
    if (re$status_code != 200) stop('Flush error (-*.*)') else message('Flushed ( Y )')
    message(as.character(length(SHARER$IN)), ' code chunk',
            if (length(SHARER$IN) > 1) 's' else '',
            ' pulled into memory.')
  }
  i <- 1
  while (i <= length(SHARER$IN)) {
    b <- SHARER$IN[[i]]
    if (grepl('F$', b[1])) {
      if (unlist(strsplit(b[1], ''))[3] == 'T') {  # case terminal / console
        rstudioapi::sendToConsole(gsub('\\n$', '', b[2]), F)  # don't auto-execute
      } else {  # case file
        dir.create(temp.dir <- tempfile())
        flnm <- file.path(temp.dir,
                          paste0(as.character(as.integer(Sys.time())),
                                 if (grepl('```', b[2])) '.Rmd' else '.R'))
        cat(b[2], file=flnm)  # redirect code string 2 file
        file.edit(flnm)
      }
      SHARER$IN[[i]][1] <<- paste0(paste0(strsplit(b[1], '')[[1]][1:3], collapse=''), 'T')
      break
    }
    i <- i + 1
  }
  return(invisible(0L))
}
