# sharer

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
SHARER$INBOX <- list()

sharer_push <- function(code=NULL, to=NULL, comment='',
                        type=if (grepl('\\.r(md)?$', code, ignore.case=T)) 'file' else 'console',
                        name=SHARER$NAME, id=SHARER$ID, 
                        hash=SHARER$HASH, store_id=SHARER$STORE_ID) {
  stopifnot(is.character(code), length(code) == 1,
            !missing(to), to %in% hash$name,
            is.character(comment), length(comment) == 1,
            type %in% c('console', 'file'),
            nchar(name) > 0, is.integer(id), nchar(store_id) > 0)
  if (file.exists(code) && grepl('\\.r(md)?$', code, ignore.case=T)) {
    bric <- paste0(readLines(code, warn=F), sep='\n', collapse='')
  } else if (nchar(code) > 0) {
    bric <- code
  } else { stop('Invalid input!') }
  headr <- paste0(as.character(hash[hash$name == to, 'id']),  # receiver
                  as.character(id),  # sender
                  if (type == 'console') 'T' else 'F',  # is code meant 4 console or file?
                  'F')  # it has not been read
  shr <- jsonlite::fromJSON(paste0('https://api.myjson.com/bins/', store_id))
  shr$brix[[as.character(as.integer(Sys.time()))]] <- c(headr, bric, comment)
  re <- httr::PUT(paste0('https://api.myjson.com/bins/', store_id),
                  body=shr, encode='json')
  if (re$status_code == 200) {
    message('[200] Push completed')
    return(invisible(0L))
  } else {
    stop('Upload error ', as.character(re$status_code), ' (-+_+)')
  }
}

sharer_show <- function(id=SHARER$ID, inbox=SHARER$INBOX,
                        hash=SHARER$HASH, store_id=SHARER$STORE_ID) {
  stopifnot(rstudioapi::isAvailable(), is.integer(id), nchar(store_id) > 0)
  if (length(inbox) == 0 ||
      all(sapply(inbox, function(b) if (grepl('T$', b[1])) T else F))) {
    if (!readline('Pull from remote? [y/n] ') == 'y') return(invisible(NULL))
    shr <- jsonlite::fromJSON(paste0('https://api.myjson.com/bins/', store_id))
    SHARER$INBOX <<- inbox <- shr$brix[sapply(shr$brix, function(b) {
      if (as.integer(unlist(strsplit(b[1], ''))[1]) == id) T else F
    })]
    if (length(inbox) == 0) return(message('No more code 4 u (-+_+)'))
    message(as.character(length(inbox)), ' code chunk',
            if (length(inbox) > 1) 's' else '',
            ' pulled into memory')
    shr.flushd <- list(hash=shr$hash, brix=shr$brix[!shr$brix %in% inbox])
    re <- httr::PUT(paste0('https://api.myjson.com/bins/', store_id),
                    body=shr.flushd, encode='json')
    if (re$status_code == 200) {
      message('Flushed remote ( Y )')
    } else {
      stop('Flush error ', as.character(re$status_code), ' (-*.*)')
    }
  }
  i <- 1
  while (i <= length(inbox)) {
    b <- inbox[[i]]
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
      message('From ', hash[hash$id == as.integer(strsplit(b[1], '')[[1]][2]), 'name'],
              if (b[3] != '') paste0(':\n', b[3]) else '')
      SHARER$INBOX[[i]][1] <<- paste0(paste0(strsplit(b[1], '')[[1]][1:3], collapse=''), 'T')
      break
    }
    i <- i + 1
  }
  message(n <- sum(sapply(SHARER$INBOX, function(b) grepl('F$', b[1]))), ' left',
          if (n > 0) ' -> Run sharer_show() again!' else '')
  return(invisible(0L))
}
