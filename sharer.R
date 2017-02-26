# sharer
#
# Remote data format in R
#   list(hash=data.frame(name=c('Biiko', 'Balou', 'Christian'),
#                        id=as.integer(c(1, 2, 3)), stringsAsFactors=F),
#        brix=list())

lapply(list('jsonlite', 'httr'), function(p) {
  if (!p %in% .packages(T)) install.packages(p)
})

SHARER <- list()
SHARER$NAME <- 'Biiko'  # 'Balou', 'Christian'
SHARER$STORE_ID <- 'q1h39'
SHARER$FILES <- file.path(.libPaths()[1], 'sharer')
if (!dir.exists(SHARER$FILES)) dir.create(SHARER$FILES)
SHARER$ID <- sapply(list(SHARER$NAME), function(n) {
  SHARER$HASH <<- jsonlite::fromJSON(paste0('https://api.myjson.com/bins/', SHARER$STORE_ID))$hash
  return(SHARER$HASH[SHARER$HASH$name == n, 'id'])  # ur personal ID
})
SHARER$IN <- list()  # timestamped chr list of brix with
#                      4 char string as first vector item,
#                      code string as second vector item;
#                      4 char code:
#                        1st id indicating receiver
#                        2nd id indicating sender
#                        3rd 'T' 4 console, 'F' 4 file
#                        4th 'T' 4 read 'F' 4 not read

# bric <- paste0(readLines('sharer.R'), sep='\n', collapse='')  # read-in .R 2 string
# cat(bric, file='tert.R')  # redirect code string 2 file

sharer_get <- function() {
  
}

sharer_push <- function(code=NULL, to=NULL, type=c('console', 'file')[1],
                        name=SHARER$NAME, id=SHARER$ID, store_id=SHARER$STORE_ID) {
  stopifnot(is.character(code), length(code) == 1, to %in% SHARER$HASH$name,
            type %in% c('console', 'file'), nchar(name) > 0, is.integer(id),
            nchar(store_id) > 0)
  if (file.exists(code)) {
    bric <- paste0(readLines(code), sep='\n', collapse='')
  } else if (nchar(code) > 0) {
    bric <- code
  } else { stop('Invalid input!') }
  if (type == 'console') type.code <- 'T' else type.code <- 'F'
  headr <- paste0(as.character(SHARER$HASH[SHARER$HASH$name == to, 'id']),  # receiver
                  id,  # sender
                  type.code,  # is code meant 4 console ('T') or file ('F')?
                  'F')  # it has not been read
  shr <- jsonlite::fromJSON(paste0('https://api.myjson.com/bins/', store_id))
  shr$brix[[as.character(as.integer(Sys.time()))]] <- c(headr, bric)
  re <- httr::PUT(paste0('https://api.myjson.com/bins/', store_id), body=shr, encode='json')
  if (re$status_code != 200) stop('Upload error ', as.character(re$status_code), ' (-+_+)')
}

sharer_show <- function() {
  
}
