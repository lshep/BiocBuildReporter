## File for Internal Helpers

remote_url_base <- "https://mghp.osn.xsede.org/bir190004-bucket01/BiocBuildReports/"

.getCacheLocation <- function(){
    tools::R_user_dir(package = "BiocBuildReporter", which = "cache")
}

.makeLocalCache <- function(){
    cache <- .getCacheLocation()
    bfc <- BiocFileCache::BiocFileCache(cache)
}

.checkInCache <- function(bfc, tblname){
    res <- bfcquery(bfc, query=tblname, field="rname")
    as.logical(nrow(res))
}

.addToCache <- function(bfc, tblname){
    fpath <- paste0(remote_url_base, tblname, ".parquet")
    bfcadd(bfc, rname=tblname, fpath=fpath) 
}

.getCacheId  <- function(bfc, tblname){
    res <- bfcquery(bfc, query=tblname, field="rname")
    bfcrid(res)
}

.getUrl <- function(tblname, useLocal, updateLocal){

    if(useLocal){
        bfc <- .makeLocalCache()
        fnd <- .checkInCache(bfc, tblname)
        if(fnd){
            message(sprintf("Reading local file for '%s'", tblname))
            rid <- .getCacheId(bfc, tblname)
            url <- ifelse(updateLocal,
                          bfcdownload(bfc, rid=rid, ask=FALSE),
                          bfcpath(bfc, rids=rid))  
        }else{
            message(sprintf("Local file for '%s' Not Found. Downloading...", tblname))
            url <- .addToCache(bfc, tblname)
        }
        
    }else{
        
        message(sprintf("reading '%s' parquet file...\n", tblname),
                "    Initial read may take a few minutes\n",
                "    Subsequent access through cache will be instantaneous.")
        
        s3fs <- S3FileSystem$create(
                                 anonymous = TRUE,
                                 endpoint_override = "https://mghp.osn.xsede.org"
                             )
        file_path <- paste0("bir190004-bucket01/BiocBuildReports/", tblname, ".parquet")
        url <- s3fs$OpenInputFile(file_path)

    }
    url
}

## r-universe build results, in the BBS table shape. The views are SQL over one
## public parquet file, maintained with the registry that publishes it:
## https://github.com/seandavi/bioc-infrastructure/blob/main/packages/registry/docs/api.md
runiverse_views_url <- "https://raw.githubusercontent.com/seandavi/bioc-infrastructure/main/packages/registry/scripts/bbs_views.sql"

.getRuniverseTable <- function(tblname, updateLocal){
    if (tblname == "propagation_status")
        stop("r-universe has no propagation_status table yet", call. = FALSE)
    key <- paste0("r-universe:", tblname)
    if (exists(key, envir = .bbs_cache) && !updateLocal) {
        message(sprintf("Using cached r-universe table '%s'", tblname))
        return(get(key, envir = .bbs_cache))
    }
    if (!requireNamespace("duckdb", quietly = TRUE) ||
        !requireNamespace("DBI", quietly = TRUE))
        stop("source = 'r-universe' needs the duckdb and DBI packages", call. = FALSE)

    message(sprintf("reading r-universe '%s' table...", tblname))
    con <- DBI::dbConnect(duckdb::duckdb())
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
    ## the R client does not autoload extensions
    for (e in c("httpfs", "json", "icu"))
        DBI::dbExecute(con, sprintf("INSTALL %s; LOAD %s", e, e))
    sql <- paste(readLines(runiverse_views_url, warn = FALSE), collapse = "\n")
    for (s in strsplit(sql, ";\\s*\n")[[1]])
        if (nzchar(trimws(s))) DBI::dbExecute(con, s)

    tbl <- as_tibble(DBI::dbGetQuery(con, sprintf("FROM %s", tblname)))
    assign(key, tbl, envir = .bbs_cache)
    tbl
}
