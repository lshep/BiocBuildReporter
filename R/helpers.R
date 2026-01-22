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
