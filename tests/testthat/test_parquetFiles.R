
test_that("get_bbs_table_works", {

    if(exists("info", envir = .bbs_cache)) rm("info", envir = .bbs_cache)
    
    expect_false(exists("info", envir = .bbs_cache))
    info <- get_bbs_table("info")
    expect_true(exists("info", envir = .bbs_cache))
    expect_true(all(c("tbl", "data.frame") %in% class(info)))
    expect_true(all(c("Package", "Version", "git_branch", "git_last_commit", "git_last_commit_date") %in% names(info)))
    expect_error(get_bbs_table("NotValidTable"))
    rm("info", envir = .bbs_cache)
})

test_that("get_all_bbs_tables_work", {

    if(exists("info", envir = .bbs_cache)) rm("info", envir = .bbs_cache)
    if(exists("propagation_status", envir = .bbs_cache)) rm("propagation_status", envir = .bbs_cache)
    if(exists("build_summary", envir = .bbs_cache)) rm("build_summary", envir = .bbs_cache)
    if(exists("info")) rm("info")
    if(exists("build_summary")) rm("build_summary")
    if(exists("propagation_status")) rm("propagation_status")
   
    expect_false(exists("info", envir = .bbs_cache))
    expect_false(exists("propagation_status", envir = .bbs_cache))
    expect_false(exists("build_summary", envir = .bbs_cache))
    expect_false(exists("propagation_status"))
    expect_false(exists("info"))
    expect_false(exists("build_summary"))   
    tmpTbls <- get_all_bbs_tables()
    expect_true(all(c("build_summary", "info", "propagation_status") %in% names(tmpTbls)))
    expect_true(exists("info", envir = .bbs_cache))
    expect_true(exists("propagation_status", envir = .bbs_cache))
    expect_true(exists("build_summary", envir = .bbs_cache))
    expect_error(get_all_bbs_tables("NotValid"))
    get_all_bbs_tables(assign_to_global=TRUE)
    expect_true(exists("propagation_status"))
    expect_true(exists("info"))
    expect_true(exists("build_summary"))
    expect_true(all(c("package", "node", "stage", "version", "status", "startedat", "endedat") %in% names(build_summary)))
   
   
})

test_that("remote_connections_available",{

    remote_url_base <- "https://mghp.osn.xsede.org/bir190004-bucket01/BiocBuildReports/"
    
    url_exists <- function(url) {
        con <- url(url)
        suppressWarnings({
            tryCatch({
                open(con)
                close(con)
                TRUE
            }, error = function(e) {
                try(close(con), silent = TRUE)
                FALSE
            })
        })
    }
    expect_error(url_exists("notthere"))
    expect_true(url_exists("https://bioconductor.org"))
    
    tbls <- c("build_summary", "info", "propagation_status")
    for(tbl in tbls){
        url <- paste0(remote_url_base, tbl, ".parquet")
        expect_true(url_exists(url))
    }
    
})
