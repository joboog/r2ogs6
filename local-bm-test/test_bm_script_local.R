# runs and compares ogs6 benchmarks run with ogs6 as reference and r2ogs6
# a call from the cli requires to give arguments
# 1st arg: ogs version e.g. "6.4.1"
# 2nd arg: "ref" or "r2ogs" or "compare"
# 3rd arg: the date of the test runs in "yyyy-mm-dd"


packrat::packrat_mode(on=T)
devtools::load_all(".")
library(dplyr)

# utils
dir_make_overwrite <- function(path) {

    if (dir.exists(path)) {
    unlink(path, recursive = TRUE)
    }
    dir.create(path, recursive = TRUE)
}

# Setup -------------------------------------------------------------------
basedir <- "tmp/benchmarks-test"
resultsdir <- "local-bm-test"
test_date <- commandArgs(trailingOnly=TRUE)[3]

if (commandArgs(trailingOnly=TRUE)[2] == "ref" |
    commandArgs(trailingOnly=TRUE)[2 ]== "r2ogs6") {

    ogs_version <- commandArgs(trailingOnly=TRUE)[1]
    ogs_repo <- paste0(basedir,"/ogs")

    if (!dir.exists(ogs_repo)) {
        system2(
            command = c(
                "git",
                "clone",
                "--depth",
                "1",
                "--branch",
                ogs_version,
                "https://gitlab.opengeosys.org/ogs/ogs.git/",
                ogs_repo
            )
        )
    }
    if (!file.exists(paste0(ogs_repo, "-", ogs_version, "-serial.sif"))) {
        system2(
            command = c(
                "wget",
                "-nv", # no verbose
                "-P",
                basedir,
                paste0("https://ogsstorage.blob.core.windows.net/binaries/ogs6/",
                       ogs_version, "/ogs-", ogs_version, "-serial.sif")
            )
        )
    }

    prjs <- r2ogs6:::get_benchmark_paths(paste0(ogs_repo, "/ProcessLib"))
    #prjs <- prjs[1:15] # for testing
    #### Exceptions #####
    # exclude some prj files
    prjs <- prjs[!grepl("\\.xml$", prjs)]
    prjs <- prjs[!grepl("TH2M/THM/slab/THM_1d_dirichlet.prj", prjs)]
    prjs <- prjs[!grepl("InvalidProjectFiles/", prjs)]
    #####################
    # print(prjs)

    # make ogs call
    ogs6_container <- paste0(basedir, "/ogs-", ogs_version, "-serial.sif")
    singularity_opts <- unlist(options("r2ogs6.default_singularity_opts"))
    if(is.null(singularity_opts)){
        singularity_opts <- ""
    }
    # include call with --app ogs flag for ogs < 6.4.1
    if (as.numeric(gsub("[.]", "", ogs_version)) <= 640) {
        ogs6_command_str <- c("singularity", "exec", singularity_opts,
                              "--app", "ogs", ogs6_container, "ogs")
    } else {
        ogs6_command_str <- c("singularity", "exec", singularity_opts,
                              ogs6_container, "ogs")
    }
    #  reorder for using 'system2()'
    if (length(ogs6_command_str)>1) {
        ogs6_sing_args <- ogs6_command_str[-1]
        ogs6_command_str <- ogs6_command_str[1]
    } else {
        ogs6_command_str <- ogs6_command
    }

}

# reference run -----------------------------------------------------------
if (commandArgs(trailingOnly=TRUE)[2] == "ref") {

    out_ref <- paste0("-o ", basedir, "/out_ref")
    dir_make_overwrite(paste0(basedir, "/out_ref"))
    dir_make_overwrite(paste0(basedir, "/out_ref/logfiles"))
    ref_exit <- tibble(benchmark = character(),
                       ref = numeric())
    for (prj in prjs[1:5]) {

        print(paste0("Running benchmark ", prj))
        prj_path <- paste0(ogs_repo, "/Tests/Data/", prj)

        log_ref <- paste0("> ", basedir, "/out_ref/logfiles/",
                          sub(".prj",".txt", basename(prj)))


        out <- system2(command = ogs6_command_str,
                       args = c(ogs6_sing_args, prj_path, out_ref, log_ref))

        ref_exit <- add_row(ref_exit,
                            benchmark = prj,
                            ref = out)
    }

    ref_exit$ogs <- ogs_version
    ref_exit$date <- test_date
    save(ref_exit, file = paste0(basedir, "/ref_exit_", test_date,".rda"))
}

# test run with r2ogs6 ----------------------------------------------------
if (commandArgs(trailingOnly=TRUE)[2] == "r2ogs6") {

    test_exit <- tibble(benchmark = character(),
                        test = numeric())
    out_test <- paste0(basedir, "/out_test")
    dir_make_overwrite(out_test)
    dir_make_overwrite(paste0(out_test, "/logfiles"))

    for (prj in prjs[1:5]) {
        print(paste0("Attempting to run benchmark ", prj))
        prj_path <- paste0(ogs_repo, "/Tests/Data/", prj)
        out <- tryCatch({
            r2ogs6:::run_benchmark(
                prj_path = prj_path,
                ogs6_bin_path = ogs6_container,
                sim_path = out_test
            )
        },
        error = function(e) {
            message("\nrun_benchmark() failed!\nOriginal error message:")
            message(e)
        })

        test_exit <- add_row(test_exit,
                            benchmark = prj,
                            test = out)
    }
    test_exit$test[which(is.na(test_exit$test))] <- 99
    test_exit$ogs <- ogs_version
    test_exit$date <- test_date
    save(test_exit, file = paste0(basedir, "/test_exit_", test_date,".rda"))
}

# compare exit codes -----------------------------------------------------
if (commandArgs(trailingOnly=TRUE)[2] == "compare") {

    load(paste0(basedir, "/ref_exit_", test_date,".rda"))
    load(paste0(basedir, "/test_exit_", test_date,".rda"))
    compare_exit <- dplyr::full_join(ref_exit, test_exit, by = "benchmark")

    print(compare_exit, n = nrow(compare_exit))
    cat("\n\n Failed benchmarks:\n")
    # benchmarks that passed with & without r2ogs6
    r2ogs6_passed <- compare_exit$test[which(compare_exit$ref == 0)] == 0

    if(!all(r2ogs6_passed)) {
        cat(paste0(compare_exit$benchmark[which(compare_exit$ref == 0)][!r2ogs6_passed],
                     collapse = "\n"))
        cat("\n\n Exit codes for r2ogs6 were nonzero for above benchmarks!\n")
    }

    n_passed <- length(r2ogs6_passed[which(r2ogs6_passed == T)])
    percent_passed <- n_passed/length(r2ogs6_passed)*100
    cat(paste("\n\n Share of passed benchmarks: ", percent_passed, " %.\n"))

    save(compare_exit,
         file = paste0(resultsdir, "/compare_exit_", test_date,".rda"))
}

print("job done")