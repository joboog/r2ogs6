# runs and comparen_passed <- length(r2ogs6_passed[which(r2ogs6_passed == T)]) ogs6 benchmarks run with ogs6 as reference and r2ogs6

# git clone --depth 1 --branch 6.4.1 https://gitlab.opengeosys.org/ogs/ogs
# wget https://ogsstorage.blob.core.windows.net/binaries/ogs6/6.4.1/ogs-6.4.1-serial.sif

# utils
dir_make_overwrite <- function(path) {

    if (dir.exists(path)) {
    unlink(path, recursive = TRUE)
    }
    dir.create(path, recursive = TRUE)
}

# Setup -------------------------------------------------------------------
if (commandArgs(trailingOnly=TRUE)[2] == "ref" |
    commandArgs(trailingOnly=TRUE)[2 ]== "r2ogs6") {

    ogs_version <- commandArgs(trailingOnly=TRUE)[1]

    if (!dir.exists("/root/ogs")) {
        system2(
            command = c(
                "git",
                "clone",
                "--depth",
                "1",
                "--branch",
                ogs_version,
                "https://gitlab.opengeosys.org/ogs/ogs.git/",
                "/root/ogs"
            )
        )
    }
    if (!file.exists(paste0("/root/ogs-", ogs_version, "-serial.sif"))) {
        system2(
            command = c(
                "wget",
                "-nv", # no verbose
                "-P",
                "/root",
                paste0("https://ogsstorage.blob.core.windows.net/binaries/ogs6/",
                       ogs_version, "/ogs-", ogs_version, "-serial.sif")
            )
        )
    }
    devtools::load_all(".")
    library(dplyr)

    ogs_repo <- "/root/ogs/"
    prjs <- r2ogs6:::get_benchmark_paths(paste0(ogs_repo, "ProcessLib"))
    #prjs <- prjs[1:15] # for testing


    #### Exceptions #####
    # exclude some prj files
    prjs <- prjs[!grepl("\\.xml$", prjs)]
    prjs <- prjs[!grepl("TH2M/THM/slab/THM_1d_dirichlet.prj", prjs)]
    prjs <- prjs[!grepl("InvalidProjectFiles/", prjs)]
    #####################
    # print(prjs)
    ogs6_container <- paste0("/root/ogs-", ogs_version, "-serial.sif")

    # include call with --app ogs flag for ogs < 6.4.1
    if (as.numeric(gsub("[.]", "", ogs_version)) <= 640) {
        ogs6_command_str <- c("singularity", "exec", "--app", "ogs",
                              ogs6_container, "ogs")
    } else {
        ogs6_command_str <- c("singularity", "exec", ogs6_container, "ogs")
    }

}

# reference run -----------------------------------------------------------
if (commandArgs(trailingOnly=TRUE)[2] == "ref") {

    out_ref <- "-o /root/out_ref"

    dir_make_overwrite("/root/out_ref/logfiles")

    ref_exit <- tibble(benchmark = character(),
                       ref = numeric())
    for (prj in prjs) {

        print(paste0("Running benchmark ", prj))
        prj_path <- paste0(ogs_repo, "Tests/Data/", prj)

        log_ref <- paste0("> /root/out_ref/logfiles/",
                          sub(".prj",".txt", basename(prj)))

        out <- system2(command = ogs6_command_str,
                       args = c(prj_path, out_ref, log_ref))

        ref_exit <- add_row(ref_exit,
                            benchmark = prj,
                            ref = out)
    }

    save(ref_exit, file = "ref_exit.rda")
    dir_make_overwrite("out_ref")
    file.copy("/root/out_ref/logfiles", to = "out_ref", recursive = TRUE)
}

# test run with r2ogs6 ----------------------------------------------------
if (commandArgs(trailingOnly=TRUE)[2] == "r2ogs6") {

    test_exit <- tibble(benchmark = character(),
                        test = numeric())
    out_test <- "/root/out_test"
    dir_make_overwrite(paste0(out_test, "/logfiles"))

    for (prj in prjs) {
        print(paste0("Attempting to run benchmark ", prj))
        prj_path <- paste0(ogs_repo, "Tests/Data/", prj)
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
    save(test_exit, file = "test_exit.rda")
    dir_make_overwrite("out-test")
    file.copy("/root/out_test/logfiles", to = "out-test", recursive = TRUE)
}

if (commandArgs(trailingOnly=TRUE)[2] == "test") {

    install.packages("dplyr")
    load("ref_exit.rda")
    load("test_exit.rda")
    test_exit <- dplyr::full_join(ref_exit, test_exit, by = "benchmark")

    print(test_exit, n = nrow(test_exit))
    cat("\n\n Failed benchmarks:\n")
    # benchmarks that passed with & without r2ogs6
    r2ogs6_passed <- test_exit$test[which(test_exit$ref == 0)] == 0

    if(!all(r2ogs6_passed)) {
        cat(paste0(test_exit$benchmark[which(test_exit$ref == 0)][!r2ogs6_passed],
                     collapse = "\n"))
        cat("\n\n Exit codes for r2ogs6 were nonzero for above benchmarks!\n")
    }

    n_passed <- length(r2ogs6_passed[which(r2ogs6_passed == T)])
    percent_passed <- n_passed/length(r2ogs6_passed)*100
    cat(paste("\n\n Share of passed benchmarks: ", percent_passed, " %."))

}

print("job done")