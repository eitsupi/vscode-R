# Shared by ordinary sess installation and the private Interactive runtime.
sess_install_command <- function(package, library) {
    status <- system2(file.path(R.home("bin"), "R"),
                      c("CMD", "INSTALL", "--clean", shQuote(paste0("--library=", library)), shQuote(package)))
    identical(as.integer(status), 0L)
}

sess_install_dependencies <- function(packages, library, repos) {
    utils::install.packages(packages, lib = library, repos = repos)
}

sess_verify_package <- function(library, required, expected_revision, interactive) {
    # Verification must not accidentally use an older namespace already loaded
    # in the terminal from which Attach was invoked.
    code <- paste(
        "a <- commandArgs(TRUE); ns <- loadNamespace('sess', lib.loc=a[1]);",
        "stopifnot(normalizePath(getNamespaceInfo(ns,'path')) == normalizePath(file.path(a[1],'sess')));",
        "stopifnot(utils::compareVersion(as.character(utils::packageVersion('sess',lib.loc=a[1])),a[2]) >= 0);",
        "stopifnot(identical(",
        "utils::packageDescription('sess',lib.loc=a[1],fields='Config/vscode-R/source-revision'),",
        "a[3]));",
        "stopifnot(all(c('connect','notify_client','request_client') %in% getNamespaceExports(ns)));",
        "if (a[4] == 'TRUE') {",
        "stopifnot(all(c('interactive_stop','display') %in% getNamespaceExports(ns)));",
        "stopifnot(all(vapply(c('interactive_start','interactive_execute'),",
        "function(n) exists(n, ns, mode='function', inherits=FALSE), FALSE)));",
        "stopifnot(utils::packageDescription('sess',lib.loc=a[1],fields='Config/vscode-R/Interactive') == '1');",
        "stopifnot(utils::packageDescription('sess',lib.loc=a[1],fields='NeedsCompilation') == 'no'); }"
    )
    status <- system2(file.path(R.home("bin"), "R"),
                      c("--vanilla", "--slave", "-e", shQuote(code), "--args",
                        shQuote(library), shQuote(required), shQuote(expected_revision), as.character(interactive)))
    if (!identical(as.integer(status), 0L)) stop("The installed sess package failed its compatibility/load check.")
}

# Pass the caller's library search path to installation/verification children,
# including project libraries, without sourcing the caller's startup profile.
sess_set_install_environment <- function(library) {
    previous <- Sys.getenv(c("R_LIBS", "R_PROFILE_USER", "R_ENVIRON_USER"),
                           unset = NA_character_, names = TRUE)
    Sys.setenv(R_LIBS = paste(unique(c(library, .libPaths())), collapse = .Platform$path.sep),
               R_PROFILE_USER = "", R_ENVIRON_USER = "")
    previous
}

sess_restore_install_environment <- function(previous) {
    Sys.unsetenv(names(previous)[is.na(previous)])
    if (any(!is.na(previous))) do.call(Sys.setenv, as.list(previous[!is.na(previous)]))
}

# Install only the given dependencies beside an already prepared managed sess.
sess_install_missing_dependencies <- function(packages, library, repos) {
    previous <- sess_set_install_environment(library)
    on.exit(sess_restore_install_environment(previous), add = TRUE)
    sess_install_dependencies(packages, library, repos)
    still_missing <- packages[!nzchar(vapply(packages, function(package) {
        system.file(package = package, lib.loc = unique(c(.libPaths(), library)))
    }, ""))]
    if (length(still_missing)) {
        stop("Could not install: ", paste(still_missing, collapse = ", "))
    }
    invisible(TRUE)
}

sess_install <- function(pkg_path, library, repos, interactive = FALSE) {
    description <- read.dcf(file.path(pkg_path, "DESCRIPTION"))
    required <- description[1L, "Version"]
    expected_revision <- description[1L, "Config/vscode-R/source-revision"]

    previous <- sess_set_install_environment(library)
    on.exit(sess_restore_install_environment(previous), add = TRUE)

    deps <- if ("Imports" %in% colnames(description)) description[1L, "Imports"] else ""
    deps <- trimws(gsub("\\s*\\(.*\\)", "", unlist(strsplit(deps, ","))))
    installed <- utils::installed.packages(lib.loc = unique(c(library, .libPaths())))
    missing <- setdiff(deps[nzchar(deps)], rownames(installed))
    if (length(missing)) {
        sess_install_dependencies(missing, library, repos)
    }
    message("Installing bundled sess from: ", pkg_path)
    if (!sess_install_command(pkg_path, library)) stop("Could not install bundled sess.")
    sess_verify_package(library, required, expected_revision, interactive)
    invisible(TRUE)
}
