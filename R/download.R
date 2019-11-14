#' Download Full Pacman Repository
#'
#' List and download all packages in a pacman repository. Useful for
#' mirroring the repository locally or to another server.
#'
#' @export
#' @rdname pkgsync
#' @name pkgsync
#' @aliases pacman
#' @param url full URL to the location of the repo
#' @param repo name of the repository db files. Usually this is equal
#' to the dirname.
#' @param destdir where to store downloaded files.
#' @examples \donttest{
#' pacman_download('http://dl.bintray.com/rtools/mingw32/')
#' pacman_download('http://dl.bintray.com/rtools/mingw64/')
#' }
pacman_ls <- function(url, repo = basename(url)){
  remote <- paste0(url, '/', repo, '.db.tar.xz')
  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  out <- file.path(tmp, basename(remote))
  curl::curl_download(remote, out)
  utils::untar(out, exdir = tmp)
  descfiles <- file.path(list.dirs(tmp, recursive = FALSE), 'desc')
  lists <- lapply(descfiles, pacman_parse)
  jsonlite:::simplify(lists)
}

#' @export
#' @rdname pkgsync
pacman_download <- function(url, repo = basename(url), destdir = basename(url)){
  url <- sub("/$", "", url)
  pkg_data <- pacman_ls(url = url, repo = repo)
  base_files <- paste0(repo, c(".db", ".files"), ".tar.xz")
  file_urls <- paste0(url, '/', c(pkg_data$filename, base_files))
  file_md5s <- pkg_data$md5sum
  dir.create(destdir, showWarnings = FALSE)
  for(i in seq_along(file_urls)){
    file_url = file_urls[i]
    path <- file.path(destdir, basename(file_url))
    curl::curl_download(file_url, path)
    if(!is.na(file_md5s[i])){
      md5 <- tools::md5sum(path)
      stopifnot(md5 == file_md5s[i])
    }
    cat(basename(file_url), " OK!\n", file = stderr())
  }
  return(pkg_data)
}

pacman_parse <- function(file){
  txt <- readLines(file)
  sets <- split(txt, cumsum(grepl("^%(.*)%$", txt)))
  names <- vapply(sets, function(x){
    tolower(sub("^%(.*)%$", "\\1", x[1]))
  }, character(1))
  values <- lapply(sets, function(x){
    Filter(nchar, x[-1])
  })
  structure(values, names = names)
}
