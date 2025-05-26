hexsticker <- function(pkg, org) {
  web <- "https://github.com/{org}/{pkg}/blob/main/man/figures/logo.png?raw=true" |>
    glue::glue()
  if (RCurl::url.exists(web)) {
    web <- '<img src="{web}" alt="{pkg}" style="height: 100px;">' |>
      glue::glue()
  } else {
    web <- NULL
  }
  web
}
repo <- function(pkg, org) {
  paste0("https://github.com/", org, "/", pkg, "/")
}
open_issue <- function(pkg, org) {
  '<a href="{repo(pkg, org)}issues/new/choose"><img src="https://img.shields.io/badge/report_issue-f6f6f6?logo=github&logoColor=black" class="img-fluid" alt="report_issue"></a>' |>
    glue::glue()
}
website <- function(pkg, org) {
  '<a href="https://{org}.github.io/{pkg}/"><img src="https://img.shields.io/badge/documentation-b3d9cf?logo=gitbook&logoColor=black" class="img-fluid" alt="documentation"></a>' |>
    glue::glue()
}
is_on_cran <- function(pkg) {
  pkg %in% rownames(available.packages())
}
manual <- function(pkg) {
  if (is_on_cran(pkg)) {
    x <- paste0("https://cran.r-project.org/web/packages/", pkg, "/", pkg, ".pdf")
    '<a href="{x}"><img src="https://img.shields.io/badge/manual-1E90FF?logo=r&logoColor=black" class="img-fluid" alt="manual"></a>' |>
      glue::glue() |>
      as.character()
  } else {
    NULL
  }
}
getVersion <- function(pkg) {
  paste0(
    '[<img src="https://www.r-pkg.org/badges/version/', pkg, 
    '" alt="CRAN version badge">](https://CRAN.R-project.org/package=', pkg, ")"
  )
}
getLastRelease <- function(pkg) {
  if (is_on_cran(pkg)) {
    link <- paste0("https://CRAN.R-project.org/package=", pkg)
    x <- readLines(link)
    id <- which(x == "<td>Published:</td>")
    x <- substr(x[id + 1], 5, 14) |>
      as.Date("%Y-%m-%d") |>
      format("%d_%b_%y")
  } else {
    link <- ""
    x <- "not_published"
  }
  paste0(
    "[![last release](https://img.shields.io/badge/last_release-", x,
    "-blue.svg)](https://CRAN.R-project.org/package=", pkg, ")"
  )
}
getFirstRelease <- function(pkg) {
  if (is_on_cran(pkg)) {
    x <- versionsDates(dplyr::tibble(package_name = pkg)) |>
      dplyr::pull("date") |>
      min() |>
      format("%d_%b_%y")
    link <- paste0("https://CRAN.R-project.org/package=", pkg)
  } else {
    link <- ""
    x <- "not_published"
  }
  paste0(
    "[![first release](https://img.shields.io/badge/first_release-", x,
    "-red.svg)](https://CRAN.R-project.org/package=", pkg, ")"
  )
}
createGrid <- function(hex, life, cran, first, last, web, issue) {
  '<div class="parent">
    <div class="div1"> {hex} </div>
    <div class="div2"> 
    <div class="div3"> {life} </div>
    <div class="div3"> {cran} </div>
    <div class="div3"> {first} </div>
    <div class="div3"> {last} </div>
    <div class="div3"> {web} </div>
    <div class="div3"> {issue} </div>
    </div>
  </div>' |>
    glue::glue()
}
readDescription <- function(package_name, organisation) {
  pat <- Sys.getenv("GITHUB_PAT")
  url <- paste0("https://raw.githubusercontent.com/", organisation, "/", package_name, "/refs/heads/main/DESCRIPTION")
  description <- httr::GET(url, httr::add_headers(Authorization = paste("token", pat))) |>
    httr::content(as = "text", encoding = "UTF-8")
  as.list(read.dcf(textConnection(description))[1,])
}
summarisePackage <- function(pkg, org) {
  # read description
  description <- readDescription(pkg, org)
  
  c(
    # pkg name
    paste0("### ", pkg), "",
    # hexsticker
    hexsticker(pkg, org),
    # title
    paste0("**", description$Title, "**"), "",
    # description
    description$Description, "",
    # website
    website(pkg, org),
    # report issue
    open_issue(pkg, org),
    # manual
    manual(pkg),
    # version
    getVersion(pkg),
    # last release
    getLastRelease(pkg),
    # first release
    getFirstRelease(pkg)
  ) |>
    paste0(collapse = "\n")
}
versionsDates <- function(pkgs) {
  # Get current CRAN info
  cran_url <- "https://cran.r-project.org"
  options(repos = c(CRAN = cran_url))
  x <- tools::CRAN_package_db() |>
    dplyr::as_tibble() |>
    dplyr::select(package_name = "Package", version = "Version", date = "Published") |>
    dplyr::filter(.data$package_name %in%.env$pkgs$package_name) |>
    dplyr::mutate(date = as.Date(.data$date))
  
  for (pkg in x$package_name) {
    # Get archive info
    archive_url <- sprintf("https://cran.r-project.org/src/contrib/Archive/%s/", pkg)
    max_attempts <- 5
    attempt <- 1
    repeat {
      result <- tryCatch(
        {
          page <- rvest::read_html(archive_url)
          break  # Exit loop on success
        },
        error = function(e) {
          message(sprintf("Attempt %d failed: %s", attempt, e$message))
          if (attempt >= max_attempts) {
            stop("Failed to connect after ", max_attempts, " attempts.")
          }
          attempt <<- attempt + 1
          Sys.sleep(10)  # Wait before retrying
          NULL
        }
      )
    }
    
    # Extract table rows
    rows <- rvest::html_elements(page, "table tr") |>
      as.character() |>
      purrr::keep(\(x) grepl("/icons/compressed.gif", x))
    version <- stringr::str_match(rows, paste0(">", pkg, "_(.*)\\.tar\\.gz<"))[, 2]
    date <- as.Date(stringr::str_match(rows, "align=\"right\">(.*)</td>")[, 2])
    
    x <- x |>
      dplyr::union_all(dplyr::tibble(
        package_name = pkg, version = version, date = date
      ))
  }
  
  x |>
    dplyr::inner_join(pkgs, by = "package_name") |>
    dplyr::arrange(.data$package_name, .data$version)
}
