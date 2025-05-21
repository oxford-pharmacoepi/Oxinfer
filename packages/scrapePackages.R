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
getFirstRelease <- function(name) {
  x <- tryCatch(
    {
      x <- "https://cran.r-project.org/src/contrib/Archive/{name}/" |>
        glue::glue() |>
        readLines()
      x <- x[grepl("align=\"right\">", x)][2]
      id <- stringr::str_locate(x, "align=\"right\">")
      substr(x, id[2]+1, id[2]+10) |>
        as.Date("%Y-%m-%d") |>
        format("%d_%b_%y")
    },
    error = function(cond) {
      return("not_published")
    }
  )
  return(x)
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
readDescription <- function(pkg, org) {
  pat <- Sys.getenv("GITHUB_PAT")
  url <- paste0("https://raw.githubusercontent.com/", org, "/", pkg, "/refs/heads/main/DESCRIPTION")
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
    getLastRelease(pkg)
  ) |>
    paste0(collapse = "\n")
}
readInfo <- function(url, info) {
  pat <- Sys.getenv("GITHUB_PAT")
  x <- list()
  page <- 1
  query_params <- list(per_page = 100)
  
  if (info %in% c("pulls", "issues")) {
    query_params$state <- "all"
  }
  
  while (TRUE) {
    query_params$page <- page
    xx <- httr::GET(
      url = url, 
      query = query_params, 
      config = httr::add_headers(Authorization = paste("token", pat))
    )$content |>
      rawToChar() |>
      jsonlite::fromJSON() |>
      formatInfo(info)
    
    if (nrow(xx) == 0) break
    
    x[[page]] <- xx
    page <- page + 1
  }
  
  dplyr::bind_rows(x)
}
formatInfo <- function(x, info) {
  if (info == "commits") {
    dplyr::tibble(
      date   = x$commit$author$date,
      author = x$commit$author$name
    )
  } else if (info == "issues") {
    dplyr::tibble(
      created_at = x$created_at,
      closed_at = x$closed_at
    )
  } else if (info == "pulls") {
    dplyr::tibble(
      created_at = x$created_at,
      merged_at = x$merged_at,
      target = x$base$ref,
      origin = x$head$ref
    )
  }
}
getInfo <- function(pkgs, info) {
  pkgs |>
    purrr::pmap(\(package_name, organisation) {
      if (organisation == "darwin-eu") {
        organisation <- "darwin-eu-dev"
      }
      commits <- paste0("https://api.github.com/repos/", organisation, "/", package_name, "/", info) |>
        readInfo(info) |>
        dplyr::mutate(package_name = .env$package_name)
    }) |>
    dplyr::bind_rows()
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
    page <- rvest::read_html(archive_url)
    
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
addNews <- function(x) {
  pkgs <- unique(x$package_name)
  x <- x |>
    dplyr::mutate(news = NA_character_)
  for (pkg in pkgs) {
    org <- unique(x$organisation[x$package_name == pkg])
    website <- paste0("https://", org, ".github.io/", pkg, "/news/index.html")
    page <- tryCatch(
      rvest::read_html(website),
      error = function(e) return(NULL)
    )
    if (!is.null(page)) {
      versions <- x$version[x$package_name == pkg]
      for (ver in versions) {
        tag <- paste0("#", pkg, "-", gsub("\\.", "", ver))
        if (!is.na(rvest::html_element(page, tag))) {
          web <- paste0(website, tag)
          x$news[x$package_name == pkg & x$version == ver] <- web
        }
      }
    }
  }
  x
}
formatNewsletter <- function(newsletter) {
  for (x in newsletter) {
    x$releases <- x$releases |>
      dplyr::mutate(message = paste0(
        "* ", .data$date, " **", .data$package_name, "** *", .data$version, "*",
        dplyr::if_else(is.na(.data$news), "", paste0(" [changelog](", .data$news, ")"))
      ))
    
    # title
    cat("##", x$title, "\n\n")
    
    # releases
    cat("### Releases\n\n")
    cat(paste0(x$releases$message, collapse = "\n"), "\n\n")

    # activity
    cat("### Activity\n\n")
    print(knitr::kable(x$activity))
    cat("\n\n")
  }
}
