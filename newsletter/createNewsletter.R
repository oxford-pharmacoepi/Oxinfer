
readInfo <- function(url, info) {
  pat <- Sys.getenv("GITHUB_PAT")
  x <- list()
  page <- 1
  query_params <- list(per_page = 100)
  
  if (info %in% c("pulls", "issues")) {
    query_params$state <- "all"
  }
  if (info == "commits") {
    query_params$since <- "2025-01-01"
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
      date   = as.Date(x$commit$author$date %||% character()),
      author = x$commit$author$name %||% character(),
      message = x$commit$message %||% character()
    )
  } else if (info == "issues") {
    dplyr::tibble(
      created_at = as.Date(x$created_at %||% character()),
      closed_at = as.Date(x$closed_at %||% character()),
      author = x$user$login %||% character(),
      comments = x$comments %||% numeric()
    )
  } else if (info == "pulls") {
    dplyr::tibble(
      created_at = as.Date(x$created_at %||% character()),
      merged_at = as.Date(x$merged_at %||% character()),
      target = x$base$ref %||% character(),
      origin = x$head$ref %||% character(),
      author = x$user$login %||% character()
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
        tag <- paste0("#", tolower(pkg), "-", gsub("\\.", "", ver))
        if (!is.na(rvest::html_element(page, tag))) {
          web <- paste0(website, tag)
          x$news[x$package_name == pkg & x$version == ver] <- web
        }
      }
    }
  }
  x
}
createNewsletter <- function(pkgs, period) {
  # get releases
  releases <- versionsDates(pkgs) |>
    addNews()
  
  # get commits
  commits <- getInfo(pkgs, "commits") |>
    dplyr::mutate(date = as.Date(date)) |>
    dplyr::group_by(package_name, date) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop")
  
  # get issues
  issues <- getInfo(pkgs, "issues") |>
    dplyr::mutate(created_at = as.Date(created_at),
                  closed_at = as.Date(closed_at))
  open_isses <- issues |>
    dplyr::rename(date = "created_at") |>
    dplyr::filter(!is.na(.data$date)) |>
    dplyr::group_by(package_name, date) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop")
  closed_isses <- issues |>
    dplyr::rename(date = "closed_at") |>
    dplyr::filter(!is.na(.data$date)) |>
    dplyr::group_by(package_name, date) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop")
  
  # get pulls
  pulls <- getInfo(pkgs, "pulls") |>
    dplyr::filter(target == "main") |>
    dplyr::mutate(merged_at = as.Date(merged_at)) |>
    dplyr::rename(date = "merged_at") |>
    dplyr::filter(!is.na(.data$date)) |>
    dplyr::group_by(package_name, date) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop")
  
  # period
  period <- dplyr::tibble(period = period) |>
    # get start and end
    dplyr::mutate(
      start = as.Date(paste(.data$period, "1"), format = "%B %Y %d"),
      end = as.Date(cut(.data$start + 31, "month")) - 1
    ) 
  
  # activity
  x <- period |>
    dplyr::cross_join(
      pkgs |> 
        dplyr::select(package_name)
    )
  activity <- x |>
    # number of commits
    dplyr::inner_join(
      x |>
        dplyr::full_join(commits, by = "package_name", relationship = "many-to-many") |>
        dplyr::group_by(.data$period, .data$package_name) |>
        dplyr::summarise(commits = dplyr::coalesce(sum(.data$n[.data$date >= .data$start & .data$date <= .data$end]), 0), .groups = "drop"),
      by = c("period", "package_name")
    ) |>
    # opened of issues
    dplyr::inner_join(
      x |>
        dplyr::full_join(open_isses, by = "package_name", relationship = "many-to-many") |>
        dplyr::group_by(.data$period, .data$package_name) |>
        dplyr::summarise(open_isses = dplyr::coalesce(sum(.data$n[.data$date >= .data$start & .data$date <= .data$end]), 0), .groups = "drop"),
      by = c("period", "package_name")
    ) |>
    # closed of issues
    dplyr::inner_join(
      x |>
        dplyr::full_join(closed_isses, by = "package_name", relationship = "many-to-many") |>
        dplyr::group_by(.data$period, .data$package_name) |>
        dplyr::summarise(closed_isses = dplyr::coalesce(sum(.data$n[.data$date >= .data$start & .data$date <= .data$end]), 0), .groups = "drop"),
      by = c("period", "package_name")
    ) |>
    # pull requests
    dplyr::inner_join(
      x |>
        dplyr::full_join(pulls, by = "package_name", relationship = "many-to-many") |>
        dplyr::group_by(.data$period, .data$package_name) |>
        dplyr::summarise(pulls = dplyr::coalesce(sum(.data$n[.data$date >= .data$start & .data$date <= .data$end]), 0), .groups = "drop"),
      by = c("period", "package_name")
    ) |>
    dplyr::mutate(
      activity = commits + open_isses + closed_isses + pulls,
      activity_label = dplyr::case_when(
        activity >= 40 ~ "On fire",
        activity >= 20 ~ "Active",
        activity > 0 ~ "Quiet",
        activity == 0 ~ "No activity"
      ),
      activity_emoji = dplyr::case_when(
        activity_label == "On fire" ~ "🔥",
        activity_label == "Active" ~ "✨",
        activity_label == "Quiet" ~ "🧊",
        activity_label == "No activity" ~ "❄️️"
      ),
      activity_order = dplyr::case_when(
        activity_label == "On fire" ~ 1,
        activity_label == "Active" ~ 2,
        activity_label == "Quiet" ~ 3,
        activity_label == "No activity" ~ 4
      )
    ) |>
    dplyr::group_by(.data$period, .data$activity_label, .data$activity_emoji, .data$activity_order) |>
    dplyr::summarise(packages = paste0(package_name, collapse = ", "), .groups = "drop") |>
    dplyr::arrange(.data$activity_order) |>
    dplyr::mutate(activity = paste0("* ", .data$activity_emoji, " **", .data$activity_label, "**: ", .data$packages, ".")) |>
    dplyr::group_by(.data$period) |>
    dplyr::summarise(activity = paste0(.data$activity, collapse = "\n"), .groups = "drop")
  
  # releases
  released <- releases |>
    dplyr::cross_join(period) |>
    dplyr::filter(.data$date >= .data$start & .data$date <= .data$end) |>
    dplyr::mutate(release = paste0("* *", .data$date, "* **", .data$package_name, "** ", .data$version, dplyr::if_else(
      is.na(.data$news), "", paste0(" [changelog](", .data$news, ")")
    ))) |>
    dplyr::arrange(dplyr::desc(.data$date)) |>
    dplyr::group_by(.data$period) |>
    dplyr::summarise(releases = paste0(.data$release, collapse = "\n"))
  
  # formatting
  period |>
    dplyr::left_join(released, by = "period") |>
    dplyr::left_join(activity, by = "period") |>
    dplyr::mutate(
      message_releases = dplyr::if_else(is.na(.data$releases), "", paste0(
        "\n\n### Releases\n\n", .data$releases
      )),
      message_activity = dplyr::if_else(is.na(.data$activity), "", paste0(
        "\n\n### Activity\n\n", .data$activity
      )),
      message = paste0("## ", .data$period, .data$message_releases, .data$message_activity)
    ) |>
    dplyr::pull(message) |>
    paste0(collapse = "\n\n")
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
    cat(x$activity)
    cat("\n\n")
  }
}
unsubscribeLink <- function(email) {
  paste0(
    "https://script.google.com/macros/s/AKfycbwPL438sqcd6WOqiKUMeD0OsT0QoSUAU5efR3Tj6rjggtdAoU8JqJjIEJnOJPjkB-8/exec?action=unsubscribe&email=",
    email
  )
}
