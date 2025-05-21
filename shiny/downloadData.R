
source(here::here("packages", "scrapePackages.R"))
allPkgs <- readr::read_csv(here::here("packages", "packages.csv"), show_col_types = FALSE)
pkgs <- allPkgs |>
  dplyr::filter(.data$dev == "ox") |>
  dplyr::select(!"dev")
commits <- getInfo(pkgs, "commits")
issues <- getInfo(pkgs, "issues")
pulls <- getInfo(pkgs, "pulls")
releases <- versionsDates(pkgs)
downloads <- cranlogs::cran_downloads(packages = pkgs$package_name, from = "2023-01-01")
dependencies <- getDependencies(pkgs)
activity <- commits |>
  dplyr::mutate(date = as.Date(.data$date)) |>
  dplyr::group_by(.data$package_name, .data$date) |>
  dplyr::summarise(count = dplyr::n(), .groups = "drop") |>
  dplyr::mutate(activity = "commits") |>
  dplyr::union_all(
    issues |>
      dplyr::rename(date = "created_at") |>
      dplyr::mutate(date = as.Date(.data$date)) |>
      dplyr::group_by(.data$package_name, .data$date) |>
      dplyr::summarise(count = dplyr::n(), .groups = "drop") |>
      dplyr::mutate(activity = "new issues")
  ) |>
  dplyr::union_all(
    issues |>
      dplyr::filter(!is.na(.data$closed_at)) |>
      dplyr::rename(date = "closed_at") |>
      dplyr::mutate(date = as.Date(.data$date)) |>
      dplyr::group_by(.data$package_name, .data$date) |>
      dplyr::summarise(count = dplyr::n(), .groups = "drop") |>
      dplyr::mutate(activity = "closed issues")
  ) |>
  dplyr::union_all(
    pulls |>
      dplyr::filter(.data$target == "main", !is.na(.data$merged_at)) |>
      dplyr::rename(date = "merged_at") |>
      dplyr::mutate(date = as.Date(.data$date)) |>
      dplyr::group_by(.data$package_name, .data$date) |>
      dplyr::summarise(count = dplyr::n(), .groups = "drop") |>
      dplyr::mutate(activity = "pull requests")
  ) |>
  dplyr::union_all(
    downloads |>
      dplyr::rename(package_name = "package") |>
      dplyr::mutate(activity = "downloads")
  )
x <- tidyr::expand_grid(
  package_name = pkgs$package_name,
  date = seq.Date(from = as.Date("2023-01-01"), as.Date(Sys.Date()), by = 1),
  activity = unique(activity$activity)
)
activity <- activity |>
  dplyr::right_join(x, by = c("package_name", "date", "activity")) |>
  dplyr::mutate(count = dplyr::coalesce(.data$count, 0))
activity <- activity |>
  dplyr::union_all(
    activity |>
      dplyr::filter(.data$activity == "new issues") |>
      dplyr::rename(new = "count") |>
      dplyr::select(!"activity") |>
      dplyr::inner_join(
        activity |>
          dplyr::filter(.data$activity == "closed issues") |>
          dplyr::rename(closed = "count") |>
          dplyr::select(!"activity"),
        by = c("package_name", "date")
      ) |>
      dplyr::group_by(.data$package_name) |>
      dplyr::arrange(.data$date) |>
      dplyr::mutate(
        cum_open = cumsum(.data$new),
        cum_closed = cumsum(.data$closed),
        count = .data$cum_open - .data$cum_closed
      ) |>
      dplyr::select(!c("new", "closed", "cum_open", "cum_closed")) |>
      dplyr::mutate(activity = "open issues") |>
      dplyr::ungroup()
  )

save(pkgs, activity, commits, issues, pulls, releases, downloads, dependencies, file = here::here("shiny", "shinyData.RData"))
