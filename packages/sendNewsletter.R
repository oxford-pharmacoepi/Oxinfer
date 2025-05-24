
library(lubridate)
source(here::here("packages", "scrapePackages.R"))

period <- format(Sys.Date() - 1, "%B %Y")

# create newsletter
newsletter <- readr::read_csv(here::here("packages", "packages.csv"), show_col_types = FALSE) |>
  dplyr::filter(.data$dev == "ox") |>
  dplyr::select(!"dev") |>
  createNewsletter(period = period)

# remove title
x <- stringr::str_replace(string = newsletter, pattern = paste0("## ", period, "\n\n"), replacement = "")

# read emails
url <- paste0("https://docs.google.com/spreadsheets/d/1FhUkmLhAvOFlqK54hJRzmZocGF1MS8hTLkn9Fpr1bSw/export?format=csv")
df <- read.csv(url)
emails <- unique(df$email)

smtp <- emayili::server(
  host = "smtp.gmail.com",
  port = 587,
  username = "oxinfer@gmail.com",
  password = Sys.getenv("OXINFER_GMAIL"),
  use_ssl = TRUE
)

subject <- paste0("Oxinfer Newsletter (", period, ")")

for (email in emails) {
  # content of the email
  content <- paste0(
    "Dear colleagues,\n\n",
    "Welcome to the latest edition of the **Oxinfer Newsletter**! Here's a summary of recent updates and activity in our packages — thank you for staying engaged with our work.\n\n",
    "---\n\n",
    x, "\n\n",
    "---\n\n",
    "Check our [**daily newsletter**](https://oxford-pharmacoepi.github.io/Oxinfer/packages/newsletter.html) for daily updates.\n\n",
    "Thank you for your continued support. If you have feedback, contributions, or suggestions, we'd love to hear from you. Remember we always welcome issues in our packages.\n\n",
    "Until next time,\n\n",
    "The **Oxinfer Team**\n\n",
    "[https://oxford-pharmacoepi.github.io/Oxinfer/](https://oxford-pharmacoepi.github.io/Oxinfer/)\n\n",
    "---\n\n",
    "If you’d like to stop receiving this newsletter, you can [unsubscribe here](", 
    unsubscribeLink(email),
    ")."
  )
  
  # create email
  toSend <- emayili::envelope() |>
    emayili::from(addr = "oxinfer@gmail.com") |>
    emayili::to(email) |>
    emayili::subject(subject = subject) |>
    emayili::html(commonmark::markdown_html(content))
  
  # send email
  smtp(toSend)
}
