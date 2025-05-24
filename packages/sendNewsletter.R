library(lubridate)
library(blastula)
library(dplyr)
library(readr)
library(here)
library(stringr)

source(here("packages", "scrapePackages.R"))

period <- format(Sys.Date() - 1, "%B %Y")

# Generate newsletter content
newsletter <- read_csv(here("packages", "packages.csv"), show_col_types = FALSE) |>
  filter(dev == "ox") |>
  select(!dev) |>
  createNewsletter(period = period)

# Strip title
body_md <- str_replace(newsletter, pattern = paste0("## ", period, "\n\n"), replacement = "")

# Load recipients
url <- "https://docs.google.com/spreadsheets/d/1FhUkmLhAvOFlqK54hJRzmZocGF1MS8hTLkn9Fpr1bSw/export?format=csv"
df <- readr::read_csv(url, show_col_types = FALSE)
emails <- unique(df$email)

subject <- paste0("Oxinfer Newsletter (", period, ")")

# Create SMTP credentials
email_creds <- blastula::creds_envvar(
  user = "oxinfer@gmail.com",
  pass_envvar = "OXINFER_GMAIL",
  host = "smtp.gmail.com",
  port = 587,
  use_ssl = TRUE
)

# Loop through and send
for (email in emails) {
  unsubscribe <- unsubscribeLink(email)
  
  body_content <- paste0(
    "Dear colleagues,\n\n",
    "Welcome to the latest edition of the **Oxinfer Newsletter**! Here's a summary of recent updates and activity in our packages — thank you for staying engaged with our work.\n\n",
    "---\n\n",
    body_md, "\n\n",
    "---\n\n",
    "Check our [**daily newsletter**](https://oxford-pharmacoepi.github.io/Oxinfer/packages/newsletter.html) for daily updates.\n\n",
    "Thank you for your continued support. If you have feedback, contributions, or suggestions, we'd love to hear from you.\n\n",
    "Until next time,\n\n",
    "The **Oxinfer Team**\n\n",
    "[https://oxford-pharmacoepi.github.io/Oxinfer/](https://oxford-pharmacoepi.github.io/Oxinfer/)\n\n",
    "---\n\n",
    "If you’d like to stop receiving this newsletter, you can [unsubscribe here](", unsubscribe, ")."
  )
  
  # Compose the message
  email_msg <- compose_email(
    body = md(body_content)
  )
  
  # Send the email
  tryCatch({
    smtp_send(
      email = email_msg,
      from = "oxinfer@gmail.com",
      to = email,
      subject = subject,
      credentials = email_creds
    )
  }, error = function(e) {
    message(sprintf("Failed to send to %s: %s", email, e$message))
  })
}
