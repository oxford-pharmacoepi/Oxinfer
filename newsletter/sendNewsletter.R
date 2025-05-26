
renv::status()
renv::restore()

source(here::here("createNewsletter.R"))

period <- format(Sys.Date() - 1, "%B %Y")

# Generate newsletter content
newsletter <- readr::read_csv(here::here("packages.csv"), show_col_types = FALSE) |>
  dplyr::filter(dev == "ox") |>
  dplyr::select(!dev) |>
  createNewsletter(period = period)

# Create newsletter
body_content <- paste0(
  "Dear colleagues,\n\n",
  "Welcome to the latest edition of the **Oxinfer Newsletter**! Here's a summary of recent updates and activity in our packages — thank you for staying engaged with our work.\n\n",
  "---\n\n",
  stringr::str_replace(newsletter, pattern = paste0("## ", period, "\n\n"), replacement = ""), "\n\n",
  "---\n\n",
  "Check our [**daily newsletter**](https://oxford-pharmacoepi.github.io/Oxinfer/packages/newsletter.html) for daily updates.\n\n",
  "Thank you for your continued support. If you have feedback, contributions, or suggestions, we'd love to hear from you.\n\n",
  "Until next time,\n\n",
  "The **Oxinfer Team**\n\n",
  "[https://oxford-pharmacoepi.github.io/Oxinfer/](https://oxford-pharmacoepi.github.io/Oxinfer/)"
)

# Load recipients
if (identical(Sys.getenv("TEST_RUN"), "true")) {
  # send to oxinfer email for test runs
  emails <- "oxinfer@gmail.com"
} else { 
  url <- "https://docs.google.com/spreadsheets/d/1FhUkmLhAvOFlqK54hJRzmZocGF1MS8hTLkn9Fpr1bSw/export?format=csv"
  df <- readr::read_csv(url, show_col_types = FALSE)
  emails <- unique(df$email)
}

emails <- emails[!is.na(emails)]

subject <- paste0("Oxinfer Newsletter (", period, ")")

# Create SMTP credentials
email_creds <- blastula::creds_envvar(
  user = "oxinfer@gmail.com",
  pass_envvar = "OXINFER_GMAIL",
  provider = "gmail"
)

options(blastula.verbose = TRUE)
# Loop through and send
for (email in emails) {
  
  # Compose the message
  unsubscribe <- unsubscribeLink(email) # unsubscribe is different for each email
  email_msg <- blastula::compose_email(
    body = blastula::md(body_content), 
    footer = blastula::md(paste0("If you’d like to stop receiving this newsletter, you can [unsubscribe here](", unsubscribe, ")."))
  )
  
  # send email
  tryCatch({
    blastula::smtp_send(
      email = email_msg,
      from = "oxinfer@gmail.com",
      to = email,
      subject = subject,
      credentials = email_creds,
      verbose = TRUE
    )
  }, error = function(e) {
    message("SMTP error: ", conditionMessage(e))
  })

}
