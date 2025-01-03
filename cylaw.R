### Package loading --------------------------------------------------------------
# List of required packages
packages <- c("tidyverse", "rvest", "stringi", "vroom", "progress", "lubridate")

# Function to check, install, and load packages
check_install_load <- function(pkgs) {
  for (pkg in pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg)  # Install the package if not already installed
    }
    library(pkg, character.only = TRUE)  # Load the package
  }
}

# Run the function
check_install_load(packages)
################################################################################


# Number of record to process at a time 
n=30

link ="https://www.cylaw.org/updates.html"
page = read_html(link) |> as.character()

# Regex to extract Date and the rest  --------------------------------------------------
regx_1 ="<h3.+top:20pt.+>\\s.+?<\\/a>(.+?)<\\/h3>[\\s\\S]*?(<ul[\\s\\S]*?<\\/ul>)"
# Regex to extract Court and the rest
regx_2="10pt;\">(.+?)<\\/h3>([\\s\\S]*?(?:<li[^>]*?>[\\s\\S]*?<\\/li>\\s*)+)"
# Regex to extract Title and the Link
regx_3  ="<a.+=\"(.+?)\">(.+?)<"
#``````````````````````````````````````````````````````````````````````````````

#------------------------------------------------------------------------------
# Function to extract text from a URL
extract_text_from_url <- function(url) {
  tryCatch({
    start_txt ="^ΠΑΓΚΥΠΡΙΟΣ[\\s\\S]+?Υπογραμμίσεων\\n*?"  
    end_text  = "(?:\n)*cylaw.org: Από το ΚΙΝOΠ/CyLii για τον Παγκύπριο Δικηγορικό Σύλλογο$"
    page <- read_html(url) # Read the webpage
    text <- 
      page |>  
      html_text2() |>   # Extract text from the page
      str_replace(start_txt, "") |> 
      str_replace(end_text, "") |> 
      str_trim()
    return(text)
  }, error = function(e) {
    message("Error fetching: ", url)
    return(NA)                                   # Return NA if there's an error
  })
}
#-----------------------------------------------------------------------------





## Prepare the initial table with Date, Court, Title , Link
original_data <-
  tibble(matches1 =stri_match_all_regex(page, regx_1)) |>   # Date and rest
  unnest_longer(matches1) |> 
  mutate(Date   =matches1[,2],
         content=matches1[,3]) |> 
  select(Date,content) |> 
  mutate(matches2 = stri_match_all_regex(content, regx_2)) |>  # Court and rest
  unnest_longer(matches2) |> 
  mutate(Court  =matches2[,2],
         content=matches2[,3]) |>
  select(Date,Court,content) |> 
  mutate(matches3 = stri_match_all_regex(content, regx_3))|>    # Title and Link
  unnest_longer(matches3) |> 
  mutate(Title = str_trim(matches3[,3]),
         Link  = str_glue("https://www.cylaw.org{matches3[,2]}") ,
  ) |> 
  select(Date,Court,Title,Link)









# Load the updated data (progress file)
if (file.exists("updated_data.csv")) {
  updated_data <- vroom("updated_data.csv")
} else {
  # If no progress file exists, start fresh
  updated_data <- original_data %>% mutate(Text = NA_character_)
}

# Merge the original dataset with the updated dataset
# Ensures new rows from the original dataset are included
data <- original_data |> 
  left_join(updated_data, by = c("Date", "Court", "Title", "Link")) |> 
  mutate(Text = coalesce(Text, NA_character_))  # Retain existing 'Text'

rm(page)  # remove the page from the memory
rm(original_data)  # remove the orginal_data



################ Setup While loop --------------------------------------


# Calculate total remaining links globally
total_remaining <- data %>%
  filter(is.na(Text)) %>%
  nrow()

# Setup global progress bar
global_pb <- progress_bar$new(
  format = "Overall Progress [:bar] :percent (:current/:total) ETA: :eta",
  total = total_remaining,
  clear = FALSE,
  width = 70
)


# Loop to process links n amount at a time --------------
while (TRUE) {
  # Filter out rows that have already been processed (Text is not NA)
  remaining_links <- data |> filter(is.na(Text))
  #n <- 30 # Process 50 rows at a time
  remaining_links <- slice(remaining_links, 1:min(nrow(remaining_links), n))

  # Check if there are no more links to process
  if (nrow(remaining_links) == 0) {
    cat("All links processed!\n")
    break
  }

  
  
  # Batch progress bar
  batch_pb <- progress_bar$new(
    format = "Batch Progress [:bar] :percent (:current/:total) ETA: :eta\n",
    total = nrow(remaining_links),
    clear = FALSE,
    width = 60,
  )
  for (i in seq_len(nrow(remaining_links))) {
    url <- remaining_links$Link[i]
    extracted_text <- extract_text_from_url(url)

    # Update the main dataframe
    data <- data %>%
      mutate(Text = if_else(Link == url, extracted_text, Text))

    # Update the batch progress bar
    batch_pb$tick()
    
    # Update the global progress bar
    global_pb$tick()
  }

  # Save progress to CSV after each batch
  write_csv(data, "updated_data.csv")
  cat("\nProgress saved to 'updated_data.csv'.\n")
}
#### End of loop