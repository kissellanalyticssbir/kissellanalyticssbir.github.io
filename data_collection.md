## Data Collection

The sbir awards are federally funded, and thus the data are publicly available. While the SBIR website has an API for downloading data, there are some technical restrictions that make it difficult to collect all data from the site. As such, I wrote a web scraper that first collected the links to every award from the department of defense (see Appendix A). I then wrote a second scraper to iterate through the list and collect all available data related to each award (see Appendix B). The data for each company that received at least one award was also available. This policies around this data were less restrictive, and thus I was able to write a much simpler program which collected that data from the API (see Appendix C)

I first cleaned each variable and converted them to the appropriate data types. As the phase variable was predicted to be a relevant factor, I recoded the variable to be more informative by forming 4 levels. Phase 1a contained awards that were in phase I, but never reached phase II. Phase 1b contained awards that were in phase I, and did reach phase II. Phase 2a contained awards that were in phase II, and had started in phase I. Lastly, Phase 2b contained awards in Phase II, but were never in phase I. I also wrote a python program to extract a number of features from the text (see Appendix E). These features included character and word count for award titles, and character count, word count, sentence count, syllable count, number of proper nouns, number of nouns, number of verbs, and five measures of reading level (i.e., Dalechall, Flesch, Fleshkincaid, Gunningfog, and Smog) for all text (i.e., title and abstract combined).


```markdown
## Scraper to obtain links for every award from www.sbir.gov for the Department of Defense.
# Load packages
library(readr)
library(dplyr)
library(rvest)
library(stringr)

# Create html from sbir
html <- read_html("https://www.sbir.gov/sbirsearch/award/all/?f%5B0%5D=im_field_agencies%3A105755&f%5B1%5D=im_field_agencies%3A105729")

# Obtain the number of search results
n_search_results <- html %>% html_nodes('.col-sm-5') %>% 
  html_text() %>% 
  str_extract("[0-9]{3,10}") %>%
  as.integer()

# Number of pages that we will need to scroll through.
n_pages_of_results <- ceiling(n_search_results / 10)-1

# Initiate a vector that will store the pages.
links_search_pages <- c()

# Create a link for every search page.
for (page_number in seq(0, n_pages_of_results)){
  
  link_search_page <- paste("https://www.sbir.gov/sbirsearch/award/all/?page=",page_number,"&f%5B0%5D=im_field_agencies%3A105755&f%5B1%5D=im_field_agencies%3A105729", sep = "")
  
  # Store each new link in the initialized vector. 
  links_search_pages <- c(links_search_pages, link_search_page)
}

#Check out structure of the links_search_pages
str(links_search_pages)

############# Collect the name and link for every award. ####################

# Initialize links and names of links.
names_of_award_page <- c("")
links_of_award_page <- c("")

# Initialize a count, so I can see where we are in the process.
count = 0

# Extract the name and links for every award.
for(link in links_search_pages){
  
  # Add in a pause to avoid using too many resources.
  Sys.sleep(.5)
  
  # Create html
  link_html <- read_html(link)
  
  # Grab name of award
  name_of_award_page <- link_html %>% html_nodes('.title a') %>% html_text()
  
  # Grab link for the award
  link_of_award_page <- link_html %>% html_nodes('.title a') %>% html_attr('href')
  
  # Add name to initialized vector
  names_of_award_page <- c(names_of_award_page, name_of_award_page)
  
  # Add link to initialized vector
  links_of_award_page <- c(links_of_award_page, link_of_award_page)
  
  # Print a count, so I can see where we are in the process.
  count = count + 1
  print(count)
}

# Construct a data frame with all names and links.
links_df <- tibble(
  award_links = links_of_award_page,
  award_names = names_of_award_page
)

# Create a variable with the entire url.
links_df$award_links_full <- c("")
links_df$award_links_full <- str_c("https://www.sbir.gov", links_df$award_links)

# Create a csv
write_csv(links_df, "award_links.csv")

#################

# Load packages
library(tidyverse)
library(rvest)
library(stringr)
library(tidyr)
library(lubridate)

# Import the links from the csv file I have stored on my google drive.
award_links <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQBHEwhc-nPaTE7Kt9LiTJTev2_-Z0Mgw3Voi2XfFyS8EnHDtmMVynT7mb_VqUNO7wEbl21hq-N0seo/pub?output=csv")

# Initiate the data frame
df_scraped_data <- tibble(
  agency = c(""),
  branch = c(""),
  contract = c(""),
  agency_tracking_number = c(""),
  amount = c(""),
  phase = c(""),
  program = c(""),
  solicitation_topic_code = c(""),
  solicitation_number = c(""),
  solicitation_year = c(""),
  award_year = c(""),
  award_start_date_proposal_award_date = c(""),
  award_end_date_contract_end_date = c(""),
  duns = c(""),
  hubzone_owned = c(""),
  woman_owned = c(""),
  socially_and_economically_disadvantaged = c(""),
  link = c(""),
  recipient_information = c(""),
  abstract = c(""),
  company = c(""),
  company_address = c(""),
  award_title = c(""),
) 

# Create function to collect data labels from a certain css selector, and then clean them so that they can be used as variable names.
create_scraped_labels <- function(css_selector){
  award_link_html %>% 
    html_nodes(css_selector) %>% 
    html_text() %>% 
    str_replace(pattern = ":", "") %>%
    str_replace_all(pattern = '[(]', "") %>%
    str_replace_all(pattern = '[)]', "") %>%
    str_replace_all(pattern = " ", "_") %>%
    tolower()
}

# Create function to collect the values from a css_selector that will go with each scraped label.
create_scraped_values <- function(css_selector){
  award_link_html %>% 
    html_nodes(css_selector) %>% 
    html_text()
}

# Create a function that takes a css_selector for scraped labels and a css_selector for scraped values. Then combine the labels and values in a data frame.
create_labels_values_dataframe <- function(labels_css_selector, values_css_selector){
  
  # Collect and clean up the labels, so that they can be used as variable names
  labels <- create_scraped_labels(labels_css_selector)
  
  # Collect the values that will go with each label.
  values <- create_scraped_values(values_css_selector)
  
  # Create a dataframe, which will be used in the process.
  labels_and_values <- tibble(labels, values) 
  
  # Turn the labels variable into the column names
  pivot_wider(data = labels_and_values, names_from = labels, values_from = values)
}

# Create function for reading html. It is written so that it will retry if the connection fails.
foo <- NULL
read_html_try <- function(x,tries) {
  message(paste("x is",x,"remaining tries",tries))
  withRestarts(
    tryCatch({
      read_html(x)
    },
    error=function(e) { invokeRestart("retry")}),
    retry = function() { 
      message("Retrying")
      Sys.sleep(10)
      stopifnot(tries > 0)
      read_html_try(x,tries-1)
    }
  )
}

######
# Initiate a count, just so I can see where it is in the process.
count = 0

# Create the list of links that we will iterate over.
award_links_full <- award_links$award_links_full
#award_links_full <- award_links$award_links_full[c(80339:85191)]


# Extract the data for every award.
for(award_link in award_links_full){
  
  # Add in a pause to avoid using too many resources
  Sys.sleep(1)
  
  # Create html
  #award_link_html <- read_html(award_link)
  award_link_html <- read_html_try(award_link,10)
  
  # Collect and clean up the labels, so that they can be used as variable names
  # Collect the values that will go with each label.
  # Create a dataframe, which will be used in the process.
  # Turn the labels variable into the column names
  df_data <- create_labels_values_dataframe(labels_css_selector = '.open-label', values_css_selector = '.open-description')
  
  # Add the link to the dataframe
  df_data$link <- paste(award_link)

  # Collect and add the recepient information (This will need to be cleaned up, if we want to use it)
  df_data$recipient_information <- award_link_html %>% html_nodes('.col-md-4 div') %>% html_text() %>% str_c(collapse = TRUE)
  
  # Collect and add the abstract
  df_data$abstract <- award_link_html %>% html_nodes('.abstract-wrapper') %>% html_text() %>% str_c(collapse = TRUE) %>% str_trim()
  
  # Add Company
  df_data$company <- award_link_html %>% html_nodes('#block-system-main a') %>% html_text()
  
  # Add Company Address
  df_data$company_address <- award_link_html %>% html_nodes('.sbc-address-wrapper') %>% html_text()
  
  # Add Award Title
  df_data$award_title <- award_link_html %>% html_nodes('.page-header') %>% html_text()

  # Add the data to the initiated dataframe
  df_scraped_data = add_row(df_scraped_data, df_data)
  
  # Print and change the count, so I know where we are in the process
  print(count)
  count = count + 1
}

write_csv(df_scraped_data, "df_scraped_data.csv")

#################

## Scraper to obtain links for every company that has received an award from www.sbir.gov.
## They make this data easier to access, so I will just download the files that they have prepared, and will combine them here in R.

# Import Packages
library(readxl)
library(dplyr)
library(readr)

# Read in the excel files directly from SBIR (They have different restrictions on this data, which will make this much easier than the other scraper)
sbir_company_data_1 <- read_excel("https://www.sbir.gov/sbirsearch/firm/all?print=xls&per_page=5000&page=1")
sbir_company_data_2 <- read_excel("https://www.sbir.gov/sbirsearch/firm/all?print=xls&per_page=5000&page=2")
sbir_company_data_3 <- read_excel("https://www.sbir.gov/sbirsearch/firm/all?print=xls&per_page=5000&page=3")
sbir_company_data_4 <- read_excel("https://www.sbir.gov/sbirsearch/firm/all?print=xls&per_page=5000&page=4")
sbir_company_data_5 <- read_excel("https://www.sbir.gov/sbirsearch/firm/all?print=xls&per_page=5000&page=5")
sbir_company_data_6 <- read_excel("https://www.sbir.gov/sbirsearch/firm/all?print=xls&per_page=5000&page=6")

#Join the company data together
sbir_company_data <- sbir_company_data_1 %>%
  full_join(sbir_company_data_2) %>%
  full_join(sbir_company_data_3) %>%
  full_join(sbir_company_data_4) %>%
  full_join(sbir_company_data_5) %>%
  full_join(sbir_company_data_6)

# Write it out to a csv file
write_csv(sbir_company_data, "sbir_company_data.csv")
```
