# Load required packages
library(polite)
library(rvest)
library(dplyr)
library(openxlsx)

# Function to scrape data from the specific table on a Wikipedia page
scrape_legislature_data <- function(url) {
  # Create a polite session
  session <- bow(url)
  
  # Scrape the page
  page <- scrape(session)
  
  # Find the table with the class "infobox sinottico"
  table_node <- page %>%
    html_node(xpath = '//table[contains(@class, "infobox sinottico")]')
  
  if (is.null(table_node)) {
    stop("Table with class 'infobox sinottico' not found.")
  }
  
  # Extract the party information from the table
  rows <- table_node %>%
    html_nodes("tr")
  
  # Function to extract party data from a row
  extract_party_info <- function(row) {
    # Extract the <a> tag if it exists
    a_tag <- row %>% html_node("th a")
    
    if (is.null(a_tag)) {
      return(NULL)  # Skip rows without party information
    }
    
    # Extract party name
    party_name <- a_tag %>%
      html_text(trim = TRUE)
    
    # Extract hyperlink
    link <- a_tag %>%
      html_attr("href")
    link <- ifelse(is.na(link), NA, paste0("https://it.wikipedia.org", link))
    
    # Extract title attribute
    title <- a_tag %>%
      html_attr("title")
    
    # Extract number of seats
    seats_div <- row %>%
      html_node("td div span.nowrap")
    seats <- if (!is.null(seats_div)) {
      seats_div %>% html_text(trim = TRUE)
    } else {
      NA
    }
    
    # Extract color from nested div structure
    color_div <- row %>%
      html_node(xpath = ".//td/div/div/div[contains(@style, 'background-color')]")
    color_style <- if (!is.null(color_div)) {
      color_div %>% html_attr("style")
    } else {
      NA
    }
    color <- if (!is.na(color_style)) {
      sub(".*background-color:([^;]+);.*", "\\1", color_style)
    } else {
      NA
    }
    
    # Check if all fields are missing
    if (is.na(party_name) & is.na(link) & is.na(title) & is.na(seats) & is.na(color)) {
      return(NULL)  # Skip rows with all fields missing
    }
    
    # Return a row of the dataframe
    data.frame(
      party = party_name,
      link = link,
      title = title,
      seats = seats,
      background_color = color,
      stringsAsFactors = FALSE
    )
  }
  
  # Extract and combine all party data
  table_data <- lapply(rows, extract_party_info)
  final_table <- do.call(rbind, table_data)
  
  return(final_table)
}



url <- "https://it.wikipedia.org/wiki/I_legislatura_della_Repubblica_Italiana"
data_paties <- scrape_legislature_data(url)
print(data_paties)
write.xlsx(data_paties, 'data_paties.xlsx')
