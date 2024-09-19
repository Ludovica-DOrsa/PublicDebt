
# Helper function to sort tuples by the second element
Sort_Tuple <- function(tup) {
  tup[order(sapply(tup, `[[`, 2))]
}

# Updated parliamentary_Coord function to generate correct radial and angular positions
parliamentary_Coord <- function(df, angle_total = 240, rows = 5, ratio = 6, initial = "NAME") {
  
  total_seats <- nrow(df)  # Total number of rows in the dataframe
  
  # Calculate how many members in each row
  seats_per_row <- ceiling(total_seats / rows)
  
  # Initialize empty list for coordinates
  coord <- list()
  
  seat_index <- 1
  
  # Loop through each row and calculate angles and radial positions
  for (row in 1:rows) {
    seats_in_this_row <- min(seats_per_row, total_seats - length(coord))
    
    # Calculate angular spacing for each seat in this row
    angle_spacing <- angle_total / seats_in_this_row
    ####################
    start_angle <- (180 - angle_total) / 2  # Start at the center of the arc
    current_angle <- start_angle + (angle_spacing / 2)  # Start in the middle of the first sector
    
    for (i in 1:seats_in_this_row) {
      coord <- append(coord, list(c(ratio + row - 1, current_angle)))
      current_angle <- current_angle + angle_spacing
      seat_index <- seat_index + 1
    }
    
    # Stop if all seats have been placed
    if (seat_index > total_seats) break
  }
  
  # Sort the coordinates
  coord <- Sort_Tuple(coord)
  
  # Add radio and theta columns to the dataframe
  df$radio <- sapply(coord, `[[`, 1)
  df$tetha <- sapply(coord, `[[`, 2)
  
  return(df)
}

