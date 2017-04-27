### Exercise 2 ###

# Load the httr and jsonlite libraries for accessing data
library("httr")
library("jsonlite")


# Create a `base.uri` variable that holds the base uri. You wil then paste endpoints to this base.
base.uri <- "https://api.spotify.com"



## As you may have noticed, it often takes multiple queries to retrieve the desired information.
## This is a perfect situation in which writing a function will allow you to better structure your
## code, and give a name to a repeated task!

# Define a function `TopTrackSearch` that takes in an artist name as an argument,
# and returns the top 10 tracks (in the US) by that artist
TopTrackSearch <- function(artist) {
  resource <- "/v1/search"
  uri = paste0(base.uri, resource)
  query.params <- list(q = artist, type = "artist")
  
  response <- GET(uri, query = query.params)
  
  artists.info <- fromJSON(content(response, "text"))
  artists.id <- artists.info$artists$items$id[1]
  
  resource <- paste0("/v1/artists/",artist.id,"/top-tracks")
  uri = paste0(base.uri, resource)
  query.params <- list(country="US")
  response <- GET(uri, query = query.params)
  
  top.tracks <- fromJSON(content(response, "text"))
  
  return(top.tracks$tracks)
}
# got up to response variable on my own, but referenced back to Tuesday's exercise's for further help 
# and the complete branch 



# What are the top 10 tracks by Beyonce?
top.tracks.beyonce <- TopTrackSearch('Beyonce')


# Use the `flatten` function to flatten the data.frame -- note what differs!
top.tracks.beyonce <- flatten(top.tracks.beyonce)

# Use the `save()` function to save the flattened data frame to a file `beyonce.Rdata`
save(top.tracks, file="beyonce.Rdata")


# Use your `dplyr` functions to get the number of the songs that appear on each album
num.songs.album <- top.tracks.beyonce %>%
  group_by(album.name) %>%
  summarise(n = n()) %>%
  arrange(-n)
num.album


### Bonus ###
# Write a function that allows you to specify a search type (artist, album, etc.), and a string,
# that returns the album/artist/etc. page of interest


# Search albums with the word "Sermon"
