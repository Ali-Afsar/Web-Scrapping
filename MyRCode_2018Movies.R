# Web Data Scraping using R for IMDB Movies rating 2018
# Install rvest Package

install.packages("rvest")

library(rvest)

# Specify the URL for the desired website to be scraped

url <- 'https://www.imdb.com/search/title/?count=100&release_date=2018,2018&title_type=feature'

# Reading the HTML Code from the Website

webpage <- read_html(url)

# Now we are going to scrap follwing data from this website
# 1.Rank
# 2.Title
# 3. Description
# 4. Runtime
# 5. Genre
# 6. Rating
# 7. Metascore
# 8. Votes
# 9. Gross Earning in Millions
# 10. Director
# 11. Actor

# 1. Rank - Now Find out the CSS selector that contains the Ranking- In my case it is (text-primary)

# Now Scrap the Ranking section using htlm node

rank_data_html <- html_nodes(webpage, '.text-primary')

# Converting ranking data to text

rank_data <- html_text(rank_data_html)

head(rank_data)

# Remove the dot from the rank_data

rank_data <- gsub("\\.","",rank_data)

class(rank_data)

# As we can see the data is in text format, now we will convert it to numeric data type

rank_data <- as.numeric(rank_data)

head(rank_data)

class(rank_data)

length(rank_data)

# 2. Title - Now again check the css selector, in my case its a link comming under Header3 having class 'lister-item-header'

# Now scrapping Title section using html node

title_data_html <- html_nodes(webpage,'.lister-item-header a')

# Covert Title data to text

title_data <- html_text(title_data_html)

print(title_data)

class(title_data)

length(title_data)

# 3. Description - Now using the same process as above to find out the selector for description and
#it is 'text-muted' just after the div of ratings-bar class

description_data_html <- html_nodes(webpage, '.ratings-bar+.text-muted')

# Convert Description to text

description_data <- html_text(description_data_html)

head(description_data)

# Now remove the "\n from the description data

description_data <- gsub("\n","", description_data)

# See the data has some spaces to remove it use trimws function

description_data <- trimws(description_data)

length(description_data)

# 4. Runtime : For Runtime Selection, we can see in css, it has class= runtime in span under a pragraph of class=text-muted

runtime_data_html <- html_nodes(webpage,'.text-muted .runtime')

# Convert it to the text

runtime_data <- html_text(runtime_data_html)

head(runtime_data)

# removing the min from the runtime_data

runtime_data <- gsub("min","",runtime_data)

# Now covert it to numeric

runtime_data <- as.numeric(runtime_data)

length(runtime_data)



# 5. Genre: Do the same process for genre

genre_data_html <- html_nodes(webpage,'.text-muted .genre')

# Converting genre to text

genre_data <- html_text(genre_data_html)

head(genre_data)
# Remove "\n and spaces from the genre_data

genre_data <- trimws(gsub("\n","",genre_data))

head(genre_data)

# Only take the First genre for each movie, remove all other

genre_data <- gsub(",.*","",genre_data)

class(genre_data)

# Now Changing genre_data to factor

genre_data <- as.factor(genre_data)

head(genre_data)

length(genre_data)


# 6. Now  scrape the rating section

rating_data_html <- html_nodes(webpage, '.ratings-imdb-rating strong')

# Convert rating to text

rating_data <- html_text(rating_data_html)

class(rating_data)

# Now covert the rating_data to numeric

rating_data <- as.numeric(rating_data)

head(rating_data)

length(rating_data)


# 8. Scrap the Votes section using CSS selection----------------------

votes_data_html <- html_nodes(webpage, '.sort-num_votes-visible span:nth-child(2)')

# Convert votes_data into text

votes_data <- html_text(votes_data_html)

# Remove comma between the votes_data

votes_data <- gsub(",","",votes_data)

head(votes_data)

# Coverting votes_data to numeric

votes_data <- as.numeric(votes_data)

length(votes_data)

# 10. Scrap the Director Section-----------------

director_data_html <- html_nodes(webpage, '.text-muted+ p a:nth-child(1)')

# Covert director data to text

director_data <- html_text(director_data_html)

length(director_data)

# Now change the director data into factor

director_data <- as.factor(director_data)

head(director_data)


# 11. Scrap Actor Section

actor_data_html <- html_nodes(webpage,'.lister-item-content .ghost+ a')

# Convert actor data to text
actor_data <- html_text(actor_data_html)

length(actor_data)

# Convert actor data to factor

actor_data <- as.factor(actor_data)



# 7***7. Scrap Metascore Section*****

metascore_data_html <- html_nodes(webpage,'.metascore')

# Converting metascore into text

metascore_data <- html_text(metascore_data_html)

class(metascore_data)

# Removing extra spaces from both side

metascore_data <- trimws(metascore_data)

# Check length of metascore data(We are scraping 100 movies records but it is of 98)

length(metascore_data)

# Now I found that Movie No 1,53,75 and 86 does not have metascore (look visually)

# Now first we will fill "NA" at first postion

metascore_data <- append(metascore_data, "NA", after = 0)

# Try to Fill 'NA' at 53,75 and 86 position

for(i in c(53,75,86)){
  
  a <- metascore_data[1:(i-1)]
  b <- metascore_data[i:length(metascore_data)]
  
  metascore_data <- append(a,list("NA"))
  metascore_data <- append(metascore_data,b)
}

length(metascore_data)

# Convert metascore data into numeric

metascore_data <- as.numeric(metascore_data)

# Lets look at the statical Summary

summary(metascore_data)


#9*** 9.  Scrap Gross Earning with Css Selector

gross_data_html <- html_nodes(webpage,'.ghost~ .text-muted+ span')

# Convert gross data to text

gross_data <- html_text(gross_data_html)

head(gross_data)

# Now remove $ and M from gross_data

gross_data <- gsub("M","",gross_data)

gross_data <- substring(gross_data,2,7)

# Check the Length 

length(gross_data)

# I found that only 84 Movies has gross earning out of 100. 
#Now fill NA to 57 data

# Now Filling "NA" at first Postion

gross_data <- append(gross_data,"NA", after = 0)

for (i in c(12,28,38,39,45,51,53,60,75,81,83,86,92,94,95)) {
  
  a <- gross_data[1:(i-1)]
  b <- gross_data[i:length(gross_data)]
  gross_data <- append(a,list("NA"))
  gross_data <- append(gross_data,b)
}

length(gross_data)

# Converting Gross data to Numeric

gross_data <- as.numeric(gross_data)


summary(gross_data)

# Finally combining all to Form a data frame

movies_2018_df <- data.frame(
  
  Rank = rank_data,
  Title = title_data,
  Description = description_data,
  Runtime = runtime_data,
  Genre = genre_data,
  Rating = rating_data,
  Metascore = metascore_data,
  Votes = votes_data,
  Gross_Earning_Millions = gross_data,
  Director = director_data,
  Actor = actor_data
)

# Check the Structure

str(movies_2018_df) 