titanic <- read.csv("train.csv")

# Call dim on titanic
dim(titanic)


pass_names <- titanic$Name

# Create titles
titles <- gsub("^.*, (.*?)\\..*$", "\\1", pass_names)

# Call unique() on titles
unique(titles)

# You can use it to call grepl() over all titles in the titles vector, with pass_names as an additional argument. If you do this properly, you'll end up with the exact same matrix described above. Simply taking the sum of this matrix should give us the total number of hits for each title, and thus the total count of males inferred from their respective titles.
pass_names <- titanic$Name
titles <- paste(",", c("Mr\\.", "Master", "Don", "Rev", "Dr\\.", "Major", "Sir", "Col", "Capt", "Jonkheer"))

ladyTitles <- paste(",", c('Mrs', 'Miss', 'Mme', 'Ms', 'Lady', 'Mlle', 'the Countess'))
# Finish the vapply() command
hits <- vapply(titles,
               FUN = grepl,
               FUN.VALUE = logical(length(pass_names)), 
               pass_names)


ladies <- vapply(ladyTitles, FUN = grepl, FUN.VALUE = logical(length(pass_names)), pass_names)

# Calculate the sum() of hits
sum(hits)

#Similarly sum() ladies
sum(ladies)

#The data set contains names of Men in the title 'Mrs' (like the old times). Will have to filter the names of all the Mrs female names in the brackets

convert_name <- function(name) {
  # women: take name from inside parentheses
  if (grepl("\\(.*?\\)", name)) {
    #gsub("^.*?\\((.*?)\\)$", "\\1", name)
    gsub("^.*?,\\s(.*?\\.)\\s.*?\\((.*?)\\)$", "\\1 \\2", name)
    # men: take name before comma and after title
  } else {
    # Finish the gsub() function
    #gsub("^(.*?),\\s[a-zA-Z\\.]*?\\s(.*?)$", "\\2 \\1", name)
    gsub("^(.*?),\\s(.*?\\.)*?\\s(.*?)$", "\\2 \\1 \\3", name)
  }
}


# Call convert_name on name
clean_pass_names <- vapply(pass_names, FUN = convert_name,
                           FUN.VALUE = character(1), USE.NAMES = FALSE)



# Print out clean_pass_names
clean_pass_names




