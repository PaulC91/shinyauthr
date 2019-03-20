# function to create a random sting with standard length 64
# modified after: https://gist.github.com/calligross/e779281b500eb93ee9e42e4d72448189
randomString <- function(n = 64) {
    paste(sample(x = c(letters, LETTERS, 0:9), size = n, replace = TRUE),
          collapse = "")
}