# Jack Webster
# Homework #3
# 11/1/22


GuessTheNumber <- function(lower, upper) {
  set.seed(NULL)
  
  if(is.na(as.numeric(lower)) || is.na(as.numeric(upper))) {
    stop("Boundaries must be numeric values")
  }
  if(as.numeric(lower) >= as.numeric(upper)){
    stop("Lower value must be less than upper value")
  }
  
  else(RandomNumber <- sample(c(as.numeric(lower):as.numeric(upper)),
                              size = 1))
  Guesses <- c()
  NumGuesses <- 0
  guess <- readline(prompt = "Guess a number: ")
  Guesses <- c(Guesses, guess)
  NumGuesses <- NumGuesses + 1
  while(guess != RandomNumber) {
    if(guess < RandomNumber){
      guess <- readline(prompt = "Too low. Try again: ")
      Guesses <- c(Guesses, guess)
      NumGuesses <- NumGuesses + 1
    }
    if(guess > RandomNumber){
      guess <- readline(prompt = "Too high. Try again: ")
      Guesses <- c(Guesses, guess)
      NumGuesses <- NumGuesses + 1
    }
  }
  cat("Correct!", "/n")
  cat("It took you", NumGuesses, "guesses to get the right number.",
      "\n\n")
  list(RandomNumber = RandomNumber,
       Guesses = Guesses,
       NumGuesses = NumGuesses)
}







