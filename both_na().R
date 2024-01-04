both_na <- function(vector1, vector2) {
  if (length(vector1) != length(vector2)) {
    stop("Vectores de distinta longitud.")
  }
  
  indices_na_vector1 <- which(is.na(vector1))
  indices_na_vector2 <- which(is.na(vector2))
  
  print("Vector 1 (indices NA):")
  print(indices_na_vector1)
  print("Vector 2 (indices NA):")
  print(indices_na_vector2)
}

vector_a <- c(1, 2, NA, 4, 5)
vector_b <- c(NA, 2, 3, 4, NA)

both_na(vector_a, vector_b)
