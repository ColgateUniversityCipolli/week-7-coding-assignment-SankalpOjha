#Write a pois.prob() function that computes P(X = x), P(X ̸= x), P(X < x), P(X ≤ x),
#P(X > x), and P (X ≥ x). Enable the user to specify the rate parameter λ.

pois.prob <- function(x, size, prob, type="<="){
  # Use dpois and ppois to conditionally return the correct probability
  
  p.eq <- dpois(x, lambda)     # P(X = x)
  p.neq <- 1 - p.eq            # P(X ≠ x)
  p.lt <- ppois(x - 1, lambda) # P(X < x)
  p.lte <- ppois(x, lambda)    # P(X ≤ x)
  p.gt  <- 1 - p.lte          # P(X > x)
  p.gte  <- 1 - p.lt         # P(X ≥ x)
  
  if (type == "=") {
    return(p.eq)
  }
  else if (type == "!=") {
    return(p.neq)
  }
  else if (type == "<") {
    return(p.lt)
  }
  else if (type == "<=") {
    return(p.lte)
  }
  else if (type == ">") {
    return(p.gt)
  }
  else if (type == ">=") {
    return(p.gte)
  }
}

#Write a beta.prob() function that computes P(X = x), P(X ̸= x), P(X < x), P(X ≤ x),
#P(X > x), and P(X ≥ x) for a beta distribution. Enable the user to specify the shape 
#parameters α and β.

beta.prob <- function(x, size, prob, type="<="){
  # Use dbeta and pbeta to conditionally return the correct probability
}