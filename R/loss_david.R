
#' @export
mat_david = matrix(c(
  -1,1,0,0,0,0,
  0,-1,1,0,0,0,
  0,0,-1,0,1,0,
  0,0,-1,1,0,0,
  0,0,0,0,-1,1
), ncol = 6, byrow = TRUE)

#' @export
names_david <- c("S", "I", "A", "D", "R", "Ru")

#' @export
transitions_david = list(c(I = +1, S = -1), # infection
                   c(I = -1, A = +1), # identification
                   c(A = -1, R = +1), # identified recovery
                   c(A = -1, D = +1), # death
                   c(I = -1, Ru = +1)) # silent recovery

#' @export
lvrates_david <- function(x, params, t) {
  output = c(
    x["S"] * x["I"] * (params$alpha0 + params$alpha / (1 + (x["A"] + x["R"] + x["D"])^params$n)) / 60431283,
    x["I"] * params$gamma,
    x["A"] * params$beta,
    x["A"] * params$delta,
    x["I"] * params$eta * params$beta
  )
  return(output)
}

#' @export
gen_david <- function(x, theta, time1, time2, inp){

  if(is.matrix(theta)){
    names_theta <- colnames(theta)
    theta <- as.numeric(theta)
    names(theta) <- c("alpha0", "alpha", "beta", "delta", "kappa", "eta", "gamma", "n")
  }

  if(is.null(names(theta))){
    names(theta) <- c("alpha0", "alpha", "beta", "delta", "kappa", "eta", "gamma", "n")
  }

  stopifnot(!is.null(names(theta)))



  # if(time2 == 1){
  #   x <- xinit_david(inp$data, theta)
  # }
  # print(x)
  #
  # names(x) <- names_david

  if(any(x < 0)){
    return(list(distance = Inf, x = "failed"))
  }

  stopifnot(!gtools::invalid(x))
  stopifnot(is.numeric(x))
  stopifnot(all(x >= 0))

  output = adaptivetau::ssa.adaptivetau(
    x,
    inp$transitions,
    inp$lvrates,
    as.list(theta),
    tf = time2
  )

  return(output)
}

#' @export
xinit_david <- function(data, theta){

  data <- as.list(data[1,])

  x = c(
    sum(as.numeric(data)) - theta["kappa"] * data$A - (data$A + data$R + data$D),
    theta["kappa"] * data$A,
    data$A,
    data$D,
    data$R,
    0
  )


  x <- round(x)
  names(x) <- names_david

  stopifnot(!gtools::invalid(x))

  # if(anyNA(x)){print(x); print(theta)}
  if(!(all(x >= 0))){print(x)}
  stopifnot(all(x >= 0))

  return(x)
}
