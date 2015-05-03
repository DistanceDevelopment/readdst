run_analysis <- function(analysis){

  cat("Call:\n", analysis$call, "\n\n")

  result <- eval(parse(text=analysis$call), env=analysis$env)

  return(result)
}
