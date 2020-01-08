#Define hard coded color schemes that are applied by the server

createBasicDmzScheme <- function(){
  colorScheme <- brewer.pal(7,'Paired')
  names(colorScheme) <- c("Ag", "O", "G", "P", "M", "A", "None")
  return(colorScheme)
}

# createBasicDmzScheme <- function(data){
#   colorScheme <- brewer.pal(nrow(data)-2,'Paired')
#   data.levels <- names(data)[-1][order(colSums(data[,-1]), decreasing = FALSE)]
#   names(colorScheme) <- data.levels
#   return(colorScheme)
# }

colorSchemeGraph3 <- function(data) {
  names <- unique(data$START_LIMB)
  amount <- length(names)
  colorScheme <- brewer.pal(amount+1,'Paired')
  colorScheme <- tail(colorScheme, amount)
  names(colorScheme) <- names
  return(colorScheme)
}

extendBasicDmzScheme <- function(){
  colorScheme <- brewer.pal(8,'Paired')
  names(colorScheme) <- c("Ag", "O", "G", "P", "M", "A", "None", "Q")
  return(colorScheme)
}

# extendBasicDmzScheme <- function(scheme, data){
#   colorScheme <- scheme
#   oldNames <- names(scheme)
#   newNames <- as.character(unique(data))
#   diff <- setdiff(newNames, oldNames)
#   extend <- brewer.pal(length(colorScheme) + length(diff), 'Paired')
#   newColor <- tail(extend, n=1)
#   names(newColor) <- diff
#   return(c(colorScheme, newColor))
# }