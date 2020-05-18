
create.plate.db <- function(direction="horizontal"){
  if (direction=="horizontal") {
    print.order <- matrix(1:96, nrow=8, ncol=12, byrow = TRUE)
  } else if (direction=="vertical") {
    print.order <- matrix(1:96, nrow=8, ncol=12)
  } else { stop("ERROR: Direction can only be 'horizontal' or 'vertical'.") }
  
  plate.vector <- paste0(LETTERS[1:8], rep(1:12,each=8))
  plate.design <- matrix(plate.vector, 8, 12)
  plate.design.db <- data.frame(Well_coord = plate.vector,
                                print.plate.order = as.vector(print.order))
  
  return(list(plate.design.db = plate.design.db, 
              plate.design = plate.design))
}