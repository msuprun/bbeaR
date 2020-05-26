Image.Plate <- function (object, image.db = NULL, summary.fcn = mean, filename = "Experiment", 
                         height = 6, width = 8, direction = "horizontal") {
  

  l <- create.plate.db(direction = direction)
  plate.design.db <- l$plate.design.db
  
  require(gridExtra)
  require(ggplot2)
  if (is.null(image.db)) {
    if (any(names(object) == "NetMFI")) {
      Binding.mat <- (object$NetMFI + 0.01)
      PD <- object$pData
    }
    if (any(names(object) == "nMFI")) {
      Binding.mat <- object$nMFI
      PD <- object$pData
    }
    if (class(object) == "ExpressionSet") {
      Binding.mat <- exprs(object)
      PD <- pData(object)
    }
    image.db <- cbind.data.frame(sample = colnames(Binding.mat), 
                                 Colour_Intensity = apply(Binding.mat, 2, summary.fcn), 
                                 PD)
  }
  range.colour <- range(image.db$Colour_Intensity)
  if (class(image.db$Plate) != "factor") {
    image.db$Plate <- factor(image.db$Plate)
  }
  plates <- levels(droplevels(image.db$Plate))
  plot.list <- vector("list", length(plates))
  names(plot.list) <- plates
  for (b in plates) {
    image.db.plate <- subset(image.db, Plate == b)
    image.db.plate <- merge(image.db.plate, plate.design.db, 
                            by = "Well_coord", all.x = TRUE, all.y = TRUE)
    image.db.plate <- mutate(image.db.plate, LETTER = factor(substr(Well_coord, 
                                                                    1, 1), levels = LETTERS[12:1]), Number = factor(mgsub(LETTERS[1:12], 
                                                                                                                          rep("", 12), Well_coord), levels = 1:12))
    p <- ggplot(image.db.plate, aes(x = Number, y = LETTER)) + theme_bw() +
      geom_tile(aes(fill = Colour_Intensity), colour = "white") + 
      scale_fill_gradient(low = "white", high = "navyblue", 
                          name = "Intensity", limits = range.colour) + 
      labs(title = paste0("Image for ", b)) + theme(title = element_text(size = 10), 
                                                    legend.text = element_text(size = 10), legend.key.size = unit(0.2, 
                                                                                                                  "in"))
    plot.list[[b]] <- p
  }
  p <- marrangeGrob(plot.list, nrow = 2, ncol = 2)
  ggsave(paste0(filename, ".PlateImage.pdf"), p, height = height, 
         width = width)
  return(p)
}
