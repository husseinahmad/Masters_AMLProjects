library(grid)

drawImage <- function(v_img) {
  # Testing the parsing: Convert each color layer into a matrix,
  # combine into an rgb object, and display as a plot
  img.r.mat <- matrix(v_img[1:(32*32)], nrow = 32, ncol=32, byrow = TRUE)
  img.g.mat <- matrix(v_img[(32*32) + 1:(32*32*2)], nrow = 32, ncol=32, byrow = TRUE)
  img.b.mat <- matrix(v_img[(32*32*2) + 1:(32*32*3)], nrow = 32, ncol=32, byrow = TRUE)
  img.col.mat <- rgb(img.r.mat, img.g.mat, img.b.mat, maxColorValue = 255)
  dim(img.col.mat) <- dim(img.r.mat)
  
  # Plot and output label

  grid.raster(img.col.mat, interpolate=FALSE)
  
  # clean up
  #remove(img, img.r.mat, img.g.mat, img.b.mat, img.col.mat)

}



datavals <- readBin("data_batch_1.bin", raw(), size = 1, n = 10000 * 3073, endian = "big")
m_data <- matrix(datavals, nrow = 10000, ncol = 3073) 
drawImage(m_data[1,-1])