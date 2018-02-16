# Read binary file and convert to integer vectors
# [Necessary because reading directly as integer() 
# reads first bit as signed otherwise]
#
# File format is 10000 records following the pattern:
# [label x 1][red x 1024][green x 1024][blue x 1024]
# NOT broken into rows, so need to be careful with "size" and "n"
#
# (See http://www.cs.toronto.edu/~kriz/cifar.html)

# function to run sanity check on photos & labels import
drawImage <- function(index) {
  # Testing the parsing: Convert each color layer into a matrix,
  # combine into an rgb object, and display as a plot
  img <- images.rgb[[index]]
  img.r.mat <- matrix(img$r, ncol=32, byrow = TRUE)
  img.g.mat <- matrix(img$g, ncol=32, byrow = TRUE)
  img.b.mat <- matrix(img$b, ncol=32, byrow = TRUE)
  img_matrix  <- rgb(img.r.mat, img.g.mat, img.b.mat, maxColorValue = 255)
  dim(img_matrix ) <- c(32,32)
  
  # Plot and output label
  library(grid)
  grid.newpage()
  grid.raster(img_matrix, interpolate=FALSE)
  
  # clean up
  remove(img, img.r.mat, img.g.mat, img.b.mat, img_matrix)
  
  labels[[1]][images.lab[[index]]]
}


labels <- read.table("cifar-10-batches-bin/batches.meta.txt")
images.rgb <- list()
images.lab <- list()
num.images = 10000 # Set to 10000 to retrieve all images per file to memory
m_data <- matrix(0,10000 * 6, 3072 + 1)
m_labels <- vector("numeric", 10000 * 6)
# Cycle through all 5 binary files
for (f in 1:6) {
  print(c("starting file numer",f))
  if( f < 6)
  {
    to.read <- file(paste("cifar-10-batches-bin/data_batch_", f, ".bin", sep=""), "rb")
  }
  else
  {
    to.read <- file("cifar-10-batches-bin/test_batch.bin", "rb")
  }
  
  for(i in 1:num.images) {
    l <- readBin(to.read, integer(), size=1, n=1, endian="big")
    r <- as.integer(readBin(to.read, raw(), size=1, n=1024, endian="big"))
    g <- as.integer(readBin(to.read, raw(), size=1, n=1024, endian="big"))
    b <- as.integer(readBin(to.read, raw(), size=1, n=1024, endian="big"))
    m_data[((f - 1) * num.images) + i,] <- c(l,r,g,b)
    index <- num.images * (f-1) + i
    images.rgb[[index]] = data.frame(r, g, b)
    images.lab[[index]] = l+1
  }
  close(to.read)
  remove(l,r,g,b,f,i,index, to.read)
}

print("writing files")
write.csv(m_data, file="cifar-10-batches-bin/allData.csv", row.names=FALSE, col.names = FALSE)
#drawImage(sample(1:(num.images), size=1))

#pca <- princomp(m_data) #,rank. = 50)