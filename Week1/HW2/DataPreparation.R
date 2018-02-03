# modification of https://gist.github.com/brendano/39760
# automatically obtains data from the web
# creates two data frames, test and train
# labels are stored in the y variables of each data frame
# can easily train many models using formula `y ~ .` syntax

# download data from http://yann.lecun.com/exdb/mnist/
#download.file("http://yann.lecun.com/exdb/mnist/train-images-idx3-ubyte.gz",
#              "train-images-idx3-ubyte.gz")
#download.file("http://yann.lecun.com/exdb/mnist/train-labels-idx1-ubyte.gz",
#              "train-labels-idx1-ubyte.gz")
#download.file("http://yann.lecun.com/exdb/mnist/t10k-images-idx3-ubyte.gz",
#              "t10k-images-idx3-ubyte.gz")
#download.file("http://yann.lecun.com/exdb/mnist/t10k-labels-idx1-ubyte.gz",
#              "t10k-labels-idx1-ubyte.gz")

# load image files
load_image_file = function(filename) {
  ret = list()
  f = file(filename, 'rb')
  readBin(f, 'integer', n = 1, size = 4, endian = 'big')
  n    = readBin(f, 'integer', n = 1, size = 4, endian = 'big')
  nrow = readBin(f, 'integer', n = 1, size = 4, endian = 'big')
  ncol = readBin(f, 'integer', n = 1, size = 4, endian = 'big')
  x = readBin(f, 'integer', n = n * nrow * ncol, size = 1, signed = FALSE)
  close(f)
  data.frame(matrix(x, ncol = nrow * ncol, byrow = TRUE))
}

# load label files
load_label_file = function(filename) {
  f = file(filename, 'rb')
  readBin(f, 'integer', n = 1, size = 4, endian = 'big')
  n = readBin(f, 'integer', n = 1, size = 4, endian = 'big')
  y = readBin(f, 'integer', n = n, size = 1, signed = FALSE)
  close(f)
  y
}

# load images
train = load_image_file("..\\..\\Datasets\\MNSIT\\train-images-idx3-ubyte")
test  = load_image_file("..\\..\\Datasets\\MNSIT\\t10k-images-idx3-ubyte")

# load labels
train$y = as.factor(load_label_file("..\\..\\Datasets\\MNSIT\\train-labels-idx1-ubyte"))
test$y  = as.factor(load_label_file("..\\..\\Datasets\\MNSIT\\t10k-labels-idx1-ubyte"))

# create pixel header
pixel_header = function(x)
{
  out = array()
  for (ix in 1:x)
  {
    out[ix] = sprintf("pixel%i", ix-1)
  }
  out[ix+1] = "class"
  return (out)
}

ph = pixel_header(784) #adds "class" at [785]
names(train) = ph      #sets header of training set data frame
names(test) = ph       #sets header of test set data frame

write.csv(train, file="mnist_train.csv", row.names=FALSE)
write.csv(test, file="mnist_test.csv", row.names=FALSE)