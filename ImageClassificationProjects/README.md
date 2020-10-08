These are two projects I did on image classification. 

1) The first project is based on the somewhat strange idea that image classification tasks with erratic input could be assisted by adding a small amount of noise.
Really the project was just an excuse for me to get my hands on tensorflow/Keras. The first goal was just to learn how to train a neural network using tensorflow. 
After that I experimented with adding small amounts of noise to the image. The slow performance of the algorithm 

2) The sequel project used a much more unruly dataset, the Chinese MNIST from Kaggle. This is a medium-sized (15,000 images) dataset of handwritten Chinese 
numerals. The dataset was organized in a somewhat idiosyncratic fashion, and a lot of the work went into rearranging it into a suitable form. I had a lot of 
trouble with switching between Pandas (from importing) and numpy (used to input to the tensorflow fitting method). Still, the overall performance was not bad: 
about 50 percent correct, much better than random (15 labels so random would be less than 7 percent). 

