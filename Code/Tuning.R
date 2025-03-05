############ This script will be used to tune the results of classification models in a generalizaeable
# way such that the results can be used as true probabilities to be propogated through in other models 

### Walking through the example from Tidyverse 

library(tidyverse)
library(probably)

print(segment_logistic) # This is the test dataset we'll use, it should contain pred_poor, pred_good, and class 

#### The common approach is the group the probabilities into bins. The convention is 10 discrete buckets from 0-1. The event rate and bin midpoint is calculated for each bin 

# cal_plot_breaks() can create binned calibration plots. 

segment_logistic %>%
  cal_plot_breaks(Class, .pred_good) #The variable and the probabilities for that variable. In my dataset, "Actual" watershed and the score for that watershed 

# We can adjust the number of bins. 
### This should be adjusted to make sure there are enough data points per bin. 

segment_logistic %>%
  cal_plot_breaks(Class, .pred_good, num_breaks = 5) #This will create 5 bins


#### Windowed approach (still tidyverse)

