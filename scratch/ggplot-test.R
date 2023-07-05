install.packages('tidyverse")
library(tidyverse)
View(mpg)
# 1. point chart with color as drive train
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = drv))

# 2. point and line chart with color as drive train  
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = drv)) +
  geom_smooth(mapping = aes(linetype = drv), se = FALSE)
  
# 3. point and line chart with color as drive train  
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = drv)) +
  geom_smooth(se = FALSE)  
  
# 4. point and line chart with color/linetype as drive train  
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = drv)) +
  geom_smooth(mapping = aes(linetype = drv, color = drv), se = FALSE)  
  
# 5. point and line chart with linetype as drive train  
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth(mapping = aes(linetype = drv), se = FALSE) 
  
# 6. point and line chart with se turn off
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth(se = FALSE)
  
# 7. area chart with se turn off  
ggplot(mpg, aes(hwy)) +
  geom_histogram(binwidth =5)
  
# 8.geom_jitter
ggplot(data = mpg, mapping =aes(x = cty, y = hwy)) + geom_jitter()

# 9. geom_boxplot()
ggplot(data = mpg, mapping =aes(x = class, y = hwy)) + geom_boxplot() +
  coord_flip()
  




  
  
  

 
