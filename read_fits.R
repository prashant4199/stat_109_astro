install.packages('FITSio')
library(FITSio)

hdulist_goodspec <- readFrameFromFITS("/Users/prashant-mac/Playground/Schoolwork/STAT E109/prj/mastar-goodspec-v2_4_3-v1_0_2.fits")
hdulist_allStars <- readFrameFromFITS("/Users/prashant-mac/Playground/Schoolwork/STAT E109/prj/mastarall-v2_4_3-v1_0_2.fits")

head(hdulist_goodspec)
head(hdulist_allStars)
