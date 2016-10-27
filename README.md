# are-we-dreaming-deeply ?

**Code exchange for s-boeck and valpesendorfer. Naming of the repository is open for discussion.**



#### Contents and changes

- HAND:

    R functions to calculate height above the nearest drainage (HAND) index. 
    
    Based on:
    
    Rennó et al., 2008. HAND, a new terrain descriptor using SRTM-DEM: Mapping terra-firme rainforest environments in Amazonia, Remote         Sensing of Environment, Volume 112, Issue 9, 15 September 2008, Pages 3469-3481, ISSN 0034-4257,
    http://dx.doi.org/10.1016/j.rse.2008.03.018.
    

- Miscellaneous R scripts: 

	+ calcThresh.R:
	
		Automatic thresholding function for greyscale images
	
		Based on:  
		
			Kittler, J. and Illingworth, J. 1986. Minimum Error Thresholding. Pattern Recognition 19 (1): 41-47

			Twele, A., Cao, W., Plank, S., Martinis, S. 2016. Sentinel-1-based flood mapping: a fully automated processing chain.
			International journal of remote sensing 37 (13: 2990-3004)
		
		
		Note: This modified Kittler & Illingworth method was adapted in favour of the 2-d Otsu method which had 
			  unsatisfying initial results.




- g-earthengine:

	Collection of various scripts for google earth-engine, both in javascript and python
	
	