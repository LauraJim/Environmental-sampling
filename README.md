# Environmental Sampling
 
The Hutchinson’s duality allows linking a point in geography to a point in an n-dimensional environmental space where ecological niches are defined. We took advantage of this relationship to design future surveys of a species of interest based on the suitibility values from an Ecological Niche Model (ENM) or Species Distribution Model (SDM). Sampling in different environmental gradients has the advantage of obtaining information that was excluded for the model calibration phase due to the lack of occurrence points. 

Potential detections of the species of interest from different suitability categories, especially those that are different from the maximum value of suitability will improve the fitting of ecological niche models by adding occurrences.

The example included here focuses on the region Ceara, in Brazil and the species Burkholderia pseudomallei (the Gram-negative bacteria known to cause the infectious disease called Melioidosis). However, the methodology of environmental sampling presented can be replicated for other states, countries, or regions. 
 
In order to reproduce the example, please follow these steps:
(1) Download or clone this repository and un-zip the folders that contain the data: ceara_shapefiles.zip, categorized_models.zip, environmental_variables.zip uncertainty_models.zip. Notice that you will need to replace these files in order to reproduce the example with another species and region of study.

(2) Open the R project called Environmental-sampling and from there, open the R script called Example_Ceara_Brazil.R.

(3) Run all the lines in Example_Ceara_Brazil.R and read the comments in each line.

We also recommend you to check out the paper in which we first presented and used this methodology:
- Romero-Alvarez et al. ( ) Data oriented environmental sampling of Burkholderia pseudomallei via ecological niche modeling: an example in Texas, United States. ADD DOI.

How to cite this code:
