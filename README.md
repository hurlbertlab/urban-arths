# urban-arths
Data, code, and output related to analyzing the effects of urbanization on arthropods.


scripts/

cc_pc_histograms: code to generate histograms of the percent impervious and forest cover within a range of distances from Caterpillars Count! sites.

covariation_assessment: code to calculate covariation between landscape metrics and different site radii used for calculation.


data/

processed/

lsm_[].csv: forest cover statistics calculated in Longleaf using the landscapemetrics package, calculated for the radius around the site center included in the name.
  siteID: the Caterpillars Count! unique identifier for a site.
  area_mn: the mean size of patches of forest cover within the defined area.
  shape_mn: the mean shape index of patches of forest cover within the defined area, where shape index index is the ratio of the actual patch perimeter to the hypothetical minimum perimeter of the patch.
  contig_mn: the mean contiguity index of forest patches within the defined area, where contiguity index reflects the spatial connectedness of cells in patches and ranges from 0 to 1.
  enn_mn: the mean Euclidean distance to the nearest neighboring patch of forest for each patch within the defined area, in meters.
  aw_cai_mn: the area-weighted core area index of patches in the defined area, where the core area index is a value from 0 to nearly 100, where 0 represents no core area and 100 would represent 100% core area, which is physically impossible.
  clumpy: the CLUMPY index of forest patches in the defined area, ranging from -1 to 1, where -1 indicates maximal disaggregation of patches, 0 indicates a random distribution, and 1 indicated maximal aggregation.
  nlsi: the normalized landscape shape index of forest patches in the defined area, increased from 0 towards 1 as patches become more disaggregated.
  pafrac: the perimeter-area fractal dimension of forest patches in the defined area, ranging from 1 to 2 with increasing patch cmplexity on the landscape.
  
percent_cover_[].csv: land cover metrics for Caterpillars Count! sites, calculated within the radius of the site center included in the name.
  siteID: see above.
  Name: the name of the site entered by the site admin in Caterpillars Count!
  devo_open - devo_high: the proportion of the defined area covered by each level of developed cover, per National Land Cover Database categorizations.
  forest_decid - forest_mix: the proportion of the defined area covered by deciduous, evergreen, or mixed forest, as classified by NLCD 2019.