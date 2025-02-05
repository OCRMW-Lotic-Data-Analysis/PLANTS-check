This app checks if AIM species richness data is regionally reasonable. A user can upload a selection of species richness data (e.g. the past hitch's data) and compare it to a static version of the USDA [PLANTS Database](https://plants.usda.gov/). The app will return whether or not the field-identified species is expected in the county it was collected in. The data input specifications and project-specific notes are explained below.

**Terrestrial**

Data file to upload: CSV of raw species richness survey data.

[Data Example](www/sample_terrestrial.csv)

Notes:

-   The "x" and "y" columns are used for point coordinates and for identifying which county the data was collected in.

-   "Plot ID" (converted to "Plot_ID") is used for plot name.

**Riparian & Wetland**

Data file to upload: CSV of raw species richness survey data.

[Data Example](www/sample_rw.csv)

Notes:

-   The raw survey data doesn't include spatial information, so the original plot locations are used. This means that if a point is moved in the field, it could be moved into a different county. While this is unlikely, it means that when the plot data is compared with the PLANTS database, the comparison would not be accurate.

-   "PlotID", extracted from "SpecRichDetailEvaluationID", is used for plot name.

**Lotic (MIM)**

Data file to upload: either Analysis or Data Entry MIM file

[Data Example](www/NB-SS-1122_McCulloskyCreek_09JUN2024_ANALYSIS.xlsm)

Notes:

-   The raw survey data doesn't include spatial information, so the original plot locations are used. This means that if a point is moved in the field, it could be moved into a different county. While this is unlikely, it means that when the plot data is compared with the PLANTS database, the comparison would not be accurate.

-   "PointID" is used for the point name.

-   You may upload multiple MIM files at the same time
