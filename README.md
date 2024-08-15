# JHU_Colossus
****

Data and code for the Colossus Team project in JHU's Theory of Machine Learning course (EN.625.742.81.SU24). Our team built a random forest model to predict national medal counts at the Summer Olympic Games.


## Workflow 

| Name | Description | 
| :--- | :--- | 
| `Final Dataset/merging_datas.ipynb` | Code that combines all various datasets into FINAL_DATA.csv. Assumes other preprocessing scripts have been run.| 
| `modeling.ipynb` | The main notebook that trains, builds, and evaluates the final model. | 




## Directories 

### data_kalyn

* raw-data: Contains unformatted Olympic data, UN human development data, and gender inequality data.
* scripts: R files used to format the contents of the raw-data directory. Cleaned data is saved as individual files in this directory.

### Final Dataset

* `FINAL_DATA.csv` - The final dataset used to train the model.
* `merging_data.ipynb` - Jupyter notebook for merging the various datasets together.

### health_data_adeh 

* original_data - Raw datasets related to economic development, government spending, and health.
* cleaning_process - Directory containing cleaning.ipynb, a Jupyter notebook used to format and clean the datasets in the original_data directory, including intermediate versions.
* `merged_all_data.csv` -  Output from cleaning.ipynb.

### Political_Data_Jon 

* `2023-Human-Freedom-Index-Data.csv` and `political-regime.csv`:  Unformatted datasets.
* `political-regime_cleaned.csv` - Cleaned data.


### Population-Arian 

* Contains unformatted population data.









