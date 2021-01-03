# VitaminB12 Deficiency
Analysis of publicly available National Health and Nutrition Examination Survey (NHANES) data from 2013-2014 to identify variables correlated with low/borderline low vitamin b12 levels <br><br>
_Data was sourced from a Kaggle [repository](https://www.kaggle.com/cdc/national-health-and-nutrition-examination-survey?select=labs.csv), originally pulled from [NHANES](https://wwwn.cdc.gov/nchs/nhanes/ContinuousNhanes/Default.aspx?BeginYear=2013)._<br>
### Within this repository there are the following files which contain more information:
* _0-DataDictionary.csv:_ contains a reduce list of variables (including definitions & values) used in model selection, binary columns indicate if a variable was used in/selected for certain parts of the data process (0=no, 1=yes)
* _1-DataPrep.R:_ first code file in R that contains script used to clean, prep, and understand data prior to modeling
