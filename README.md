# Teaching Large Language Models to Infer Information from Baseball Play Descriptions

The files in this repository were used to fine-tune a Large Language Model to return the number of runs a baseball team scored in a game and which team won the game using the descirptions of the scoring plays within the game. 

- tm_project_data.R is an R script that creates a set of training and testing documents and responses from raw Statcast game data

- The .csv files included in this repository are the set of training and testing data used in this project

- Run Calculaion Trainer.ipynb and Game Information Trainer.ipynb are Jupyter Notebooks (meant to run in Google Colab) that fine-tune the LLM for the relevant tasks

- Create Responses.ipynb is a Jupyter Notebook (meant to run in Google Colab) that returns response data from a set of test prompts

- tm_response_process.R is an R script that extracts relevant information from the model response data

- The .xlsx files included in this repository show the results from manual evaluation of the model responses in this project

- The folders included in this repository contain the files required to load the trained adapter models
