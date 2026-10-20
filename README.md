# An open-source deep-learning model for classifying the content of social media images across European natural sites

This repository contains the code and statistical analyses for the paper:

Barrere J, Macia FM, Chivulescu S., van Dijk J, van Eupen M, Giuca RC, Luque S, 
Roche P, Schirpke U, Tappeiner U, Tardieu L, Zulian G & Lenormand M (2026)
**An open-source deep-learning model for classifying the content of social media images across European natural sites** 
(in preparation)


The code requires prior installation of [R](https://cran.r-project.org/) and 
[Python](https://www.python.org/), and is structured as follows:

- The notebook **AI.ipynb** contains the code to train the deep-learning model 
and export the outputs of the different experiments. To run, it needs access to 
GPU calculation resources, and a data folder containing the images which can 
be made available upon request.
- The files **requirements.txt** and **install.sh** are used by the main 
notebook to install all dependencies for the deep-learning model.
- **`model.py`** contains all functions and classes required for the 
deep-learning model.
- The different R scripts in the **paper** directory contain the code used to 
format the outputs of the notebook and produce the tables and figures for the 
paper. 