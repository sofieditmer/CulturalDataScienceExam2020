<p align="center" width="100%"><img width="33%" src="https://github.com/sofieditmer/CulturalDataScienceExamProject2020/blob/main/Sk%C3%A6rmbillede%202020-12-03%20kl.%2011.45.47.png"></p>

# Cultural Data Science Exam Project 2020 | Aarhus University
￼This repository contains the contents of the final exam project in the Cultural Data Science course at Aarhus University conducted in the fall of 2020, as well as all resources and materials needed in order to recreate the contents of the project. The primary goal of this project was to contribute with an interactive application in which it is possible to conduct quantitative text analysis which includes exploring semantic relations among words, generating a word cloud that displays the most frequent words within in a text in a highly visual manner, as well as perform sentiment analysis to discover the sentiment expressed by a self-chosen text or collection of texts. This platform is developed using the Shiny-package developed for R, that facilitates development of interactive applications within the RStudio environment. 


# How To Recreate the Shiny Application

1. Install R and RStudio on your local computer

2. Clone this repository to your local computer. This is done by clicking the green button named "Code" and copy the HTTPS-link. Now go to your bash on your local computer and navigate the location you want the repository to be using the cd-command, e.g. cd /Users/YourName/SomeFolder. When you have navigated to the preferred location you simply write git clone and paste the HTTPS-link and press ENTER.

3. Download this model to your local computer and place it in your repository: https://drive.google.com/uc?export=download&id=16L6WTOHk81qgkX7gT7XSPBtBr_qK4p-F. It is very important that this model is in the same location as the shiny app script in order to the shiny app to work properly.

4. In RStudio run the Setup-script provided within the repository: https://github.com/sofieditmer/CulturalDataScienceExam2020/blob/main/Scripts/Setup-Script.Rmd. Running this script ensures that all necessary packages are installed in order to run the shiny application.

5. In RStudio run the Shiny application, which is an R-script provided within the repository: https://github.com/sofieditmer/CulturalDataScienceExam2020/blob/main/Scripts/RShiny_Application.R

# Proof-of-Concept Video
Watch this video to see how the application works. Click on the link below to watch the proof of concept video:
[![Watch the video](https://github.com/sofieditmer/CulturalDataScienceExamProject2020/blob/main/Sk%C3%A6rmbillede%202020-12-03%20kl.%2011.39.22.png)](https://youtu.be/o_xUvygcZ10)

# LICENSE 
[MIT](https://github.com/sofieditmer/CulturalDataScienceExam2020/blob/main/LICENSE.md)
