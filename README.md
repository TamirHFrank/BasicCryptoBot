# BasicCryptoBot

This bot is a great way to lay the foundations of automatic crypto trading. It is a side project of mine that is far from complete, but I want to share my progress with the community as I continue to improve the model. <br/>

So far, this logs the user into Robinhood, allows them to choose their desired currency, input a few other key components, and then begin trading! More specifically, this graphs realtime data pulled from Robinhood's API and runs a simple regression to decide whether or not to buy / sell. <br/>

I recommend starting with the Portfolio Manager.R file, while keeping the other two open as well. The first steps are installing all the necessary packages, changing the working directory to your desired folder, and updating your Robinhood login info at the beginning of the 'main' function at the bottom of the file. From there, try editing the num.points, delay, and the second number of the for loop to get your desired starting data points, prediction time, and program runtime respectively. Feel free to reach out with any questions!

There is a long list of items that I would like to improve, but the top priorities include the following: <br/>
    1) Timing of the order - right now the outstanding order is canceled too quickly for the API to properly process, so I need to figure out an efficient way to execute <br/>
    2) Stronger prediction model - currently, this is running a simple linear regression on basic data points, so, for obvious reasons, this will need to be improved <br/>
    3) Clean up the code - lots of redundant lines, sloppy fixes, and random print statements, but again this is a work in progress as I debug and improve so bear with me :)<br/>
    4) Save the predictions and data collected into an excel sheet - once saved, further analysis can be performed for other side purposes. 
