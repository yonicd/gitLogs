# gitLogs

On Twitter I saw a [bot](https://twitter.com/gepuro) that was tracking my R package activity when I created a new repo with a package in it. It pointed to a list of R packages on [github](http://rpkg.gepuro.net/).

I used that list to scrape all those repositories for the DESCRIPTION file in each pacakge (if there was one), and store them in a json file. 

With this file a lot can be learned on the activity in the unknown jungle of R package development. 

  - How many depends/imports packages have on github?
  - What fields are being currently developed?
  - Who is currently developing new ideas?
  
Hopefully this file will give answers to a few of those lingering questions of what is going on in github?

Below is a short summary of the ranking of packages used in the repositories by depend, import and suggest

![](https://raw.githubusercontent.com/yonicd/ciderhouse/master/gitLogs/git_ranks.png)

The script to scrape the directories and the current json (compressed as an Rdata file) can be found in the gitLogs subdirectory.