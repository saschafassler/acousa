# HOWTO: Use the acousa Repository #
The following page describes how to setup and get started with using the acousa survey analysis repository. The repository is a version control system for analysis code, allowing multiple users to collaborate on a common survey analysis project by sharing and synchronising their code. In addition, it also provides version control on a line-by-line basis - all versions are saved, and its always possible to go back!

## System Requirements ##
You will need the following software to use the acousa repository.
  * R (version 2.14.2 or greater) for your system. [Available here](http://cran.r-project.org/).
  * An SVN (subversion) client. For Windows users, we recommend [TortoiseSVN](http://tortoisesvn.tigris.org/). Linux users will typically have access to such a client through their package distribution system.
  * Some way to write and edit R code. For Windows, try [RStudio](http://rstudio.org/) or [Tinn-R](http://sourceforge.net/projects/tinn-r/). For Linux, you almost certaintly already have [Vi](http://en.wikipedia.org/wiki/Vi)

## Getting the Code ##
Once you've got all the necessary bits and pieces, the next step is to fetch a "working copy" of the acousa repository from the server. The SVN client that you have installed takes care of this. The general process is client specific, but generally involves two steps 1. Create a directory for your working copy (use D:/Repository/ACOUSA/ in order for the standard analysis code to recognise it) and 2. "Checking out" the repository.

We'll assume you can handle #1 yourself, but number 2 is a bit trickier, and is of course dependent on your SVN client. There are a number of good resources available e.g. for windows
  * On the [mseflr](http://code.google.com/p/mseflr/) project wiki

There are some key points to note here.
  * Firstly there are two different ways to checkout the code, depending on whether you want to commit the changes back to the repository or not. If you checkout the repository from `http://acousa.googlecode.com/svn/trunk/ ` it will not be possible to commit your changes back - however, the benefit is that **anyone** can get access to the repository, without the need for a google account.
  * Alternatively, you can check out the code from `https://acousa.googlecode.com/svn/trunk/ ` (note the **https**, rather than http), in which case you will be able to commit your changes back. The disadvantage is that this requires a google account (e.g. gmail.com).
  * Finally, there is a wee trap for beginners when logging in for the first time - make sure that you use the password for the project that googlecode supplies you - **not** your login password to google itself (slightly counter-intuitive, I know)! This is detailed on in the [mseflr](http://code.google.com/p/mseflr/wiki/UsingTortoiseSVN) page under "Project member checkout".

## Installing the acousaR package ##
Once you've checked out a copy of the acousa repository, you're nearly ready to start running the survey analysis. You will also need the acousaR package, which contains the underlying code. As Google has discontinued the download facility it used to provide on its code project sites, binary copies of the newest acousaR package version can no longer be downloaded here.
Therefore, the latest version of the package is now hosted at [GitHub](https://github.com/) and can be downloaded from:

https://github.com/saschafassler/acousaR/releases

You could of course also create these yourself using the source code from the repository. Alternatively you can request them by sending an email to any of the repository Owners.