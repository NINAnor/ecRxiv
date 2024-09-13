## How to contribute

[Here](https://github.com/NINAnor/ecRxiv) you can submit technical documentation and ecological justification for your ecosystem condition indicator(s) and have it published on the online web application in a standardised format along side other indicators from anywhere in the world.

As a minimum requirement, the documentation must be renderable as an html, and you must submit a separate metadata file (excel template is provided). However, we encourage, and facilitate, the publication of fully reproducible workflows in the form of quarto files and associated data files.

## Workflow

The Workflow is outlined [here](https://www.youtube.com/watch?v=vaFVaC7A48o) 

The submission and publication of indicator documentation is also described in the numbered workflow below.


1.  **Fork the main branch of this repository (repo)**. The main branch contains a lot of data files, but creating a fork does not cause create copies of the files themselves, and does not lead to higher data storage requirements.
2.  **Make a partial clone your forked repo**. Although it is possible to edit the files in your fork directly through GitHub in the web browser, most users will prefer to make a local copy of the repo and edit files in a separate software, such as RStudio. To avoid copying all the data files and git history for all the other indicators on ecRxiv, we highly recommend making a partial clone, and not a full (normal) clone. To do this you need to talk to git via the command line (don't be scared!). As a general solution, open the folder where you want to clone the repo into, right click, and open Git Bash. Alternatively, if working in RStudio, you can write directly in the terminal window:
  
  `git clone --filter=blob:none --no-checkout https://github.com/<USER>/<REPO>`

This clones just the .git folder, but does not copy down any bigger data files from the web. Now you can choose which directories (folders) you want to clone. This is called sparse checkout. First, move working directory one folder up:

`cd <REPO>`

Then initiate sparse checkout based on the root folder:

`git sparse-checkout init --cone`

Checkout root folder (downloads blobs (files), some of which you'll need):

`git checkout <BRANCH NAME>`

BRANCH NAME is likely to be `main` in the above. Now, specify additional folders to download:

`git sparse-checkout set <dir1>/<dir2>`

If you are starting to work on a new indicator (not updating an existing indicator), dir1 should be `indicators` and dir2 should be `template`


3.  Copy the `template` folder and name the new folder, and files and folders inside it, according to our [naming convention](#naming-convention). Make sure not to simply rename the `template` folder - make a real copy.
4.  Document your indicator using the templates provided. Smaller data files can be stored in the `data/` folder. Fill in `metadata.xlsx` (don't rename this file as it is automatically read and used to populate tables in the quarto file). Keep your forked repo up to date with your work by routinely pushig your changes.
5.  Render the quarto file to html
6.  Do a pull request (PR) from your forked repo to the `main` branch in this repo.
7.  Administrators of this repo will check that the submission (the PR) is done properly and that all files have been named in the correct way. If you have submitted data and code, this review will also include checking that the code is able to locate the data and run, and that proper code annotation is provided. Revise the PR until it reached the requirements set by the administrators.
8.  The PR will be merged with the `main` branch and the indicator documentation (the rendered html) will be published on an online web application.
9.  The publicly available documentation is now subject to voluntary review, for which there are separate guidelines.
10.  The PR authors can revise their documentation as many time as needed, and in response to review by peers. Major revisions may result in the creation of a new version number for the original indicator.

### Naming convention

All indicators are given a unique code:

`CO_NAME_XXX_YYY`

where

-   `CO` = two letter country code (ISO 3166-1 alpha-2) indicator the country that the indicator applies to, or if it applies to multiple countries, the country of the main affiliation for the main author.

-   `NAME` = four letter code for the indicator name (usually the first four letters of the full name, e.g. ALIE for the the indicator Alien Species)

-   `XXX` = three digit major version number. Major versions include substantial updates to the methodology or data origins behind the indicator. If the same indicator is used more-or-less in the same way for two ecosystem types, version number can also refer to ecosystem type. 

-   `YYY` = three digit minor version number. Minor version changes include updates bigger than just an increase in the data (e.g. from a yearly update), but smaller than a major update.
