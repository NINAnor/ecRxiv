![alt text](https://github.com/NINAnor/ecRxiv/blob/main/docs/_ecrxiv_logo_hovedlogo_tekst_under.png?raw=true)

[![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-2.1-4baaaa.svg)](docs/code_of_conduct.md)

## NEWS
ecRciv is now receiving indicator documentation. These are being subjected to both code reviews and other more general reviews.
Code reviews are typically found under old/closed pull requests, whilst other reviews are found under _issues_ by filtering for the tag _REVIEW_.

We plan to publish and make the documentation (the fact sheets) available through an online web application. This app is under active development and will be made available soon.

The internal (to NINA) sandbox version can be viewed [here](https://internal-shiny.nina.no/ecRxiv/)

There are currently three indicators documented on ecRxiv

- Woody encroachment, LiDRA-based [[NO_GJEN_001](http://htmlpreview.github.io/?https://github.com/NINAnor/ecRxiv/blob/main/indicators/NO_GJEN_001/R/NO_GJEN_001.html)]
- Woody encroachemnt, aerial images based [[NO_GJEN_002](http://htmlpreview.github.io/?https://github.com/NINAnor/ecRxiv/blob/main/indicators/NO_GJEN_002/R/NO_GJEN_002.html)]
- Pollinator potential [[NO_POPO_001](http://htmlpreview.github.io/?https://github.com/NINAnor/ecRxiv/blob/main/indicators/NO_POPO_001/R/NO_POPO_001.html)]

All are beta versions (v. 000.001), and under active review and developement. 

All versions of the indicators get their own stable url, so you can always cite and view any version.




## Rationale

Ecosystem condition indicators are used in ecosystem condition assessments and accounts to monitor the quality and integrity of nature. These assessments and accounts can be important policy instruments that guide nature governance. When presented in reports aimed for managers and policy makers, indicators are often described just enough for them to be interpretable. However, indicator calculations/designs are often quite technical and include many steps that cannot be described fully in a report that may include several indicators and indices. There is a need to document these workflows in order to validate and review their soundness and relevance for describing ecosystem condition. This documentation will also be a benefit to the ecosystem condition indicator community of practice because it makes it easier to learn from each other and develop the field quicker.

## How to use ecRxiv

On this site (this GitHub repository) you can submit technical documentation and ecological justification for your ecosystem condition indicator(s) and have it published on an online web application in a standardised format along side other indicators from anywhere in the world.

As a minimum requirement, the documentation must be renderable as an html, and you must submit a separate metadata file (excel template is provided). However, we encourage, and facilitate, the publication of fully reproducible workflows in the form of quarto files and associated data files.

### How to review an indicator
Indicator reviews are handles though GitHub Issues. Got to the issues tab and select *New Issue*. 
If you are adding a complete review, similar to peer reviewing a scientific article, use the *Indicator review - checklist* template.
If you just want to add a comment and a more simple review, use the *Indicator feedback* template.

### How to review a PR and update the shiny app (for admins)
This is for ecRxiv administrators who first come incontact with a new PR. 
Open a new issue on GitHub and chose the *PR review - checklist* template. Follow the instructions there.

### How to submitt indicator documentation to ecRxiv

[![Tutorial video](https://img.youtube.com/vi/wLYnORKZ1ZQ/0.jpg)](https://www.youtube.com/watch?v=wLYnORKZ1ZQ)

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

`CO_NAME_XXX`

where

-   `CO` = two letter country code (ISO 3166-1 alpha-2) indicator the country that the indicator applies to, or if it applies to multiple countries, the country of the main affiliation for the main author.

-   `NAME` = four letter code for the indicator name (usually the first four letters of the full name, e.g. ALIE for the the indicator Alien Species)

-   `XXX` = three digit **major version number**. Major versions include substantial updates to the methodology or data origins behind the indicator. If the same indicator is used more-or-less in the same way for two ecosystem types, version number can also refer to ecosystem type. 


Each indicator (each unique indicator ID) will also have its own **version number**,
where 000.001 denotes a dirst draft not ready for wide use, 
and 001.000 is used for the first operational version.
The version number is not part of the indicator ID, and subsequent versions overwrite each other.


