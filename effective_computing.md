---
title: "Some tips for effective computing"
layout: default
---


> Imagine what a harmonious world it could be if every single
> person, both young and old shared a little of what he is good at
> doing.
> -- Quincy Jones

## My preferences

I use Windows for most of my work related stuff! I use Ubuntu linux
and a macbook for most of my personal stuff.

These are the software tools/packages (emphasized ones more frequently than alternatives) that I use most frequently:

1. **vim**/gvim and emacs.
1. **R** [my R related notes](NOTES_R), **NumPy/SciPy**, **J**, **Julia**, and **Factor**.
1. **NetLogo**, and GAMA.
1. **Python**, **OCaml**, **Haskell**, Scala, Lua, CLisp,   ruby, Clojure, and sometimes Perl
1. **PostgreSQL+PostGIS**, and **SQLite**.
1. ENVI, ERDAS Imagine, **Maptitude**, and **ArcGIS**/ArcGIS Pro.
1. **JabRef**, and ~~EndNote~~ Zotero (for bibliography management).
1. **Markdown (pandoc)**, LaTeX (MikTeX), and MS Office whenever I cannot get away with it!
1. Git.
1. Paint.NET, Inkscape.

**NOTE: ** I’m willing to answer questions about any of these tools (provided I know the answer)!

# General guidelines

1. All dates/times should be in ISO-8601 format. I.e., dates should be represented as `YYYY.MM.DD`, `YYYYMMDD`, or `YYYY/MM/DD` and time should be represented as `HHMMSS` or `HH:MM:SS`.
1. If there is a possibility of confusion of timezone, the time should be, preferably, in UTC timezone or include the local time zone offset.  See the explanation on the [http://www.iso.org/iso/date_and_time_format](ISO page).
1. Filenames should comprise of alphanumeric characters and underscores.  Try to avoid special characters, spaces, and punctuation as a part of filename.
1. Filenames should contain enough context information that is independent of folder structure.

## Programming related

1. Use version control software!
1. Comment your code.
1. Pick any coding guideline that you like and stick with it.  Be consistent.
1. Learn to use a plain text editor efficiently.
1. Learn `grep/awk/sed`!
1. Use `make/ant` or create batch scripts.

## Database related

1. All tables should have **PRIMARY KEY**.
1. Create a schema, named as the username used to login, after a `createdb`. Command (assuming username is 'vijay'): `CREATE SCHEMA vijay;` This makes porting data (during update/migration) much easier.
1. Default `DateStyle` is set to `ISO, MDY`. Change it to `ISO, YMD`. Command: `SET DateStyle = 'ISO, YMD';`
1. Always store `timestamp with time zone` (or `timestamptz`). This internally stores timestamp in UTC which gets displayed based on session timezone!
1. PostgreSQL allows DDL in `BEGIN-COMMIT/ROLLBACK` transaction blocks.  ALWAYS USE TRANSACTIONS!
1. Use `AS` when defining column/table aliases even though it is optional.  This simplifies writing queries.
1. Try to save the schema/table creation commands in a separate SQL script.  This will be useful in the future when you need to create the tables without the data. Also, you can version control this script!

## GIS related

1. If you are joining features to an external Excel (xls[x]/csv) file ensure that there is no space in the column titles! Especially at the end of column titles!  
1. Always make changes in an edit session! This is the closest thing <!-- similar --> to transactions(in database parlance).

* * * * * 

## Solutions to some _pesky_ computer issues

### LaTeX: Symbols not available!

I tried to use rhd and Diamond symbols for a LaTeX document and kept on getting the error: `LaTeX Error: Command \rhd not provided in base LaTeX2e.` This is because we need an extra package. Put `\usepackage{latexsym}` to get many more symbols in LaTeX.
I found this solution [here](http://www.cs.cmu.edu/~nbeckman/problem.html).  Apparently there are many packages that provide all kind of symbols for use in LaTeX. See the comprehensive listing [here](http://carroll.aset.psu.edu/pub/CTAN/info/symbols/comprehensive/symbols-a4.pdf).  

### MS Word: Cannot figure out what's causing this issue?

Use MS word in draft/outline view. Set the “Style area pane width...” to some practical value and solve many annoying problems. Shauna Kelly’s page at <http://www.shaunakelly.com/> under the “Making the most of Word in your business” title lists solutions and best practices for common MS Word issues!

* * * * *


## Some useful resources

- [How to name files](https://speakerdeck.com/jennybc/how-to-name-files)
- [Recommendations for Limitations on Image Filenaming for managing image collections and image databases](http://www.controlledvocabulary.com/imagedatabases/filename_limits.html)
- [Best of Vim Tips](http://rayninfo.co.uk/vimtips.html)
- [How to Organize Your Files](http://www.cs.jhu.edu/~jason/advice/how-to-organize-your-files.html)
- [LaTeX/General Guidelines](http://en.wikibooks.org/wiki/LaTeX/General_Guidelines)
- [Layout Tips for Technical Papers in Microsoft Word 2000.](http://research.microsoft.com/en-us/um/people/jckrumm/word%20tips/technical%20publishing.htm)
- [A Guide to Bulletproofing Your Data.](https://github.com/propublica/guides/blob/master/data-bulletproofing.md)</a>
