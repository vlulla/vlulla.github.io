---
title: "Good Documentation"
layout: "post"
---

There is a great writeup about documentation at <https://documentation.divio.com/>. That site 
states that there are four kinds of documentation: *tutorials*, *how-to guides*, *explanation*,
and *reference*. Reading this made me realize why I like using [R](https://www.r-project.org) so
much. Not only does `R` have great documentation but it also follows many of the
points listed in the above link. Every time I use `R`'s documentation I invariably
learn something new or make some connection which had escaped me in the past.

# R's documentation

One of the primary reasons why R is my favorite tool is its amazing help system.
R's help system is invoked by using `?` command. For e.g., if I want to learn about
`lm` (linear model) I just type `?lm` in the console session and it will open up the help
related to that function. R's help system can be a great source of learning if
you know how to peruse it. Here's how I go about using it:

- Read the **Description** which is at the start of the page.
  It generally lists the mnemonic that might help you remember the command.
  For e.g., `lm` stands for linear model, `str` stands for \*str\*ucture of
  an R object. Becoming aware of the mnemonic makes it easier to remember, and
  later recollect, various functions provided by R.
- Briefly skim the **Usage** section. See the various ways in which the function
  can be called and how it is used. Don't worry about it too much if it doesn't make
  too much sense at the moment.
- Scroll all the way to the bottom of the help page where you will find the **Examples**
  section. Read the examples and literally (i.e., no copy/paste) type them in your
  R console to learn how to use the function. You can also use the `example`
  function to run all the code in the Examples section automatically. For instance,
  typing `example(str)` in the R console will run the examples from `str` function.
  However, I discourage using `example` function until you are a bit more comfortable with R.
- Now scroll back up to the **Usage** section again and reread it. It will now, hopefully,
  make a lot more sense of various ways of using the function.
- Finally, scroll back to the **Examples** section where you will find the **See Also**
  section just above it. The functions/commands/options listed in this **See Also**
  section are somehow related with the current function that you are currently
  learning about. *Consistently exploring the recommendations made in the 
  **See Also** section is the surest way to improve your proficiency in R.*

The brilliance of R's documentation system is that it aids in most of the different tasks
listed in the link above. The brief **Usage** section at the top of the documentation page
and the **Examples** section at the end of the page give you a self-contained
*tutorial* and *how-to guide*! The **Details** section provides the *Explanation*
about the function and all its input parameters where as the **Value** section
explains the output of the function. The **See Also** section shows how this topic
is related with other similar topics which helps in learning how this function
fits into the R environment. All these various sections not only aid in your data
exploration but also helps you in getting a better understanding of the R environment
and the tools it provides.
