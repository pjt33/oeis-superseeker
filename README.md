# OEIS Superseeker

This repository contains the source code for the [OEIS Superseeker](https://oeis.org/ol.html), which is available as a
[monolithic file](https://oeis.org/ol_source.txt) but does not seem to have been distributed as a repository of files.

`README.txt` contains the header information from the monolithic file.

`notes.txt` contains other descriptive text from the monolithic file which didn't seem to belong anywhere else.

`Makefile` was added as preferable to comments explaining which files are source and what their targets should be called.

## Dependencies

I haven't yet looked through the files in detail, but the dependencies seem to include:

* C and Fortran compilers (my `Makefile` uses gcc and gfortran)
* Perl
* sh and ksh
* Maple
* Mathematica
* sendmail (although this can probably be removed quite easily in a refactor which ditches the mail UI)

and there are a few hard-coded paths which will need fixing.
