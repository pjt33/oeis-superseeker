Source Code For Email Servers and Superseeker

N. J. A. Sloane

Aug 15 2010

The email server and Superseeker have been on the web since 1994.
They are described in http://www.research.att.com/~njas/sequences/ol.html.

Now that the OEIS is owned by The OEIS Foundation Inc (see http://oeisf.org),
is about to become a wiki, and will be moved to a new host
(see http://oeis.org/classic/ and http://oeis.org/wiki/)
a number of people have suggested that the email server and
Superseeker should go "open source".
Of course they will also be moved to the new host.
We already have licenses for Maple and Mathematica on
that machine.

Here therefore is the present version of the source code 
for these two services.

Keep in mind that these programs were written by me alone,
over a period of 16 years, and were not intended to be
made public.

They make use - with permission - of several programs written by friends,
which may not be in the public domain.

These programs were run on a series of machines,
especially one called "fry", and most recently on "prim".
These have different operating systems, so some of the commands and subprograms
come in two versions.

The programs use the plain flat file containing all the sequences, which
is called "cat25". Of course this changes all the time.

$ wc cat25
  2761057  22805802 187271736 cat25

Many thanks to Mira Bernstein, Harm Derksen, Olivier Gerard,
Christian Kratthentaler, John Linderman, Simon Plouffe,
Bruno Salvy, Paul Zimmermann and others whose programs
are incorporated in Superseeker.
