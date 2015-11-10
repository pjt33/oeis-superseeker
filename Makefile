all: dehtmlP getfrom2 proc8 proc9

dehtmlP:
	gcc -o dehtmlP dehtml.c

getfrom2:
	gcc -o getfrom2 -DSTANDALONE getfrom.c

proc8:
	gfortran -o proc8 superproc8.f

proc9:
	gfortran -o proc9 superproc9.f

clean:
	$(RM) dehtmlP getfrom2 proc8 proc9
