c superproc8.f 
c looks if diffs are periodic
c compile with  f77 -o proc8 superproc8.f
	implicit integer(a-z)
	integer tab(0:100,0:100)
	nmax=100
	read(*,*)n
	if(n.ge.3)goto 1
	write(*,2)
2	format("FAILURE")
	stop
1	continue
	if(n.gt.nmax)n=nmax
c read seq
	do 10 i=0,n-1
10	read(*,*)tab(0,i)
c look at diffs of level "lev"
	do 20 lev=0,n-3
c set ntm = no of terms at this level
	ntm=n-lev
	if(lev.eq.0)goto 21
c need to compute diffs to get this level
	do 30 i=0,ntm-1
30	tab(lev,i)=tab(lev-1,i+1)-tab(lev-1,i)
21 	continue
c see if periodic
c try init segment of length nseg
	do 40 nseg=1,ntm-2
c see if rest of row agrees with prefix
	do 50 i=nseg,ntm-1
	j=mod(i,nseg)
c is tab(lev,i) same as prefix ?
	if(tab(lev,i).ne.tab(lev,j))goto 60 
50	continue
c success
	write(*,200)
200	format("TEST: ARE DIFFERENCES OF SOME LEVEL PERIODIC?")
	write(*,*)"SUCCESS: Differences of order ",lev
	write(*,*)"appear to be periodic.  If so next 2 terms are:"
c compute 2 more terms!
	j=mod(ntm,nseg)
	tab(lev,ntm)=tab(lev,j)
	tab(lev,ntm+1)=tab(lev,j+1)
c go upwards
	if(lev.eq.0)goto 71
	do 70 ilev=lev-1,0,-1
c fill in row ilev
	itm=n-ilev
	tab(ilev,itm)=tab(ilev,itm-1)+tab(ilev+1,itm-1)
	tab(ilev,itm+1)=tab(ilev,itm)+tab(ilev+1,itm)
70	continue
71	continue
	write(*,51)tab(0,n),tab(0,n+1)
51	format(2i15)
	write(*,*)" "
c temp dump table
c	write(*,*)"table"
c	do 80 i=0,n-2
c80	write(*,81)(tab(i,j),j=0,n-i+1)
81	format(18i4)
	stop
60	continue
40	continue
20	continue
	write(*,2)
	stop
	end
