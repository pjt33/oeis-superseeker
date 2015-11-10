c superproc9.f 
c gets char fns of binary seq
c fc -o proc9 superproc9.f
	implicit integer(a-z)
	integer lis(200),new(6,200),len(6)
	nmax=200
	read(*,*)n
	if(n.ge.5)goto 1
	write(*,2)
2	format("FAILURE")
	stop
1	continue
	if(n.gt.nmax)n=nmax
c read seq
	do 10 i=1,n
10	read(*,*)lis(i)
c temp print
c	write(*,100)(lis(i),i=1,n)
100	format(18i4)
c find the levels
	a=lis(1)
	b=a
	do 3 i=2,n
	if(lis(i).eq.a)goto 3
	b=lis(i)
	goto 4
3	continue
4	continue
c levels are a and b
c Char seq. 1, original seq with a,b -> 1,2
	do 22 i=1,n
	j=2
	if(lis(i).eq.a)j=1
22	new(1,i)=j
	len(1)=n
c Char seq. 2, original seq with a,b -> 2,1
	do 23 i=1,n
	j=2
	if(lis(i).eq.b)j=1
23	new(2,i)=j
	len(2)=n
c Char seq. 3, char fn of a's
	at=0
	do 20 i=1,n
	if(lis(i).eq.b) goto 20
	at=at+1
	new(3,at)=i
20	continue
	len(3)=at
c Char seq. 4, char fn of b's
	at=0
	do 21 i=1,n
	if(lis(i).eq.a) goto 21
	at=at+1
	new(4,at)=i
21	continue
	len(4)=at
c Char seq. 5, the run lengths
c Char seq. 6, the derivative
c change to 1's and 2's
	do 26 i=1,n
	j=2
	if(lis(i).eq.a)j=1
26	lis(i)=j
c look for the runs
	at=0
	dat=0
	v=lis(1)
	run=1
	i=2
30	continue
	if(lis(i).eq.v) goto 33
c there was a change
c log old run that just ended
	at=at+1
	dat=dat+1
	new(5,at)=run
	new(6,dat)=i-1
c start new run
	run=1
	v=lis(i)
	if(i.ge.n)goto 31
	i=i+1
	goto 30
c the run continues
33	run=run+1
	if(i.eq.n)goto 31
	i=i+1
	goto 30
c at end
31	continue
c	at=at+1
c	dat=dat+1
c	new(5,at)=run
c	new(6,dat)=run
	len(5)=at
	len(6)=dat
c done with #5
c print
	write(*,52)
52	format("CUT HERE")
	write(*,40)
	write(*,41)
	write(*,42)
	write(*,43)
40	format("TEST: BINARY SEQUENCE (with entries a,b say).")
41	format("The 6 characteristic sequences, all equivalent to the")
42	format("original (replace a,b by 1,2; by 2,1; positions of a's,")
43	format("of b's; run lengths; derivative) are:")
	write(*,*)" "
	do 99 j=1,6
	write(*,100)(new(j,i),i=1,len(j))
	write(*,*)" "
99	continue
c trivial?
	do 44 i=1,6
	if(len(i).lt.3)goto 45
44	continue
	goto 46
c yes trivial
45	continue
	write(*,47)
	write(*,48)
47	format("SUCCESS: Since one of these has length < 3, the original")
48	format("sequence was trivial.")
	stop
46	continue
c not triv, print in "f" style
	write(*,49)
49	format("Here is the result of looking up these sequences")
50	format("in the Encyclopedia:")
	write(*,53)
53	format("LAST LINE OF INTRO")
	do 54 j=1,6
	write(*,55)
55	format(3hf , , $ )
	do 56 i=1,len(j)
56	write(*,57)new(j,i)
57	format(i5, 1h, ,$)
	write(*,58)
58	format(1h )
54	continue
	write(*,*)"ZZZZ"
	stop
	end
