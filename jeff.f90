program CwaCalulator



implicit none

real::cwa
integer::i
character(len=20)::program

print*,"Please type your Programme of Study"
read*,program

print*,"Type in Your  CWA"
read*,cwa


do i=1,1
if(cwa.le.45)then
	print*,"F"
end if

if ((cwa>=46) .AND. (cwa<=55))then
	print*,"D"
end if	

if((cwa>=56) .AND. (cwa<=64))then 
	print*,"C"

end if
if((cwa>=65) .AND. (cwa<=69))then 
	print*,"B"
 
end if
if(cwa>=70)then 
	print*,"A"
end if 
end do
end program CwaCalulator
