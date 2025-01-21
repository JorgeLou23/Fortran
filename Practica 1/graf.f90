program graf
implicit none
integer:: i,np=256
real:: a,b,h,x,y
a=-6.0
b=6.0
h=(b-a)/np
open(unit=1,file='tabla.txt')
do i=0, np
       x=a+i*h
       y=sin(x)
       write(1,*) x,y
end do
close(unit=1)

end program graf