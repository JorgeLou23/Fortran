module nwt
implicit none

contains

subroutine difdiv(n,x,y,d)

integer, intent(in) :: n
real, dimension (0:n), intent(in) :: x,y
real, dimension (0:n, 0:n), intent(out) :: d
integer :: i,k

do i=0,n
   d(i,0)=y(i)
end do
do k=1,n
	do i=0,n-k
		if (x(i)==x(i+k)) then
			d(i,k)=d(i+1,k-1)/k
			d(i+1,k-1)=d(i,k-1)
		else
			d(i,k)=(d(i+1,k-1)-d(i,k-1))/(x(i+k)-x(i))
		end if
	end do
end do

end subroutine difdiv

function p(t,n,x,d)
real :: p
real, intent(in) :: t
integer, intent(in) :: n
real, dimension(0:n), intent(in) :: x
real, dimension(0:n,0:n), intent(in) :: d
integer :: k
p=d(0,n)
do k=n-1,0,-1
	p=d(0,k)+(t-x(k))*p
end do

end function p

end module nwt



program intnwt
use nwt
implicit none

integer:: i,np=256,n
integer, parameter:: nmax=20
real::h,a,b,t
real,dimension(0:nmax):: x,y
real, dimension (0:nmax, 0:nmax) :: d
character(len=40):: outfile
integer:: stat

a=-5
b=5
n=12

10 format("nwt_runge_equi_", i2.2,".txt")
write(outfile,10) n



do i=0,n
	x(i)=a+(b-a)*i/n
	y(i)=1/(1+(x(i))**2)
end do

call difdiv(n,x(0:n),y(0:n),d(0:n,0:n))

open(unit=2,file=outfile)
h=(b-a)/np
do i=0,np
     t=a+i*h
     write(2,*) t,p(t,n,x(0:n),d(0:n,0:n))
end do
close(unit=2)

end program intnwt