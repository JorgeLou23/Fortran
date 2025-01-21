module funciones
implicit none

contains

function f1(x)
real, intent(in) :: x
real :: f1
	f1=x**4
end function f1

function f2(x)
real, intent(in) :: x
real :: f2
	f2=1.0/(x**2+1.0)
end function f2

function f3(x)
real, intent(in) :: x
real :: f3
	f3=sqrt(max(x-x**2,0.0))
end function f3

end module funciones

program quadrature
use funciones
implicit none

procedure(f1), pointer :: f
integer :: n, m, iopt, i, j
integer, parameter :: nmax=20
real, dimension(0:nmax) :: x, w 
real :: a, b, h, q
character(len=40):: infile,outfile
integer:: stat

print*, "nombre del fichero de datos"
read*, infile


open(unit=1,file=infile)
read(1,*,iostat=stat) n
do i=0,nmax
     read(1,*,iostat=stat) x(i),w(i)
     if(is_iostat_end(stat)) exit
end do
close(unit=1)

print*, '1 -> int x**4 dx	 =', 1.0/5.0
print*, '2 -> int 1.0/(x**2+1.0) =', acos(-1.0)/4.0
print*, '3 -> int sqrt(x-x**2)	 =', acos(-1.0)/8.0
read*, iopt

select case(iopt)
case(2)
f=>f2
case(3)
f=>f3
case default
f=>f1
end select

a=0.0
b=1.0

print*, "numero de subintervalos m ="
read*, m

h=(b-a)/m
q=0.0
do j=0,m-1
	do i=0,n
	 	q=q+w(i)*f(a+(j+x(i))*h)
	end do
end do
q=h*q	
	

print*, "integral entre 0 y 1 de f:", q




end program quadrature
