module funciones
implicit none

contains

function f(x)
real, intent(in) :: x
real :: f
	f=exp(-x**2)
end function f


end module funciones

program quadrature
use funciones
implicit none

integer :: n, m, iopt, i, j, k
integer, parameter :: nmax=20
real, dimension(0:nmax) :: x, w 
real :: a, b, h, q
real :: pi=acos(-1.0)

print*, "extremo de integracion:"
read*, b

a=0.0
h=0.01
m=(b-a)/h
q=0.0


do i=0,m-1
	w(0)=1.0/6.0
	w(1)=4.0/6.0
	w(2)=1.0/6.0
	do j=0,2
		x(j)=h*(i+j/2.0)
		q=q+w(j)*f(x(j))
	end do
	
end do



q=2/sqrt(pi)*h*q	
	

print*, "integral entre 0 y 1.0 de f:", q




end program quadrature
