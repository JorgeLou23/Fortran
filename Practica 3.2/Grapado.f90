module tri
implicit none

contains

subroutine trisim(k,a,c,b,x)
integer, intent(in) :: k
real, dimension (1:k), intent(in) :: a,b
real, dimension (1:k-1), intent(in) :: c
real, dimension (1:k), intent(out) :: x
real, dimension (1:k) :: d
real, dimension (1:k-1) :: l
integer :: i

d(1)=a(1)
do i=1,k-1
	l(i)=c(i)/d(i)
	d(i+1)=a(i+1)-l(i)*c(i)
end do
x(1)=b(1)
do i=2,k
	x(i)=b(i)-l(i-1)*x(i-1)
end do
x(k)=x(k)/d(k)
do i=k-1,1,-1
	x(i)=x(i)/d(i)-l(i)*x(i+1)
end do

end subroutine trisim



function s(t,n,x,y,z,h)
real :: s
real, intent(in) :: t
integer, intent(in) :: n
real, dimension(0:n), intent(in) :: x,y,z
real, dimension(0:n-1), intent(in) :: h
integer :: i
real :: lambda0,lambda1

do i=0,n-2
	if (t<(x(i+1))) exit
end do
lambda0=(x(i+1)-t)/h(i)
lambda1=(t-x(i))/h(i)
s=y(i)*lambda0+y(i+1)*lambda1-(lambda0*lambda1*h(i)*h(i))*(z(i)*(lambda0+1)+z(i+1)*(lambda1+1))/6

end function s

end module tri



program grapado
use tri
implicit none

integer :: i,n,l, np=256
integer, parameter :: nmax=20
real,dimension(0:nmax):: x,y,h,d,z,a,b
real :: t, m_0, m_n



character(len=40):: infile,outfile
integer:: stat

print*, "nombre del fichero de datos"
read*, infile
outfile="grapado_"//infile


open(unit=1,file=infile)
do i=0,nmax
     read(1,*,iostat=stat) x(i),y(i)
     if(is_iostat_end(stat)) exit
end do
close(unit=1)
n=i-1




print*, "valor de la derivada en el primer extremo"
read*, m_0

print*, "valor de la derivada en el segundo extremo"
read*, m_n


do i=0,n-1
	h(i) = x(i+1)-x(i)
	d(i) = (y(i+1)-y(i))/h(i)
end do
do i=1,n-1
	a(i) = 2*(h(i-1)+h(i))
	b(i) = 6*(d(i)-d(i-1))
end do
a(0)=2*h(0)
a(n)=2*h(n-1)
b(0)=6*(d(0)-m_0)
b(n)=6*(m_n-d(n-1))

call trisim(n+1,a(0:n),h(0:n-1),b(0:n),z(0:n))



open(unit=2,file=outfile)
do l=0,np
	t=x(0)+l*(x(n)-x(0))/np
	write(2,*) t,s(t,n,x(0:n),y(0:n),z(0:n),h(0:n-1))
end do
close(unit=2)



end program grapado

