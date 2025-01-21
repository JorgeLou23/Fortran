program intnwt

module nwt
implicit none
contains
!subrutina diferencias divididas
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
      if x(i)=x(i+k)
         d(i,k)=d(i+1,k-1)*(1/k)
         d(i+1,k-1)=d(i,k-1)
      else if
         d(i,k)=(d(i+1,k-1)-d(i,k-1))/(x(i+k)-x(i))
   end do
end do
  
end subroutine difdiv

!declaracion de funcion

function p(t,n,x,d)
real :: p
real,intent(in) :: t
integer, intent(in) :: n
real, dimension (0:n), intent(in) :: x
real, dimension (0:n, 0:n), intent(in) :: x
integer :: !rellenar
!rellenar
end function p
end module nwt













end intnwt