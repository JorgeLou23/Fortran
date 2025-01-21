program intlag
implicit none

integer:: i,n,k,j
integer, parameter:: nmax=20
real,dimension(0:nmax):: x,y
integer:: l,np=256
real:: a,b,h,t,p,s
character(len=40):: infile,outfile
integer:: stat

print*, "nombre del fichero de datos"
read*, infile
outfile="lag_"//infile


open(unit=1,file=infile)
do i=0,nmax
     read(1,*,iostat=stat) x(i),y(i)
     if(is_iostat_end(stat)) exit
end do
close(unit=1)
n=i-1



a=x(0)
b=x(n)

open(unit=2,file=outfile)
h=(b-a)/np
do l=0,np
     t=a+l*h
     p=0.0
     do k=0,n
          s=y(k)
          do j=0,k-1
		s=s*(t-x(j))/(x(k)-x(j))
          end do
          do j=k+1,n
		s=s*(t-x(j))/(x(k)-x(j))
          end do
          p = p+s
     end do
     write(2,*) t,p
end do
close(unit=2)

end program intlag