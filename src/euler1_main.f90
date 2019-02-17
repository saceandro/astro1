!solve ODE dy/dx=-ycosx, y(0)=1
!calling euler_sub.f90
!using function f in euler_func.f90
!
program euler1_main
implicit none

real(8) xmin, xmax, dx
integer N

real(8) x1, y1, y2
real(8) xdata(101), ydata(101), yadata(101)
integer i, j

real(8), parameter :: pi = 4.00d0*atan(1.00d0)

real(8) :: g
external g
!___________________________________________

xmin=0.0D0
xmax=pi
N=101

dx=(xmax-xmin)/dble(N-1)

x1=xmin
y1=1.0D0

xdata(1)=x1
ydata(1)=y1
yadata(1)=g(x1)

do i=2,N
   call euler(x1,y1,dx,y2)
   x1=x1+dx
   y1=y2
   xdata(i)=x1
   ydata(i)=y1
   yadata(i)=g(x1)
enddo

open(unit=1, file='tmp_euler1.dat')
do j=1,N
   write(1,'(e14.7,3x,e14.7,3x,e14.7)')xdata(j),ydata(j),yadata(j)
enddo
close(1)

end program euler1_main





