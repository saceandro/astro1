! test of euler method
! dy/dx = -xy, y(0) = 2
!
! calling euler.f90
!
program euler_test
implicit none

real(8) xmin, xmax, dx
integer Nstep      ! number of steps from xmin to xmax

real(8) x1, y1, y2
real(8) xdata(101),ydata(101)
integer istep, idat
!_______________________________________________


xmin = 0.0D0 ! minimum x
xmax = 1.0D0 ! maximum x
Nstep = 101 ! step number : dx = (x2-x1)/(Nstep-1)

dx = (xmax - xmin)/dble(Nstep-1) ! increment in x

   x1 = xmin
   y1 = 2.0D0  ! initial condition
!
   xdata(1) = x1
   ydata(1) = y1
!    storing data in arrays xdata & ydata


do istep = 1,Nstep
   call euler(x1,y1,dx,y2)  ! solving equation from x1 to x1+dx
                            ! the result is y2
   xdata(istep) = x1+dx  ! storing data in array
   ydata(istep) = y2  ! storing data in array
!
   x1 = x1 + dx
   y1 = y2   ! for the next step solution
end do ! end of loop

!-- output data to a file

open(unit=1,file='out.dat')

do idat = 1,Nstep
   write(1,'(e14.7,1x,e14.7)')xdata(idat),ydata(idat)
end do
close(1)

end program euler_test
