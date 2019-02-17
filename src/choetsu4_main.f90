!main program of test of choetsu4_sub.f90
! solving transcendental function "x-exp(-ux)=0"
!
program choetsu4
implicit none

real(8) x_ini, u, eps
!   x_ini =>initial guess of root of the equation
!   u => parameter of
!   eps=> small tolerance number with which convergence is ditermined
!   error=> error in solved root

real(8) x_root !output variable from choetsu4_sub.f90
real(8) du !increment of parameter "u"
integer t 

!____________________________

open(unit=1, file='tmp_choetsu4.dat')

eps=1.00D-10
x_ini=0.00D0

du = (1.00D0-(-1.00D-1))/dble(110) !increment of u

do t=0,110
   u=-1.00D-1 + du*t

call find_root(x_ini,u,eps,x_root)

write(1, '(e14.8,3x,e14.8,3x,e14.8)') u, x_root
enddo

close(1)

end program choetsu4
