! main program for test of sub1.f90
! solving equation   "x-cos(u*x**2) = 0"
!
program main_1
implicit none
!=== definition of variables ===
real(8) x_ini, u, eps, error  ! these are input variable to sub1
!   x_ini => initial guess of root of the equation
!   u => parameter of 
!   eps => small tolerance number with which 
!          convergence is detemined
!   error => error in solved root

real(8) x_root    ! these are output variable from sub1
!   x_root => root of equation


real(8) du  ! increment of parameter "u"
integer ir  ! integer to be used as a counter of do-loop
!___________ below is main part ___________________________


open(unit = 1, file = 'test.dat')

! set up some parameters
eps = 1.00D-7
x_ini = 3.00D-1


du = (1.00D0-1.00D-1)/dble(100)  ! increment of u


do ir = 1,101
   u = 1.00D-1 + du * dble(ir-1)  
           ! u is chaged 100 times from u=0.1 to 1.0
           !   with equal spacing of du

   call find_root(x_ini, u, eps, error, x_root)

   write(1,'(e14.8,1x,e14.8,1x,e14.8)') u  &
                  , x_root, error
enddo

close(1)

end program main_1
