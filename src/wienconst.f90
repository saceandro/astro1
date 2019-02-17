! solve x+3(exp(-x)-1)=0 (x > 0) using Newton-Raphson method
! constant of Wien's displacement law C=kB*x(max)/h (/s/K)
! where x(max) is the root of above transcendental equation

program wienconst
implicit none
!___________________________________________________
! definition of variables
real(8), parameter :: hpl = 6.626069D-34 ! Planck constant (J.s)
real(8), parameter :: kB = 1.380650D-23 ! Boltzmann constant (J/K)
real(8) C ! constant of Wien's displacement law (/s/K)
real(8) a ! initial guess of root of the equation
real(8) x ! variable
real(8) z ! variable
real(8) f ! function f=x+3(exp(-x)-1)
real(8) df ! derivative of f  i.e. df=1-3exp(-x)
real(8) delta 
!    approximate value of the difference between x and root of the equation
real(8) epsilon ! tolerable error between f and 0
!___________________________________________________
!from here main part starts

a=2.80D0
epsilon=1.00D-10
z=a

do
   x=z
   f=x+3*(exp(-x)-1)
   if (abs(f) < epsilon) exit
   df=1-3*exp(-x)
   delta=-f/df
   z=x+delta
enddo 

C=kB*x/hpl

write(*,'(e14.6)') C

end program wienconst

