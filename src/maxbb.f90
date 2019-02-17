! solve x+3(exp(-x)-1)=0 using Newton-Raphson method

program choetsu2
implicit none

! definition of variables
real(8) a ! initial value of x
real(8) x ! variable
real(8) z ! variable
real(8) f ! function f=x+3(exp(-x)-1)=0
real(8) df ! derivative of f  i.e. df=1-3exp(-x)
real(8) delta ! difference between x and real solution of f(x)=0
real(8) epsilon ! allowed error

a=2.800000
epsilon=1.000000D-10
z=a

do
   x=z
   f=x+3*(exp(-X)-1)
   df=1-3*exp(-x)
   delta=-f/df
   z=x+delta
   if (abs(x-z) < epsilon) exit
enddo 

write(*,'(e14.7)') x

end program choetsu2
