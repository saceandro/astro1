! solve x-cos(x)=0 using Newton-Raphson method

program choetsu2
implicit none

! definition of variables
real(8) a ! initial value of x
real(8) x ! variable
real(8) f ! function f=x-cos(x)
real(8) df ! derivative of f  i.e. df=1+sin(x)
real(8) delta ! difference between x and real solution of f(x)=0
real(8) epsilon ! allowed error

a=0.73
epsilon=1.00D-10
x=a

do
   f=x-cos(x)
   df=1+sin(x)
   delta=-f/df
   x=x+delta
   if (abs(f) < epsilon) exit
enddo 

write(*,'(e14.6)') x

end program choetsu2



 
