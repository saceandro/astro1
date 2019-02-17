! plot exp(-x**2)*sin(x)

program gauss_sin
implicit none

!definition of the variables
real(8) f
real(8) x
real(8) x0
real(8) x1
integer i

x0=-1
x1=1

open(unit=1, file='tmp_gauss_sin.d', status='new') !opening output data file

do i = 1,50

   x = x0 + (x1-x0)*(i-1)/49
   !equally spacing points of variable x:[x0,x1]
   
   f=exp(-x**2)*sin(x) !computing function f
   
   write(1, '(e14.7,3x,e14.7)') x, f
   ! Data output:
   !    1st column: x
   !    2nd column: f
enddo

close(1) ! closing the output data file

end program gauss_sin



        
   







