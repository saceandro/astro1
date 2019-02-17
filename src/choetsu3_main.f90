!main program of solving x-cos(u*x^2)=0
!calling function subprogram "choetsu_cos"
!
program choetsu3
implicit none

real(8) u, x0 !parameter, solution of transcendental equation
real(8) :: choetsu_cos ! the name of function called in this
integer t

!main part
open(unit=1, file='tmp_choetsu3.dat', status='new')

do t = 0, 50
   u=-0.1 + (1-(-0.1))*t/50
   x0=choetsu_cos(u)
   write(1,'(e14.7,3x,e14.7)') u, x0
enddo

close(1)

end program choetsu3
