!analytical root y=exp(-sinx)
!
function g(x) result(funcg)
implicit none

real(8), intent(in) :: x
real(8) funcg

!_____________________________________

funcg=exp(-sin(x))

end function g
