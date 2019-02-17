! right hand side of dy/dx = F(x,y)
!
function Func(x,y) result(F)
implicit none

real(8), intent(in) :: x,y
real(8) F
!_________________________________________

F = -x * y

end function Func
