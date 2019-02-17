!right hand side of dy/dx=-ysinx
!
function f(x,y) result(func)
implicit none

real(8), intent(in) :: x,y
real(8) func

!_____________________________________________

func=-y*cos(x)

end function f
