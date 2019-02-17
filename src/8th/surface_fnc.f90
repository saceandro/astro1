subroutine right_hand_side(N,x,y,Fnc)
implicit none

real(8), intent(in) :: N,x,y(2)
real(8), intent(out) :: Fnc(2)

!____________________________________

Fnc(1) = y(2)
Fnc(2) = -2.00D0*y(2)/x - abs(y(1))**N

end subroutine right_hand_side 
