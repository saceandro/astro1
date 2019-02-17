subroutine right_hand_side(y,Fnc)
implicit none

real(8), intent(in) :: y(6)
real(8), intent(out) :: Fnc(6)

real(8), parameter :: ni=2.00D0

!______________________________________

Fnc(1) = y(4)
Fnc(2) = y(5)
Fnc(3) = y(6)
Fnc(4) = -ni*y(4)*y(5)/y(2)/(y(2)-ni)
Fnc(5) = -(y(2)-ni)*y(4)**ni/y(2)**3.00D0 &
         -y(5)**ni/y(2)/(y(2)-ni) +(y(2)-ni)*y(6)**ni
Fnc(6) = -ni*y(5)*y(6)/y(2)

end subroutine right_hand_side






