subroutine right_hand_side(A,B,mmwe,rhoc,pc,r,y,Fnc)
implicit none

real(8),intent(in) :: r,y(2),A,B,mmwe,rhoc,pc
real(8),intent(out) :: Fnc(2)

Fnc(1) = y(2)
Fnc(2) = -2.00D0*y(2)/r - (A*mmwe/rhoc)*((A*mmwe*pc*y(1)/8.00D0/B/rhoc+1.00D0)&
         **2.00D0 - 1.00D0)**(3.00D0/2.00D0)

end subroutine right_hand_side
