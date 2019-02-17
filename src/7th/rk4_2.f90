subroutine rk4(dx,yi,yf)
implicit none

real(8), intent(in) :: dx,yi(6)
real(8), intent(out) :: yf(6)

real(8) Fnc(6) ! right-hand-side array of differential equation.
               ! Computed in subroutine "rhs"

real(8) eta_a(6), eta_b(6), eta_c(6), eta_d(6)
real(8) yh(6)  ! these are defined to compute eta_b to eta_d

!______________________________________


call right_hand_side(yi,Fnc)
eta_a(1:6) = Fnc(1:6)*dx
yh(1:6) = yi(1:6) + eta_a(1:6)*5.00D-1

call right_hand_side(yh,Fnc)
eta_b(1:6) = Fnc(1:6)*dx
yh(1:6) = yi(1:6) + eta_b(1:6)*5.00D-1

call right_hand_side(yh,Fnc)
eta_c(1:6) = Fnc(1:6)*dx
yh(1:6) = yi(1:6) + eta_c(1:6)

call right_hand_side(yh,Fnc)
eta_d(1:6) = Fnc(1:6)*dx
yf(1:6) = yi(1:6) + (eta_a(1:6) + 2.00D0*eta_b(1:6) +2.00D0*eta_c(1:6) &
                    + eta_d(1:6))/6.00D0

end subroutine rk4




