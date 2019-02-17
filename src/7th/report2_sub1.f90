!
! 4th order Runge-Kutta subroutine
!
!
subroutine rk4(dx,yi,yf)
implicit none

real(8), intent(in) :: dx,yi(6)
real(8), intent(out) :: yf(6)

real(8) Fnc(6) ! right-hand-side array of differential equation.
               ! Computed in subroutine "rhs"

real(8) eta_a(6), eta_b(6), eta_c(6), eta_d(6)
real(8) yh(6)  ! these are defined to compute eta_b to eta_d
!________________________________________________________


! [1] compute eta_a
call right_hand_side(yi,Fnc) ! compute right hand side for (yi)
eta_a(1:6) = Fnc(1:6) * dx
yh(1:6) = yi(1:6) + eta_a(1:6)*5.00D-1 ! this is used to compute eta_b

! [2] compute eta_b
call right_hand_side(yh,Fnc) ! compute right hand side for (x,yi)
eta_b(1:6) = Fnc(1:6) * dx
yh(1:6) = yi(1:6) + eta_b(1:6)*5.00D-1 ! this is used to compute eta_c


! [3] compute eta_c
call right_hand_side(yh,Fnc) ! compute right hand side for (x,yi)
eta_c(1:6) = Fnc(1:6) * dx
yh(1:6) = yi(1:6) + eta_c(1:6) ! this is used to compute eta_d

! [4] compute eta_d
call right_hand_side(yh,Fnc) ! compute right hand side for (x,yi)
eta_d(1:6) = Fnc(1:6) * dx

! [5] assembling all eta's and compute next step solution at x+dx
yf(1:6) = yi(1:6) + (eta_a(1:6) + 2.00D0*eta_b(1:6) &
                        + 2.00D0*eta_c(1:6) + eta_d(1:6) )/6.00D0


end subroutine rk4
