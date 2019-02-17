!
! 4th order Runge-Kutta subroutine
!
!
subroutine rk4(A,B,mmwe,rhoc,pc,x,dx,yi,yf)
implicit none

real(8), intent(in) :: x,dx,yi(2),A,B,mmwe,rhoc,pc
real(8), intent(out) :: yf(2)

real(8) Fnc(2) ! right-hand-side array of differential equation.
               ! Computed in subroutine "rhs"

real(8) eta_a(2), eta_b(2), eta_c(2), eta_d(2)
real(8) xh, yh(2)  ! these are defined to compute eta_b to eta_d
integer ieq
!________________________________________________________

xh = x + dx * 5.00D-1  ! xh = x + dx/2

! [1] compute eta_a
call right_hand_side(A,B,mmwe,rhoc,pc,x,yi,Fnc) ! compute right hand side for (x,yi)
do ieq = 1,2
   eta_a(ieq) = Fnc(ieq) * dx
   yh(ieq) = yi(ieq) + eta_a(ieq)*5.00D-1 ! this is used to compute eta_b
enddo

! [2] compute eta_b
call right_hand_side(A,B,mmwe,rhoc,pc,xh,yh,Fnc) ! compute right hand side for (x,yi)
do ieq = 1,2
   eta_b(ieq) = Fnc(ieq) * dx
   yh(ieq) = yi(ieq) + eta_b(ieq)*5.00D-1 ! this is used to compute eta_c
enddo

! [3] compute eta_c
call right_hand_side(A,B,mmwe,rhoc,pc,xh,yh,Fnc) ! compute right hand side for (x,yi)
do ieq = 1,2
   eta_c(ieq) = Fnc(ieq) * dx
   yh(ieq) = yi(ieq) + eta_c(ieq) ! this is used to compute eta_d
enddo

! [4] compute eta_b
call right_hand_side(A,B,mmwe,rhoc,pc,x+dx,yh,Fnc) ! compute right hand side for (x,yi)
do ieq = 1,2
   eta_d(ieq) = Fnc(ieq) * dx
enddo

! [5] assembling all eta's and compute next step solution at x+dx
do ieq = 1,2
   yf(ieq) = yi(ieq) + (eta_a(ieq) + 2.00D0*eta_b(ieq) &
                        + 2.00D0*eta_c(ieq) + eta_d(ieq) )/6.00D0
enddo


end subroutine rk4
