subroutine ODE(yi,yf,dt)
implicit none
 
real(8), intent(in) :: yi(4), dt
real(8), intent(out) :: yf(4)
real(8) y(4)
real(8) f(4)
real(8) dy(4)
real(8) eta(4)
integer j


y(1:4)=yi(1:4)

call orbit_eq(y,f)

eta(1:4)=(dt/2.00D0)*f(1:4)

y(1:4)=y(1:4) + eta(1:4)

dy(1:4)=f(1:4)

yf(1:4)=y(1:4) + dy(1:4)*dt


end subroutine ODE
