subroutine euler_sys(t,dt,yi,yf)
implicit none

real(8), intent(in) :: t,dt,yi(3)
real(8), intent(out) :: yf(3)
real(8) dy(3)
integer j


call LHS(t,yi,dy)

do j=1,3
   yf(j)=yi(j) + dy(j)*dt
enddo

end subroutine euler_sys
