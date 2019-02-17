subroutine orbit_eq(y,f)
implicit none

real(8), intent(in) :: y(4)
real(8), parameter :: ni=2.00D0
real(8), intent(out) :: f(4)

f(1)=y(3)
f(2)=y(4)
f(3)=y(1)*y(4)**ni - y(1)**(-ni)
f(4)=-ni*y(3)*y(4)/y(1)

end subroutine orbit_eq



