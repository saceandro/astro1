! example of subroutine
subroutine find_root(x_i, u0, epsilon, err, x_root)
implicit none

!=== definition of variables ===
real(8), intent(in) :: x_i, u0, epsilon
!          input variable passed from main_1
real(8), intent(out) :: x_root, err
!          output variable passed to main_1

real(8) x1, x2
!--- x1 & x2 are local variables used only inside this subroutine


!__________________________________________

! main part of subroutine starts

x1 = x_i  ! set x1 to be input x_i

do
   x2 = cos(u0 * x1**2) ! tikuji dainyu (x = cos(u*x**2)
   err = abs(x2-x1)/(abs(x1+x2)/2.0D0)  ! computing err
   if (err < epsilon) exit
   x1 = x2  ! if err > epsilon, set x2 to be x1 of the next loop
end do

x_root = x2  ! converged solution

end subroutine find_root

