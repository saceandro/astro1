!solve x-exp(-ux)=0
!
subroutine find_root(x_ini,u,eps,x_root)
implicit none

real(8), intent(in) :: x_ini, u, eps !input variable passed from choetsu4_main
real(8), intent(out) :: x_root !output variable passed from choetsu4_sub
real(8) x, z !variables
real(8) f !function f=x-exp(-ux)
real(8) df !derivative of function f i.e. df=1+u*exp(-ux)
real(8) delta !difference between x and the real solutionof f(x)=0

!---------------------------
 
x=x_ini

do
z=x
f=z-exp(-u*z)
df=1+u*exp(-u*z)
delta=-f/df
x=z+delta
if (abs(delta) < eps) exit
enddo

x_root=x ! converged solution

end subroutine find_root


   
