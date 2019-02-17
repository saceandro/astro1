! example of function subprogram "kokutai"
!
! Intensity = 2hc/lambda^3 (exp((hc/kT)/lambda)-1)
!
! input: T(K), nu (Hz)
! output: intensity
!
function kokutai(T,nu) result(Intensity)
implicit none

real(8), parameter :: hpl = 6.62606D-27 ! planck const.
real(8), parameter :: c0 = 2.99792458D10 ! velocity of light
real(8), parameter :: kB = 1.38065D-16 ! boltzmann const.

real(8), intent(in) ::  T, nu ! input variables(temperature, frequency)
real(8) Intensity !  output varianble

integer inu
real(8) cof1, cof2
!________________________________________________________

cof1 = 2.00D0*hpl/c0**2
cof2 = hpl/(kB*T)

   Intensity = cof1*nu**3 / (exp(cof2*nu)-1.00D0)

end function kokutai
