!! example: black body radiation as a function of frequency
!
!
! Intensity = 2h*nu**3/c**2 * 1/(exp((h*nu/(kT))-1))
!
program blackbody ! Declaing the name of the program.
!
implicit none     ! All the variables must be declared explicitly.

!definition of variables
real(8) :: hpl = 6.62606D-27 !Planck constant(erg.s)
real(8) :: c0 = 2.99792458D10 ! speed of light(cm/s)
real(8) :: kB = 1.38065D-16 !Boltzmann constant(erg/K)

real(8) cof1, cof2 ! coefficients appearing in the expression of BB

real(8) T, nu, Intensity ! temperature, frequency of light, intensity

real(8) nu1, nu2 ! nu1: minimum frequency considered here
                 ! nu2: maximum frequency considereed here

integer inu ! integer to be used to count the frequency-grid points
!____________________________________________________________________
! From here main part starts

nu1 = 5.00D-1 !mininum frequency of of electromagnetic wave(in GHz)
nu2 = 1.00D3 !maximum frequency of electromagnetic wave(in GHz)

cof1 = 2.00D0*hpl/c0**2

T = 2.725D0  ! temperature of black body radiation (K)
             ! for cosmic microwave background, T=2.725K
cof2 = hpl/(kB*T)


open(unit=1, file='out.d', status='new')
! opening output data file

do inu = 1,1000 ! number of sampling points of frequency is 1000

   nu = 1.0D9 * 10.**(log10(nu1) &
        + (log10(nu2)-log10(nu1))/dble(999)*dble(inu-1))
! equally spacing points of variable nu:[nu1,nu2] in log_10(nu) 

   Intensity = cof1*nu**3 / (exp(cof2*nu)-1.00D0)
! computing intensity


   write(1,'(e14.7, 1x, e14.7)')  &
        nu/1.00D9, Intensity/1.0D3  
!        Data output :
!           1st column : frequency in GHz
!           2nd column : intensity in W/m^2/str/Hz

enddo

close(1)  ! closing the output data file

end program blackbody ! program ends here
