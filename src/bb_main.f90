! main program of black body
! calling function subprogram "kokutai"
!
! Intensity = 2hc/lambda^3 (exp((hc/kT)/lambda)-1)
!
program blackbody
implicit none

real(8) T, nu, Intensity ! temperature, frequency, intensity

integer inu
real(8) nu1, nu2

real(8), external :: kokutai  ! the name of the function called in this

!________________________________________________________
! main part

nu1 = 5.00D-1 ! minimum frequency (in GHz)
nu2 = 1.00D3 ! maximum frequency (in GHz)

T = 2.725D0  ! temperature (in K)


open(unit=1, file='out.d', status='new')
! output data file

do inu = 1,1000 ! frequency loop

   nu = 1.0D9 * 10.**(log10(nu1) &
        + (log10(nu2)-log10(nu1))/dble(999)*dble(inu-1))

   Intensity = kokutai(T,nu)  ! calling 'kokutai' to compute intensity
                         ! passing T & nu as arguments

   write(1,'(e14.7, 1x, e14.7)')  &
        nu/1.00D9, Intensity/1.0D3  
!           frequency in GHz, intensity in W/m^2/str/Hz
enddo

close(1)

end program blackbody
