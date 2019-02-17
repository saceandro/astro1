program fermi_temp
implicit none

real(8),parameter :: pi=4.00D0*atan(1.00D0) !π
real(8),parameter :: c=2.99792458D8 ![m/s]
real(8),parameter :: h=6.626068D-34 ![m^2*kg/s]
real(8),parameter :: me=9.10938D-31 ![kg]
real(8),parameter :: mu=1.66054D-27 ![kg]
real(8) rho_const,p_const,x,p,E,T,n,pf,V
real(8),parameter :: rho = 1.811D9 ![kg/m^3]
real(8),parameter :: mmwe = 2.00D0
real(8),parameter :: k = 1.38065D-23 ![m^2*kg/s^2/K]
real(8),parameter :: M = 1.989D30 ![kg]
real(8),parameter :: R = 6.40D6 ![m]

V=4.00D0*pi*R**3.00D0/3.00D0

n = rho/2.00D0/mu

pf = (h/2.00D0)*(3.00D0*n/pi)**(1.00D0/3.00D0)

E = c*(pf**2.00D0+(me*c)**2.00D0)**5.00D-1 - me*c**2.00D0

T = E/k ![K]

write(*,*) T

end program fermi_temp
