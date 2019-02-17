program fermi_const
implicit none

real(8),parameter :: pi=4.00D0*atan(1.00D0) !π
real(8),parameter :: c=2.99792458D8 ![m/s]
real(8),parameter :: h=6.626068D-34 ![m^2*kg/s]
real(8),parameter :: me=9.10938D-31 ![kg]
real(8),parameter :: mu=1.66054D-27 ![kg]
real(8) A,p_const,x,p,E,T
real(8),parameter :: rho = 1.811D9 ![kg/m^3]
real(8),parameter :: mmwe = 2.00D0
real(8),parameter :: k = 1.38065D-23 ![m^2*kg/s^2/K]
A = 8.00D0*pi*mu*(me*c)**3.00D0/3.00D0/h**3.00D0

p_const = pi* me**4.00D0* c**5.00D0/3.00D0/h**3.00D0

write(*,*) A
write(*,*) p_const

x = (rho/rho_const/mmwe)**(1.00D0/3.00D0)

p = p_const*(x*(2.00D0*x**2.00D0-3.00D0)*(x**2.00D0+1.00D0)**5.00D-1+3.00D0*&
    log(x+(x**2.00D0+1.00D0)**5.00D-1))

end program fermi_const
