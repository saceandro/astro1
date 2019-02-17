program suntemp
implicit none

real(8) x,dx,yi(2),yf(2)
!    x: r
! y(1): lamda
! y(2): dlamda/dr
real(8) xi, xf, N, R, M, lamdaR, rhoc, r0, m0, mmw, Pc, Tc, q, mmwI, mmwe, age,age0, T7
integer j,k
real(8), parameter :: pi=4.00D0*atan(1.00D0)
real(8), parameter :: Ms = 1.989D30    ![kg] :Mass of the sun
real(8), parameter :: Rs = 6.955D8     ![m] :Rudius of the sun
real(8), parameter :: Xh = dble(0.730)!Rate of hydrogen at the center of the sun
real(8), parameter :: Xhe = dble(0.270) !Rate of helium at the center of the sun
real(8), parameter :: G = 6.67428D-11  ![m^3/s^2/kg] :gravitational constant
real(8), parameter :: mu = 1.66054D-27 ![kg] :atomic mass unit
real(8), parameter :: kb = 1.38065D-23  ![m^2*kg/s^2/K] :Boltzmann constant
real(8), parameter :: tau = 365.242D0*2.40D1*3.60D3*1.00D8 ![s/10^8yr]

N=3.00D0 !polytropic model of N=3
xi = 1.00D-10
xf = 1.00D1
k = 150001 !Number of steps
dx = (xf-xi)/dble(k-1)

x = xi
yi(1) = 1 - xi**2.00D0/6.00D0 + N*xi**4.00D0/1.20D2
yi(2) = - xi/3.00D0 - N*xi**3.00D0/3.00D1

do j=2,k
   x=x+dx
   call rk4(N,x,dx,yi,yf)
   write(1,'(e14.7,1x,e14.7,1x,e14.7)') x,yf(1:2)
   if ((yi(1)>=0).and.(yf(1)<=0)) then
      R = x - yf(1)*dx/(yf(1)-yi(1)) !dimensionless radius of the sun in this model
      lamdaR = yf(2) - yf(1)*(yf(2)-yi(2))/(yf(1)-yi(1)) !dlamda/dr at r=R
      exit
   endif
   yi(1:2) = yf(1:2)
enddo

M = -r**2.00D0 * lamdaR !dimensionless mass of the sun in this model
m0 = Ms/M
r0 = Rs/R

rhoc = m0/(4.*pi*r0**3.00D0) ! Density at the center of the sun [kg/m^3]

Pc = 4.00D0*pi*G*(rhoc**2.00D0)*(r0**2.00D0)/(N+1.00D0) 
! Pressure at the center of the sun [Pa]

mmwI = 1.00D0/(Xh + Xhe/4.00D0) !mean molecular weight of kanzendenriion

mmwe = 1.00D0/(Xh + Xhe/2.00D0) !mean molecular weight of electron

mmw = 1.00D0/(1.00D0/mmwI + 1.00D0/mmwe) !mean molecular weight of whole gas

Tc = mmw*mu*Pc/kb/rhoc ! Temperature at the center of the sun [K]

T7= Tc/1.00D7

q = 2.39D10 *rhoc**2.00D0 *Xh**2.00D0 *T7**(-2.00D0/3.00D0)&
   *(1.00D0 + 0.0264D0 *T7**(1.00D0/3.00D0) +0.0504D0 *T7**(2.00D0/3.00D0)&
     +0.0095D0 *T7) *exp(-15.7D0 *T7**(-1.00D0/3.00D0)) !reaction rate [/m^3/s]

age = rhoc*Xh/4.00D0/mmwI/mu/q ![s]

age0 =  age/tau ![10^8yr]

open(unit=1,file='suntemp_tmp.dat')

write(1,'(A33,1x,e14.7,1x,A8)') 'Density at the center of the sun:',&
                                rhoc,'[kg/m^3]'
write(1,'(A34,1x,e14.7,1x,A4)') 'Pressure at the center of the sun:',Pc,'[Pa]'
write(1,'(A37,1x,e14.7,1x,A3)') 'Temperature at the center of the sun:',Tc,'[K]'
write(1,'(A8,1x,e14.7,1x,A59)') 'It takes',age,&
               '[s] for hydrogen to be burned out at the center of the sun.'
write(1,'(A8,1x,e14.7,1x,A64)') 'It takes',age0,&
         '[10^8yr] for hydrogen to be burned out at the center of the sun.'

close(1)

end program suntemp
