! solve x-cos(x)=0

program choetsu1
implicit none

! definition of variables
real(8) a ! initial value
real(8) x ! variable
real(8) z ! variable
real(8) G ! function G(x)=cos(x)
real(8) epsilon ! allowed error

a=0.73
epsilon=1.00D-10
z=a

do 
   x=z
   G=cos(x)
   z=G
   if (abs(z-cos(z)) < epsilon) exit
enddo

write(*,'(e14.6)') z

end program choetsu1



