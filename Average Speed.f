      implicit none
      real intg,a,b,c,c1,c2,h,x,fa,fb,fi,fx,R,M,T,pi,c_exact
      integer i
      parameter(R=8.314)
      pi=4*atan(1.0)
      T=273.0
      write(*,*)"Give the molar mass of the gas molecule"
      read(*,*)M
      M=M/1000.0
      c_exact=sqrt((8*R*T)/(pi*M))
      
      write(*,*)"Give the lower limit of speed in m/s"
      read(*,*)a
      write(*,*)"Give the upper limit of speed in m/s"
      read(*,*)b
      write(*,*)"Give the increment value of speed"
      read(*,*)h
      c1=4*pi*(M/(2*pi*R*T))**(1.5)
      c2=-(M/(2*R*T))

      intg=0.0
      c=a

      do 10 i=1,1000000000
      c=c+h
      if(c.ge.b) GOTO 20
      call trapfunc(c1,c2,h,c,fi)
      intg=intg+fi*h
   10 end do
      continue
      
   20 call trapfunc(c1,c2,h,a,fa)
      call trapfunc(c1,c2,h,b,fb)
      intg=intg+(0.5*(fa+fb)*h)
      write(*,*)"Average speed (Numerically) is:",intg
      write(*,*)"Average speed (Analytic.) is:",c_exact
      pause
      stop
      end

      subroutine trapfunc(c1,c2,h,x,fx)
      fx=c1*(x**3)*exp(c2*(x**2))
      return
      end
