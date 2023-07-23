      implicit none
      real intg,a,b,c,h,c1,c2,c_exact,R,pi,M,T,fa,fb,fi
      integer i
      parameter(R=8.314)
      pi= 4*atan(1.0)
      T=298.0
      write(*,*) "Give the molar mass of the gas molecule"
      read(*,*)M
      M=M/1000.0
      c_exact=sqrt((3*R*T)/M)

      write(*,*)"Type the value of lower limit of speed in m/s"
      read(*,*)a
      write(*,*)"Type the value of upper limit of speed in m/s"
      read(*,*)b
      write(*,*)"Type the increment in speed in m/s"
      read(*,*)h
      c1=4*pi*(M/(2*pi*R*T))**(1.5)
      c2=(-(M/(2*R*T)))
      
      intg=0.0
      c=a

      do 10 i=1,100000000
      c=c+h
      if(c.ge.b) goto 20
      call Trapfunc(c1,c2,h,c,fi)
      intg=intg+fi*h
  10  end do
      continue
      
  20  call trapfunc(c1,c2,h,a,fa)
      call trapfunc(c1,c2,h,b,fb)
      intg=intg+0.5*(fa+fb)*h
      intg=intg**(0.5)
      write(*,*)"RMS value of c (calculated) is:",c_exact
      write(*,*)"RMS value of c (analytic.) is:",intg
      pause
      stop
      end
      
      subroutine trapfunc(c1,c2,h,x,fx)
      fx=c1*(x**4)* exp(c2*(x**2))
      return
      end
