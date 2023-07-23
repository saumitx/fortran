      PROGRAM maxwell
      real intg, a, b, h, C, c1, c2, R, pi, M, T, fa, fb, fi, X, FX
      integer i
      parameter(R=8.314)
      pi=4*atan(1.0)
      M=0.044
      T=273.0
      
      write(*,*)"Type the lower value of speed in m/s"
      read(*,*)a
      write(*,*)"Type the upper limit of speed in m/s"
      read(*,*)b
      write(*,*)"Type the increment in speed for calculation"
      read(*,*)h
      c1=4*pi*(M/(2*pi*R*T))**(1.5)
      c2=M/(2*R*T)
      
      intg=0.0
      C=a
      
      DO 10 i=1, 100000000
      C=C+h
      if(C.ge.b) GO TO 20
      call Trapfunc(c1, c2, h, C, fi)
      intg=intg+fi*h
      write(*,*)"C=",C," Cumulative area under the curve=",intg
  10  END DO
      CONTINUE
      
  20  call Trapfunc(c1, c2, h, a, fa) !calculation function at c=a
      call Trapfunc(c1, c2, h, b, fb) !calculating function at c=b
      intg=intg+0.5*(fa+fb)*h !Evaluating the inegral using trapezoidal rule
      write(*,*)"The value of the integral is=",intg
      write(1,*)"The value of the integral from ",a," to ",b," is ",intg
      close(1)
      PAUSE
      STOP
      end

      subroutine Trapfunc(c1,c2,h,X,FX)
      FX=c1*(X**2)*(exp(-(X**2)*c2))
      return
      END
