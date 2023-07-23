      implicit none
      real intg1,intg2,a,b,h,E,c1,c2,R,pi,T,fa1,fa2
      real fb1,fb2,fi1,fi2,FX1,FX2,X,E_exact
      integer i
      parameter(R=8.314)
      pi=4*atan(1.0)
      T=298.0

      write(*,*) "Type the value of lower limit of Energy (in J/mol)"
      read(*,*)a
      write(*,*)"Type the value of upper limit of Energy (in J/mol)"
      read(*,*)b
      write(*,*)"Type the increment in energy"
      read(*,*)h
      c1=(2/sqrt(pi))*(1.0/(R*T))**(1.5)
      c2=1/(R*T)
      intg1=0.0
      intg2=0.0
      E=a
      DO 10 i=1,100000000
      E=E+h
      IF(E.ge.b) GO TO 20
      call Trapfunc(c1,c2,h,E,fi1,fi2)
      intg1=intg1+fi1*h
      intg2=intg2+fi2*h

      write(*,*)"E=",E," Cumulative area under the curve=",intg1
   10 END DO
      CONTINUE

      CONTINUE
   20 call Trapfunc(c1,c2,h,a,fa1,fa2)
      call Trapfunc(c1,c2,h,b,fb1,fb2)
      intg1=intg1+0.5*(fa1+fb1)*h
      intg2=
      E_exact=(3.0/2.0)*R*T
      write(*,*)"The value of integral from ",a," to ",b," is =",intg1
      write(*,*)"The calculated value of average energy is ",intg2
      write(*,*)"The exact value of average energy/mole is =",E_exact
      pause
      stop
      end
      subroutine Trapfunc(c1,c2,h,X,FX1,FX2)
      FX1=c1*SQRT(X)*exp(-(X*c2))
      FX2=c1*SQRT(X)*exp(-(X*c2))
      return
      end
