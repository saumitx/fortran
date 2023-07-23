      real intg,intg_e,h,T,Tlo,Thi,dcpfa,dcpfb,dcpfi
      real a1,b1,c1,a2,b2,c2,a3,b3,c3,da,db,dc
      integer i,z
      a1=29.07
      a2=26.98
      a3=25.89
      b1=-0.837E-3
      b2=5.917E-3
      b3=32.58E-3
      c1=2.012E-6
      c2=-0.337E-6
      c3=-3.046E-6
      da=a3-1.5*a1-0.5*a2
      db=b3-1.5*b1-0.5*b2
      dc=c3-1.5*c1-0.5*c2
      write(*,*)"Enter the value of lower limit of K"
      read(*,*)Tlo
      write(*,*)"Enter the upper limit of temperature in K"
      read(*,*)Thi
      write(*,*)"Enter the number of points (should be an odd number)"
      read(*,*)z
      h=(Thi-Tlo)/(z-1)
      intg=0.0
      T=Tlo
      Do 10 i=1,z-2
      T=T+h
      call simpfunc(da,db,dc,T,dcpfi)
      if((real(i)/2).ne.(i/2)) then
      intg=intg+4.0*dcpfi
      else
      intg=intg+2.0*dcpfi
      end if
   10 continue

      call simpfunc(da,db,dc,Tlo,dcpfa)
      call simpfunc(da,db,dc,Thi,dcpfb)
      intg=(h/3.0)*(intg+dcpfa+dcpfb)
      intg_e=da*(Thi-Tlo)+0.5*db*(Thi**2.0-Tlo**2.0)
      intg_e=intg_e+(1.0/3.0)*dc*(Thi**3.0-Tlo**3.0)
      write(*,*)"The value of the integral is=",intg
      write(*,*)"The change in del Hf from ",Tlo," to ",Thi," is:",intg
      write(*,*)"The analytically calculated value is=",intg
      write(*,*)"The exact value of integral is=",intg_e
      pause
      end
      subroutine simpfunc(da,db,dc,x,fx)
      real x,fx
      fx=da+db*x+dc*(x**2)
      return
      end
