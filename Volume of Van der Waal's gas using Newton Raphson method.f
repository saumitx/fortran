      real a,b,c,d,P,Vguess,Vn1,Xn,T,F,dF
      real vda,vdb
      integer i
      parameter(R=8.314)
      t=0
      write(*,*)"Give the guess value of Vanderwaal's constant a and b"
      read(*,*)vda,vdb
      write(*,*)"Give the value of P,T(in K), No. of moles respectively"
      read(*,*)P,T,Xn
      a=P
      b=-Xn*((P*vdb)+(R*T))
      c=(Xn**2)*vda
      d=-vda*(Xn**3)*vdb
      Vexact=(Xn*R*T)/P
      write(*,*)"Give the guess value of volume"
      read(*,*)Vguess

   15 Fx_volume=(a*Vguess**3)+(b*Vguess**2)+(c*Vguess)+d
      dFx_volume=(3*a*Vguess**2)+(2*b*Vguess)+c
      Vn1=Vguess-Fx_volume/dFx_volume
      if((ABS(Vn1-Vguess))<= 0.00001)GO TO 20
      Vguess=Vn1
      write(*,*)Vguess
      GO TO 15
      
   20 write(*,*)"Calculated volume of VanderWaal's gas:",Vguess
      write(*,*)"Exact volume of VanderWaal's gas:",Vexact
      pause
      stop
      end

      
