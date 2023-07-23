      real a, b, c, ka, alpha, pH1, conc
      real alpha_guess, alpha_n_1, Fx_alpha, dFx_alpha, pH2
      
      
      write(*,*)"Give the value of ka of weak acid"
      Read(*,*)ka
  5   write(*,*)"Give the value of the concentration of the weak acid"
      read(*,*)conc
      if(conc.EQ.0.0)then
      GO TO 25
      end if
      a=conc
      b=ka
      c=-ka
      alpha=(-b+SQRT(b**2-4*a*c))/(2*a)
      
      
   10 write(*,*)"Give the guess value of degree of dissociaion"
      read(*,*)alpha_guess
      
      
   15 Fx_alpha=a*alpha_guess**2+b*alpha_guess+c
      dFx_alpha=2*a*alpha_guess+b
      alpha_n_1=alpha_guess-Fx_alpha/dFx_alpha
      if((ABS(alpha_n_1-alpha_guess)<= 0.00001))GO TO 20
      alpha_guess=alpha_n_1
      GO TO 15


  20  pH1=-LOG10(conc*alpha)
      pH2=-LOG10(conc*alpha_guess)
      
      write(*,*)"The values at concentration:",conc," are :"
      write(*,*)"pH1:",pH1
      write(*,*)"pH2:",pH2
      write(*,*)"Alpha:",alpha
      write(*,*)"Guess:",alpha_guess
      GO TO 5
  25  STOP
      end

