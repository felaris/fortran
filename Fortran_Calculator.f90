program try
  implicit none !
  
  
!declaring Real values for the quadratic equation
  REAL::x,y,z,ans1,ans2,a,b,c,opt1
  
  !declaring all constant
  REAL::acceleration , pi=3.141593,KE,PE

  !declaring all needed quantities
  REAL::force,weight,height,velocity,density,mass,mass1,volume,volume1,pressure,area

  !the is for the input section
  REAL::opt2,opt3,opt4,aut1,aut2,aut3,aut4,inp1,inp2,a1,a2,b1,b2,c1,c2,D,Dx,Dy
  
  
  !Welcome interface
  PRINT*,"    ***Welcome to Fortran Basic Calculator***"
  PRINT*,""
  PRINT*,""  
  
  PRINT*,"         What Would you like to do    "
  PRINT*,""
  PRINT*," 1. Quadratic Equation"
  PRINT*," 2. Simultaneous Equation"
  PRINT*," 3. Calculate for Force"
  PRINT*," 4. Calculate Density"
  PRINT*," 5. Calculate Kinetic Energy"
  PRINT*," 6. Calculate Potential Energy"

  
  PRINT*,""
  PRINT*," Type the Option  number "
  PRINT*," "
  READ*,opt1

  

  


 
  IF (opt1==1)then

     !starting interface for quadratic equation
  PRINT*,"     ****QUADRATIC EQUATION****"
  PRINT*," "
     
  PRINT*," Please follow the instructions carefully"
      
  PRINT*," "
  PRINT*,"Type the Value for a"
  READ*,a
  PRINT*,"   "
  
  PRINT*,"Type the Value for b"
  READ*,b
  PRINT*,"   "
  
  
  PRINT*,"Type the Value for c"
  READ*,c
  PRINT*,"   "
  
  
  !Equations for  the almighty quadratic formular
 
  x= SQRT((b**2)-(4*a*c))  
  y=2*a
  z=-b
  
  ans1=(z-x)/y      !answer for the first X, that is x1
  ans2= (z+x)/y     !answer for the second x, that is x2
  
  PRINT*,"x1 "
  PRINT"(F20.3)",ans1   
  PRINT*,"x2 :"
  PRINT"(F20.3)",ans2
  



 !an option for the simultaneous Equation
  ELSE IF (opt1==2)then
   
   !Starting interface for Simultaneous Equation
  PRINT*,"   ***Simultaneous Equation***   "
  PRINT*," Type in the values for the a1,b1,c1 :"
  READ*,a1
  READ*,b1
  READ*,c1
  PRINT*,""

  PRINT*," Type in values for the a2,b2,c2" 
  
  READ*,a2
  READ*,b2
  READ*,c2

  PRINT*,""
  !using Cramer's Rule
  
  D=(a1*b2)-(a2*b1)
  Dx=(c1*b2)-(c2*b1)
  Dy=(a1*c2)-(a2*c2)

  x=Dx/D
  y=Dy/D
 PRINT*," value for x is :"
  
PRINT"(F14.3)",x

PRINT*," value for y is :"

PRINT"(F14.3)",y






  !an option for the force
ELSE IF (opt1==3)then
   !FORCE INTERFACE
     PRINT*,"    ***Force***     "
     PRINT*,""
     PRINT*," Which Parameters are you using to calculate for the force"
     PRINT*,""
     PRINT*," 1. mass and acceleration"
     PRINT*," 2. pressure and Area"
     PRINT*,"  "
     READ*,inp1
    IF (inp1==1)then
     PRINT*,"    What is the mass "
     READ*,mass
     PRINT*,"   is the mass in Kilograms or grams "
     PRINT*,""
     PRINT*," 1. Kilograms"
     PRINT*," 2. Gramms"
     READ*,aut1
     IF(aut1==1)then
        mass1=mass
     ELSE IF(aut1==2)then
        mass1=mass/1000
     END IF
     
      PRINT*,""
     PRINT*,"   Which Acceleration do you want to use"
     PRINT*," 1. Acceleration = 9.8ms-2"
     PRINT*," 2. Acceleration = 10.00-2"
     READ*,aut2
     IF(aut2==1)then
        acceleration=9.8
     ELSE IF(aut2==2)then
        acceleration =10
  END IF
      !If statement for using pressure and area to find force  
    ELSE IF(inp1==2)then
    PRINT*,"    What is the Pressure "
    READ*,pressure
        
     PRINT*,"   is the Area in m2 or cm2 "
     PRINT*,""
     PRINT*," 1. Metres square (m2)"
     PRINT*," 2. Centimetres (cm2)"
     READ*,aut1
     IF(aut1==1)then
       area =area
     ELSE IF(aut1==2)then
        area=area/100
     END IF
     
     
     PRINT*,""
     PRINT*,"The FORCE In NEWTONS OR Kgms_2  is:"
     force=pressure*area
     PRINT"(F100.4)",force  !printing output using the real format descriptor
  END IF


  
   !code for density

  ELSE IF (opt1==4)then
     PRINT*,"      ***Density***"
      PRINT*,"    What is the mass "
     READ*,mass
     PRINT*,"   is the mass in Kilograms or grams "
     PRINT*,""
     PRINT*," 1. Kilograms"
     PRINT*," 2. Gramms"
     READ*,aut1
     PRINT*," what is the volume"
     PRINT*,""
     READ*,volume
     PRINT*,""
     PRINT*,"       Which measurement is the volume "
     PRINT*," 1. metres cube (m3)"
     PRINT*," 2. centimetres cube (cm3)"
     PRINT*," 3. mililitres(ml)"
     PRINT*," 4. litres (l)"
    
     READ*,aut2
     IF(aut1==1)then 
        volume=volume
     ELSE IF(aut1==2)then
        volume=volume/100
     ELSE IF(aut1==3)then
        volume=volume*0.000001
      ELSE IF(aut1==4)then
        volume=volume/1000
     END IF

     density = mass/volume
     PRINT*," The Density is :"
     PRINT"(F15.3)",density    !printing an output for the density
          
     


     !code for Kinetic Energy 
  ELSE IF (opt1==5)then
     PRINT*,"  ***Kinetic Energy*** "
     PRINT*,""
     PRINT*,"    What is the mass "
     READ*,mass
     PRINT*,"   is the mass in Kilograms or grams : "
     PRINT*,""
     PRINT*," 1. Kilograms"
     PRINT*," 2. Gramms"
     READ*,aut1
     IF(aut1==1)then
        mass1=mass
     ELSE IF(aut1==2)then
        mass1=mass/1000
        END IF
     PRINT*,""
     PRINT*," What is the velocity in ms-1 :"
     READ*,velocity
     PRINT*,""
     KE=0.5*((mass1)*(velocity**2))
     PRINT"(F100.4)",KE
     
     

  ELSE IF (opt1==6)then
     PRINT*,"  *** Potential Energy ***"
     PRINT*,"    What is the mass "
     READ*,mass
     PRINT*,"   is the mass in Kilograms or grams "
     PRINT*,""
     PRINT*," 1. Kilograms"
     PRINT*," 2. Gramms"
     READ*,aut1
     IF(aut1==1)then
        mass1=mass
     ELSE IF(aut1==2)then
        mass1=mass/1000
     END IF
     PRINT*,"    What is the height "
     READ*,height
     PRINT*,"   is the height in Metres or Centimetres "
     PRINT*,""
     PRINT*," 1. Metres"
     PRINT*," 2. Centimetres"
     READ*,aut2
     IF(aut2==1)then
        height=height
     ELSE IF(aut1==2)then
        height=height/100

     END IF
     PRINT*,""
     PRINT*,"   Which Acceleration do you want to use"
     PRINT*," 1. Acceleration = 9.8ms-2"
     PRINT*," 2. Acceleration = 10.00-2"
     READ*,aut2
     IF(aut3==1)then
        acceleration=9.8
     ELSE IF(aut3==2)then
        acceleration =10
     END IF
     PE=mass1*acceleration*height
     PRINT"(F100.4)",PE
    
  ELSE IF (opt1>=7)then
  PRINT*,"Option Error , Please Type in The Correct Value Next Time:"
  READ*,opt1
   


END IF



  
 END PROGRAM try

