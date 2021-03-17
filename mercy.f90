program mercy

implicit none 

REAL::mass, g =9.8  !g is acceleration 
REAL::force
INTEGER::option1, i ,j !i and j is for the counting , option1 is for inout options

print*,"Welcome  to Force Calculator , Please follow the instructions "

print*,"What do you want to solve for "

print*," 1. Mass"
print*," 2. Force "



read*, option1

IF (option1== 1)THEN
	DO j=1,1,1 ! j = 1,1,1 means do the below equations starting from 1 end at 1 and in steps of 1
		print*,"what is the Force"
		read*,force
		mass=force/g ! formular for mass 
		print*,mass,"Kg"
	END DO
END IF

IF (option1 == 2)THEN
	DO i=1,1,1
		print*,"what is the mass"
		print*,"Type the mass "
		read*,mass
		force= mass*g ! formular for Force
		print*,force,"Newton"
	END DO

END IF







END program mercy
