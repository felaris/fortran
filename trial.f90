program try

implicit none 

REAL::mass,volume , density 

INTEGER:: input1 ,a,b,c,d !i and j is for the counting , option1 is for inout options

print*,"Calculate for Density"

print*,"Type in the mass : "
read*,mass



print*," Type in the Volume "
read*,volume

print*," Select the the volume and mass"

print*," 1. Mass in Kg and Volume in cm3"
print*," 2. Mass in g and Volume in m3"
print*," 3. Mass in Kg and Volume in m3"
print*," 4. Mass in g and Volume in cm3"



read*, input1

IF (input1== 1)THEN
	DO a=1,1
		volume = volume / 100
		density = mass / volume
		
		print*,density ,"kg/m3"
	END DO

END IF

IF (input1== 2)THEN
	DO b=1,1
		mass = volume / 100
		density = mass / volume
		
		print*,density ,"kg/m3"
	END DO
END IF

IF (input1== 3)THEN
	DO c=1,1
		
		density=mass / volume
		
		print*,density ,"kg/m3"
	END DO
END IF

IF (input1== 4)THEN
	DO d=1,1
		volume= volume / 100
		mass = mass / 100
		density = mass / volume
		
		print*,density ,"kg/m3"
	END DO
END IF








END program try
