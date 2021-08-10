REAL FUNCTION mean (array, min_value, max_value)
	IMPLICIT NONE
	
	REAL, DIMENSION(:), INTENT(IN) :: array
	REAL, INTENT(IN), OPTIONAL :: min_value, max_value
	
	LOGICAL :: minimum_check, maximum_check
	INTEGER :: i, count=0
	REAL :: sum_elems=0.0
		
	minimum_check = PRESENT(min_value)
	maximum_check = PRESENT(max_value)

	SELECT CASE (minimum_check .OR. maximum_check)
	CASE(.FALSE.)
	sum_elems = SUM(array)
	count= SIZE(array)
	
	CASE(.TRUE.)

	DO i=LBOUND(array, 1), UBOUND(array, 1), 1
		IF(minimum_check .AND. array(i)<min_value) CYCLE
		If(maximum_check .AND. array(i)>max_value) CYCLE
		sum_elems = sum_elems + array(i)
		count = count + 1
	END DO
	END SELECT
	
	IF (count > 0) THEN
		mean = sum_elems/count
	ELSE
		PRINT*, "No itmes found in the specified range"
		mean = 0.0
	END IF
END FUNCTION mean	

PROGRAM main
	IMPLICIT NONE	
	
	INTERFACE 
		REAL FUNCTION mean (array, min_value, max_value)
			IMPLICIT NONE
			REAL, DIMENSION(:), INTENT(IN) :: array
			REAL, INTENT(IN), OPTIONAL :: min_value, max_value
		END FUNCTION mean	
	END INTERFACE
	
	
	REAL, DIMENSION(:), ALLOCATABLE :: array
	REAL :: minimum, maximum, value
	INTEGER :: ios, i, n
	
	OPEN(UNIT=6, FILE="results.txt", STATUS="UNKNOWN", &
	     ACTION="WRITE", POSITION="APPEND", IOSTAT=ios)
	IF (ios /= 0) THEN
		PRINT*, "An error occured could not open a file for the output resutls !!"
	END IF
	
	PRINT*, "Enter the number of values te be averaged: "
	READ*, n
	ALLOCATE(array(n))
	
	PRINT*, "Enter the values te be averaged in order: "
	Do i=1, n
		PRINT*, "Enter value No. ", i
		READ*, value
		array(i) = value
	END DO
	
	PRINT*, "Enter the start/minimum boundary interval (if any): (Enter 0 if none) "
	READ*, minimum
	PRINT*, "Enter the final/maximum boundary interval (if any): (Enter 0 if none) "
	READ*, maximum
	
	IF (minimum > 0.0 .AND. maximum > 0.0) THEN
		PRINT*, mean(array, minimum, maximum)
		WRITE(UNIT=6, FMT=*)  mean(array)
	ELSE IF( minimum > 0.0 .AND. maximum <= 0.0) THEN
		PRINT*, mean(array, min_value = minimum)
		WRITE(UNIT=6, FMT=*)  mean(array)
	ELSE IF( minimum <= 0.0 .AND. maximum > 0.0) THEN
		PRINT*, mean(array, max_value = maximum)
		WRITE(UNIT=6, FMT=*)  mean(array)
	ELSE
		PRINT*, mean(array)
		WRITE(UNIT=6, FMT=*)  mean(array)
	END IF

END PROGRAM main
