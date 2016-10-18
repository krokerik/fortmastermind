SUBROUTINE print_colour(colour)
	INTEGER :: colour
	WRITE(*, '(A, I0, A)', ADVANCE='NO') ,ACHAR(27)//'[1;',colour,'m*'//&
	&ACHAR(27)//'[0m'
END SUBROUTINE print_colour

SUBROUTINE get_code(num_pegs, code)
	INTEGER, INTENT(in)                       :: num_pegs
	INTEGER, DIMENSION(num_pegs), INTENT(out) :: code
	INTEGER, ALLOCATABLE                      :: seed(:)
	INTEGER                                   :: seed_size, time, i
	REAL                                      :: r(num_pegs)
	
	CALL RANDOM_SEED()
	CALL RANDOM_SEED(size = seed_size)
	ALLOCATE(seed(seed_size))
	CALL RANDOM_SEED(get = seed)
	CALL SYSTEM_CLOCK(count = time)
	seed = time
	CALL RANDOM_SEED(put = seed)
	CALL RANDOM_NUMBER(r)
	DEALLOCATE(seed)
	DO i=1,num_pegs
		code(i) = NINT(r(i)*5)+31
	END DO
END SUBROUTINE get_code

PROGRAM MASTERMIND
	IMPLICIT NONE
	INTEGER, PARAMETER           :: num_pegs = 4
	INTEGER                      :: i
	INTEGER, DIMENSION(num_pegs) :: code
	
	PRINT*,'testing colours'
	PRINT*,achar(27)//'[1;31mRED'
	PRINT*,achar(27)//'[1;32mGREEN'
	PRINT*,achar(27)//'[1;33mYELLOW'
	PRINT*,achar(27)//'[1;34mBLUE'
	PRINT*,achar(27)//'[1;35mPURPLE'
	PRINT*,achar(27)//'[1;36mCYAN'
	PRINT*,achar(27)//'[0m' !reset the colours
	PRINT*,'input is now normal'
	CALL get_code(num_pegs, code)
	DO i=1,num_pegs
		CALL print_colour(code(i))
	END DO
	PRINT*,'' !empty newline
END PROGRAM MASTERMIND