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

SUBROUTINE get_guess(num_pegs, guess)
	INTEGER, INTENT(in)                       :: num_pegs
	INTEGER, DIMENSION(num_pegs), INTENT(out) :: guess
	INTEGER                                   :: i, selected

	i=1
	DO
		IF (i > num_pegs) EXIT
		WRITE(*, '(A, I0)'), 'Pick colour for peg #', i
		PRINT *, '1. '//achar(27)//'[1;31mRED'//    achar(27)//'[0m'
		PRINT *, '2. '//achar(27)//'[1;32mGREEN'//  achar(27)//'[0m'
		PRINT *, '3. '//achar(27)//'[1;33mYELLOW'// achar(27)//'[0m'
		PRINT *, '4. '//achar(27)//'[1;34mBLUE'//   achar(27)//'[0m'
		PRINT *, '5. '//achar(27)//'[1;35mPURPLE'// achar(27)//'[0m'
		PRINT *, '6. '//achar(27)//'[1;36mCYAN'//   achar(27)//'[0m'

		READ(*,*), selected
		SELECT CASE(selected)
			CASE(1)
				guess(i) = 31
			CASE(2)
				guess(i) = 32
			CASE(3)
				guess(i) = 33
			CASE(4)
				guess(i) = 34
			CASE(5)
				guess(i) = 35
			CASE(6)
				guess(i) = 36
			CASE DEFAULT
				WRITE(*, '(I0, A)'), selected,' is an invalid choice.'
				i = i - 1
		END SELECT

		i = i + 1
	END DO
END SUBROUTINE get_guess

PROGRAM MASTERMIND
	IMPLICIT NONE
	INTEGER, PARAMETER           :: num_pegs = 4
	INTEGER                      :: i
	INTEGER, DIMENSION(num_pegs) :: code
	INTEGER, DIMENSION(num_pegs) :: guess
	
	PRINT*,'testing colours'
	PRINT*,achar(27)//'[1;31mRED'
	PRINT*,achar(27)//'[1;32mGREEN'
	PRINT*,achar(27)//'[1;33mYELLOW'
	PRINT*,achar(27)//'[1;34mBLUE'
	PRINT*,achar(27)//'[1;35mPURPLE'
	PRINT*,achar(27)//'[1;36mCYAN'
	PRINT*,achar(27)//'[0m' !reset the colours

	CALL get_code(num_pegs, code)
	DO i=1,num_pegs
		CALL print_colour(code(i))
	END DO
	PRINT*,'' !empty newline
	CALL get_guess(num_pegs, guess)
	!WRITE(*,"('',A1,A,$)") '',CHAR(13)
	DO i=1,num_pegs
		CALL print_colour(guess(i))
	END DO
	PRINT*,'' !empty newline
END PROGRAM MASTERMIND
