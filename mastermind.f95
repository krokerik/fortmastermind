# define RED         31
# define GREEN       32
# define YELLOW      33
# define BLUE        34
# define PURPLE      35
# define CYAN        36
# define CORRECT     42
# define WRONG_PLACE 43
# define WRONG       45

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
		code(i) = NINT(r(i)*5)+31 !range 31-36
	END DO
END SUBROUTINE get_code

SUBROUTINE get_guess(num_pegs, guess)
	INTEGER, INTENT(in)                       :: num_pegs
	INTEGER, DIMENSION(num_pegs), INTENT(out) :: guess
	INTEGER                                   :: i, selected


	WRITE(*, "(A,I2,A)"), '1. '//achar(27)//'[1;', RED, 'mRED'//achar(27)//'[0m'
	WRITE(*, "(A,I2,A)"), '2. '//achar(27)//'[1;', GREEN,&
	                      &'mGREEN'//achar(27)//'[0m'
	WRITE(*, "(A,I2,A)"), '3. '//achar(27)//'[1;', YELLOW,&
	                      &'mYELLOW'//achar(27)//'[0m'
	WRITE(*, "(A,I2,A)"), '4. '//achar(27)//'[1;', BLUE,&
	                      &'mBLUE'//achar(27)//'[0m'
	WRITE(*, "(A,I2,A)"), '5. '//achar(27)//'[1;', PURPLE,&
	                      &'mPURPLE'//achar(27)//'[0m'
	WRITE(*, "(A,I2,A)"), '6. '//achar(27)//'[1;', CYAN,&
	                      &'mCYAN'//achar(27)//'[0m'

	i=1
	DO
		IF (i > num_pegs) EXIT
		WRITE(*, '(A, I0)'), 'Pick colour for peg #', i
		READ(*,*), selected
		SELECT CASE(selected)
			CASE(1)
				guess(i) = RED
			CASE(2)
				guess(i) = GREEN
			CASE(3)
				guess(i) = YELLOW
			CASE(4)
				guess(i) = BLUE
			CASE(5)
				guess(i) = PURPLE
			CASE(6)
				guess(i) = CYAN
			CASE DEFAULT
				WRITE(*, '(I0, A)'), selected,' is an invalid choice.'
				i = i - 1
		END SELECT

		i = i + 1
	END DO
END SUBROUTINE get_guess

SUBROUTINE print_result(num_pegs, guess, code, num_correct)
	INTEGER, INTENT(in)                       :: num_pegs
	INTEGER, DIMENSION(num_pegs), INTENT(in)  :: guess, code
	INTEGER                                   :: i, j, res
	INTEGER, INTENT(out)                      :: num_correct

	num_correct = 0

	PRINT*, ACHAR(WRONG)//' = wrong colour'
	PRINT*, ACHAR(WRONG_PLACE)//' = right colour, wrong place'
	PRINT*, ACHAR(CORRECT)//' = right colour, right place'

	DO i=1,num_pegs
		res=WRONG
		DO j=1,num_pegs
			IF (guess(i) == code(j) .AND. i == j) THEN
				res = CORRECT
				num_correct = num_correct + 1
			ELSE IF (guess(i) == code(j) .AND. res /= CORRECT) THEN
				res = WRONG_PLACE
			END IF
		END DO
		WRITE(*, '(A, I0, A)', ADVANCE='NO'), ACHAR(res)
	END DO
	PRINT*,'' !newline
END SUBROUTINE print_result

PROGRAM MASTERMIND
	IMPLICIT NONE
	INTEGER, PARAMETER           :: num_pegs = 4
	INTEGER                      :: i, num_correct = 0, num_guesses = 0
	INTEGER, DIMENSION(num_pegs) :: code,guess
	
	CALL get_code(num_pegs, code)

	DO WHILE (num_correct < num_pegs)
		num_guesses = num_guesses + 1
		CALL get_guess(num_pegs, guess)
		CALL print_result(num_pegs, guess, code, num_correct);

		DO i=1,num_pegs
			CALL print_colour(guess(i))
		END DO
		PRINT*,'' !empty newline
	END DO

	WRITE(*,"(A, I1, A)"), "Congratulations! You completed the puzzle in ",&
	                       &num_guesses, " attempts."
END PROGRAM MASTERMIND
