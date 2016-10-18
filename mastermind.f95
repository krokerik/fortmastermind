subroutine print_colour(colour)
	integer :: colour
	write(*, '(A, I0, A)', advance='no') ,achar(27)//'[1;',colour,'m*'//&
	&achar(27)//'[0m'
end subroutine print_colour

program MASTERMIND
	implicit none
	INTEGER, PARAMETER           :: num_pegs = 4
	INTEGER                      :: i, time
	INTEGER, DIMENSION(num_pegs) :: code
	REAL                         :: r(num_pegs)
	INTEGER, ALLOCATABLE         :: seed(:)
	
	call RANDOM_SEED()
	call RANDOM_SEED(size = i)
	allocate(seed(i))
	call RANDOM_SEED(get = seed)
	call SYSTEM_CLOCK(count = time)
	seed = time
	call RANDOM_SEED(put = seed)
	call RANDOM_NUMBER(r)
	
	print*,'testing colours'
	print*,achar(27)//'[1;31mRED'
	print*,achar(27)//'[1;32mGREEN'
	print*,achar(27)//'[1;33mYELLOW'
	print*,achar(27)//'[1;34mBLUE'
	print*,achar(27)//'[1;35mPURPLE'
	print*,achar(27)//'[1;36mCYAN'
	print*,achar(27)//'[0m' !reset the colours
	print*,'input is now normal'
	
	do i=1,num_pegs
		code(i) = NINT(r(i)*5)+31
		call print_colour(code(i))
	end do
	
	print*,'' !empty newline
	deallocate(seed)
end program MASTERMIND