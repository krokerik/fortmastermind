subroutine print_colour(colour)
	integer :: colour
	write(*, '(A, I0, A)', advance='no') ,achar(27)//'[1;',colour,'m*'//&
	&achar(27)//'[0m'
end subroutine print_colour

program MASTERMIND
	implicit none
	print*,'testing colours'
	print*,achar(27)//'[1;31mRED'
	print*,achar(27)//'[1;32mGREEN'
	print*,achar(27)//'[1;33mYELLOW'
	print*,achar(27)//'[1;34mBLUE'
	print*,achar(27)//'[1;35mPURPLE'
	print*,achar(27)//'[1;36mCYAN'
	print*,achar(27)//'[0m' !reset the colours
	print*,'input is now normal'
	call print_colour(31)
	call print_colour(32)
	call print_colour(33)
	call print_colour(34)
	call print_colour(35)
	call print_colour(36)
	print*,'' !empty newline
end program MASTERMIND