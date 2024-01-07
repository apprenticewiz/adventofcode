subroutine usage
    implicit none

    character(len=255) :: cmdline, progname
    integer :: length, istatus, i

    call get_command (cmdline, length, istatus)
    if ( istatus .eq. 0 ) then
        i = index(cmdline, " ")
        progname = cmdline(1:i)
        write (*, "(3a)") "usage: ", trim(progname), " <file>"
    end if
    stop
end subroutine

function process(filename)
    implicit none

    integer :: process
    character(*) :: filename

    integer, parameter :: unit_number = 71
    integer :: istatus = -1
    character(len=255) :: line

    character(len=1), dimension(10), parameter :: digit_strs = &
        ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]
    integer :: min_left, max_right
    integer :: left_index, right_index, temp_index
    character :: left_digit, right_digit, curr_digit
    integer :: sum = 0
    integer :: i, j, line_len
    logical :: ok

    open(unit=unit_number, file=trim(filename), status="old", action="read", &
        iostat=istatus)
    if ( istatus .ne. 0 ) then
        write (*, *) "file not found: ", trim(filename)
        stop 1
    end if
    do
        read (unit_number, "(a)", iostat=istatus) line
        if ( istatus .ne. 0 ) exit
        min_left = len_trim(line) + 1
        max_right = 0
        do i = 1, 10
            line_len = len_trim(line)
            curr_digit = digit_strs(i)
            left_index = index(line, curr_digit)
            if ( left_index .ne. 0 .and. left_index .lt. min_left ) then
                 min_left = left_index
                 left_digit = curr_digit
             end if
            right_index = index(line, curr_digit, back=.true.)
            if ( right_index .ne. 0 .and. right_index .gt. max_right ) then
                 max_right = right_index
                 right_digit = curr_digit
             end if
        end do
        sum = sum + (ichar(left_digit) - ichar("0")) * 10 + &
            (ichar(right_digit) - ichar("0"))
    end do
    close(unit=unit_number)
    process = sum
end function

program day01a
    implicit none

    integer :: process

    character(len=255) :: filename, res_str
    integer :: argc, res

    argc = command_argument_count()
    if ( argc < 1 ) then
        call usage()
    end if
    call get_command_argument(1, filename)
    res = process(filename)
    write (res_str, "(i10)") res
    write (*, "(2a)") "result = ", trim(adjustl(res_str))
end program
