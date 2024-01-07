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

function word_to_digit(word)
    implicit none

    character(len=5) :: word_to_digit
    character(*) :: word

    character(len=5), dimension(10), parameter :: words = &
        [character(len=5) :: "zero", "one", "two", "three", "four", "five", &
         "six", "seven", "eight", "nine"]
    character(len=5), dimension(10), parameter :: digits_str = &
        [character(len=5) :: "0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]
    integer :: i

    do i = 1, 10
        if ( word == words(i) ) then
            word_to_digit = digits_str(i)
        end if
    end do
end function

function process(filename)
    implicit none

    integer :: process
    character(*) :: filename

    character(len=5) :: word_to_digit

    integer, parameter :: unit_number = 9
    integer :: istatus
    character(len=255) :: line

    character(len=5), dimension(20), parameter :: digit_strs = &
        [character(len=5) :: "0", "1", "2", "3", "4", "5", "6", "7", "8", &
         "9", "zero", "one", "two", "three", "four", "five", "six", "seven", &
         "eight", "nine"]
    integer :: min_left, max_right
    integer :: left_index, right_index, temp_index
    character(len=5) :: left_digit, right_digit, curr_digit
    integer :: sum = 0
    integer :: i, j, line_len

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
        do i = 1, 20
            line_len = len_trim(line)
            curr_digit = digit_strs(i)
            left_index = index(line, trim(curr_digit))
            if ( left_index .ne. 0 .and. left_index .lt. min_left ) then
                 min_left = left_index
                 left_digit = curr_digit
            end if
            right_index = index(line, trim(curr_digit), back=.true.)
            if ( right_index .ne. 0 .and. right_index .gt. max_right ) then
                 max_right = right_index
                 right_digit = curr_digit
             end if
        end do
        if ( len_trim(left_digit) > 1 ) then
            left_digit = word_to_digit(left_digit)
        end if
        if ( len_trim(right_digit) > 1 ) then
            right_digit = word_to_digit(right_digit)
        end if
        sum = sum + (ichar(left_digit(1:1)) - ichar("0")) * 10 + &
            (ichar(right_digit(1:1)) - ichar("0"))
    end do
    close(unit=unit_number)
    process = sum
end function

program day01b
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
