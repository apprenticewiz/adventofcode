module num_arrays
    type :: num_array
        integer, dimension(64) :: array
        integer :: len
    end type
end module

subroutine usage
    implicit none

    character(len = 256) :: cmdline, progname
    integer :: length, istatus, i

    call get_command (cmdline, length, istatus)
    if ( istatus .eq. 0 ) then
        i = index(cmdline, " ")
        progname = cmdline(1:i)
        write (*, "(3a)") "usage: ", trim(progname), " <file>"
    end if
    stop
end subroutine

function is_digit(ch)
    implicit none

    character, intent(in) :: ch

    logical :: is_digit

    is_digit = (iachar(ch) .ge. iachar('0')) .and. (iachar(ch) .le. iachar('9'))
end function

function scan_numbers(input_str)
    use num_arrays

    implicit none

    character(*), intent(in) :: input_str

    type(num_array) :: scan_numbers

    logical :: is_digit

    type(num_array) :: nums
    integer :: pos, start_pos
    character(len=256) :: num_str
    integer :: num
    integer :: i
    logical :: scanning_number

    pos = 1
    i = 1
    scanning_number = .false.
    do
        if ( scanning_number ) then
            if ( .not. is_digit(input_str(pos:pos)) ) then
                num_str = input_str(start_pos:pos - 1)
                read (num_str, *) num
                nums%array(i) = num
                i = i + 1
                scanning_number = .false.
            end if
        else
            if ( is_digit(input_str(pos:pos)) ) then
                start_pos = pos
                scanning_number = .true.
            end if
        end if
        pos = pos + 1
        if ( pos .gt. len_trim(input_str) ) exit
    end do
    if ( scanning_number ) then
        num_str = input_str(start_pos:len_trim(input_str))
        read (num_str, *) num
        nums%array(i) = num
        i = i + 1
    end if
    nums%len = i - 1
    scan_numbers = nums
end function

function process(filename)
    use num_arrays

    implicit none

    character(*), intent(in) :: filename

    integer :: process

    type(num_array) :: scan_numbers

    integer :: res = 0
    integer, parameter :: unit_number = 71
    integer :: istatus = -1
    integer, dimension(256) :: instances = 0
    character(len=256) :: game_num_str, line, rest, winning_str, hand_str
    type(num_array) :: winning_set
    type(num_array) :: hand_set
    integer :: pos, game_num, i, j, common_count, copies

    open(unit=unit_number, file=trim(filename), status="old", action="read", &
        iostat=istatus)
    if ( istatus .ne. 0 ) then
        write (*, *) "file not found: ", trim(filename)
        stop 1
    end if
    do
        read (unit_number, "(a)", iostat=istatus) line
        if ( istatus .ne. 0 ) exit
        pos = index(line, ": ")
        if ( pos .eq. 0 ) exit
        game_num_str = trim(line(5:pos - 1))
        read(game_num_str, *) game_num
        rest = trim(line(pos + 2:))
        pos = index(rest,  " | ")
        if ( pos .eq. 0 ) exit
        winning_str = trim(rest(1:pos - 1))
        winning_set = scan_numbers(winning_str)
        hand_str = trim(rest(pos + 3:))
        hand_set = scan_numbers(hand_str)
        common_count = 0
        do i = 1, winning_set%len
            do j = 1, hand_set%len
                if ( winning_set%array(i) .eq. hand_set%array(j) ) then
                    common_count = common_count + 1
                end if
            end do
        end do
        do i = (game_num + 1), (game_num + common_count)
            copies = instances(i) + 1 + instances(game_num)
            instances(i) = copies
        end do
        res = res + 1
    end do
    do i = 1, game_num
        res = res + instances(i)
    end do
    process = res
end function

program day04a
    implicit none

    integer :: process

    character(len = 256) :: filename, res_str
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
