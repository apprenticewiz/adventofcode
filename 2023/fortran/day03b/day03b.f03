module position_types
    type :: position
        integer :: row
        integer :: col
    end type

    type :: number_location
        character(len = 3) :: number
        type(position) :: pos
    end type

    type :: gear_location
        character :: gear
        type(position) :: pos
    end type
end module

! As with C, Fortran does not have extensible vectors unlike most modern
! languages.  So instead, we roll our own linked list types here to
! provide similar functionality.

module lists
    type :: node
        class(*), pointer :: data => null()
        type(node), pointer :: next => null()
    end type

contains

    subroutine append_list(list, data)
        implicit none

        type(node), pointer, intent(inout) :: list
        class(*), intent(in), pointer :: data

        type(node), pointer :: curr, temp

        curr => list
        allocate(temp)
        temp%data => data
        temp%next => null()
        if ( .not. associated(list) ) then
            list => temp
        else
            do while ( associated(curr%next) )
                curr => curr%next
            end do
            curr%next => temp
        end if
        nullify(curr)
        nullify(temp)
    end subroutine

    subroutine free_list(list)
        implicit none

        type(node), pointer, intent(inout) :: list

        type(node), pointer :: curr, temp

        curr => list
        do while ( associated(curr) )
            temp => curr
            if ( associated(temp%data) ) then
                deallocate(temp%data)
                nullify(temp%data)
                nullify(temp%next)
            end if
            curr => curr%next
            deallocate(temp)
            nullify(temp)
        end do
        nullify(curr)
        nullify(list)
    end subroutine
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

function build_numbers(filename)
    use position_types
    use lists

    implicit none

    character(*), intent(in) :: filename

    type(node), pointer :: build_numbers

    integer, parameter :: unit_number = 71
    integer :: istatus = -1
    character(len = 256) :: line

    type(node), pointer :: number_locs => null()
    type(number_location), pointer :: number_loc => null()
    integer :: row, col
    logical :: scanning_number = .false.
    integer :: numidx = 1
    character :: ch
    character(len = 3) :: number = ""
    class (*), pointer :: ptr

    open(unit=unit_number, file=trim(filename), status="old", action="read", &
        iostat=istatus)
    if ( istatus .ne. 0 ) then
        write (*, *) "file not found: ", trim(filename)
        stop 1
    end if
    row = 1
    do
        read(unit_number, "(a)", iostat=istatus) line
        if ( istatus .ne. 0 ) exit
        do col = 1, len_trim(line)
            ch = line(col:col)
            if ( scanning_number ) then
                if ( iachar(ch) .ge. iachar('0') .and. iachar(ch) .le. iachar('9') ) then
                    number(numidx:numidx) = ch
                    numidx = numidx + 1
                else
                    number_loc%number = number
                    numidx = 1
                    allocate(ptr, source=number_loc)
                    !print *, number_loc%number
                    call append_list(number_locs, ptr)
                    nullify(ptr)
                    nullify(number_loc)
                    number = ""
                    scanning_number = .false.
                end if
            else
                if ( iachar(ch) .ge. iachar('0') .and. iachar(ch) .le. iachar('9') ) then
                    number(numidx:numidx) = ch
                    numidx = numidx + 1
                    allocate(number_loc)
                    number_loc%pos%row = row
                    number_loc%pos%col = col
                    scanning_number = .true.
                end if
            end if
        end do
        if ( scanning_number ) then
            number_loc%number = number
            numidx = 1
            allocate(ptr, source=number_loc)
            call append_list(number_locs, ptr)
            nullify(ptr)
            nullify(number_loc)
            number = ""
            scanning_number = .false.
        end if
        row = row + 1
    end do
    close(unit = unit_number)
    build_numbers => number_locs
end function

function build_gears(filename)
    use position_types
    use lists

    implicit none

    character(*), intent(in) :: filename

    type(node), pointer :: build_gears

    integer, parameter :: unit_number = 71
    integer :: istatus = -1
    character(len = 256) :: line

    type(node), pointer :: gear_locs => null()
    type(gear_location), pointer :: gear_loc => null()
    integer :: row, col
    character :: ch
    class (*), pointer :: ptr

    open(unit=unit_number, file=trim(filename), status="old", action="read", &
        iostat=istatus)
    if ( istatus .ne. 0 ) then
        write (*, *) "file not found: ", trim(filename)
        stop 1
    end if
    row = 1
    do
        read(unit_number, "(a)", iostat=istatus) line
        if ( istatus .ne. 0 ) exit
        do col = 1, len_trim(line)
            ch = line(col:col)
            if ( ch .eq. '*' ) then
                allocate(gear_loc)
                gear_loc%pos%row = row
                gear_loc%pos%col = col
                gear_loc%gear = ch
                allocate(ptr, source=gear_loc)
                call append_list(gear_locs, ptr)
                nullify(ptr)
                nullify(gear_loc)
            end if
        end do
        row = row + 1
    end do
    close(unit = unit_number)
    build_gears => gear_locs
end function

function check_gears(number_locs, gear_locs)
    use position_types
    use lists

    implicit none

    type(node), target, intent(in) :: number_locs
    type(node), target, intent(in) :: gear_locs

    integer :: check_gears

    type(node), pointer :: numberptr => null()
    type(node), pointer :: gearptr => null()
    type(number_location), pointer :: number_loc
    type(gear_location), pointer :: gear_loc
    integer :: gear_num
    integer :: result = 0
    integer :: adj_count, prod
    integer :: adj_row, adj_col, num_col, delta_row, delta_col
    logical :: skip = .false.

    gearptr => gear_locs
    do while ( associated(gearptr) )
        adj_count = 0
        prod = 1
        numberptr => number_locs
        select type ( gear_loc => gearptr%data )
            type is ( gear_location )
                do while ( associated(numberptr) )
                    skip = .false.
                    select type ( number_loc => numberptr%data )
                        type is ( number_location )
                            do delta_row = -1, 1
                                adj_row = gear_loc%pos%row + delta_row
                                do delta_col = -1, 1
                                    adj_col = gear_loc%pos%col + delta_col
                                    do num_col = number_loc%pos%col, (number_loc%pos%col + len_trim(number_loc%number) - 1)
                                        if ( adj_row .eq. number_loc%pos%row .and. adj_col .eq. num_col ) then
                                            read (number_loc%number, *) gear_num
                                            adj_count = adj_count + 1
                                            prod = prod * gear_num
                                            skip = .true.
                                            exit
                                        end if
                                    end do
                                    if ( skip ) exit
                                end do
                                if ( skip ) exit
                            end do
                    end select
                    numberptr => numberptr%next
                end do
        end select
        if ( adj_count .eq. 2 ) then
            result = result + prod
        end if
        nullify(numberptr)
        gearptr => gearptr%next
    end do
    nullify(gearptr)
    check_gears = result
end function

function process(filename)
    use position_types
    use lists

    implicit none

    character(*), intent(in) :: filename

    integer :: process

    interface
        function build_numbers(filename)
            use position_types
            use lists
            implicit none
            character(len=*), intent(in) :: filename
            type(node), pointer :: build_numbers
        end function

        function build_gears(filename)
            use position_types
            use lists
            implicit none
            character(len=*), intent(in) :: filename
            type(node), pointer :: build_gears
        end function

        function check_gears(number_locs, gear_locs)
            use position_types
            use lists

            implicit none

            type(node), target, intent(in) :: number_locs
            type(node), target, intent(in) :: gear_locs

            integer :: check_gears
        end function
    end interface

    type(node), pointer :: number_locs => null()
    type(node), pointer :: gear_locs => null()
    integer :: result

    number_locs => build_numbers(filename)
    gear_locs => build_gears(filename)
    result = check_gears(number_locs, gear_locs)
    call free_list(number_locs)
    call free_list(gear_locs)
    process = result
end function

program day03a
    use position_types
    use lists

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
