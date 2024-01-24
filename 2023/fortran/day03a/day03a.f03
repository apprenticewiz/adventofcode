module position_types
    type :: position
        integer :: row
        integer :: col
    end type

    type :: number_location
        character(len = 3) :: number
        type(position) :: pos
    end type

    type :: part_location
        character :: part
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

function build_parts(filename)
    use position_types
    use lists

    implicit none

    character(*), intent(in) :: filename

    type(node), pointer :: build_parts

    integer, parameter :: unit_number = 71
    integer :: istatus = -1
    character(len = 256) :: line

    type(node), pointer :: part_locs => null()
    type(part_location), pointer :: part_loc => null()
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
            if ( .not. (iachar(ch) .ge. iachar('0') .and. iachar(ch) .le. iachar('9')) .and. ch .ne. '.' ) then
                allocate(part_loc)
                part_loc%pos%row = row
                part_loc%pos%col = col
                part_loc%part = ch
                allocate(ptr, source=part_loc)
                call append_list(part_locs, ptr)
                nullify(ptr)
                nullify(part_loc)
            end if
        end do
        row = row + 1
    end do
    close(unit = unit_number)
    build_parts => part_locs
end function

function check_parts(number_locs, part_locs)
    use position_types
    use lists

    implicit none

    type(node), target, intent(in) :: number_locs
    type(node), target, intent(in) :: part_locs

    integer :: check_parts

    type(node), pointer :: numberptr => null()
    type(node), pointer :: partptr => null()
    type(number_location), pointer :: number_loc
    type(part_location), pointer :: part_loc
    class(*), pointer :: ptr
    integer :: part_num
    integer :: result = 0
    integer :: adjacent_count
    integer :: length
    integer :: adj_row, adj_col, num_col, delta_row, delta_col

    numberptr => number_locs
    do while ( associated(numberptr) )
        select type ( number_loc => numberptr%data )
            type is ( number_location )
                length = len_trim(number_loc%number)
                adjacent_count = 0
                do num_col = number_loc%pos%col, (number_loc%pos%col + length - 1)
                    do delta_row = -1, 1
                        adj_row = number_loc%pos%row + delta_row
                        do delta_col = -1, 1
                            adj_col = num_col + delta_col
                            partptr => part_locs
                            do while ( associated(partptr) )
                                select type ( part_loc => partptr%data )
                                    type is ( part_location )
                                        if ( adj_row .eq. part_loc%pos%row .and. adj_col .eq. part_loc%pos%col ) then
                                            adjacent_count = adjacent_count + 1
                                        end if
                                end select
                                partptr => partptr%next
                            end do
                            nullify(partptr)
                        end do
                    end do
                end do
                if ( adjacent_count .ne. 0 ) then
                    read (number_loc%number, *) part_num
                    result = result + part_num
                end if
        end select
        numberptr => numberptr%next
    end do
    nullify(numberptr)
    check_parts = result
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

        function build_parts(filename)
            use position_types
            use lists
            implicit none
            character(len=*), intent(in) :: filename
            type(node), pointer :: build_parts
        end function

        function check_parts(number_locs, part_locs)
            use position_types
            use lists

            implicit none

            type(node), target, intent(in) :: number_locs
            type(node), target, intent(in) :: part_locs

            integer :: check_parts
        end function
    end interface

    type(node), pointer :: number_locs => null()
    type(node), pointer :: part_locs => null()
    integer :: result

    number_locs => build_numbers(filename)
    part_locs => build_parts(filename)
    result = check_parts(number_locs, part_locs)
    call free_list(number_locs)
    call free_list(part_locs)
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
