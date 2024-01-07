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
    character(len=256) :: line

    integer :: res = 0
    integer :: red_needed, green_needed, blue_needed
    integer :: game_num, amount
    integer :: pos, inner_pos, color_pos
    character(len=256) :: game_part, draws_part, game_num_part, &
        draw_part, color_amount, amount_str, color

    open(unit=unit_number, file=trim(filename), status="old", action="read", &
        iostat=istatus)
    if ( istatus .ne. 0 ) then
        write (*, *) "file not found: ", trim(filename)
        stop 1
    end if
    do
        read (unit_number, "(a)", iostat=istatus) line
        if ( istatus .ne. 0 ) exit
        red_needed = 0
        green_needed = 0
        blue_needed = 0
        pos = index(line, ": ")
        if ( pos .gt. 0 ) then
            game_part = trim(line(1:pos - 1))
            draws_part = trim(line(pos + 2:))
            pos = index(game_part, ' ')
            if ( pos .gt. 0 ) then
                game_num_part = trim(game_part(pos + 1:))
                read (game_num_part, *) game_num
                do
                    pos = index(draws_part, "; ")
                    if ( pos .gt. 0 ) then
                        draw_part = trim(draws_part(1:pos - 1))
                        draws_part = trim(draws_part(pos + 2:))
                    else
                        draw_part = draws_part
                    end if
                    do
                        inner_pos = index(draw_part, ", ")
                        if ( inner_pos .gt. 0 ) then
                            color_amount = trim(draw_part(1:inner_pos - 1))
                            draw_part = trim(draw_part(inner_pos + 2:))
                        else
                            color_amount = draw_part
                        end if
                        color_pos = index(color_amount, ' ')
                        amount_str = trim(color_amount(1:color_pos - 1))
                        read (amount_str, *) amount
                        color = trim(color_amount(color_pos + 1:))
                        if ( trim(color) .eq. "red" .and. amount .gt. red_needed ) then
                            red_needed = amount
                        else if ( trim(color) .eq. "green" .and. amount .gt. green_needed ) then
                            green_needed = amount
                        else if ( trim(color) .eq. "blue" .and. amount .gt. blue_needed ) then
                            blue_needed = amount
                        end if
                        if ( inner_pos .eq. 0 ) then
                            exit
                        end if
                    end do
                    if ( pos .eq. 0 ) then
                        exit
                    end if
                end do
                res = res + red_needed * green_needed * blue_needed
            end if
        end if
    end do
    close(unit=unit_number)
    process = res
end function

program day02b
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
