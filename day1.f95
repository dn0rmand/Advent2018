module Day1

    use tools
    use hashtbl

    private :: Part1, Part2
    public :: Day1Solve

contains

integer function Part1()
    integer :: frequency
    integer :: file
    integer :: num

    frequency = 0

    file = openFile(1)

    do while(readInteger(file, num))
        frequency = frequency + num
    end do

    call closeFile(file)

    Part1 = frequency
end function Part1

! integer function Part2()
integer function Part2()
    logical :: found
    integer :: frequency
    integer :: file
    integer :: R
    integer :: num
    integer :: steps 

    type(hash_tbl_sll) :: visited

    call visited%init(1031)

    R = 0
    found = .false.

    file = openFile(1)

    steps = 0
    frequency = 0
    do while (.not. found)
        rewind(file)
        do while(.not. found .and. readInteger(file, num))
            frequency = frequency + num
            call visited%has(key=frequency, exists=found)
            if (.not. found) then
              call visited%put(key=frequency, val=frequency)
            else
              ! already visited
              R = frequency
            end if
        end do
    end do

    call closeFile(file)

    Part2 = R
  end function Part2

subroutine Day1Solve()
    integer :: R
    print *, '--- Day 1 ---'
    R =  Part1()
    print *, 'Answer to part 1 is ', R
    R = Part2()
    print *, 'Answer to part 2 is ', R
end subroutine Day1Solve

end module Day1