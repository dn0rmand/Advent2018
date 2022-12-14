module Day1

    use tools

    private :: Part1, Part2
    public :: Day1Solve

type Frequency
    integer :: key
    integer :: value
    type(Frequency), pointer :: next => null()
end type Frequency

type FrequencyPtr
    type(Frequency), pointer :: ptr
end type FrequencyPtr

type HashTable
    type(FrequencyPtr), dimension(0:4096) :: buckets
end type HashTable

contains

subroutine set(hash, key, value)
    type(HashTable) :: hash
    integer :: key
    integer :: value
    integer :: code
    type(FrequencyPtr) :: ptr
    type(Frequency), pointer :: current

    code = modulo(key, 4096)
    ptr = hash%buckets(code)
    current = ptr%ptr

    do while ((associated(current)) .and. (current%key .ne. key))
        current = current%next
    end do

    if (associated(current)) then
        current%value = value
    else 
        allocate(current)
        current%key = key
        current%value= value
    end if
end subroutine set

logical function get(hash, key, value)    
    type(HashTable), intent(in) :: hash
    integer, intent(in) :: key
    integer, intent(out) :: value
    integer :: code
    type(Frequency), pointer :: current
    type(FrequencyPtr) :: ptr

    code = key
    if (code < 0) then 
        code = -code
    end if
    code = modulo(key, 4096)
    ptr = hash%buckets(code)

    current = ptr%ptr

    do while ((associated(current)) .and. (current%key .ne. key))
        current = current%next
    end do
 
    if (associated(current)) then
        value = current%value
        get = .true.
    else 
        get = .false.
    end if
end function get

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
subroutine Part2(R)
    logical :: found
    integer :: frequency
    integer :: file
    integer, intent(out) :: R
    integer :: num
    type(HashTable) :: visited

    frequency = 0
    R = 0
    found = .false.

    file = openFile(1)

    do while (.not. found)
        rewind(file)
        do while(.not. found .and. readInteger(file, num))
            frequency = frequency + num
            print *, frequency
            if (get(visited, frequency, num)) then
                ! already visited
                R = frequency
                found = .true.
            else
                call set(visited, frequency, frequency)
            end if
        end do
    end do

    call closeFile(file)

end subroutine Part2

subroutine Day1Solve()
    integer :: R
    print *, '--- Day 1 ---'
    print *, 'Answer to part 1 is ', Part1()
    call Part2(R)
    print *, 'Answer to part 2 is ', R
end subroutine Day1Solve

end module Day1