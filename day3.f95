module Day3

  use tools

  private :: Part1, Part2
  public :: Day3Solve

  type :: t_claim
    integer :: id
    integer :: x
    integer :: y
    integer :: width
    integer :: height
  end type

  type :: t_input
    type(t_claim), dimension(1500) :: claims
    integer :: counts
  end type

contains

type(t_input) function LoadInput()
  integer :: file
  integer :: idx1, idx2, idx3, idx4
  character(:), allocatable :: line
  type(t_input) :: input 
  type(t_claim) :: claim

  input%counts = 0

  file = openFile(3)

  do while(readLine(file, line))
    idx1 =  index(line, ' @')
    idx2 =  index(line, ',')
    idx3 =  index(line, ': ')
    idx4 =  index(line, 'x')

    read(line(2:idx1-1) , *) claim%id
    read(line(idx1+2:idx2-1) , *) claim%x
    read(line(idx2+1:idx3-1) , *) claim%y
    read(line(idx3+1:idx4-1) , *) claim%width
    read(line(idx4+1:) , *) claim%height

    deallocate(line)

    input%counts = input%counts+1
    input%claims(input%counts) = claim
  end do

  call closeFile(file)

  LoadInput = input
end function LoadInput

logical function doOverlap(claim_1, claim_2)
  type(t_claim) :: claim_1, claim_2
  integer :: l1x, l1y, r1x, r1y
  integer :: l2x, l2y, r2x, r2y

  l1x = claim_1%x
  l1y = claim_1%y
  r1x = l1x + claim_1%width - 1
  r1y = l1y + claim_1%height - 1

  l2x = claim_2%x
  l2y = claim_2%y
  r2x = l2x + claim_2%width - 1
  r2y = l2y + claim_2%height - 1

  doOverlap = .true.

  if (l1x > r2x .or. r1x < l2x) then
    doOverlap = .false.    
  end if

  if (l1y > r2y .or. r1y < l2y) then
    doOverlap = .false.
  end if 

end function doOverlap

integer function Part1()
  type(t_input) :: input
  type(t_claim) :: claim
  integer, dimension(:,:), allocatable :: pixels
  integer :: result, x, y, i, w, h

  input = LoadInput()

  allocate(pixels(1024, 1024))

  result = 0
  do i = 1, input%counts
    claim = input%claims(i)
    do w = 1, claim%width
      x = claim%x + w
      do h = 1, claim%height
        y = claim%y + h
        if (pixels(x, y) == 1) then
          result = result + 1
        end if
        pixels(x, y) = pixels(x, y) + 1
      end do
    end do
  end do
  deallocate(pixels)

  Part1 = result
end function Part1

integer function Part2()
  type(t_input) :: input
  type(t_claim) :: claim_1, claim_2
  integer :: i, j
  logical :: overlap(1500)

  input = LoadInput()
  Part2 = 0

  overlap = .false.
  ! do i = 1, input%counts-1
  !   overlap(input%claims(i)%id) = .false.
  ! end do  

  do i = 1, input%counts-1
    claim_1 = input%claims(i)
    do j = i+1,  input%counts
      claim_2  = input%claims(j)      
      if (doOverlap(claim_1, claim_2)) then
        overlap(claim_1%id) = .true.
        overlap(claim_2%id) = .true.
      end if
    end do
    if (.not. overlap(claim_1%id)) then
      Part2 = claim_1%id
      exit
    end if
  end do

end function Part2

subroutine Day3Solve()
  integer :: answer

  print *, '--- Day 3 ---'

  answer = Part1()
  print *, 'Answer to part 1 is ', answer

  answer = Part2()
  print *, 'Answer to part 2 is ', answer
end subroutine Day3Solve

end module Day3
