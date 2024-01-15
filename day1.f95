module Day1

   use tools
   use hashtbl

   private :: Part1, Part2
   public :: Day1Solve

   type :: t_input
      integer :: frequencies(1500)
      integer :: size
   end type
contains

   type(t_input) function LoadInput()
      integer :: file
      integer :: frequency
      type(t_input) :: input

      input%size = 0

      file = openFile(1)

      do while (readInteger(file, frequency))
         input%size = input%size + 1
         input%frequencies(input%size) = frequency
      end do

      call closeFile(file)

      LoadInput = input
   end function LoadInput

   integer function Part1()
      type(t_input) :: input

      input = LoadInput()

      Part1 = sum(input%frequencies(1:input%size))
   end function Part1

   integer function Part2()
      logical :: found
      integer :: frequency, i
      type(t_input) :: input
      type(hash_tbl_sll) :: visited

      input = LoadInput()

      call visited%init(1031)

      found = .false.
      frequency = 0
      i = 0
      Part2 = 0

      do while (.not. found)
         i = modulo(i, input%size) + 1
         frequency = frequency + input%frequencies(i)
         found = visited%has(key=frequency)
         if (.not. found) then
            call visited%put(key=frequency, val=frequency)
         else
            Part2 = frequency
         end if
      end do
   end function

   subroutine Day1Solve()
      integer :: R
      print *, '--- Day 1 ---'
      R = Part1()
      print *, 'Answer to part 1 is ', R
      R = Part2()
      print *, 'Answer to part 2 is ', R
   end subroutine Day1Solve

end module Day1
