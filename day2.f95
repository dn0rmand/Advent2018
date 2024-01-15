module Day2

   use tools

   private :: Part1, Part2
   public :: Day2Solve

contains

   subroutine LoadInput(lines, count)
      integer :: file
      character(:), allocatable :: line
      character(len=26), allocatable, intent(out)  :: lines(:)
      character(len=26) :: tmp(300)
      integer :: i
      integer, intent(out) :: count

      count = 0

      file = openFile(2)

      do while (readLine(file, line))
         count = count + 1
         tmp(count) = line
         deallocate (line)
      end do

      call closeFile(file)

      allocate (lines(count))

      i = 0
      do while (i < count)
         i = i + 1
         lines(i) = tmp(i)
      end do
   end subroutine LoadInput

   integer function Part1()
      integer :: count, items_2, items_3, i, j, k, l
      logical :: has_2, has_3
      integer :: A
      character :: c
      integer :: counts(26)
      character(len=26) :: current
      character(len=26), allocatable :: lines(:)

      A = iachar('a') - 1
      call loadInput(lines, count)

      items_2 = 0
      items_3 = 0

      do i = 1, count
         current = lines(i)
         l = len(current)
         counts = 0
         do j = 1, l
            c = current(j:j)
            k = iachar(c) - A
            counts(k) = counts(k) + 1
         end do
         has_2 = .false.
         has_3 = .false.
         do k = 1, 26
            if (counts(k) == 2) then
               has_2 = .true.
            end if
            if (counts(k) == 3) then
               has_3 = .true.
            end if
         end do
         if (has_2) then
            items_2 = items_2 + 1
         end if
         if (has_3) then
            items_3 = items_3 + 1
         end if
      end do

      Part1 = items_2*items_3
   end function Part1

   character(len=26) function Part2()
      integer :: count, i, j, k, l1, l2, diffs, diffIdx
      character(len=26) :: s1, s2
      character(len=26), allocatable :: lines(:)

      call loadInput(lines, count)

      Part2 = 'Not found'

      do i = 1, count - 1
         s1 = lines(i)
         l1 = len(s1)
         do j = i + 1, count
            s2 = lines(j)
            l2 = len(s2)
            if (l1 /= l2) then
               Part2 = 'ERROR'
               return
            end if
            diffs = 0
            do k = 1, l1
               if (s1(k:k) /= s2(k:k)) then
                  diffs = diffs + 1
                  diffIdx = k
                  if (diffs > 1) then
                     exit
                  end if
               end if
            end do
            if (diffs == 1) then
               Part2 = s1(:diffIdx - 1)//s1(diffIdx + 1:)
               return
            end if
         end do
      end do

      Part2 = 'No found'
   end function Part2

   subroutine Day2Solve()
      integer :: answer1
      character(len=26) :: answer2

      print *, '--- Day 2 ---'

      answer1 = Part1()
      print *, 'Answer to part 1 is ', answer1

      answer2 = Part2()
      print *, 'Answer to part 2 is ', answer2
   end subroutine Day2Solve

end module Day2
