module Day2

  use tools

  private :: Part1, Part2
  public :: Day2Solve

contains

integer function Part1()
  Part1 = 1
end function Part1

integer function Part2()
  Part2 = 2
end function Part2

subroutine Day2Solve()
  print *, '--- Day 2 ---'
  print *, 'Answer to part 1 is ', Part1()
  print *, 'Answer to part 2 is ', Part2()
end subroutine Day2Solve

end module Day2