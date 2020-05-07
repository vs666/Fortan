program Factorial
    implicit none
    integer :: ans
    integer :: n
    integer :: i
    read(*,*) n
    ans=1
    do 1 i=1,n,1
        ans=ans*i
    1 continue  ! 1 is a label
    write(*,*) n,"! = ",ans

end program Factorial