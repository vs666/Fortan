module Globe
    implicit none
    integer, dimension(:), allocatable :: ar    
end module Globe

program main
    use Globe
    implicit none
    integer :: fib
    integer :: n,t
    integer :: length,i
    length=1000
    allocate(ar(length))
    lp1: do i=1,length
        ar(i)=4
end do lp1
    ar(1)=0
    ar(2)=1
    read(*,*) t
    lp2: do i=1,t
    read(*,*) n
    write(*,*) "result is ",fib(n,length)
end do lp2
end program main

recursive function fib(term,length) result(fn)
    use Globe
    integer,intent(in) :: term
    integer,intent(in) :: length
    integer :: fn
    if(term <=0) then
        fn=0
    end if
    if (ar(term)==4) then
        fn = fib(term-1,length) + fib(term-2,length)
    else
        fn = ar(term)
    end if

end function fib