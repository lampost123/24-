RECURSIVE subroutine search(s)
    use mymodule
    implicit none
    type(state),INTENT(IN) ::s
    type(state)::t
    integer::num,x,y
    if ( s%n==1 ) then
        if (abs(s%number(1)-24)<0.000001 ) then 
            print*,s%expression(1)
            !read(*,*)
            !stop
        end if
    else 
        num=s%n
        do x = 1, num-1, 1
            do y = x+1, num, 1
                !加法
                t=s
                t%n=t%n-1
                t%number(x)=t%number(x)+t%number(y)
                t%number(y)=t%number(num)
                t%expression(x)="("// trim(t%expression(x)) // "+" // trim(t%expression(y)) // ")"
                !print*,t%expression(x)
                t%expression(y)=t%expression(num)
                call search(t)
                !减法1
                t=s
                t%n=t%n-1
                t%number(x)=t%number(x)-t%number(y)
                t%number(y)=t%number(num)
                t%expression(x)="("//trim(t%expression(x))//"-"//trim(t%expression(y))//")"
                t%expression(y)=t%expression(num)
                call search(t)
                t=s
                t%n=t%n-1
                t%number(x)=t%number(y)-t%number(x)
                t%number(y)=t%number(num)
                t%expression(x)="("//trim(t%expression(y))//"-"//trim(t%expression(x))//")"
                t%expression(y)=t%expression(num)
                call search(t)
                !乘法
                t=s
                t%n=t%n-1
                t%number(x)=t%number(x)*t%number(y)
                t%number(y)=t%number(num)
                t%expression(x)="("//trim(t%expression(x))//"*"//trim(t%expression(y))//")"
                t%expression(y)=t%expression(num)
                call search(t)
                !除法
                t=s
                t%n=t%n-1
                t%number(x)=t%number(x)/t%number(y)
                t%number(y)=t%number(num)
                t%expression(x)="("//trim(t%expression(x))//"/"//trim(t%expression(y))//")"
                t%expression(y)=t%expression(num)
                call search(t)
                t=s
                t%n=t%n-1
                t%number(x)=t%number(y)/t%number(x)
                t%number(y)=t%number(num)
                t%expression(x)="("//trim(t%expression(y))//"/"//trim(t%expression(x))//")"
                t%expression(y)=t%expression(num)
                call search(t)
            end do
        end do
    end if
end subroutine search
PROGRAM main
    use mymodule
    use StringUtils
    implicit none
    integer(2)::a(4)
    integer(2)::i
    type(state) ::s
    character(len=20)::tempStr
    s%n=4
    READ(*, *) (s%number(i), i = 1, 4) 
    DO i = 1, 4
    WRITE(tempStr,'(I0)') INT(s%number(i))  ! 将整型转换为字符型
    s%expression(i) = trim(tempStr)  ! 去除末尾的空格
    END DO
    call search(s)
    read(*,*)
END

