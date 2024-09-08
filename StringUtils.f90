MODULE StringUtils
  IMPLICIT NONE
  CONTAINS

  ! 删除字符串中的所有空格
  FUNCTION REMOVE_SPACES(inputStr) RESULT(outputStr)
    CHARACTER(LEN=*), INTENT(IN) :: inputStr
    CHARACTER(LEN=:), ALLOCATABLE :: outputStr
    INTEGER :: i, j, nonSpaceCount

    ! 统计非空格字符数
    nonSpaceCount = 0
    DO i = 1, LEN(inputStr)
      IF (inputStr(i:i) /= ' ') THEN
        nonSpaceCount = nonSpaceCount + 1
      END IF
    END DO

    ! 根据非空格字符数分配空间
    ALLOCATE(CHARACTER(LEN=nonSpaceCount) :: outputStr)

    ! 将非空格字符复制到 outputStr
    j = 1
    DO i = 1, LEN(inputStr)
      IF (inputStr(i:i) /= ' ') THEN
        outputStr(j:j) = inputStr(i:i)
        j = j + 1
      END IF
    END DO
  END FUNCTION REMOVE_SPACES
END MODULE StringUtils