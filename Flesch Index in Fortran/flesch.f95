! CIS*3190 Assignment 4 - Connor Todd 1039174

program flesch

    implicit none

    !VARIABLE DECLARATIONS
    integer :: i, j, fileLenth, fileLines, truth_val, truth_val1, truth_val2, truth_val3
    real :: numWords, numSenctences, numSyll, ret_val
    real :: fleschIndex, gradeLevel, avgSen, avgSyll
    character (len = 64) :: fileName
    character (len = 512), allocatable :: fileArr(:)
    character (len = 512) :: cS

    fileLines = 0
    fileLenth = 0
    numWords = 0
    numSenctences = 0
    numSyll = 0
    fleschIndex = 0
    gradeLevel = 0
    ret_val = 0
    truth_val = 0
    truth_val1 = 0
    truth_val2 = 0
    truth_val3 = 0
    

    ! User input for file name
    print *,'PLEASE ENTER FILE NAME:'
    read *,fileName
    
    !get number of line sin file
    call getFileLines(fileName, fileLines)

    ! Read text file into array of string
    allocate(fileArr(fileLines))
    rewind(1)

    !read file into array and print to stdout
    do i = 1, fileLines
        read(1,'(A)') fileArr(i)
        write(*,*) trim(fileArr(i))
    end do

    !ITERATE THROUGH TEXT LINE BY LINE
    do i = 1, fileLines

        cS = fileArr(i)
        j = 0

        !ITERATE THROUGH LINE CHARACTER BY CHARACTER
        do while(j < len(trim(cS)) + 1)
            

            !CASE: WORD IS 3 LETTERS OR LESS, ONLY 1 SYLLABLE
            call checkLetter(cs(j-1:j-1), truth_val)
            if(truth_val /= 1) then
                call checkLetter(cs(j:j), truth_val)
                call checkLetter(cs(j+1:j+1), truth_val1)
                call checkLetter(cs(j+2:j+2), truth_val2)
                call checkLetter(cs(j+3:j+3), truth_val3)

                if(truth_val == 1 .and. truth_val1 /= 1) then
                    numSyll = numSyll + 1
                    j = j + 1
                else if(truth_val == 1 .and. truth_val1 == 1 .and. truth_val2 /= 1) then
                    numSyll = numSyll + 1
                    j = j + 2
                else if(truth_val == 1 .and. truth_val1 == 1 .and. truth_val2 == 1 .and. truth_val3 /= 1) then
                    numSyll = numSyll + 1
                    j = j + 3
                end if
            end if

            !CASE: 2 VOWELS IN A ROW
            call checkVowel(cS(j:j), truth_val)
            call checkVowel(cS(j+1:j+1), truth_val2)
            if(truth_val == 1 .and. truth_val2 == 1) then
                numSyll = numSyll - 1
                j = j + 1
            end if

            !CASE: VOWEL
            call checkVowel(cS(j:j), truth_val)
            if(truth_val == 1) then
                numSyll = numSyll + 1
            end if

            !CASE: IS WORD
            if(cS(j:j) == ' ') then
                if(cS(j+1:j+1) /= '.' .or. cS(j+1:j+1) /= '!' .or. cS(j+1:j+1) /= '?') then
                    numWords = numWords + 1
                end if
            else if(cS(j:j) == '.') then
                numWords = numWords + 1
                numSenctences = numSenctences + 1
            else if(cS(j:j) == '!') then
                numWords = numWords + 1
                numSenctences = numSenctences + 1
            else if(cS(j:j) == '?') then
                numWords = numWords + 1
                numSenctences = numSenctences + 1
            else if(j == len(trim(cS))) then
                numWords = numWords + 1
            end if

            j = j + 1
        end do
    end do

    deallocate(fileArr)
    close(1)


    avgSen = numWords / numSenctences
    avgSyll = numSyll / numWords

    !CALCULATE FLESCH INDEX AND GRADE LEVEL
    call calcFleschIndex(avgSen, avgSyll, fleschIndex)
    call calcGradeLevel(avgSen, avgSyll, gradeLevel)




    !Print output of details
    Print *, '  OUTPUT of ', fileName
    Print *, '--------------------------------------------------'
    print  '("   Number of Words             =  "(F8.1))', (numWords)
    print  '("   Number of Sentences         =  "(F8.1))', (numSenctences)
    print  '("   Number of Syllables         =  "(F8.1))', (numSyll)
    !Print *, '  Number of Sentences         =   ', numSenctences 
    !Print *, '  Number of Syllables         =   ', numSyll 
    Print *, '  Words per Sentences         =   ', avgSen 
    Print *, '  Syllables per Word          =   ', avgSyll 
    Print *, '  Flesch Readability Index    =   ', fleschIndex 
    Print *, '  Flesch-Kincaid Grade Level  =   ', gradeLevel


end program flesch

subroutine calcFleschIndex(avgSen, avgSyll, fleschIndex)
    real avgSen
    real avgSyll
    real fleschIndex

    fleschIndex = 206.835 - 1.015 * avgSen - 84.6 * avgSyll

    return
end subroutine

subroutine calcGradeLevel(avgSen, avgSyll, gradeLevel)
    real avgSen
    real avgSyll
    real gradeLevel

    gradeLevel = (0.39 * avgSen) + (11.8 * avgSyll) - 15.59

    return
end subroutine

subroutine getFileLines(fileName, fileLines)

    character (len = 64) fileName
    integer fileLines
    integer :: readErr
    character (len = 256) :: CTMP

    readErr = 0

    open(unit = 1, file = fileName)

    ! Read the file into a string
    do while (readErr == 0)
        fileLines = fileLines + 1
        read(1,*,iostat=readErr) CTMP
    end do
    fileLines = fileLines - 1
    
    return
end subroutine

subroutine checkVowel(currentLetter, truth_val)

    character currentLetter
    integer truth_val

    truth_val = 0

    if(currentLetter == 'a' .or. currentLetter == 'A') then
        truth_val = 1
    else if(currentLetter == 'e' .or. currentLetter == 'E') then
        truth_val = 1
    else if(currentLetter == 'i' .or. currentLetter == 'I') then
        truth_val = 1
    else if(currentLetter == 'o' .or. currentLetter == 'o') then
        truth_val = 1
    else if(currentLetter == 'u' .or. currentLetter == 'u') then
        truth_val = 1
    else if(currentLetter == 'y' .or. currentLetter == 'Y') then
        truth_val = 1
    else
        truth_val = 0
    end if
    
    return
end subroutine


subroutine checkLetter(currentLetter, truth_val)

    character currentLetter
    integer truth_val

    truth_val = 0

    if (currentLetter == 'a' .or. currentLetter == 'A') then
        truth_val = 1
    else if (currentLetter == 'b' .or. currentLetter == 'B') then
        truth_val = 1
    else if (currentLetter == 'c' .or. currentLetter == 'C') then
        truth_val = 1
    else if (currentLetter == 'd' .or. currentLetter == 'D') then
        truth_val = 1
    else if (currentLetter == 'e' .or. currentLetter == 'E') then
        truth_val = 1
    else if (currentLetter == 'f' .or. currentLetter == 'F') then
        truth_val = 1
    else if (currentLetter == 'g' .or. currentLetter == 'G') then
        truth_val = 1
    else if (currentLetter == 'h' .or. currentLetter == 'H') then
        truth_val = 1
    else if (currentLetter == 'i' .or. currentLetter == 'I') then
        truth_val = 1
    else if (currentLetter == 'j' .or. currentLetter == 'J') then
        truth_val = 1
    else if (currentLetter == 'k' .or. currentLetter == 'K') then
        truth_val = 1
    else if (currentLetter == 'l' .or. currentLetter == 'L') then
        truth_val = 1
    else if (currentLetter == 'm' .or. currentLetter == 'M') then
        truth_val = 1
    else if (currentLetter == 'n' .or. currentLetter == 'N') then
        truth_val = 1
    else if (currentLetter == 'o' .or. currentLetter == 'O') then
        truth_val = 1
    else if (currentLetter == 'p' .or. currentLetter == 'P') then
        truth_val = 1
    else if (currentLetter == 'q' .or. currentLetter == 'Q') then
        truth_val = 1
    else if (currentLetter == 'r' .or. currentLetter == 'R') then
        truth_val = 1
    else if (currentLetter == 's' .or. currentLetter == 'S') then
        truth_val = 1
    else if (currentLetter == 't' .or. currentLetter == 'T') then
        truth_val = 1
    else if (currentLetter == 'u' .or. currentLetter == 'U') then
        truth_val = 1
    else if (currentLetter == 'v' .or. currentLetter == 'V') then
        truth_val = 1
    else if (currentLetter == 'w' .or. currentLetter == 'W') then
        truth_val = 1
    else if (currentLetter == 'x' .or. currentLetter == 'X') then
        truth_val = 1
    else if (currentLetter == 'y' .or. currentLetter == 'Y') then
        truth_val = 1
    else if (currentLetter == 'z' .or. currentLetter == 'Z') then
        truth_val = 1
    else
        truth_val = 0
    end if 
    
    return
end subroutine