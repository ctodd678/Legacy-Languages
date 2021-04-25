! CIS*3190 Assignment 1 - Connor Todd 1039174

program dangerWrapper

    implicit none

    !VARIABLE INITIALIZATION
    real :: dry, wet, windSpeed, bui, percip, ffm, afm, ffs, tsi, fli, dryFactor
    real :: snow, herbState

    ! A bunch of user input for required danger vals
    Print *, "INPUT:"
    print *, '  ENTER DRY BULB TEMP:'
    read *,dry
    print *,dry

    print *, '  ENTER WET BULB TEMP:'
    read *,wet
    print *, wet

    print *, '  Is there snow? (0 for no, 1 for yes)'
    read *,snow
    print *,snow

    print *, '  ENTER WIND SPEED MPH:'
    read *,windSpeed
    print *,windSpeed

    print *, '  ENTER BUILD UP INDEX:'
    read *,bui
    print *,bui

    print *, '  HERB STATE: '
    read *,herbState
    print *,herbState

    print *, '  ENTER PERCIPITATION:'
    read *,percip
    print *,percip

    !call danger subroutine to calculate values
    call danger(dry, wet, windSpeed, bui, percip, ffm, afm, ffs, tsi, fli, snow, herbState, dryFactor)

    !Print output of danger 
    Print *, 'OUTPUT:'
    Print *, '  Fine Fuel Moisture      =   ', ffm
    Print *, '  Adjusted Fuel Mousture  =   ', afm 
    Print *, '  Fine Fuel Spread        =   ', ffs 
    Print *, '  Timber Spread Index     =   ', tsi 
    Print *, '  Fire Load Index         =   ', fli
    Print *, '  Build Up Index          =   ', bui


end program dangerWrapper

subroutine danger(dry, wet, windSpeed, bui, percip, ffm, afm, ffs, tsi, fli, snow, herbState, dryFactor)

    !initialize dangers variables
    ffm = 99.0
    afm = 99.0
    dryFactor = 0
    fli = 0
    ffs = 1.0
    tsi = 1.0

    !NO snow 
    if(snow <= 0) then
            !calculate FMM and dry factor
            call calcFFM(dry, wet, ffm)
            call calcDry(ffm, dryFactor)

            !if ffm <= 1 then set ffm to 1
            if(ffm <= 1) then
                ffm = 1
            end if

            !adjust ffm for herbstate
            ffm = ffm + (herbState - 1) * 5.0
            
            if(percip > 0.1) then
                call calcBUI(bui, percip)
            end if

            if(bui < 0) then
                bui = 0
            end if

            !add dryFactor to BUI
            bui = bui + dryFactor

            !calculate AFM
            call calcAFM(afm, ffm, bui)

            if(afm > 30.0) then
                if(ffm > 30.0) then
                    ffs = 1
                    tsi = 1
                    return
                else 
                    tsi = 1
                end if
            end if
            !calculate FFS and TSI
            call calcFFS(ffs, windSpeed, ffm)
            call calcTSI(tsi, windSpeed, afm)

            !if ffs or tsi greater than 99 set to 99
            if(ffs > 99) then
                ffs = 99
            end if

            if(tsi > 99) then
                tsi = 99
            end if

             !if ffs or bui less or equal to 0 set to 0
            if(tsi <= 0) then
                return
            end if

            if(bui <= 0) then
                return
            end if

            !calculate fli
            call calcFLI(fli, bui, tsi)

            !if fli is not greater than 0 then set to 0
            if(fli <= 0) then
                fli = 0
            else
                fli = 10.0 ** fli
            end if
            return
    !snow condition
    else
        ffs = 0
        tsi = 0
        fli = 0
        if(percip > 0.1) then
            call calcBUI(bui, percip)
            if(bui < 0) then
                bui = 0
            else
                return
            end if
        else
            return
        end if
    end if
    return
end

subroutine calcDRY(ffm, dryFactor)
    integer :: i    !loop index
    dimension d(6)  !dry factor constants array
    d(1) = 16.0
    d(2) = 10.0
    d(3) = 7.0
    d(4) = 5.0
    d(5) = 4.0
    d(6) = 3.0

    !do loop used to findd dry factor
    do i = 1,6,1
        if((ffm - d(i)) <= 0) then
            dryFactor = 7
        else 
            dryFactor = i - 1
            exit
        end if
    end do 

    return 
end 

subroutine calcFFM(dry, wet, ffm)

    real :: tempDiff
    tempDiff = dry - wet

    !logic with constants to find fine fuel moisture
    if(tempDiff <= 4.5) then
        ffm = 30.0 * exp(-0.185900 * tempDiff)
    else if(tempDiff <= 12.5) then
        ffm = 19.2 * exp(-0.085900 * tempDiff)
    else if(tempDiff <= 27.5) then
        ffm = 13.8 * exp(-0.059660 * tempDiff)
    else if(tempDiff > 27.5) then
        ffm = 22.5 * exp(-0.077373 * tempDiff)
    end if

    return 
end 

subroutine calcAFM(afm, ffm, bui)

    !formual to find adjusted fuel moisture
    afm = 0.9*ffm + 0.5 + 9.5 * exp(-bui/50)

    if(afm > 99) then
        afm = 99.0
    end if
    return
end 

subroutine calcFFS(ffs, windSpeed, ffm)

    !logic with constants to find fine fuel spread
    if(windSpeed < 14) then
        ffs = 0.01312 * (windSpeed + 6.) * ((33. - ffm) ** 1.65) - 3.
        if(ffs <= 1.0) then
            ffs = 1.0
        end if
    else if(windSpeed > 14) then
        ffs = 0.00918 * (windSpeed + 14.) * ((33. - ffm) ** 1.65) - 3.
    end if

    return
end

subroutine calcTSI(tsi, windSpeed, afm)

    !logic with constants to timber spread index
    if(windSpeed < 14) then
        tsi = 0.01312 * (windSpeed + 6.0) * ((33.0 - afm) ** 1.65) - 3.0
        if(tsi <= 1.0) then
            tsi = 1.0
        end if
    else if(windSpeed > 14) then
        tsi = 0.00918 * (windSpeed + 14.0) * ((33.0 - afm) ** 1.65) - 3.0
    end if


    return
end

subroutine calcBUI(bui, percip)

    !formula to calculate build up index
    bui = -50.0 * alog(1.0 - (1.0 - exp(-bui / 50.0)) * exp(-1.175 * (percip - 0.1)))

    return
end

subroutine calcFLI(fli, bui, tsi)

    !formual to calculate fire load index
    fli = 1.75 * alog10(tsi) + 0.32 * alog10(bui) - 1.640

    return
end
