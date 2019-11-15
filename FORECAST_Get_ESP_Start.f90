SUBROUTINE FORECAST_GET_ESP_START
    
    !This subroutine gets all indexes corresponding to the same calendar day, resampled from historical observations        
    use VARS_PREV
    use VARS_MAIN
    implicit none
    
    integer iT_ESP, it_loop, Julian_ESP
    integer ESP_Month, ESP_Day, ESP_Year, IDIA_updated    
    
    !Check for years with 366 days. IF day is 29/02, uses the previous day
    if (IDIA==29.AND.IMES==2)then
        IDIA_updated=28
    else
        IDIA_updated=IDIA
    endif
    
    !Search for rainfall IT positions for each ensemble member
    do iT_ESP=1,nForecast_Members
        do it_loop=ESP_start_loop,NT_ESP
            
            Julian_ESP=IDINI_ESP+INT((it_loop+HORAINI-1)/(86400./DTP)) 	! Gets calendar day (big number)
		    CALL CALDAT(Julian_ESP,ESP_Month,ESP_Day,ESP_Year)
            
            !Check if both day and month correspond to the current forecast day and month
            if (ESP_Month==IMES.AND.ESP_Day==IDIA_updated) then 
                ITCHUVA_ESP(it_ESP)=it_loop
                ESP_start_loop=it_loop+1
                exit    
            end if
        end do
    end do
		
    
  END SUBROUTINE
    
    