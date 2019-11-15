SUBROUTINE FORECAST_run

    USE VARS_MAIN
    USE VARS_INERC
    USE VARS_PREV
    USE IFPORT
    
    character strTrash  
    character(len=12) :: PQ_filename
    character(len=12) :: format_string_PQ
    integer NT_forecast_total
    LOGICAL :: file_exists
    
    !Adding directory of Rainfall and Discharge forecasts
    INPUT_DIRECTORY_FORECAST = TRIM(adjustl(('.\P_forecast\')))
    INPUT_DIRECTORY_HOTSTART = TRIM(adjustl(('.\Hotstart\')))
    !OUTPUT_DIRECTORY_FORECAST = TRIM(adjustl(('.\Q_forecast_ESP\')))
    OUTPUT_DIRECTORY_FORECAST = TRIM(adjustl(('.\Q_forecast\')))
    
    !Open the output file with the forecast description 
       open(41,FILE=INPUT_DIRECTORY // 'Forecast.prev',STATUS='OLD',ACTION='READ')
         
       READ(41,*)strTrash
       WRITE(*,*)'Reading Forecast information'
       READ(41,*)NT_forecast
       READ(41,*)strTrash
       READ(41,*)NT_zeroP
       READ(41,*)strTrash
       READ(41,*)nForecast_Members
       READ(41,*)strTrash
       READ(41,*)IT_initial_hot
       READ(41,*)strTrash
       READ(41,*)nForecast_days
       READ(41,*)strTrash
       READ(41,*)flag_ESP
       READ(41,*)strTrash
       READ(41,*)NT_ESP
       READ(41,*)strTrash
       READ(41,*)strTrash
       READ(41,"(3I10)")ESP_IDIA,ESP_IMES,ESP_IANO ! Reads initial time of simulation: day, month, year, and hour

ALLOCATE(ITCHUVA_ESP(nForecast_Members)) !Allocate ESP starting IT positions       
IDINI_ESP=JULDAY(ESP_IDIA,ESP_IMES,ESP_IANO) !Calculate the first day of the historical observations in the Julian calendar. Value corresponds to 1/1/1990

write(*,*)'Warning, ESP flag = ',flag_ESP,'. Press ENTER to Continue'
pause

iForecast_day=0        
NT_forecast_total = NT_forecast + NT_zeroP !Numerical forecast + zero rainfall timesteps

 !Forecast day loop   
DO WHILE (iForecast_day<nForecast_days) 
           
    !Setting the name of files for P and Q
    IT_current_PQ=IT_initial_hot + iForecast_day   
    iForecast_day=iForecast_day+1
    
      if (IT_current_PQ < 10) then
            format_string_PQ = "(A4,I1)"
            write (PQ_filename,format_string_PQ)'0000',IT_current_PQ
            
        elseif (IT_current_PQ < 100) then
            format_string_PQ = "(A3,I2)"
            write (PQ_filename,format_string_PQ)'000',IT_current_PQ
        elseif (IT_current_PQ < 1000) then
            format_string_PQ = "(A2,I3)"
            write (PQ_filename,format_string_PQ)'00',IT_current_PQ
        elseif (IT_current_PQ < 10000) then
            format_string_PQ = "(A1,I4)"
            write (PQ_filename,format_string_PQ)'0',IT_current_PQ
        endif
      
      !Check if file exists; if not, then go to the next forecast file  
      if (flag_ESP==0) then 
        INQUIRE(FILE=INPUT_DIRECTORY_FORECAST // 'P_reforecast_ECMWF_'// trim(PQ_filename) // '.pbi', EXIST=file_exists)    
        IF(file_exists==0) then 
            write(*,*)'Cycling IT number = ', IT_current_PQ
            CYCLE  
        endif
      else !In the case of ESP, check if hotstart exists
        INQUIRE(FILE=INPUT_DIRECTORY_HOTSTART // 'StateVars_WB_'// trim(PQ_filename) // '.hot', EXIST=file_exists)              
            IF(file_exists==0) then 
                write(*,*)'Cycling IT number = ', IT_current_PQ
            CYCLE  
        endif
      end if
      
      
      if (NT_forecast>0) then !Only if NT of forecast intervals is > 0; otherwise, forecast will be with zero rainfall only
        OPEN(FILPFORECAST,FILE=INPUT_DIRECTORY_FORECAST // 'P_reforecast_ECMWF_'// trim(PQ_filename) // '.pbi',STATUS='old',RECL=NC,FORM='UNFORMATTED',ACCESS='DIRECT') !Input binary file with interpolated rainfall data
      end if 
        
      OPEN(FILQFORECAST,FILE=OUTPUT_DIRECTORY_FORECAST // 'Q_reforecast_ECMWF_'// trim(PQ_filename) // '.QBI',STATUS='REPLACE',RECL=NC,FORM='UNFORMATTED',ACCESS='DIRECT') 
      !OPEN(FILQFORECAST,FILE=OUTPUT_DIRECTORY_FORECAST // 'Q_forecast_ESP_'// trim(PQ_filename) // '.QBI',STATUS='REPLACE',RECL=NC,FORM='UNFORMATTED',ACCESS='DIRECT') 
      !OPEN(FILTUD,FILE=OUTPUT_DIRECTORY_FORECAST // 'HAND_forecast_'// trim(PQ_filename) // '.HBI',STATUS='REPLACE',RECL=NC,FORM='UNFORMATTED',ACCESS='DIRECT') 

      
    ITCHUVA=IT_current_PQ !Set current time interval of observed rainfall
    ITFLOW=0
    iForecast_Member=0
    ESP_start_loop=1  
    
    !Member loop
  DO WHILE (iForecast_Member<nForecast_Members)  
      
      IT=0
      iForecast_Member=iForecast_Member+1
      ITCHUVA_PREV=int((iForecast_Member-1)*NT_forecast)  !Restart time interval of rainfall forecast file
      
      CALL CONDINIC !Generate initial conditions
      CALL FORECAST_flood_READHOT !hotstart is called every change of forecast member 
            
    ! Time Loop      
    DO WHILE (IT<NT_Forecast_total)
		
		IT=IT+1
		write(*,*)'Current Hotstart= ', IT_current_PQ, ', member=', iForecast_Member, ', Lead Time=' , IT !write time interval on screen			         
        
		JDIA=IDINI+INT((IT+(IT_current_PQ-1)+HORAINI-1)/(86400./DTP)) 	! Gets calendar day (big number)
		CALL CALDAT(JDIA,IMES,IDIA,IANO)

		DIAH(IT)=IDIA !stores day corresponding to iT time interval
		MESH(IT)=IMES !stores month corresponding to iT time interval
		ANOH(IT)=IANO !stores year corresponding to iT time interval
		HORAH(IT)=MOD(IT,24)+HORAINI   !hour of day corresponding to iT time interval
      
        if (flag_ESP==1.AND.IT==1.AND.iForecast_Member==1) then
            !Reads precipitation data for current resampled precipitation from past observations
            CALL FORECAST_GET_ESP_START    
        endif 
        
		!Onl (Only if predictability is on
        if (IT>1.AND.flag_ESP==1) then !???
            ITCHUVA_ESP(iForecast_Member)=ITCHUVA_ESP(iForecast_Member)+1
        elseif (flag_ESP==0) then
    		ITCHUVA_PREV=ITCHUVA_PREV+1    
        end if
        
        !write(*,*)'IT chuva member = ', ITCHUVA_ESP(iForecast_member) 
        
        ! Read rainfall data for all catchments in iT time-step
		CALL FORECAST_read_rainfall        
        
		! Reads climate data
		TONTEM=TA
		CALL LECLIMA
	
		! Calls catchment routing/discharge for lateral inflow
        DINFILT=0.0
		CALL CELULA		
				
	    ! Call main river routing routine using Muskingum-Cunge Method
		if(hdFLAG(1)==0)then									
		    CALL REDE
		endif     
		
		! Calculates lateral inflow 
        do ic=1,nc
		    QCEL2(IC)=QBAS(IC)+QINT(IC)+QSUP(IC) !sums surface, subsurface and 
		enddo

        ! Calls river routing routine using Inertial Method
        if(hdFLAG0>0)then
            CALL flood_inercial
        endif        
                
        !Modified in order to write results in disk for each IT #Vinicius Siqueira 21/12/2016
        !=====================================================================
       !do ic=1,nc
            !QTUDO(IC,IT)=QJ2(IC) !stores discharge at all time steps 
            !YTUDO(IC,IT)=Yfl(IC) !stores water level at all time steps 
            !HTUDO(IC,IT)=Hfl(IC) !stores water depth at all time steps 
            !QVIZTUDO(IC,IT)=Q2viz(IC) !stores lateral connection discharge at all time steps  
            !HAND(ic)=real(Hfl(ic)-DBLE(HRIO(ic)))            
       !enddo                       
       
            DO IC=1,NC
                QTUDO(iC)=QJ2(iC)                          !! Sly + Vinicius  !! Alocar 4 bytes
            ENDDO
            
    
       ITFLOW=ITFLOW+1
        !Write results at each timestep #Vinicius Siqueira 21/12/2016
        WRITE(FILQFORECAST,REC=ITFLOW)(QTUDO(IC),IC=1,NC) !Flow                
        !WRITE(1976,REC=IT)(HAND(IC),IC=1,NC) !hand values
             
                    	
	ENDDO !End time loop
  ENDDO !End of member loop	
  
  !Closing files after simulating all members
  CLOSE(FILPFORECAST)
  CLOSE(FILQFORECAST)
  
ENDDO !End of forecast day loop  

DEALLOCATE(ITCHUVA_ESP)

75	FORMAT(I6,8F10.4)
66	FORMAT(<nC>F10.4)

     
	RETURN       
             
    
END SUBROUTINE