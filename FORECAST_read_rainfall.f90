
    !---------------------------------------------------------------------------------
    !  Licensing:
    !
    !    This code is distributed under the ...
    !
    !  Version/Modified: 
    !
    !    2014.26.11 - 25 November 2014 (By: Rodrigo Paiva)    
    !
    !  Authors:
    !
    !    Original fortran version by Walter Collischonn
    !    Present fortran version by:
    !    * Walter Collischonn
    !    * Rodrigo Cauduro Dias de Paiva
    !    * Diogo da Costa Buarque
    !    * Paulo Pontes Rógenes
    !    * Mino  Viana Sorribas
    !    * Fernando Mainardi Fan
    !    * Juan Martin Bravo 
    !    * Vinicius Alencar Siqueira
    !
    !  Main Reference:
    !
    !    Walter Collischonn,
    !    Modelo de Grandes Bacias - Thesis
    !    Porto Alegre, 2001
    !    ISBN: XXXXXXXXXXX,
    !
    !---------------------------------------------------------------------------------
    !  Variables and Parameters:
    !
    !   *Variables declarations and routines calls are all commented below.
	!	* All variables are global!?
    !
    !---------------------------------------------------------------------------------		

	SUBROUTINE FORECAST_read_rainfall
	USE VARS_MAIN
    USE VARS_PREV
    
	IMPLICIT NONE
	INTEGER KC	
        
    if (IT>NT_forecast) then
       !Assumes zero rainfall for all catchments if IT > forecast horizon (quantitative precipitation)
       P=0.0     
    elseif (flag_ESP==1) then 
        !If flag ESP is on, use resampled observation
        READ(FILPLU,REC=ITCHUVA_ESP(iForecast_Member))(P(KC),KC=1,NC)     
    else
        ! Reads forecast precipitation data at current time interval (ITCHUVA_PREV)
	    READ(FILPFORECAST,REC=ITCHUVA_PREV)(P(KC),KC=1,NC)
	end if
	
	RETURN
	END