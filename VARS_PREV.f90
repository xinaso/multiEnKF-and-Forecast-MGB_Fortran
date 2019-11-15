MODULE VARS_PREV
    
    CHARACTER(13) INPUT_DIRECTORY_FORECAST !('.\P_forecast\')
    !CHARACTER(17) OUTPUT_DIRECTORY_FORECAST !('.\Q_forecast_ESP\')
    CHARACTER(13) OUTPUT_DIRECTORY_FORECAST !('.\Q_forecast\')
    CHARACTER(11) INPUT_DIRECTORY_HOTSTART !('.\Hotsart\')
    
    integer NT_forecast !number of days in forecast horizon
    integer NT_zeroP !number of days with zero rainfall, after forecast horizon
    integer nForecast_Members !number of members in ensemble
    integer IT_initial_hot !Number of the IT corresponding to the initial hotstart file = number of days after 1/1/2010 - South America Version
    integer iForecast_Member !Index of forecast member
    integer nForecast_days !Number of consequent days in which forecast will be issued
    integer iForecast_day !Index of foreast day 
    integer flag_ESP      !Flag to turn on ESP computations
    
    integer IT_current_PQ !IT starting from number of hotstart file
    integer ITFLOW, ITCHUVA_PREV
    integer ESP_start_loop !Starting IT Positions to search for ESP days 
    integer IDINI_ESP    !Start day of observed precipitation (historical observations)
    integer NT_ESP      !Number of time steps for observed precipitation (historical observations)
    integer ESP_IDIA, ESP_IMES, ESP_IANO
    integer, allocatable :: ITCHUVA_ESP(:) !IT Positions of ESP members
    
    integer, parameter :: FILPFORECAST = 2050  
    integer, parameter :: FILQFORECAST = 2051
    
END MODULE