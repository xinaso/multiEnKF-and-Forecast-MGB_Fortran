	!---------------------------------------------------------------------------------
    !  Discussion:
    ! 
    !    This subroutine generates a level x volume measured from COTA_AREA.flp data  (Inertial version).
    !
    !
    ! Usage:
    !
    ! *
    !
    ! uses modules, functions, and subroutines
    !
    ! * USE VARS_MAIN
    ! * USE VARS_INERC (only to Inertial version)
    !
    ! opens
    !
    ! * no files are created in this routine
    !
    ! reads
    !
    ! * no files are created in this routine
    !
    ! creates
    !
    ! * no files are created in this routine
    !
    !---------------------------------------------------------------------------------
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license. - VER ISSO.
    !
    !  Version/Modified: 
    !
    ! 2015.07.06 - 07 July 2015 (by Paulo Pontes)
    !
    !  Authors:
    !
    !    Original fortran version by Walter Collischonn
    !    Present fortran version by:
    !    * Walter Collischonn
    !    * Rodrigo Cauduro Dias de Paiva
    !    * Diogo da Costa Buarque
    !    * Paulo Pontes R�genes
    !    * Mino  Viana Sorribas
    !    * Fernando Mainardi Fan
    !    * Juan Martin Bravo 
    !
    !  Main Reference:
    !
    !    Walter Collischonn,
    !    Modelo de Grandes Bacias - Thesis
    !    Porto Alegre, 2001
    !    ISBN: XXXXXXXXXXX,
    !
    !---------------------------------------------------------------------------------
    ! Variables and Parameters:
    ! *Variables declarations and routines calls are all commented below.
    !---------------------------------------------------------------------------------
    ! End of header
    !---------------------------------------------------------------------------------
    
    SUBROUTINE flood_TAB

    !Variables and parameters
    USE VARS_MAIN
    USE VARS_INERC
    IMPLICIT NONE
    INTEGER K
    REAL VOLINCR

    DO IC=1,NC
        
        !From 1 to 3 (points), it is calculated the area and volume of river cross-section and updates the units to meters
        ZTAB(1,IC)=ZFUNDOFL(IC)-HRIO(IC) 
        VTAB(1,IC)=0.0 
        ATAB(1,IC)=0.0
        ZTAB(2,IC)=ZFUNDOFL(IC) 
        VTAB(2,IC)=DBLE(BRIO(IC))*HRIO(IC)*DBLE(SRIO(IC))*1000.0 
        ATAB(2,IC)=0.0
        ZTAB(3,IC)=ZFL(1,IC) 
        VTAB(3,IC)=VTAB(2,IC)+0.5*(AFL(1,IC)+0.0)*(ZTAB(3,IC)-ZTAB(2,IC))*1000000.0
        ATAB(3,IC)=AFL(1,IC)
        
        !From next points the volume are calculated using area and level from trapezium approach
        DO K=1,NPFL(IC)-1
            ZTAB(K+3,IC)=ZFL(K+1,IC)
            VOLINCR=(ZFL(K+1,IC)-ZFL(K,IC))*1000000.0*((AFL(K,IC)+AFL(K+1,IC))*0.5) 
            VTAB(K+3,IC)=VTAB(K+2,IC)+VOLINCR
            ATAB(K+3,IC)=AFL(K+1,IC)
        ENDDO
        
    ENDDO 

!!!!IF(1==1)THEN !USAR A CURVA HIPSOM�TRICA COTAX�REAXVOLUME PARA ALGUMAS SUB-BACIAS APENAS. NAS OUTRAS, A �REA ALAGADA � NULA
!!!!    
!!!!    DO IC=1,NC
!!!!        ZTAB(1,IC)=ZFUNDOFL(IC)-HRIO(IC) !PRIMEIRO PONTO DA TABELA COTA-VOLUME CORRESPONDE AO FUNDO DO RIO
!!!!        VTAB(1,IC)=0.0 !PRIMEIRO PONTO DA TABELA COTA-VOLUME CORRESPONDE AO FUNDO DO RIO E O VOLUME � ZERO
!!!!        ATAB(1,IC)=0.0
!!!!        ZTAB(2,IC)=ZFUNDOFL(IC) !SEGUNDO PONTO DA TABELA COTA-VOLUME CORRESPONDE � SITUA��O EM QUE A CALHA EST� CHEIA
!!!!        VTAB(2,IC)=DBLE(BRIO(IC))*HRIO(IC)*DBLE(SRIO(IC))*1000.0 !CALCULA VOLUME DENTRO DA CALHA MENOR
!!!!        ATAB(2,IC)=0.0
!!!!        
!!!!        SELECT CASE (IBAC(IC)) !IDENTIFICA AS SUB-BACIAS A QUAL CADA MINIBACIA IC PERTENCE
!!!!        
!!!!        CASE (1:999) !SUB-BACIAS DO ALTO-PARAN� QUE N�O TEM �REA ALAGADA RELEVANTE
!!!!        
!!!!            !PARA OS PROXIMOS NPFL PONTOS DA TABELA � ACRESCENTADO UM METRO SEM A INFLU�NCIA DA PLAN�CIE
!!!!            DO K=1,NPFL(IC)
!!!!                ZTAB(K+2,IC)=ZFUNDOFL(IC)+K
!!!!                VTAB(K+2,IC)=DBLE(BRIO(IC))*(HRIO(IC)+K)*DBLE(SRIO(IC))*1000.0 !CALCULA VOLUME DENTRO DA CALHA MENOR
!!!!                ATAB(K+2,IC)=0.0
!!!!            ENDDO !FIM DO LOOP DAS COTAS DA TABELA COTA-VOLUME
!!!!           
!!!!        CASE DEFAULT !SUB-BACIAS COM �REA ALAGADA
!!!!        
!!!!            ZTAB(3,IC)=ZFL(1,IC) !TERCEIRO PONTO DA TABELA COTA-VOLUME CORRESPONDE � SITUA��O DO PRIMEIRO PONTO TAB COTA-AREA
!!!!            VTAB(3,IC)=VTAB(2,IC)+0.5*(AFL(1,IC)+0.0)*(ZTAB(3,IC)-ZTAB(2,IC))*1000000.0
!!!!            ATAB(3,IC)=AFL(1,IC)
!!!!            
!!!!            !PARA OS PROXIMOS NPFL PONTOS DA TABELA CONTINUA INTEGRANDO AS �REAS POR TRAP�ZIOS PARA OBTER VOLUMES ADICIONAIS
!!!!            DO K=1,NPFL(IC)-1
!!!!                ZTAB(K+3,IC)=ZFL(K+1,IC)
!!!!                VOLINCR=(ZFL(K+1,IC)-ZFL(K,IC))*1000000.0*((AFL(K,IC)+AFL(K+1,IC))*0.5) !VOLUME INCREMENTAL POR TRAP�ZIOS
!!!!                VTAB(K+3,IC)=VTAB(K+2,IC)+VOLINCR
!!!!                ATAB(K+3,IC)=AFL(K+1,IC)
!!!!            ENDDO !FIM DO LOOP DAS COTAS DA TABELA COTA-VOLUME
!!!!        
!!!!        END SELECT
!!!!         
!!!!    ENDDO !FIM DO LOOP DAS MINIBACIAS    
!!!!    
!!!!    ENDIF
    
    
    RETURN
    END
