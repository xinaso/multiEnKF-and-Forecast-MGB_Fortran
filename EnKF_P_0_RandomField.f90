   !---------------------------------------------------------------------------------
    !  Licensing:
    !
	!Modelo de Grandes Bacias, South America version (MGB-SA). 
    !Copyright (C) 2019  Hidrologia de Grande Escala (HGE)
	!
    !This program is free software: you can redistribute it and/or modify
    !it under the terms of the GNU General Public License as published by
    !the Free Software Foundation, either version 3 of the License, or
    !any later version.
	!
    !This program is distributed in the hope that it will be useful,
    !but WITHOUT ANY WARRANTY; without even the implied warranty of
    !MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    !GNU General Public License for more details.
	!
    !You should have received a copy of the GNU General Public License
    !along with this program.  If not, see <https://www.gnu.org/licenses/>.	
	!
	!---------------------------------------------------------------------------------
    !  Version/Modified: 
    !
    !    2017.20.10 - 20 October 2017 (By: Sly Wongchuig)    
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
	!	 * Ayan Santos Fleischmann
	!	 * Sly Wongchuig Correa
    !
    !  Main Reference:
    !
    !    Rodrigo Paiva,
    !    Paiva, R.C.D. Collischonn, W., Bonnet, M.P., de Goncalves, L.G.G., Calmant Stéphane, Getirana, A., da Silva J. S., 2013.
    !    Assimilating in situ and radar altimetry data into a large-scale hydrologic-hydrodynamic model for streamflow forecast 
    !    in the Amazon. Hydrol. Earth Syst. Sci., 17, 2929-2946.
    
    !    Sly Wongchuig,
    !    Thesis
    !    Porto Alegre, 2019
    !
	!---------------------------------------------------------------------------------
    !  Discussion:
    ! 
    !   This sub-routine is used to perturb the rainfall
    !
    !
    !---------------------------------------------------------------------------------
	! ********************************  Description of main variables:  ***********************
    ! *Variables declarations and routines calls are all commented below.
    ! End of header
    !---------------------------------------------------------------------------------
    subroutine RandomField

use m_sample2D
USE EnKF_MOD
USE VARS_MAIN

implicit none
real,allocatable:: A2(:,:,:),noise1(:)


integer:: nrens,nre
logical:: samp_fix
integer:: nbusca
integer:: ilat,ilon,ilat0,ilon0
real:: lat0,lon0,dist,somadist
real:: Draio
real:: BIAS,ro  
!----------------------------------------------------------------------------------------------

BIAS=0.0
! Gera campo aleatorio com distribuicao normal N(0,1) e covariancia igual a e^-1 para distancia igual ao decorrelation length rh:
nrens=1
nre=1
samp_fix=.false.
allocate(A2(nlon,nlat,nrens))
allocate(noise1(nC))

call sample2D(A2,nlon,nlat,nrens,nre,rh,samp_fix)


! Interpola variavel aleatoria para cada minibacia:
noise1=0.0
do iC=1,nC
    !COnsidera um raio de busca maximo igual ao lado de um quadrado. Se muito pequeno, usa igual a resolucao do grid A2
    ! El area de la minibacia es convertida a grados 
    Draio=0.5*ACEL(iC)**0.5/105.00
    Draio=max(Draio,res)
    nbusca=int(Draio/res)+1     ! Devuelve el valor entero
    nbusca=int(nbusca*1.5)+1 ! usa um raio de teste um pouco maior
    
    ilon0=int((X(iC)-lonmin)/res)+1
    ilat0=int((Y(iC)-latmin)/res)+1
    
    somadist=0.0
    do ilon=ilon0-nbusca,ilon0+nbusca
        if (ilon<1.or.ilon>nlon) cycle
        do ilat=ilat0-nbusca,ilat0+nbusca
            if (ilat<1.or.ilat>nlat) cycle
            
            lat0=latmin+res*(ilat-1)
            lon0=lonmin+res*(ilon-1)
            
            dist=((lat0-Y(iC))**2+(lon0-X(iC))**2)**0.5
            if (dist<=Draio) then
                dist=1/dist**2
                noise1(iC)=noise1(iC)+dist*A2(ilon,ilat,1)
                somadist=somadist+dist
            endif
        enddo
    enddo        
    noise1(iC)=noise1(iC)/somadist
enddo

ro=1.-1./tal

if (iT==1) ro=0.0
noise(:,iEns)=ro*noise(:,iEns)+(1.-ro**2)**0.5*noise1

if (iEns==1) write(66616662,*) ro,noise(50,iEns),noise(500,iEns),noise(676,iEns)
noise1=noise(:,iEns)


! Aplica equacao 2 de Nijssen e Lettenmaier (2004)
noise1=(log(ErroP**2+1.))**0.5*noise1
noise1=exp(noise1)
noise1=(1.+BIAS)*noise1/(ErroP**2+1.)**0.5
!P=P*noise1      !! Sly + Vinicius
P=P*real(noise1)

if (iEns==1) write(66616663,*) ro,noise1(50),noise1(500),noise1(676)

deallocate(A2,noise1)


end subroutine
