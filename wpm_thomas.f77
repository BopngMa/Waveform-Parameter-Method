	  PROGRAM MAIN

!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>!
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>!
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>!
!1     FORMAT(A100)
!2     FORMAT(10F7.0)
!3     FORMAT(3I5)
4     FORMAT(////5X,24HSONIC CUTOFF ALTITUDE = ,F7.0,6H  FEET,
     1//5X,17HPHI FOR CUTOFF = ,F5.0,9H  DEGREES)
5     FORMAT(5X,A,///)
6     FORMAT(5X,18HFLIGHT ALTITUDE = ,F7.0,6H  FEET,//5X,14HMACH NUMBER 
     1= ,F6.3,//5X,10HHEADING = ,F6.1,9H  DEGREES,//5X,20HFLIGHT PATH AN
     2GLE = ,F6.2,9H  DEGREES,//5X,8HM-DOT = ,F6.4,6H  /SEC,//5X,10HPSI-
     3DOT = ,F6.3,13H  DEGREES/SEC,//5X,12HGAMMA-DOT = ,F6.3,13H  DEGREE 
     4S/SEC,//5X,21HAIRCRAFT LONGITUDE = ,F8.3,9H  DEGREES,//5X,20HAIRCR
     5AFT LATITUDE = ,F7.3,9H  DEGREES,//5X,18HAIRCRAFT LENGTH = ,F5.1,
     66H  FEET,////5X,22HATMOSPHERIC CONDITIONS)
7     FORMAT(/5X,6HPOG = ,F7.2,5H  PSF)
8     FORMAT(//9X,2HZT,11X,2HTO,11X3HZWE,11X,3HVOW,11X,3HZWN,11X,3HVON/
     18X,4HFEET,9X,5HDEG F,9X,4HFEET,9X,6HFT/SEC, 9X,4HFEET,9X,
     26HFT/SEC/)
9     FORMAT(F14.1,F11.1,F15.1,F12.1,F16.1,F12.1)
10    FORMAT(25X,F15.1,F12.1,F16.1,F12.1)
11    FORMAT(25X,F15.1,F12.1)
12    FORMAT(52X,F16.1,F12.1)
13    FORMAT(F14.1,F11.1,27X,F16.1,F12.1)
14    FORMAT(F14.1,F11.1)
15    FORMAT(F14.1,F11.1,F15.1,F12.1)
16    FORMAT(//////5X,16HINITIAL WAVEFORM,13X,6HR/L = ,F5.1,13X,6HPHI = 
     1,F7.2,8H DEGREES,///16X,3HX/L,17X,4HDP/P,//(F20.3,F21.5))
19    FORMAT(1H1)
20    FORMAT(///5X, 23HWAVEFORM AT ALTITUDE = ,F8.1,6H FEET,// 9X,12HLON
     1GITUDE = ,F8.3,9H  DEGREES,/9X,11HLATITUDE = ,F7.3,9H  DEGREES,//)
21    FORMAT(13X,7HT, MSEC,15X,6HP, PSF,//(F20.1,F20.3))
22    FORMAT(13X,7HX, FEET,14X,6HP, PSF,//(F20.2,F20.3))
23    FORMAT(13X,7HT, MSEC,15X,4HDP/P,//(F20.1,F20.6))
24    FORMAT(13X,7HX, FEET,14X,4HDP/P,//(F20.2,F20.6))
27    FORMAT(8X,34HRAY TUBE AREA GOES TO ZERO AT Z = ,F8.1,6H  FEET)
28    FORMAT(/5X,28HCONSTANT PRESSURE ATMOSPHERE)
29    FORMAT(///5X, 23HGROUND-RAY INTERSECTION,//9X,12HLONGITUDE = ,F8.3
     1,9H  DEGREES,/9X,11HLATITUDE = ,F7.3,9H  DEGREES,/9X,29HDISTANCE F
     2ROM GROUND TRACK = ,F6.2,7H  MILES,////5X,22HWAVEFORM AT THE GROUN
     3D,10X,20HREFLECTION FACTOR = ,F4.2,//)
!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<!
!<<<<<<<<<<<<<<<<<<<<<<<<<FORMAT END<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<!
!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<!
!    
!     PROGRAM MAIN
!   
      INTEGER OCODE
      REAL    MACH,N(3,4),K1,M(1000),LAMDA(1000),ML,MAG,MDOT,LATP,LONGP
      DIMENSION R(3,4),ZT(100),TO(100),ZWE(100),VOE(100),
     1          ZWN(100),VON(100),ALT(50),X(1000),DPP(1000),F1(1000),
     1          F2(1000),P(1000),DP(1000),XX(1000),PP(1000),VO(3),
     2          AO1(4),VO1(3,4)
      CHARACTER(LEN=100) TITLE
      DOUBLE PRECISION MMACH,FFPA,HHEAD,MU
!   
!     OPEN THE INPUT FILE AND READ
!   
      OPEN(55,FILE='INDATA.IN')
      READ(55,'(a)') TITLE
      READ(55,*) !====MACH====FLTALT====!
      READ(55,*) MACH,FLTALT
      READ(55,*) !====MDOT====PSIDOT====GAMDOT====FPA====!
      READ(55,*) MDOT,PSIDOT,GAMDOT,FPA
      READ(55,*) !====LONGP====LATP====HEAD====!
      READ(55,*) LONGP,LATP,HEAD
      READ(55,*) !====POG====!
      READ(55,*) POG
      READ(55,*) !====NTEMP====NWINDE====NWINDN====!
      READ(55,*) NTEMP,NWINDE,NWINDN
      READ(55,*) !====ZT OF TEMPERATURE====!
      READ(55,*) (ZT(I),I=1,NTEMP)
      READ(55,*) !====TEMPERATURE VIA ZT====!
      READ(55,*) (TO(I),I=1,NTEMP)
      READ(55,*) !====ZWE OF EAST WIND====!
      READ(55,*) (ZWE(I),I=1,NWINDE)
      READ(55,*) !====EAST WIND====!
      READ(55,*) (VOE(I),I=1,NWINDE)
      READ(55,*) !====ZWN OF NORTH WIND====!
      READ(55,*) (ZWN(I),I=1,NWINDN)
      READ(55,*) !====NORTH WIND====!
      READ(55,*) (VON(I),I=1,NWINDN)
      READ(55,*) !====NALT====!
      READ(55,*) NALT
      READ(55,*) (ALT(I),I=1,NALT)
      READ(55,*) !====OCODE====!
      READ(55,*) OCODE
      READ(55,*) !====REFLECT====!
      READ(55,*) REFL
      READ(55,*) !====ROVERL====PHI====!
      READ(55,*) ROVERL,PHI
      READ(55,*) !====NX OF XI-DP====!
      READ(55,*) NX
      READ(55,*) !====XI====!
      READ(55,*) (X(I),I=1,NX)
      READ(55,*) !====DPP====!
      READ(55,*) (DPP(I),I=1,NX)
      READ(55,*) !====AL====ML====!
      READ(55,*) AL,ML
      CLOSE(55)
!   
!     END READ
!   
!   
!     缩放尺寸
!   
      DELTAT = 0.1
      PHISAV = PHI
      RO     = ROVERL*AL
      G      = 32.26-0.17*COS(LATP/57.296)**2                                    !    not understand
      IF(POG .GT. 0.0) GOTO 45
      G      = 0.
      POG    = 1.0
45    ICUT   = 0
      DPHI   = -10.
      IF(PHI .LT. 0.) DPHI=10.
      XOFF   = X(1)
      DO 50 I=1,NX
50    X(I)   = (X(I)-XOFF)/ML
      DO 60 I=1,NTEMP
60    ZT(I)  = 1000.0*ZT(I)
      DO 70 I=1,NWINDE
70    ZWE(I) = 1000.0*ZWE(I)
      DO 80 I=1,NWINDN
80    ZWN(I) = 1000.0*ZWN(I)
!   
!     缩放结束
!   
      OPEN(66,FILE='OUT.DAT')
      
!   
!     WRITE THE CALCULATED CONDITIONS
!   
      
      WRITE(66,5) TRIM(TITLE)
      WRITE(66,6) FLTALT,MACH,HEAD,FPA,MDOT,PSIDOT,GAMDOT,LONGP,LATP,AL
      IF(G .GT. 0.) GOTO 85
      WRITE(66,28)
      GOTO 90
85    WRITE(66,7) POG
90    WRITE(66,8)
!   
!     WRITE Z---TO,VOE,VON
!   
      I      = 1
100   IF(I .GT. NTEMP) GOTO 110
      IF(I .GT. NWINDE) GOTO 140
      IF(I .GT. NWINDN) GOTO 160
      WRITE(66,9) ZT(I),TO(I),ZWE(I),VOE(I),ZWN(I),VON(I)
      GOTO 170
110   IF(I .GT. NWINDE) GOTO 130
      IF(I .GT. NWINDN) GOTO 120
      WRITE(66,10) ZWE(I),VOE(I),ZWN(I),VON(I)
      GOTO 170
120   WRITE(66,11) ZWE(I),VOE(I)
      GOTO 170
130   IF(I .GT. NWINDN) GOTO 180
      WRITE(66,12) ZWN(I),VON(I)
      GOTO 170
140   IF(I .GT. NWINDN) GOTO 150
      WRITE(66,13) ZT(I),TO(I),ZWN(I),VON(I)
      GOTO 170
150   WRITE(66,14) ZT(I),TO(I)
      GOTO 170
160   WRITE(66,15) ZT(I),TO(I),ZWE(I),VOE(I)
170   I      = I+1
      GOTO 100
!   
!     END WRITE THE Z----TO,VOE,VON
!   
180   IF(I .LT. 10) WRITE(66,19)
      WRITE(66,16) ROVERL,PHI,(X(I),DPP(I),I=1,NX)
      WRITE(66,19)
!   
!     END WRITE THE CALCULATED CONDITIONS
!   
    
!     I DO NOT THE MEAN THE MODULE
200   IF(ICUT .EQ. 0) GOTO 230
      IF(PHISAV .LT. 0.) GOTO 210
      IF(PHI .GT. 0.) GOTO 230
      DPHI   = 1.0
      GOTO 220
210   IF(PHI .LT. 0.) GOTO 230
      DPHI   = -1.0
220   PHI    = 0.
!     ----------------------------
230   MMACH  = MACH
      FFPA   = FPA/57.296
      HHEAD  = HEAD/57.296
      PHI    = PHI/57.296
      R(1,1) = 0.                                                               ! X-COORDINATE OF RAY
      R(2,1) = 0.                                                               ! Y-              RAY
      R(3,1) = FLTALT                                                           ! Z-              RAY
      
      IZERO  = 0
      IFORK  = 1
      II     = 1
      IRAY   = 2
      H      = FLTALT
!
!     GET THE SOUND SPEED-AO , WINDEEST AND WINDNORTH AT H
!
240   DO 250 J=2,NTEMP
      IF(ZT(J) .LT. H) GOTO 250
      AO     = 49.1*SQRT(TO(J-1)+(H-ZT(J-1))/(ZT(J)-ZT(J-1))*(TO(J)-
     1         TO(J-1))+459.67)
      GOTO 260
250   CONTINUE
260   DO 270 J=2,NWINDE
      IF(ZWE(J) .LT. H) GOTO 270
      VO(1)  = VOE(J-1)+(H-ZWE(J-1))/(ZWE(J)-ZWE(J-1))*(VOE(J)-VOE(J-1))
      GOTO 280
270   CONTINUE
280   DO 290 J = 2, NWINDN
      IF( ZWN(J) .LT. H) GOTO 290
      VO(2)  = VON(J-1)+(H-ZWN(J-1))/(ZWN(J)-ZWN(J-1))*(VON(J)-VON(J-1))
      GOTO 300
290   CONTINUE
!
!     END THE GET VARIBALES AT H
!
!     DO 
300   IF(IRAY .GT. 1 .OR. ICUT .EQ. 1 ) GOTO 340
      PO     = POG
      DO 330 J=2,NTEMP
      Z2     = ZT(J)
      IF(H .LE. Z2) Z2 = H
      IF(TO(J-1) .EQ. TO(J)) GOTO 310
      TOZ    = (TO(J)-TO(J-1))/(ZT(J)-ZT(J-1))
      PO     = PO*(1.+TOZ/(TO(J-1)+459.67)*(Z2-ZT(J-1)))**
     1         (-1./53.353/TOZ)
      GOTO 320
310   PO     = PO*EXP((ZT(J-1)-Z2)/53.353/(TO(J-1)+459.67))
320   IF(Z2 .EQ. H) GOTO 340
330   CONTINUE
!     ENDDO

340   GOTO (350,460,580), IFORK                                                 !依据不同的ifork 跳转不同的模块        

350   VOXH   = VO(1)
      VOYH   = VO(2)
      VX     = MACH*AO*COS(FPA/57.296)*SIN(HEAD/57.296)
      VY     = MACH*AO*COS(FPA/57.296)*COS(HEAD/57.296)
      
      IRAY   = 0
360   IRAY   = IRAY+1
      GOTO (410,390,380,370,430), IRAY                                          !依据不同的iray（射线）跳转不同的模块
      
370   R(1,4) = R(1,3)
      R(2,4) = R(2,3)
      R(3,4) = R(3,3)
      GOTO 400
      
380   DT     = 0.17453*RO/AO/SQRT(MACH**2-1.0)                                  !飞机机动
      MMACH  = MMACH+MDOT*DT
      FFPA   = FFPA+GAMDOT/57.296*DT
      HHEAD  = HHEAD+PSIDOT/57.296*DT
      PHI    = PHI-0.017453
      R(1,3) = R(1,1)+(VX+VO(1))*DT
      R(2,3) = R(2,1)+(VY+VO(2))*DT
      R(3,3) = R(3,1)+MACH*AO*SIN(FPA/57.296)*DT
      GOTO 410
      
390   R(1,2) = R(1,1)
      R(2,2) = R(2,1)
      R(3,2) = R(3,1)
400   PHI    = PHI+0.017453
      GOTO 420
      
410   MU     = DASIN(1.0/MMACH)
420   N(1,IRAY) = DCOS(FFPA)*DSIN(HHEAD)*DSIN(MU)-DCOS(HHEAD)*DCOS(MU)
     1            *SIN(PHI)+DSIN(FFPA)*DSIN(HHEAD)*DCOS(MU)*COS(PHI)
      N(2,IRAY) = DCOS(FFPA)*DCOS(HHEAD)*DSIN(MU)+DSIN(HHEAD)*DCOS(MU)
     1            *SIN(PHI)+DSIN(FFPA)*DCOS(HHEAD)*DCOS(MU)*COS(PHI)
      N(3,IRAY) = DSIN(FFPA)*DSIN(MU)-DCOS(FFPA)*DCOS(MU)*COS(PHI)
      IF(IRAY .EQ. 4) PHI = (PHI-0.017453)*57.296
      GOTO 360
      
430   DO 440 IRAY=1,4
      IF(IRAY .EQ. 1 ) DELT = RO/AO/DCOS(MU)
      IF(IRAY .EQ. 3 ) DELT = DELT-DT
      R(1,IRAY) = R(1,IRAY)+(AO*N(1,IRAY)+VO(1))*DELT
      R(2,IRAY) = R(2,IRAY)+(AO*N(2,IRAY)+VO(2))*DELT
440   R(3,IRAY) = R(3,IRAY)+AO*N(3,IRAY)*DELT

      A1     = ((R(2,4)-R(2,1))*(R(3,3)-R(3,2))-(R(3,4)-R(3,1))*(R(2,3)
     1         -R(2,2)))*N(1,1)+((R(3,4)-R(3,1))*(R(1,3)-R(1,2))-(R(1,4)
     2         -R(1,1))*(R(3,3)-R(3,2)))*N(2,1)+((R(1,4)-R(1,1))*(R(2,3)
     3         -R(2,2))-(R(2,4)-R(2,1))*(R(1,3)-R(1,2)))*N(3,1)
      IFORK  = 2
      IRAY   = 0
450   IRAY   = IRAY+1
      H      = R(3,IRAY)
      GOTO 240
      
460   AO1(IRAY)   = AO
      VO1(1,IRAY) = VO(1)
      VO1(2,IRAY) = VO(2)
      IF(IRAY .LT. 4) GOTO 450
      IF(ICUT .EQ. 1) GOTO 530
      
      PO1 = PO
      CN1 = AO1(1)+VO1(1,1)*N(1,1)+VO1(2,1)*N(2,1)
      DO 470 I=1,NX
      P(I) = DPP(I)*PO
470   X(I) = X(I)/MACH*AL/CN1

      K    = 1
      J    = 2
      NN   = NX-1
      M(1) = 0.
!     DO I=1,NX
      DO 520 I=1,NX
      IF(K .EQ. 2) GOTO 510
      IF(I .EQ. NX) GOTO 500
      IF(X(I) .EQ. X(I+1)) GOTO 480
      DP(J)    = 0.
      LAMDA(J) = X(I+1)-X(I)
      M(J)     = (P(I+1)-P(I))/LAMDA(J)
      J        = J+1
      GOTO 520
      
480   K     = 2
      DP(J) = P(I+1)-P(I)
      IF(I+2 .GT. NX) GOTO 490
      LAMDA(J) = X(I+2)-X(I+1)
      M(J)     = (P(I+2)-P(I+1))/LAMDA(J)
      NN       = NN-1
      J        = J+1
      GOTO 520
      
490   M(J)     = 0.
      NN       = NN-1
      GOTO 520
      
500   DP(J)    = 0.
      M(J)     = 0.
510   K        = 1
520   CONTINUE
!     ENDDO

530   KRITE    = 0
      IF(DELTAT .EQ. 0.2) GOTO 535
      DELTAT   = 0.0002*SQRT(R(1,1)**2+R(2,1)**2+(FLTALT-R(3,1))**2)
      IF(DELTAT .GT. 0.2) DELTAT = 0.2
535   IF(R(3,1)+AO1(1)*N(3,1)*DELTAT-ALT(II)) 540,540,550                       !if( express<0 ) goto 540; if(express==0) goto 540;if(express>0)goto 550
540   DELT     = (R(3,1)-ALT(II))/AO1(1)/(-N(3,1))
      KRITE    = 1
      GOTO 560
      
550   DELT     = DELTAT
560   IRAY     = 0
      AOS      = AO1(1)

570   IRAY     = IRAY+1
      IF(IRAY .EQ. 5) GOTO 610
      R(1,IRAY) = R(1,IRAY)+(AO1(IRAY)*N(1,IRAY)+VO1(1,IRAY))*DELT
      R(2,IRAY) = R(2,IRAY)+(AO1(IRAY)*N(2,IRAY)+VO1(2,IRAY))*DELT
      R(3,IRAY) = R(3,IRAY)+AO1(IRAY)*N(3,IRAY)*DELT
      H         = R(3,IRAY)
      IFORK     = 3
      GOTO 240
      
580   DZ          = -AO1(IRAY)*N(3,IRAY)*DELT
      AOZ         = (AO1(IRAY)-AO)/DZ
      VOXZ        = (VO1(1,IRAY)-VO(1))/DZ
      VOYZ        = (VO1(2,IRAY)-VO(2))/DZ
      AO1(IRAY)   = AO
      VO1(1,IRAY) = VO(1)
      VO1(2,IRAY) = VO(2)
      CC          = -(N(1,IRAY)*VOXZ+N(2,IRAY)*VOYZ+AOZ)
      DN1         = -N(1,IRAY)*N(3,IRAY)*CC*DELT
      DN2         = -N(2,IRAY)*N(3,IRAY)*CC*DELT
      DN3         = (N(1,IRAY)**2+N(2,IRAY)**2)*CC*DELT
      MAG         = SQRT((N(1,IRAY)+DN1)**2+(N(2,IRAY)+DN2)**2
     1              +(N(3,IRAY)+DN3)**2)
      N(1,IRAY)   = (N(1,IRAY)+DN1)/MAG
      N(2,IRAY)   = (N(2,IRAY)+DN2)/MAG
      N(3,IRAY)   = (N(3,IRAY)+DN3)/MAG
      IF(N(3,IRAY) .LT. -0.017453) GOTO 570
      IF(ICUT .EQ. 0) GOTO 600
      IF(ABS(DPHI) .GT. 2.0) GOTO 590
      WRITE(66,4) HCUT,PHI
      GOTO 1090
      
590   PHI = PHI+DPHI
      GOTO 200

600   HCUT = R(3,1)
      ICUT = 1
      IPHI = PHI
      PHI  = IPHI
      GOTO 590
      
610   IF(ICUT .EQ. 1 .OR. IZERO .EQ. 1) GOTO 921
      CN   = AO1(1)+VO1(1,1)*N(1,1)+VO1(2,1)*N(2,1)
      A    = ((R(2,4)-R(2,1))*(R(3,3)-R(3,2))-(R(3,4)-R(3,1))*(R(2,3)
     1       -R(2,2)))*N(1,1)+((R(3,4)-R(3,1))*(R(1,3)-R(1,2))-(R(1,4)
     2       -R(1,1))*(R(3,3)-R(3,2)))*N(2,1)+((R(1,4)-R(1,1))*(R(2,3)
     3       -R(2,2))-(R(2,4)-R(2,1))*(R(1,3)-R(1,2)))*N(3,1)
      IF(A .GT. 0.) GOTO 620
      HZERO = R(3,1)
      IZERO = 1
      GOTO 921
      
620   C1    = 1.714*(AO1(1)+AOS)/(PO1+PO)/(CN+CN1)
      C2    = ((AO1(1)-AOS)/(AO1(1)+AOS)+2.8*DZ*G/(AO1(1)+AOS)**2
     1        -2.*(CN-CN1)/(CN+CN1)-(A-A1)/(A+A1))/DELT
      PO1   = PO
      CN1   = CN
      A1    = A
630   DELTS = DELT
      TP    = (EXP(C2*DELT)-1.0)/C2
      N1    = NN+1
      N2    = NN+2
      K     = N2
640   J1    = 1

650   DO 660 J=1,N2
      IF(M(J) .EQ. M(K) .AND. J1 .EQ. 2 .AND. J .NE. K) 
     1      M(J) = 0.9999*M(J)
      F1(J) = 1.0-C1*M(J)*TP
      IF(F1(J) .LE. 0. .AND. J .NE. K) GOTO 670
660   CONTINUE
      GOTO 680

670   K     = J
      J1    = 2
      TP    = 1.0/C1/M(J)
      GOTO 650
      
680   TK    = C1*TP
      IF(J1 .EQ. 1) GOTO 690
      IF(DP(K) .EQ. 0. .AND. DP(K+1) .EQ. 0.) GOTO 690
      IF(DP(K) .EQ. 0.) GOTO 760
      IF(DP(K+1) .EQ. 0.) GOTO 780
      GOTO 800
      
690   KF    = 1
      J     = 1
      
695   J     = J+1
      IF(J .GT. N1) GOTO 840
      IF(J .EQ. K) GOTO 695
      
700   IF(ABS(TK*(M(J)-M(J-1))/F1(J)) .GT. 0.001) GOTO 710
      IF(ABS(TK*(M(J)-M(J+1))/F1(J)) .GT. 0.001) GOTO 705
      F2(J) = LAMDA(J)-(DP(J)+DP(J+1))*TK/F1(J)/2.
      GOTO (730,820), KF

705   F2(J) = LAMDA(J)-DP(J)*TK/F1(J)/2.-DP(J+1)/(M(J)-M(J+1))
     1        *(SQRT(F1(J+1)/F1(J))-1.0)
      GOTO (730,820), KF

710   F2(J) = LAMDA(J)-DP(J)/(M(J)-M(J-1))*(SQRT(F1(J-1)/F1(J))-1.0)
     1        -DP(J+1)*TK/F1(J)/2.
      IF(ABS(TK*(M(J)-M(J+1))/F1(J)) .GT. 0.001) GOTO 720
      F2(J) = LAMDA(J)-DP(J)/(M(J)-M(J-1))*(SQRT(F1(J-1)/F1(J))-1.0)
     1        -DP(J+1)*TK/F1(J)/2.
      GOTO (730,820), KF

720   IF(F1(J+1) .LT. 0.) F1(J+1)=0
      F2(J) = LAMDA(J)-DP(J)/(M(J)-M(J-1))*(SQRT(F1(J-1)/F1(J))-1.0)
     1        -DP(J+1)/(M(J)-M(J+1))*(SQRT(F1(J+1)/F1(J))-1.0)
      GOTO (730,820), KF

730   IF(F2(J))  740,735,695

735   IF(J1 .EQ. 2) LAMDA(J)=1.0001*LAMDA(J)
      IF(J1 .EQ. 2) GOTO 700
      K     = J
      GOTO 695
740   K     = J
750   IF(DP(K) .GT. 0.) GOTO 770

      IF(ABS(TK*(M(K)-M(K+1))/F1(K)) .GT. 0.001) GOTO 760
      TP    = 2.*LAMDA(K)/C1/(DP(K+1)+2.0*M(K)*LAMDA(K))
      GOTO 640
760   TP    = (1.-(1.+LAMDA(K)/DP(K+1)*(M(K)-M(K+1)))**2)/(C1*(M(K+1)-
     1        M(K)*(1.0+LAMDA(K)/DP(K+1)*(M(K)-M(K+1)))**2))
      GOTO 640

770   IF(DP(K+1) .GT. 0.) GOTO 790
      IF(ABS(TK*(M(K)-M(K-1))/F1(K)) .GT. 0.001)  GOTO 780
      TP    = 2.*LAMDA(K)/C1/(DP(K)+2.*M(K)*LAMDA(K))
      GOTO 640
      
780   TP    = (1.-(1.+LAMDA(K)/DP(K)*(M(K)-M(K-1)))**2)/(C1*(M(K-1)-
     1        M(K)*(1.+LAMDA(K)/DP(K)*(M(K)-M(K-1)))**2))
      GOTO 640
      
790   IF(ABS(TK*(M(K)-M(K-1))/F1(K)) .GT. 0.001) GOTO 800
      IF(ABS(TK*(M(K)-M(K+1))/F1(K)) .GT. 0.001) GOTO 800
      TP    = 2.*LAMDA(K)/C1/(DP(K)+DP(K+1)+2.*M(K)*LAMDA(K))
      GOTO 640

800   TP1   = 0.
      TP2   = TP
      KF    = 2
      IC    = 0
!     DO
810   IC    = IC+1
      IF(IC .GT. 20) GOTO 640
      TP    = (TP1+TP2)/2.
      TK    = C1*TP
      F1(J) = 1.0-C1*M(J)*TP
      F1(J-1) = 1.0-C1*M(J-1)*TP
      F1(J+1) = 1.0-C1*M(J+1)*TP
      GOTO 700
      
820   IF(F2(K) .LE. 0.) GOTO 830
      TP1   = TP
      GOTO 810
      
830   TP2   = TP
      GOTO 810
      
840   DELT  = ALOG(TP*C2+1.0)/C2
      E2    = EXP(C2*DELT)
      IF(K .EQ. N2) GOTO 850
      DPSUM = DP(K)+DP(K+1)
      IF(K .EQ. 2) GOTO 870
850   K11   = K-1

      DO 860 J=2,K11
      M(J)  = M(J)/F1(J)*E2
      DP(J) = DP(J)*E2/SQRT(F1(J)*F1(J-1))
860   LAMDA(J) = F1(J)*F2(J)

      IF(K .EQ. N2) GOTO 920
870   K22   = K+1
      IF(DPSUM .EQ. 0.) GOTO 880
      DP(K) = E2*(DP(K)/SQRT(F1(K)*F1(K-1))+DP(K+1)/SQRT(F1(K)*F1(K+1)))
      GOTO 890
      
880   DP(K) = M(K)*LAMDA(K)*E2
890   M(K)  = M(K+1)/F1(K+1)*E2
      IF(K .EQ. N1) GOTO 910
      
      DO 900 J=K22,N1
      M(J)  = M(J+1)/F1(J+1)*E2
      DP(J) = DP(J+1)*E2/SQRT(F1(J+1)*F1(J))
900   LAMDA(J-1) = F1(J)*F2(J)

910   NN    = NN-1
      DELT  = DELTS-DELT
      GOTO 630
      
920   DP(N2)= DP(N2)*E2/SQRT(F1(N2)*F1(N1))
921   IF(KRITE .EQ. 0) GOTO 530
      IF(ICUT .EQ. 0) GOTO 922
      II    = II+1
      IF(II .LE. NALT) GOTO 530
      DPHI  = 1.0
      IF(PHISAV .LT. 0.) DPHI = -1.
      GOTO 590

922   IF(IZERO .EQ. 0) GOTO 926
      II    = II+1
      IF(II .LE. NALT) GOTO 530
      IF(ALT(NALT) .NE. 0.) GOTO 924
      II    = II-1
      GOTO 990
      
924   NALT  = NALT+1
      ALT(NALT) = 0.
      GOTO 530
      
926   XX(1) = 0.
      PP(1) = 0.
      J     = 2
      DO 940 L= 2,N1
      IF(DP(L) .GT. 0.) GOTO 930
      XX(J) = XX(J-1)+LAMDA(L)
      PP(J) = PP(J-1)+M(L)*LAMDA(L)
      J     = J+1
      GOTO 940
930   XX(J) = XX(J-1)
      PP(J) = PP(J-1)+DP(L)
      XX(J+1) = XX(J)+LAMDA(L)
      PP(J+1) = PP(J)+M(L)*LAMDA(L)
      J     = J+2
940   CONTINUE
      
      IF(DP(N2) .EQ. 0.) GOTO 950
      XX(J) = XX(J-1)
      PP(J) = PP(J-1)+DP(N2)
      J     = J+1
950   J     = J-1
      IF(ALT(II) .NE. 0.) GOTO 970
      DO 960 L=1,J
960   PP(L) = PP(L)*REFL
970   IF(POG .NE. 1.0) GOTO 990
      DO 980 L=1,J
980   PP(L) = PP(L)/PO
990   RLAT  = R(2,1)/(20890070.+ALT(II))*57.296+LATP
      RLONG = -R(1,1)/(20890070.+ALT(II))*57.296/COS(RLAT/57.296)+LONGP
      IF(ALT(II) .NE. 0.) GOTO 1000
      DGT   = (R(2,1)*SIN(HEAD/57.296)-R(1,1)*COS(HEAD/57.296))/5280.
      WRITE(66,29) RLONG,RLAT,DGT,REFL
      IF(IZERO) 1010,1010,1080
1000  WRITE(66,20) ALT(II),RLONG,RLAT
1010  IF(OCODE .EQ. 2) GOTO 1040
      DO 1020 L=1,J
1020  XX(L) = XX(L)*1000.
      IF(POG .EQ. 1.) GOTO 1030
      WRITE(66,21) (XX(L),PP(L),L=1,J)
      GOTO 1070
      
1030  WRITE(66,23) (XX(L),PP(L),L=1,J)
      GOTO 1070
1040  C     = ((VX+VOXH)*N(1,1)+(VY+VOYH)*N(2,1))/SQRT((VX+VOXH)**2
     1        +(VY+VOYH)**2)
      DO 1050 L=1,J
1050  XX(L) = XX(L)*CN/C
      IF (POG .EQ. 1.) GOTO 1060
      WRITE(66,22) (XX(L),PP(L),L=1,J)
      GOTO 1070
1060  WRITE(66,24) (XX(L),PP(L),L=1,J)
1070  II    = II+1
      KRITE = 0
      IF(II-NALT) 530,530,1090
1080  WRITE(66,27) HZERO
1090  WRITE(66,19)
!      GOTO 40
      CLOSE(66)
      
!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<!
!<<<<<<<<<<<<<<<<<<<<Thomas waveform end<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<!
!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<!
