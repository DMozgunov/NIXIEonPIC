EESchema Schematic File Version 4
LIBS:NIXIEonPIC-cache
EELAYER 26 0
EELAYER END
$Descr A4 11693 8268
encoding utf-8
Sheet 1 1
Title "NIXIE clock on PIC16F628"
Date "2018-08-08"
Rev "3"
Comp ""
Comment1 ""
Comment2 "Mozgunov D."
Comment3 ""
Comment4 ""
$EndDescr
$Comp
L MCU_Microchip_PIC16:PIC16F628A-IP U?
U 1 1 5B6F4551
P 2850 2500
F 0 "U?" H 3050 3100 50  0000 C CNN
F 1 "PIC16F628A-IP" H 2200 3150 50  0000 C CNN
F 2 "" H 2850 2500 50  0001 C CIN
F 3 "http://ww1.microchip.com/downloads/en/DeviceDoc/40300c.pdf" H 2850 2500 50  0001 C CNN
	1    2850 2500
	1    0    0    -1  
$EndComp
$Comp
L Diode:1N4148 D?
U 1 1 5B6F472C
P 2850 1500
F 0 "D?" V 2896 1421 50  0000 R CNN
F 1 "1N4148" H 2950 1600 50  0000 R CNN
F 2 "Diode_THT:D_DO-35_SOD27_P7.62mm_Horizontal" H 2850 1325 50  0001 C CNN
F 3 "http://www.nxp.com/documents/data_sheet/1N4148_1N4448.pdf" H 2850 1500 50  0001 C CNN
	1    2850 1500
	0    -1   -1   0   
$EndComp
$Comp
L Diode:1N4148 D?
U 1 1 5B6F486B
P 2400 1350
F 0 "D?" H 2400 1150 50  0000 C CNN
F 1 "1N4148" H 2400 1250 50  0000 C CNN
F 2 "Diode_THT:D_DO-35_SOD27_P7.62mm_Horizontal" H 2400 1175 50  0001 C CNN
F 3 "http://www.nxp.com/documents/data_sheet/1N4148_1N4448.pdf" H 2400 1350 50  0001 C CNN
	1    2400 1350
	-1   0    0    1   
$EndComp
$Comp
L Device:Battery_Cell BT?
U 1 1 5B6F4959
P 1950 1350
F 0 "BT?" V 1850 1300 50  0000 R CNN
F 1 "3V Li" V 1800 1550 50  0000 R CNN
F 2 "" V 1950 1410 50  0001 C CNN
F 3 "~" V 1950 1410 50  0001 C CNN
	1    1950 1350
	0    1    1    0   
$EndComp
$Comp
L Transistor_BJT:MPSA42 Q?
U 1 1 5B6F4A65
P 6350 3650
F 0 "Q?" H 6541 3696 50  0000 L CNN
F 1 "MPSA42" V 6550 3350 50  0000 L CNN
F 2 "Package_TO_SOT_THT:TO-92_Inline" H 6550 3575 50  0001 L CIN
F 3 "http://www.onsemi.com/pub_link/Collateral/MPSA42-D.PDF" H 6350 3650 50  0001 L CNN
	1    6350 3650
	1    0    0    -1  
$EndComp
$Comp
L Transistor_BJT:MPSA92 Q?
U 1 1 5B6F4B14
P 6000 4250
F 0 "Q?" H 6191 4296 50  0000 L CNN
F 1 "MPSA92" V 5950 4350 50  0000 L CNN
F 2 "Package_TO_SOT_THT:TO-92_Inline" H 6200 4175 50  0001 L CIN
F 3 "http://www.onsemi.com/pub_link/Collateral/MPSA92-D.PDF" H 6000 4250 50  0001 L CNN
	1    6000 4250
	1    0    0    1   
$EndComp
$Comp
L Device:R R?
U 1 1 5B6F5184
P 6100 3450
F 0 "R?" H 6170 3496 50  0000 L CNN
F 1 "10k" H 6170 3405 50  0000 L CNN
F 2 "" V 6030 3450 50  0001 C CNN
F 3 "~" H 6100 3450 50  0001 C CNN
	1    6100 3450
	1    0    0    -1  
$EndComp
$Comp
L Device:D D?
U 1 1 5B6F5450
P 6450 4100
F 0 "D?" V 6550 4250 50  0000 R CNN
F 1 "FR105" H 6500 4200 50  0000 R CNN
F 2 "" H 6450 4100 50  0001 C CNN
F 3 "~" H 6450 4100 50  0001 C CNN
	1    6450 4100
	0    -1   -1   0   
$EndComp
Wire Wire Line
	6450 3850 6450 3950
$Comp
L power:GND #PWR?
U 1 1 5B6F56CC
P 2850 3300
F 0 "#PWR?" H 2850 3050 50  0001 C CNN
F 1 "GND" H 2855 3127 50  0000 C CNN
F 2 "" H 2850 3300 50  0001 C CNN
F 3 "" H 2850 3300 50  0001 C CNN
	1    2850 3300
	1    0    0    -1  
$EndComp
$Comp
L power:GND #PWR?
U 1 1 5B6F5714
P 6100 4500
F 0 "#PWR?" H 6100 4250 50  0001 C CNN
F 1 "GND" H 6250 4400 50  0000 C CNN
F 2 "" H 6100 4500 50  0001 C CNN
F 3 "" H 6100 4500 50  0001 C CNN
	1    6100 4500
	1    0    0    -1  
$EndComp
$Comp
L Device:R R?
U 1 1 5B6B5752
P 6100 3850
F 0 "R?" H 6170 3896 50  0000 L CNN
F 1 "1M" H 6170 3805 50  0000 L CNN
F 2 "" V 6030 3850 50  0001 C CNN
F 3 "~" H 6100 3850 50  0001 C CNN
	1    6100 3850
	1    0    0    -1  
$EndComp
Wire Wire Line
	6100 3600 6100 3650
Wire Wire Line
	6150 3650 6100 3650
Connection ~ 6100 3650
Wire Wire Line
	6100 3650 6100 3700
Wire Wire Line
	6100 4000 6100 4050
Wire Wire Line
	6100 4450 6100 4500
Wire Wire Line
	5800 4250 5750 4250
Wire Wire Line
	6100 3300 6100 3250
Wire Wire Line
	6450 3250 6450 3450
$Comp
L Device:R R?
U 1 1 5B6B6389
P 5750 4050
F 0 "R?" V 5650 4050 50  0000 C CNN
F 1 "10k" V 5850 4050 50  0000 C CNN
F 2 "" V 5680 4050 50  0001 C CNN
F 3 "~" H 5750 4050 50  0001 C CNN
	1    5750 4050
	-1   0    0    1   
$EndComp
Wire Wire Line
	5750 4200 5750 4250
$Comp
L Transistor_BJT:MPSA42 Q?
U 1 1 5B6B6D5A
P 7250 3650
F 0 "Q?" H 7441 3696 50  0000 L CNN
F 1 "MPSA42" V 7450 3350 50  0000 L CNN
F 2 "Package_TO_SOT_THT:TO-92_Inline" H 7450 3575 50  0001 L CIN
F 3 "http://www.onsemi.com/pub_link/Collateral/MPSA42-D.PDF" H 7250 3650 50  0001 L CNN
	1    7250 3650
	1    0    0    -1  
$EndComp
$Comp
L Transistor_BJT:MPSA92 Q?
U 1 1 5B6B6D61
P 6900 4250
F 0 "Q?" H 7091 4296 50  0000 L CNN
F 1 "MPSA92" V 6850 4350 50  0000 L CNN
F 2 "Package_TO_SOT_THT:TO-92_Inline" H 7100 4175 50  0001 L CIN
F 3 "http://www.onsemi.com/pub_link/Collateral/MPSA92-D.PDF" H 6900 4250 50  0001 L CNN
	1    6900 4250
	1    0    0    1   
$EndComp
$Comp
L Device:R R?
U 1 1 5B6B6D68
P 7000 3450
F 0 "R?" H 7070 3496 50  0000 L CNN
F 1 "10k" H 7070 3405 50  0000 L CNN
F 2 "" V 6930 3450 50  0001 C CNN
F 3 "~" H 7000 3450 50  0001 C CNN
	1    7000 3450
	1    0    0    -1  
$EndComp
$Comp
L Device:D D?
U 1 1 5B6B6D6F
P 7350 4100
F 0 "D?" V 7450 4250 50  0000 R CNN
F 1 "FR105" H 7400 4200 50  0000 R CNN
F 2 "" H 7350 4100 50  0001 C CNN
F 3 "~" H 7350 4100 50  0001 C CNN
	1    7350 4100
	0    -1   -1   0   
$EndComp
Wire Wire Line
	7350 3850 7350 3950
$Comp
L power:GND #PWR?
U 1 1 5B6B6D77
P 7000 4500
F 0 "#PWR?" H 7000 4250 50  0001 C CNN
F 1 "GND" H 7150 4400 50  0000 C CNN
F 2 "" H 7000 4500 50  0001 C CNN
F 3 "" H 7000 4500 50  0001 C CNN
	1    7000 4500
	1    0    0    -1  
$EndComp
$Comp
L Device:R R?
U 1 1 5B6B6D7D
P 7000 3850
F 0 "R?" H 7070 3896 50  0000 L CNN
F 1 "1M" H 7070 3805 50  0000 L CNN
F 2 "" V 6930 3850 50  0001 C CNN
F 3 "~" H 7000 3850 50  0001 C CNN
	1    7000 3850
	1    0    0    -1  
$EndComp
Wire Wire Line
	7000 3600 7000 3650
Wire Wire Line
	7050 3650 7000 3650
Connection ~ 7000 3650
Wire Wire Line
	7000 3650 7000 3700
Wire Wire Line
	7000 4000 7000 4050
Wire Wire Line
	7000 4450 7000 4500
Wire Wire Line
	6700 4250 6650 4250
Wire Wire Line
	7000 3300 7000 3250
Wire Wire Line
	7350 3250 7350 3450
$Comp
L Device:R R?
U 1 1 5B6B6D8E
P 6650 4050
F 0 "R?" V 6550 4050 50  0000 C CNN
F 1 "10k" V 6750 4050 50  0000 C CNN
F 2 "" V 6580 4050 50  0001 C CNN
F 3 "~" H 6650 4050 50  0001 C CNN
	1    6650 4050
	-1   0    0    1   
$EndComp
Wire Wire Line
	6650 4200 6650 4250
$Comp
L Transistor_BJT:MPSA42 Q?
U 1 1 5B6B72A0
P 8150 3650
F 0 "Q?" H 8341 3696 50  0000 L CNN
F 1 "MPSA42" V 8350 3350 50  0000 L CNN
F 2 "Package_TO_SOT_THT:TO-92_Inline" H 8350 3575 50  0001 L CIN
F 3 "http://www.onsemi.com/pub_link/Collateral/MPSA42-D.PDF" H 8150 3650 50  0001 L CNN
	1    8150 3650
	1    0    0    -1  
$EndComp
$Comp
L Transistor_BJT:MPSA92 Q?
U 1 1 5B6B72A7
P 7800 4250
F 0 "Q?" H 7991 4296 50  0000 L CNN
F 1 "MPSA92" V 7750 4350 50  0000 L CNN
F 2 "Package_TO_SOT_THT:TO-92_Inline" H 8000 4175 50  0001 L CIN
F 3 "http://www.onsemi.com/pub_link/Collateral/MPSA92-D.PDF" H 7800 4250 50  0001 L CNN
	1    7800 4250
	1    0    0    1   
$EndComp
$Comp
L Device:R R?
U 1 1 5B6B72AE
P 7900 3450
F 0 "R?" H 7970 3496 50  0000 L CNN
F 1 "10k" H 7970 3405 50  0000 L CNN
F 2 "" V 7830 3450 50  0001 C CNN
F 3 "~" H 7900 3450 50  0001 C CNN
	1    7900 3450
	1    0    0    -1  
$EndComp
$Comp
L Device:D D?
U 1 1 5B6B72B5
P 8250 4100
F 0 "D?" V 8350 4250 50  0000 R CNN
F 1 "FR105" H 8300 4200 50  0000 R CNN
F 2 "" H 8250 4100 50  0001 C CNN
F 3 "~" H 8250 4100 50  0001 C CNN
	1    8250 4100
	0    -1   -1   0   
$EndComp
Wire Wire Line
	8250 3850 8250 3950
$Comp
L power:GND #PWR?
U 1 1 5B6B72BD
P 7900 4500
F 0 "#PWR?" H 7900 4250 50  0001 C CNN
F 1 "GND" H 8050 4400 50  0000 C CNN
F 2 "" H 7900 4500 50  0001 C CNN
F 3 "" H 7900 4500 50  0001 C CNN
	1    7900 4500
	1    0    0    -1  
$EndComp
$Comp
L Device:R R?
U 1 1 5B6B72C3
P 7900 3850
F 0 "R?" H 7970 3896 50  0000 L CNN
F 1 "1M" H 7970 3805 50  0000 L CNN
F 2 "" V 7830 3850 50  0001 C CNN
F 3 "~" H 7900 3850 50  0001 C CNN
	1    7900 3850
	1    0    0    -1  
$EndComp
Wire Wire Line
	7900 3600 7900 3650
Wire Wire Line
	7950 3650 7900 3650
Connection ~ 7900 3650
Wire Wire Line
	7900 3650 7900 3700
Wire Wire Line
	7900 4000 7900 4050
Wire Wire Line
	7900 4450 7900 4500
Wire Wire Line
	7600 4250 7550 4250
Wire Wire Line
	7900 3300 7900 3250
Wire Wire Line
	8250 3250 8250 3450
$Comp
L Device:R R?
U 1 1 5B6B72D4
P 7550 4050
F 0 "R?" V 7450 4050 50  0000 C CNN
F 1 "10k" V 7650 4050 50  0000 C CNN
F 2 "" V 7480 4050 50  0001 C CNN
F 3 "~" H 7550 4050 50  0001 C CNN
	1    7550 4050
	-1   0    0    1   
$EndComp
Wire Wire Line
	7550 4200 7550 4250
$Comp
L Transistor_BJT:MPSA42 Q?
U 1 1 5B6B785A
P 9050 3650
F 0 "Q?" H 9241 3696 50  0000 L CNN
F 1 "MPSA42" V 9250 3350 50  0000 L CNN
F 2 "Package_TO_SOT_THT:TO-92_Inline" H 9250 3575 50  0001 L CIN
F 3 "http://www.onsemi.com/pub_link/Collateral/MPSA42-D.PDF" H 9050 3650 50  0001 L CNN
	1    9050 3650
	1    0    0    -1  
$EndComp
$Comp
L Transistor_BJT:MPSA92 Q?
U 1 1 5B6B7861
P 8700 4250
F 0 "Q?" H 8891 4296 50  0000 L CNN
F 1 "MPSA92" V 8650 4350 50  0000 L CNN
F 2 "Package_TO_SOT_THT:TO-92_Inline" H 8900 4175 50  0001 L CIN
F 3 "http://www.onsemi.com/pub_link/Collateral/MPSA92-D.PDF" H 8700 4250 50  0001 L CNN
	1    8700 4250
	1    0    0    1   
$EndComp
$Comp
L Device:R R?
U 1 1 5B6B7868
P 8800 3450
F 0 "R?" H 8870 3496 50  0000 L CNN
F 1 "10k" H 8870 3405 50  0000 L CNN
F 2 "" V 8730 3450 50  0001 C CNN
F 3 "~" H 8800 3450 50  0001 C CNN
	1    8800 3450
	1    0    0    -1  
$EndComp
$Comp
L Device:D D?
U 1 1 5B6B786F
P 9150 4100
F 0 "D?" V 9250 4250 50  0000 R CNN
F 1 "FR105" H 9200 4200 50  0000 R CNN
F 2 "" H 9150 4100 50  0001 C CNN
F 3 "~" H 9150 4100 50  0001 C CNN
	1    9150 4100
	0    -1   -1   0   
$EndComp
Wire Wire Line
	9150 3850 9150 3950
$Comp
L power:GND #PWR?
U 1 1 5B6B7877
P 8800 4500
F 0 "#PWR?" H 8800 4250 50  0001 C CNN
F 1 "GND" H 8950 4400 50  0000 C CNN
F 2 "" H 8800 4500 50  0001 C CNN
F 3 "" H 8800 4500 50  0001 C CNN
	1    8800 4500
	1    0    0    -1  
$EndComp
$Comp
L Device:R R?
U 1 1 5B6B787D
P 8800 3850
F 0 "R?" H 8870 3896 50  0000 L CNN
F 1 "1M" H 8870 3805 50  0000 L CNN
F 2 "" V 8730 3850 50  0001 C CNN
F 3 "~" H 8800 3850 50  0001 C CNN
	1    8800 3850
	1    0    0    -1  
$EndComp
Wire Wire Line
	8800 3600 8800 3650
Wire Wire Line
	8850 3650 8800 3650
Connection ~ 8800 3650
Wire Wire Line
	8800 3650 8800 3700
Wire Wire Line
	8800 4000 8800 4050
Wire Wire Line
	8800 4450 8800 4500
Wire Wire Line
	8500 4250 8450 4250
Wire Wire Line
	8800 3300 8800 3250
Wire Wire Line
	9150 3250 9150 3450
$Comp
L Device:R R?
U 1 1 5B6B788E
P 8450 4050
F 0 "R?" V 8350 4050 50  0000 C CNN
F 1 "10k" V 8550 4050 50  0000 C CNN
F 2 "" V 8380 4050 50  0001 C CNN
F 3 "~" H 8450 4050 50  0001 C CNN
	1    8450 4050
	-1   0    0    1   
$EndComp
Wire Wire Line
	8450 4200 8450 4250
$Comp
L Transistor_BJT:MPSA42 Q?
U 1 1 5B6B80CD
P 9950 3650
F 0 "Q?" H 10141 3696 50  0000 L CNN
F 1 "MPSA42" V 10150 3350 50  0000 L CNN
F 2 "Package_TO_SOT_THT:TO-92_Inline" H 10150 3575 50  0001 L CIN
F 3 "http://www.onsemi.com/pub_link/Collateral/MPSA42-D.PDF" H 9950 3650 50  0001 L CNN
	1    9950 3650
	1    0    0    -1  
$EndComp
$Comp
L Transistor_BJT:MPSA92 Q?
U 1 1 5B6B80D4
P 9600 4250
F 0 "Q?" H 9791 4296 50  0000 L CNN
F 1 "MPSA92" V 9550 4350 50  0000 L CNN
F 2 "Package_TO_SOT_THT:TO-92_Inline" H 9800 4175 50  0001 L CIN
F 3 "http://www.onsemi.com/pub_link/Collateral/MPSA92-D.PDF" H 9600 4250 50  0001 L CNN
	1    9600 4250
	1    0    0    1   
$EndComp
$Comp
L Device:R R?
U 1 1 5B6B80DB
P 9700 3450
F 0 "R?" H 9770 3496 50  0000 L CNN
F 1 "10k" H 9770 3405 50  0000 L CNN
F 2 "" V 9630 3450 50  0001 C CNN
F 3 "~" H 9700 3450 50  0001 C CNN
	1    9700 3450
	1    0    0    -1  
$EndComp
$Comp
L Device:D D?
U 1 1 5B6B80E2
P 10050 4100
F 0 "D?" V 10150 4250 50  0000 R CNN
F 1 "FR105" H 10100 4200 50  0000 R CNN
F 2 "" H 10050 4100 50  0001 C CNN
F 3 "~" H 10050 4100 50  0001 C CNN
	1    10050 4100
	0    -1   -1   0   
$EndComp
Wire Wire Line
	10050 3850 10050 3950
$Comp
L power:GND #PWR?
U 1 1 5B6B80EA
P 9700 4500
F 0 "#PWR?" H 9700 4250 50  0001 C CNN
F 1 "GND" H 9850 4400 50  0000 C CNN
F 2 "" H 9700 4500 50  0001 C CNN
F 3 "" H 9700 4500 50  0001 C CNN
	1    9700 4500
	1    0    0    -1  
$EndComp
$Comp
L Device:R R?
U 1 1 5B6B80F0
P 9700 3850
F 0 "R?" H 9770 3896 50  0000 L CNN
F 1 "1M" H 9770 3805 50  0000 L CNN
F 2 "" V 9630 3850 50  0001 C CNN
F 3 "~" H 9700 3850 50  0001 C CNN
	1    9700 3850
	1    0    0    -1  
$EndComp
Wire Wire Line
	9700 3600 9700 3650
Wire Wire Line
	9750 3650 9700 3650
Connection ~ 9700 3650
Wire Wire Line
	9700 3650 9700 3700
Wire Wire Line
	9700 4000 9700 4050
Wire Wire Line
	9700 4450 9700 4500
Wire Wire Line
	9400 4250 9350 4250
Wire Wire Line
	9700 3300 9700 3250
Wire Wire Line
	10050 3250 10050 3450
$Comp
L Device:R R?
U 1 1 5B6B8101
P 9350 4050
F 0 "R?" V 9250 4050 50  0000 C CNN
F 1 "10k" V 9450 4050 50  0000 C CNN
F 2 "" V 9280 4050 50  0001 C CNN
F 3 "~" H 9350 4050 50  0001 C CNN
	1    9350 4050
	-1   0    0    1   
$EndComp
Wire Wire Line
	9350 4200 9350 4250
$Comp
L Transistor_BJT:MPSA42 Q?
U 1 1 5B6B8D97
P 10850 3650
F 0 "Q?" H 11041 3696 50  0000 L CNN
F 1 "MPSA42" V 11050 3350 50  0000 L CNN
F 2 "Package_TO_SOT_THT:TO-92_Inline" H 11050 3575 50  0001 L CIN
F 3 "http://www.onsemi.com/pub_link/Collateral/MPSA42-D.PDF" H 10850 3650 50  0001 L CNN
	1    10850 3650
	1    0    0    -1  
$EndComp
$Comp
L Transistor_BJT:MPSA92 Q?
U 1 1 5B6B8D9E
P 10500 4250
F 0 "Q?" H 10691 4296 50  0000 L CNN
F 1 "MPSA92" V 10450 4350 50  0000 L CNN
F 2 "Package_TO_SOT_THT:TO-92_Inline" H 10700 4175 50  0001 L CIN
F 3 "http://www.onsemi.com/pub_link/Collateral/MPSA92-D.PDF" H 10500 4250 50  0001 L CNN
	1    10500 4250
	1    0    0    1   
$EndComp
$Comp
L Device:R R?
U 1 1 5B6B8DA5
P 10600 3450
F 0 "R?" H 10670 3496 50  0000 L CNN
F 1 "10k" H 10670 3405 50  0000 L CNN
F 2 "" V 10530 3450 50  0001 C CNN
F 3 "~" H 10600 3450 50  0001 C CNN
	1    10600 3450
	1    0    0    -1  
$EndComp
$Comp
L Device:D D?
U 1 1 5B6B8DAC
P 10950 4100
F 0 "D?" V 11050 4250 50  0000 R CNN
F 1 "FR105" H 11000 4200 50  0000 R CNN
F 2 "" H 10950 4100 50  0001 C CNN
F 3 "~" H 10950 4100 50  0001 C CNN
	1    10950 4100
	0    -1   -1   0   
$EndComp
Wire Wire Line
	10950 3850 10950 3950
$Comp
L power:GND #PWR?
U 1 1 5B6B8DB4
P 10600 4500
F 0 "#PWR?" H 10600 4250 50  0001 C CNN
F 1 "GND" H 10750 4400 50  0000 C CNN
F 2 "" H 10600 4500 50  0001 C CNN
F 3 "" H 10600 4500 50  0001 C CNN
	1    10600 4500
	1    0    0    -1  
$EndComp
$Comp
L Device:R R?
U 1 1 5B6B8DBA
P 10600 3850
F 0 "R?" H 10670 3896 50  0000 L CNN
F 1 "1M" H 10670 3805 50  0000 L CNN
F 2 "" V 10530 3850 50  0001 C CNN
F 3 "~" H 10600 3850 50  0001 C CNN
	1    10600 3850
	1    0    0    -1  
$EndComp
Wire Wire Line
	10600 3600 10600 3650
Wire Wire Line
	10650 3650 10600 3650
Connection ~ 10600 3650
Wire Wire Line
	10600 3650 10600 3700
Wire Wire Line
	10600 4000 10600 4050
Wire Wire Line
	10600 4450 10600 4500
Wire Wire Line
	10300 4250 10250 4250
Wire Wire Line
	10600 3300 10600 3250
Wire Wire Line
	10950 3250 10950 3450
$Comp
L Device:R R?
U 1 1 5B6B8DCB
P 10250 4050
F 0 "R?" V 10150 4050 50  0000 C CNN
F 1 "10k" V 10350 4050 50  0000 C CNN
F 2 "" V 10180 4050 50  0001 C CNN
F 3 "~" H 10250 4050 50  0001 C CNN
	1    10250 4050
	-1   0    0    1   
$EndComp
Wire Wire Line
	10250 4200 10250 4250
$Comp
L power:+5V #PWR?
U 1 1 5B6BFDC7
P 2850 1250
F 0 "#PWR?" H 2850 1100 50  0001 C CNN
F 1 "+5V" H 2865 1423 50  0000 C CNN
F 2 "" H 2850 1250 50  0001 C CNN
F 3 "" H 2850 1250 50  0001 C CNN
	1    2850 1250
	1    0    0    -1  
$EndComp
Wire Wire Line
	2850 1250 2850 1350
Wire Wire Line
	2150 1350 2250 1350
Wire Wire Line
	2850 3300 2850 3250
Connection ~ 2850 3250
Wire Wire Line
	2850 3250 2850 3200
$Comp
L Device:Crystal Y?
U 1 1 5B6CF964
P 1900 3400
F 0 "Y?" V 2000 3500 50  0000 C CNN
F 1 "32768Hz" V 1650 3500 50  0000 C CNN
F 2 "" H 1900 3400 50  0001 C CNN
F 3 "~" H 1900 3400 50  0001 C CNN
	1    1900 3400
	0    -1   -1   0   
$EndComp
$Comp
L Device:C C?
U 1 1 5B6D6438
P 2150 3550
F 0 "C?" V 2250 3700 50  0000 L CNN
F 1 "22pF" V 2100 3700 50  0000 L CNN
F 2 "" H 2188 3400 50  0001 C CNN
F 3 "~" H 2150 3550 50  0001 C CNN
	1    2150 3550
	0    1    1    0   
$EndComp
$Comp
L Device:C C?
U 1 1 5B6D810E
P 2150 3250
F 0 "C?" V 2050 3350 50  0000 L CNN
F 1 "22pF" V 2050 3000 50  0000 L CNN
F 2 "" H 2188 3100 50  0001 C CNN
F 3 "~" H 2150 3250 50  0001 C CNN
	1    2150 3250
	0    1    1    0   
$EndComp
Wire Wire Line
	2000 3250 1900 3250
Wire Wire Line
	2000 3550 1900 3550
$Comp
L russian-nixies:IN-17 N?
U 1 1 5B6B64B4
P 6050 5600
F 0 "N?" H 6100 6300 50  0000 C CNN
F 1 "IN-17" H 6100 6449 50  0000 C CNN
F 2 "russian-nixies-IN-17" H 6050 5750 50  0001 C CNN
F 3 "" H 6050 5600 50  0001 C CNN
	1    6050 5600
	1    0    0    -1  
$EndComp
Wire Wire Line
	8250 4250 8250 5600
Wire Wire Line
	10050 4250 10050 5600
Wire Wire Line
	10950 4250 10950 5600
Wire Wire Line
	6450 4250 6450 5600
Wire Wire Line
	7350 4250 7350 5600
Entry Wire Line
	5700 5100 5600 5200
Entry Wire Line
	5700 5200 5600 5300
Entry Wire Line
	5700 5300 5600 5400
Entry Wire Line
	5700 5400 5600 5500
Entry Wire Line
	5700 5500 5600 5600
Entry Wire Line
	5700 5600 5600 5700
Entry Wire Line
	5700 5700 5600 5800
Entry Wire Line
	5700 5800 5600 5900
Entry Wire Line
	5700 5900 5600 6000
Entry Wire Line
	5700 6000 5600 6100
Wire Wire Line
	5700 5100 5750 5100
Wire Wire Line
	5750 5200 5700 5200
Wire Wire Line
	5750 5300 5700 5300
Wire Wire Line
	5750 5400 5700 5400
Wire Wire Line
	5750 5500 5700 5500
Wire Wire Line
	5750 5600 5700 5600
Wire Wire Line
	5750 5700 5700 5700
Wire Wire Line
	5750 5800 5700 5800
Wire Wire Line
	5750 5900 5700 5900
Wire Wire Line
	5750 6000 5700 6000
Wire Wire Line
	9150 4250 9150 5600
$Comp
L russian-nixies:K155ID1 U?
U 1 1 5B6B9CCF
P 2650 6250
F 0 "U?" H 2650 6847 60  0000 C CNN
F 1 "K155ID1" H 2650 6741 60  0000 C CNN
F 2 "" H 2650 6250 60  0000 C CNN
F 3 "" H 2650 6250 60  0000 C CNN
	1    2650 6250
	1    0    0    -1  
$EndComp
Entry Wire Line
	3350 5900 3450 6000
Entry Wire Line
	3350 6000 3450 6100
Entry Wire Line
	3350 6100 3450 6200
Entry Wire Line
	3350 6200 3450 6300
Entry Wire Line
	3350 6400 3450 6500
Entry Wire Line
	3350 6500 3450 6600
Entry Wire Line
	3350 6600 3450 6700
Wire Wire Line
	2300 3250 2850 3250
Connection ~ 2300 3250
Wire Wire Line
	1900 3250 1700 3250
Wire Wire Line
	1700 3250 1700 2800
Wire Wire Line
	1700 2800 1750 2800
Connection ~ 1900 3250
Wire Wire Line
	1750 2700 1600 2700
Wire Wire Line
	1600 2700 1600 3550
Wire Wire Line
	1600 3550 1900 3550
Connection ~ 1900 3550
Wire Wire Line
	2000 6300 1300 6300
Wire Wire Line
	3400 6300 3400 5600
$Comp
L Device:C C?
U 1 1 5B752056
P 2400 1650
F 0 "C?" V 2300 1350 50  0000 L CNN
F 1 "100nF" V 2250 1550 50  0000 L CNN
F 2 "" H 2438 1500 50  0001 C CNN
F 3 "~" H 2400 1650 50  0001 C CNN
	1    2400 1650
	0    1    1    0   
$EndComp
Wire Wire Line
	1500 1350 1500 1650
Wire Wire Line
	2300 3250 2300 3550
Connection ~ 2300 3550
$Comp
L Device:C C?
U 1 1 5B765457
P 1300 7100
F 0 "C?" H 1415 7146 50  0000 L CNN
F 1 "100nF" H 1415 7055 50  0000 L CNN
F 2 "" H 1338 6950 50  0001 C CNN
F 3 "~" H 1300 7100 50  0001 C CNN
	1    1300 7100
	1    0    0    -1  
$EndComp
$Comp
L power:GND #PWR?
U 1 1 5B765547
P 1300 7350
F 0 "#PWR?" H 1300 7100 50  0001 C CNN
F 1 "GND" H 1305 7177 50  0000 C CNN
F 2 "" H 1300 7350 50  0001 C CNN
F 3 "" H 1300 7350 50  0001 C CNN
	1    1300 7350
	1    0    0    -1  
$EndComp
Wire Wire Line
	1300 7250 1300 7350
Entry Wire Line
	9250 3100 9350 3200
Entry Wire Line
	10150 3100 10250 3200
Entry Wire Line
	8350 3100 8450 3200
Entry Wire Line
	7450 3100 7550 3200
Entry Wire Line
	6550 3100 6650 3200
Entry Wire Line
	5650 3100 5750 3200
Entry Wire Line
	3650 4950 3750 4850
Entry Wire Line
	3650 4850 3750 4750
Entry Wire Line
	3650 4750 3750 4650
Entry Wire Line
	3650 4650 3750 4550
Entry Wire Line
	3650 4550 3750 4450
Entry Wire Line
	3650 4450 3750 4350
Wire Wire Line
	3600 4450 3650 4450
Wire Wire Line
	3600 4550 3650 4550
Wire Wire Line
	3650 4650 3600 4650
Wire Wire Line
	3600 4750 3650 4750
Wire Wire Line
	3650 4850 3600 4850
Wire Wire Line
	3600 4950 3650 4950
$Comp
L Device:C C?
U 1 1 5B8922A2
P 2600 5450
F 0 "C?" V 2550 5600 50  0000 C CNN
F 1 "100nF" V 2439 5450 50  0000 C CNN
F 2 "" H 2638 5300 50  0001 C CNN
F 3 "~" H 2600 5450 50  0001 C CNN
	1    2600 5450
	0    1    1    0   
$EndComp
Entry Wire Line
	3350 6700 3450 6800
Entry Wire Line
	3350 6800 3450 6900
Entry Wire Line
	3350 6900 3450 7000
Wire Wire Line
	2000 6600 1950 6600
Wire Wire Line
	1950 6600 1950 6700
Wire Wire Line
	2000 6000 1750 6000
Wire Wire Line
	1750 6000 1750 6800
Wire Wire Line
	3300 5900 3350 5900
Wire Wire Line
	3300 6000 3350 6000
Wire Wire Line
	3300 6100 3350 6100
Wire Wire Line
	3300 6200 3350 6200
Wire Wire Line
	3300 6300 3400 6300
Wire Wire Line
	3350 6400 3300 6400
Wire Wire Line
	3350 6500 3300 6500
Wire Wire Line
	3350 6600 3300 6600
Wire Wire Line
	1950 6700 3350 6700
Wire Wire Line
	1750 6800 3350 6800
$Comp
L 74xx:74LS138 U?
U 1 1 5B9D1B3E
P 3100 4750
F 0 "U?" H 2650 5200 50  0000 C CNN
F 1 "74LS138" H 2850 5300 50  0000 C CNN
F 2 "" H 3100 4750 50  0001 C CNN
F 3 "http://www.ti.com/lit/gpn/sn74LS138" H 3100 4750 50  0001 C CNN
	1    3100 4750
	1    0    0    -1  
$EndComp
Wire Wire Line
	3100 5450 3100 5600
Connection ~ 3100 5600
Wire Wire Line
	3100 5600 3400 5600
Wire Wire Line
	2750 5450 3100 5450
Connection ~ 3100 5450
Wire Wire Line
	6100 3250 6450 3250
Connection ~ 6450 3250
Connection ~ 7000 3250
Wire Wire Line
	7000 3250 7350 3250
Connection ~ 7350 3250
Connection ~ 7900 3250
Wire Wire Line
	7900 3250 8250 3250
Connection ~ 8250 3250
Connection ~ 8800 3250
Wire Wire Line
	8800 3250 9150 3250
Connection ~ 9150 3250
Wire Wire Line
	9700 3250 10050 3250
Connection ~ 10050 3250
Connection ~ 10600 3250
Wire Wire Line
	10600 3250 10950 3250
Connection ~ 9700 3250
Entry Bus Bus
	5500 6400 5600 6300
Entry Bus Bus
	6400 6400 6500 6300
Entry Bus Bus
	7300 6400 7400 6300
Entry Bus Bus
	8200 6400 8300 6300
Entry Bus Bus
	9100 6400 9200 6300
Entry Bus Bus
	10000 6400 10100 6300
$Comp
L russian-nixies:IN-17 N?
U 1 1 5B769E3C
P 6950 5600
F 0 "N?" H 7000 6300 50  0000 C CNN
F 1 "IN-17" H 7000 6449 50  0000 C CNN
F 2 "russian-nixies-IN-17" H 6950 5750 50  0001 C CNN
F 3 "" H 6950 5600 50  0001 C CNN
	1    6950 5600
	1    0    0    -1  
$EndComp
Entry Wire Line
	6600 5100 6500 5200
Entry Wire Line
	6600 5200 6500 5300
Entry Wire Line
	6600 5300 6500 5400
Entry Wire Line
	6600 5400 6500 5500
Entry Wire Line
	6600 5500 6500 5600
Entry Wire Line
	6600 5600 6500 5700
Entry Wire Line
	6600 5700 6500 5800
Entry Wire Line
	6600 5800 6500 5900
Entry Wire Line
	6600 5900 6500 6000
Entry Wire Line
	6600 6000 6500 6100
Wire Wire Line
	6600 5100 6650 5100
Wire Wire Line
	6650 5200 6600 5200
Wire Wire Line
	6650 5300 6600 5300
Wire Wire Line
	6650 5400 6600 5400
Wire Wire Line
	6650 5500 6600 5500
Wire Wire Line
	6650 5600 6600 5600
Wire Wire Line
	6650 5700 6600 5700
Wire Wire Line
	6650 5800 6600 5800
Wire Wire Line
	6650 5900 6600 5900
Wire Wire Line
	6650 6000 6600 6000
$Comp
L russian-nixies:IN-17 N?
U 1 1 5B7746DE
P 7850 5600
F 0 "N?" H 7900 6300 50  0000 C CNN
F 1 "IN-17" H 7900 6449 50  0000 C CNN
F 2 "russian-nixies-IN-17" H 7850 5750 50  0001 C CNN
F 3 "" H 7850 5600 50  0001 C CNN
	1    7850 5600
	1    0    0    -1  
$EndComp
Entry Wire Line
	7500 5100 7400 5200
Entry Wire Line
	7500 5200 7400 5300
Entry Wire Line
	7500 5300 7400 5400
Entry Wire Line
	7500 5400 7400 5500
Entry Wire Line
	7500 5500 7400 5600
Entry Wire Line
	7500 5600 7400 5700
Entry Wire Line
	7500 5700 7400 5800
Entry Wire Line
	7500 5800 7400 5900
Entry Wire Line
	7500 5900 7400 6000
Entry Wire Line
	7500 6000 7400 6100
Wire Wire Line
	7500 5100 7550 5100
Wire Wire Line
	7550 5200 7500 5200
Wire Wire Line
	7550 5300 7500 5300
Wire Wire Line
	7550 5400 7500 5400
Wire Wire Line
	7550 5500 7500 5500
Wire Wire Line
	7550 5600 7500 5600
Wire Wire Line
	7550 5700 7500 5700
Wire Wire Line
	7550 5800 7500 5800
Wire Wire Line
	7550 5900 7500 5900
Wire Wire Line
	7550 6000 7500 6000
$Comp
L russian-nixies:IN-17 N?
U 1 1 5B77FEA8
P 8750 5600
F 0 "N?" H 8800 6300 50  0000 C CNN
F 1 "IN-17" H 8800 6449 50  0000 C CNN
F 2 "russian-nixies-IN-17" H 8750 5750 50  0001 C CNN
F 3 "" H 8750 5600 50  0001 C CNN
	1    8750 5600
	1    0    0    -1  
$EndComp
Entry Wire Line
	8400 5100 8300 5200
Entry Wire Line
	8400 5200 8300 5300
Entry Wire Line
	8400 5300 8300 5400
Entry Wire Line
	8400 5400 8300 5500
Entry Wire Line
	8400 5500 8300 5600
Entry Wire Line
	8400 5600 8300 5700
Entry Wire Line
	8400 5700 8300 5800
Entry Wire Line
	8400 5800 8300 5900
Entry Wire Line
	8400 5900 8300 6000
Entry Wire Line
	8400 6000 8300 6100
Wire Wire Line
	8400 5100 8450 5100
Wire Wire Line
	8450 5200 8400 5200
Wire Wire Line
	8450 5300 8400 5300
Wire Wire Line
	8450 5400 8400 5400
Wire Wire Line
	8450 5500 8400 5500
Wire Wire Line
	8450 5600 8400 5600
Wire Wire Line
	8450 5700 8400 5700
Wire Wire Line
	8450 5800 8400 5800
Wire Wire Line
	8450 5900 8400 5900
Wire Wire Line
	8450 6000 8400 6000
$Comp
L russian-nixies:IN-17 N?
U 1 1 5B78C632
P 9650 5600
F 0 "N?" H 9700 6300 50  0000 C CNN
F 1 "IN-17" H 9700 6449 50  0000 C CNN
F 2 "russian-nixies-IN-17" H 9650 5750 50  0001 C CNN
F 3 "" H 9650 5600 50  0001 C CNN
	1    9650 5600
	1    0    0    -1  
$EndComp
Entry Wire Line
	9300 5100 9200 5200
Entry Wire Line
	9300 5200 9200 5300
Entry Wire Line
	9300 5300 9200 5400
Entry Wire Line
	9300 5400 9200 5500
Entry Wire Line
	9300 5500 9200 5600
Entry Wire Line
	9300 5600 9200 5700
Entry Wire Line
	9300 5700 9200 5800
Entry Wire Line
	9300 5800 9200 5900
Entry Wire Line
	9300 5900 9200 6000
Entry Wire Line
	9300 6000 9200 6100
Wire Wire Line
	9300 5100 9350 5100
Wire Wire Line
	9350 5200 9300 5200
Wire Wire Line
	9350 5300 9300 5300
Wire Wire Line
	9350 5400 9300 5400
Wire Wire Line
	9350 5500 9300 5500
Wire Wire Line
	9350 5600 9300 5600
Wire Wire Line
	9350 5700 9300 5700
Wire Wire Line
	9350 5800 9300 5800
Wire Wire Line
	9350 5900 9300 5900
Wire Wire Line
	9350 6000 9300 6000
$Comp
L russian-nixies:IN-17 N?
U 1 1 5B799E1E
P 10550 5600
F 0 "N?" H 10600 6300 50  0000 C CNN
F 1 "IN-17" H 10600 6449 50  0000 C CNN
F 2 "russian-nixies-IN-17" H 10550 5750 50  0001 C CNN
F 3 "" H 10550 5600 50  0001 C CNN
	1    10550 5600
	1    0    0    -1  
$EndComp
Entry Wire Line
	10200 5100 10100 5200
Entry Wire Line
	10200 5200 10100 5300
Entry Wire Line
	10200 5300 10100 5400
Entry Wire Line
	10200 5400 10100 5500
Entry Wire Line
	10200 5500 10100 5600
Entry Wire Line
	10200 5600 10100 5700
Entry Wire Line
	10200 5700 10100 5800
Entry Wire Line
	10200 5800 10100 5900
Entry Wire Line
	10200 5900 10100 6000
Entry Wire Line
	10200 6000 10100 6100
Wire Wire Line
	10200 5100 10250 5100
Wire Wire Line
	10250 5200 10200 5200
Wire Wire Line
	10250 5300 10200 5300
Wire Wire Line
	10250 5400 10200 5400
Wire Wire Line
	10250 5500 10200 5500
Wire Wire Line
	10250 5600 10200 5600
Wire Wire Line
	10250 5700 10200 5700
Wire Wire Line
	10250 5800 10200 5800
Wire Wire Line
	10250 5900 10200 5900
Wire Wire Line
	10250 6000 10200 6000
Wire Wire Line
	5750 3200 5750 3900
Wire Wire Line
	6650 3200 6650 3250
Wire Wire Line
	6650 3250 6650 3900
Wire Wire Line
	6450 3250 7000 3250
Wire Wire Line
	8250 3250 8800 3250
Wire Wire Line
	7550 3200 7550 3250
Wire Wire Line
	7550 3250 7550 3900
Wire Wire Line
	7350 3250 7900 3250
Wire Wire Line
	8450 3900 8450 3200
Wire Wire Line
	10250 3200 10250 3250
Wire Wire Line
	10250 3250 10250 3900
Wire Wire Line
	10050 3250 10600 3250
Wire Wire Line
	9350 3250 9350 3900
Wire Wire Line
	9350 3200 9350 3250
Wire Wire Line
	9150 3250 9700 3250
$Comp
L Device:R R?
U 1 1 5B7FE7E5
P 2400 4950
F 0 "R?" V 2300 4950 50  0000 C CNN
F 1 "1K" V 2400 4950 50  0000 C CNN
F 2 "" V 2330 4950 50  0001 C CNN
F 3 "~" H 2400 4950 50  0001 C CNN
	1    2400 4950
	0    1    1    0   
$EndComp
Wire Wire Line
	1300 5450 2100 5450
$Comp
L Device:R R?
U 1 1 5B83C3B5
P 2400 5050
F 0 "R?" V 2350 5250 50  0000 C CNN
F 1 "1K" V 2400 5050 50  0000 C CNN
F 2 "" V 2330 5050 50  0001 C CNN
F 3 "~" H 2400 5050 50  0001 C CNN
	1    2400 5050
	0    1    1    0   
$EndComp
$Comp
L Device:R R?
U 1 1 5B84B8A0
P 2400 5150
F 0 "R?" V 2500 5150 50  0000 C CNN
F 1 "1K" V 2400 5150 50  0000 C CNN
F 2 "" V 2330 5150 50  0001 C CNN
F 3 "~" H 2400 5150 50  0001 C CNN
	1    2400 5150
	0    1    1    0   
$EndComp
Wire Wire Line
	2250 5050 2200 5050
Wire Wire Line
	2200 5050 2200 5150
Wire Wire Line
	2200 5150 2250 5150
Wire Wire Line
	2200 5150 2200 5600
Connection ~ 2200 5150
Wire Wire Line
	2200 5600 3100 5600
Wire Wire Line
	2250 4950 2100 4950
Wire Wire Line
	2100 4950 2100 5450
Connection ~ 2100 5450
Wire Wire Line
	2100 5450 2450 5450
Wire Wire Line
	2600 4950 2550 4950
Wire Wire Line
	2600 5150 2550 5150
Wire Wire Line
	2550 5050 2600 5050
Wire Wire Line
	1750 2600 1400 2600
Wire Wire Line
	1750 2500 1300 2500
$Comp
L Device:Lamp_Neon NE?
U 1 1 5B917340
P 4300 5750
F 0 "NE?" H 4428 5796 50  0000 L CNN
F 1 "Lamp_Neon" V 4150 5500 50  0000 L CNN
F 2 "" V 4300 5850 50  0001 C CNN
F 3 "~" V 4300 5850 50  0001 C CNN
	1    4300 5750
	1    0    0    -1  
$EndComp
$Comp
L Device:Lamp_Neon NE?
U 1 1 5B9174ED
P 5150 5750
F 0 "NE?" H 5278 5796 50  0000 L CNN
F 1 "Lamp_Neon" V 5000 5500 50  0000 L CNN
F 2 "" V 5150 5850 50  0001 C CNN
F 3 "~" V 5150 5850 50  0001 C CNN
	1    5150 5750
	1    0    0    -1  
$EndComp
Wire Bus Line
	3450 7050 5350 7050
Wire Bus Line
	5350 7050 5350 6400
$Comp
L Transistor_BJT:MPSA42 Q?
U 1 1 5B9DA87F
P 4200 5250
F 0 "Q?" H 4391 5296 50  0000 L CNN
F 1 "MPSA42" V 4400 4950 50  0000 L CNN
F 2 "Package_TO_SOT_THT:TO-92_Inline" H 4400 5175 50  0001 L CIN
F 3 "http://www.onsemi.com/pub_link/Collateral/MPSA42-D.PDF" H 4200 5250 50  0001 L CNN
	1    4200 5250
	1    0    0    -1  
$EndComp
$Comp
L Transistor_BJT:MPSA42 Q?
U 1 1 5B9EC215
P 5050 5250
F 0 "Q?" H 5241 5296 50  0000 L CNN
F 1 "MPSA42" V 5250 4950 50  0000 L CNN
F 2 "Package_TO_SOT_THT:TO-92_Inline" H 5250 5175 50  0001 L CIN
F 3 "http://www.onsemi.com/pub_link/Collateral/MPSA42-D.PDF" H 5050 5250 50  0001 L CNN
	1    5050 5250
	1    0    0    -1  
$EndComp
$Comp
L Device:R R?
U 1 1 5BA20F7A
P 5150 4850
F 0 "R?" V 5050 4850 50  0000 C CNN
F 1 "470k" V 5250 4850 50  0000 C CNN
F 2 "" V 5080 4850 50  0001 C CNN
F 3 "~" H 5150 4850 50  0001 C CNN
	1    5150 4850
	-1   0    0    1   
$EndComp
$Comp
L Device:R R?
U 1 1 5BA3290E
P 4300 4850
F 0 "R?" V 4200 4850 50  0000 C CNN
F 1 "470k" V 4400 4850 50  0000 C CNN
F 2 "" V 4230 4850 50  0001 C CNN
F 3 "~" H 4300 4850 50  0001 C CNN
	1    4300 4850
	-1   0    0    1   
$EndComp
Wire Wire Line
	4300 5000 4300 5050
Wire Wire Line
	4300 5450 4300 5550
Wire Wire Line
	5150 5000 5150 5050
Wire Wire Line
	5150 5450 5150 5550
$Comp
L power:GND #PWR?
U 1 1 5BA8BE82
P 4300 6050
F 0 "#PWR?" H 4300 5800 50  0001 C CNN
F 1 "GND" H 4450 5950 50  0000 C CNN
F 2 "" H 4300 6050 50  0001 C CNN
F 3 "" H 4300 6050 50  0001 C CNN
	1    4300 6050
	1    0    0    -1  
$EndComp
$Comp
L power:GND #PWR?
U 1 1 5BA9E175
P 5150 6050
F 0 "#PWR?" H 5150 5800 50  0001 C CNN
F 1 "GND" H 5300 5950 50  0000 C CNN
F 2 "" H 5150 6050 50  0001 C CNN
F 3 "" H 5150 6050 50  0001 C CNN
	1    5150 6050
	1    0    0    -1  
$EndComp
Wire Wire Line
	4300 5950 4300 6050
Wire Wire Line
	5150 5950 5150 6050
Wire Wire Line
	4300 4700 4300 4600
Wire Wire Line
	4300 4600 5150 4600
Wire Wire Line
	5150 4600 5150 4700
Wire Wire Line
	5150 4600 5150 3250
Wire Wire Line
	5150 3250 6100 3250
Connection ~ 5150 4600
Connection ~ 6100 3250
$Comp
L Device:R R?
U 1 1 5BB0E5FB
P 3950 5050
F 0 "R?" V 3850 5050 50  0000 C CNN
F 1 "33k" V 4000 5300 50  0000 C CNN
F 2 "" V 3880 5050 50  0001 C CNN
F 3 "~" H 3950 5050 50  0001 C CNN
	1    3950 5050
	-1   0    0    1   
$EndComp
$Comp
L Device:R R?
U 1 1 5BB216A5
P 4800 5050
F 0 "R?" V 4700 5050 50  0000 C CNN
F 1 "33k" V 4900 5050 50  0000 C CNN
F 2 "" V 4730 5050 50  0001 C CNN
F 3 "~" H 4800 5050 50  0001 C CNN
	1    4800 5050
	-1   0    0    1   
$EndComp
Wire Wire Line
	3950 5200 3950 5250
Wire Wire Line
	3950 5250 4000 5250
Wire Wire Line
	4800 5200 4800 5250
Wire Wire Line
	4800 5250 4850 5250
Wire Wire Line
	4800 4900 4800 4550
Wire Wire Line
	4800 4550 3800 4550
Wire Wire Line
	3800 5150 3600 5150
Wire Wire Line
	3800 4550 3800 5150
Wire Wire Line
	3600 5050 3750 5050
Wire Wire Line
	3750 5050 3750 4900
Wire Wire Line
	3750 4900 3950 4900
Wire Wire Line
	2550 4450 2600 4450
Wire Wire Line
	1400 4550 2600 4550
Wire Wire Line
	1400 2600 1400 4550
Wire Wire Line
	1300 4650 2600 4650
Wire Wire Line
	1300 2500 1300 3800
Wire Wire Line
	2850 1650 2850 1800
Wire Wire Line
	1500 1350 1850 1350
Wire Wire Line
	2250 1650 1500 1650
Connection ~ 1500 1650
Wire Wire Line
	2550 1350 2600 1350
Wire Wire Line
	2600 1350 2600 1650
Wire Wire Line
	2600 1650 2550 1650
Wire Wire Line
	2600 1650 2850 1650
Connection ~ 2600 1650
Connection ~ 2850 1650
Connection ~ 2200 5050
Wire Wire Line
	2300 4450 2200 4450
Wire Wire Line
	2200 4450 2200 5050
Wire Wire Line
	1650 5900 1650 6900
Wire Wire Line
	1650 5900 2000 5900
Wire Wire Line
	1650 6900 3350 6900
Wire Wire Line
	1300 6300 1300 6950
Connection ~ 1300 6300
Wire Bus Line
	3750 3450 5050 3450
Wire Bus Line
	5050 3450 5050 3100
$Comp
L Device:R R?
U 1 1 5BEBBA93
P 4100 1500
F 0 "R?" H 4030 1454 50  0000 R CNN
F 1 "10k" H 4030 1545 50  0000 R CNN
F 2 "" V 4030 1500 50  0001 C CNN
F 3 "~" H 4100 1500 50  0001 C CNN
	1    4100 1500
	-1   0    0    1   
$EndComp
Connection ~ 2850 1350
$Comp
L pspice:INDUCTOR L?
U 1 1 5BF7B8BF
P 9600 2050
F 0 "L?" H 9600 2265 50  0000 C CNN
F 1 "100uH" H 9600 2174 50  0000 C CNN
F 2 "" H 9600 2050 50  0001 C CNN
F 3 "" H 9600 2050 50  0001 C CNN
	1    9600 2050
	1    0    0    -1  
$EndComp
$Comp
L Device:CP C?
U 1 1 5BF7B9DF
P 9250 2550
F 0 "C?" H 9368 2596 50  0000 L CNN
F 1 "50mkF 16V" V 9100 2400 50  0000 L CNN
F 2 "" H 9288 2400 50  0001 C CNN
F 3 "~" H 9250 2550 50  0001 C CNN
	1    9250 2550
	1    0    0    -1  
$EndComp
$Comp
L Device:D D?
U 1 1 5BF7BCC4
P 10100 2050
F 0 "D?" H 10150 1850 50  0000 R CNN
F 1 "FR105" H 10200 1950 50  0000 R CNN
F 2 "" H 10100 2050 50  0001 C CNN
F 3 "~" H 10100 2050 50  0001 C CNN
	1    10100 2050
	-1   0    0    1   
$EndComp
$Comp
L Transistor_FET:IRF6674 Q?
U 1 1 5BF94253
P 9800 2350
F 0 "Q?" H 9750 2500 50  0000 L CNN
F 1 "IRF820" V 10050 2250 50  0000 L CNN
F 2 "Package_DirectFET:DirectFET_MZ" H 9800 2350 50  0001 C CIN
F 3 "https://www.infineon.com/dgdl/irf6674pbf.pdf?fileId=5546d462533600a4015355ec9f0d1a66" H 9800 2350 50  0001 L CNN
	1    9800 2350
	1    0    0    -1  
$EndComp
$Comp
L Device:R R?
U 1 1 5BF943B4
P 9550 2550
F 0 "R?" H 9620 2596 50  0000 L CNN
F 1 "100" H 9620 2505 50  0000 L CNN
F 2 "" V 9480 2550 50  0001 C CNN
F 3 "~" H 9550 2550 50  0001 C CNN
	1    9550 2550
	1    0    0    -1  
$EndComp
$Comp
L Device:R_POT_TRIM RV?
U 1 1 5BF9471D
P 10300 2650
F 0 "RV?" H 10230 2696 50  0000 R CNN
F 1 "1k" V 10500 2850 50  0000 R CNN
F 2 "" H 10300 2650 50  0001 C CNN
F 3 "~" H 10300 2650 50  0001 C CNN
	1    10300 2650
	1    0    0    -1  
$EndComp
$Comp
L Device:CP C?
U 1 1 5C025E71
P 10700 2250
F 0 "C?" H 10818 2296 50  0000 L CNN
F 1 "2.2 mkF 250V" V 10850 1650 50  0000 L CNN
F 2 "" H 10738 2100 50  0001 C CNN
F 3 "~" H 10700 2250 50  0001 C CNN
	1    10700 2250
	1    0    0    -1  
$EndComp
$Comp
L Device:R R?
U 1 1 5C03E26F
P 10300 2250
F 0 "R?" H 10370 2296 50  0000 L CNN
F 1 "220k" H 10370 2205 50  0000 L CNN
F 2 "" V 10230 2250 50  0001 C CNN
F 3 "~" H 10300 2250 50  0001 C CNN
	1    10300 2250
	1    0    0    -1  
$EndComp
Wire Wire Line
	9250 2050 9350 2050
Wire Wire Line
	9850 2050 9900 2050
Wire Wire Line
	10250 2050 10300 2050
Wire Wire Line
	10300 2050 10300 2100
Wire Wire Line
	10300 2050 10700 2050
Wire Wire Line
	10700 2050 10700 2100
Connection ~ 10300 2050
Wire Wire Line
	10300 2400 10300 2450
Wire Wire Line
	9900 2150 9900 2050
Connection ~ 9900 2050
Wire Wire Line
	9900 2050 9950 2050
Wire Wire Line
	9550 2400 9550 2350
Wire Wire Line
	9550 2350 9600 2350
Wire Wire Line
	9250 2050 9250 2400
Wire Wire Line
	9250 2700 9250 2900
Wire Wire Line
	9250 2900 9550 2900
Wire Wire Line
	10700 2900 10700 2400
Wire Wire Line
	10300 2800 10300 2900
Connection ~ 10300 2900
Wire Wire Line
	10300 2900 10700 2900
Wire Wire Line
	10450 2650 10450 2450
Wire Wire Line
	10450 2450 10300 2450
Connection ~ 10300 2450
Wire Wire Line
	10300 2450 10300 2500
Wire Wire Line
	9900 2550 9900 2900
Connection ~ 9900 2900
Wire Wire Line
	9900 2900 10300 2900
Wire Wire Line
	9550 2700 9550 2900
Connection ~ 9550 2900
Wire Wire Line
	9550 2900 9900 2900
Wire Wire Line
	10700 2050 10950 2050
Wire Wire Line
	10950 2050 10950 3250
Connection ~ 10700 2050
Connection ~ 10950 3250
$Comp
L power:GND #PWR?
U 1 1 5C243986
P 10700 3000
F 0 "#PWR?" H 10700 2750 50  0001 C CNN
F 1 "GND" H 10850 2900 50  0000 C CNN
F 2 "" H 10700 3000 50  0001 C CNN
F 3 "" H 10700 3000 50  0001 C CNN
	1    10700 3000
	1    0    0    -1  
$EndComp
Wire Wire Line
	10700 2900 10700 3000
Connection ~ 10700 2900
Wire Wire Line
	1750 2400 1300 2400
Wire Wire Line
	1300 2400 1300 850 
Wire Wire Line
	1300 850  7500 850 
Wire Wire Line
	9150 850  9150 2350
Wire Wire Line
	9150 2350 9550 2350
Connection ~ 9550 2350
$Comp
L Transistor_BJT:MPSA42 Q?
U 1 1 5C297665
P 8800 2650
F 0 "Q?" H 8991 2696 50  0000 L CNN
F 1 "BC547" V 9000 2350 50  0000 L CNN
F 2 "Package_TO_SOT_THT:TO-92_Inline" H 9000 2575 50  0001 L CIN
F 3 "http://www.onsemi.com/pub_link/Collateral/MPSA42-D.PDF" H 8800 2650 50  0001 L CNN
	1    8800 2650
	-1   0    0    -1  
$EndComp
Wire Wire Line
	8700 2850 8700 2900
Wire Wire Line
	8700 2900 9250 2900
Connection ~ 9250 2900
Wire Wire Line
	9000 2650 9000 2750
Wire Wire Line
	9000 2750 10150 2750
Wire Wire Line
	10150 2750 10150 2450
Wire Wire Line
	10150 2450 10300 2450
$Comp
L Device:R R?
U 1 1 5C3275C2
P 8700 2250
F 0 "R?" H 8770 2296 50  0000 L CNN
F 1 "56k" H 8770 2205 50  0000 L CNN
F 2 "" V 8630 2250 50  0001 C CNN
F 3 "~" H 8700 2250 50  0001 C CNN
	1    8700 2250
	1    0    0    -1  
$EndComp
Wire Wire Line
	8700 2400 8700 2450
Wire Wire Line
	8700 2100 8700 2050
Wire Wire Line
	8700 2050 9250 2050
Connection ~ 9250 2050
Wire Wire Line
	8700 2050 8700 1350
Connection ~ 8700 2050
Wire Wire Line
	2850 1350 4100 1350
Connection ~ 4100 1350
Wire Wire Line
	4100 1350 4500 1350
Wire Wire Line
	3950 2600 4100 2600
Wire Wire Line
	4100 2600 4100 1650
Wire Wire Line
	1750 2100 1400 2100
Wire Wire Line
	1400 2100 1400 950 
Wire Wire Line
	1400 950  8600 950 
Wire Wire Line
	8600 950  8600 2450
Wire Wire Line
	8600 2450 8700 2450
Connection ~ 8700 2450
Wire Wire Line
	2000 6200 1850 6200
Wire Wire Line
	4000 2800 3950 2800
Wire Wire Line
	2000 6500 1550 6500
Wire Wire Line
	4100 2700 3950 2700
Wire Wire Line
	2000 6400 1200 6400
Wire Wire Line
	4200 2500 3950 2500
Wire Wire Line
	3950 2400 4300 2400
Wire Wire Line
	4300 2400 4300 2700
Wire Wire Line
	1950 6100 2000 6100
Wire Wire Line
	2300 3550 2300 4450
Wire Wire Line
	1500 4450 2200 4450
Connection ~ 2200 4450
Wire Wire Line
	1850 3700 4000 3700
Wire Wire Line
	4000 3700 4000 3000
Wire Wire Line
	4100 3800 1550 3800
Wire Wire Line
	4100 3800 4100 2900
Wire Wire Line
	1200 3900 4200 3900
Wire Wire Line
	4200 3900 4200 2800
Wire Wire Line
	1950 4000 4300 4000
Wire Wire Line
	3950 2300 4400 2300
Wire Wire Line
	4400 2300 4400 4100
Wire Wire Line
	4400 4100 2550 4100
Wire Wire Line
	2550 4100 2550 4450
Wire Wire Line
	1300 5450 1300 6300
Wire Wire Line
	2100 4950 2100 4150
Wire Wire Line
	2100 4150 3100 4150
Connection ~ 2100 4950
Wire Wire Line
	3100 4150 3100 3150
Wire Wire Line
	3100 3150 4500 3150
Wire Wire Line
	4500 3150 4500 1350
Connection ~ 3100 4150
Connection ~ 4500 1350
Wire Wire Line
	4500 1350 4900 1350
$Comp
L Device:R R?
U 1 1 5C5FF6FB
P 1000 2000
F 0 "R?" H 930 1954 50  0000 R CNN
F 1 "10k" H 930 2045 50  0000 R CNN
F 2 "" V 930 2000 50  0001 C CNN
F 3 "~" H 1000 2000 50  0001 C CNN
	1    1000 2000
	-1   0    0    1   
$EndComp
$Comp
L Device:R R?
U 1 1 5C61EDC9
P 700 2000
F 0 "R?" H 630 1954 50  0000 R CNN
F 1 "10k" H 630 2045 50  0000 R CNN
F 2 "" V 630 2000 50  0001 C CNN
F 3 "~" H 700 2000 50  0001 C CNN
	1    700  2000
	-1   0    0    1   
$EndComp
Wire Wire Line
	700  1850 700  1800
Wire Wire Line
	700  1800 1000 1800
Wire Wire Line
	1000 1800 1000 1850
Connection ~ 1000 1800
Connection ~ 2850 1800
Wire Wire Line
	1000 2150 1000 2200
Wire Wire Line
	700  2300 700  2150
$Comp
L Connector:Conn_01x03_Male J?
U 1 1 5C6E160C
P 700 2700
F 0 "J?" H 850 2950 50  0000 C CNN
F 1 "USART" V 600 2700 50  0000 C CNN
F 2 "" H 700 2700 50  0001 C CNN
F 3 "~" H 700 2700 50  0001 C CNN
	1    700  2700
	1    0    0    -1  
$EndComp
Wire Wire Line
	1000 2200 1000 2600
Wire Wire Line
	1000 2600 900  2600
Connection ~ 1000 2200
Wire Wire Line
	900  2800 1100 2800
Wire Wire Line
	1100 2800 1100 2300
Connection ~ 1100 2300
Wire Wire Line
	1100 2300 700  2300
$Comp
L Connector:Conn_01x06_Male J?
U 1 1 5C74C701
P 750 3500
F 0 "J?" H 900 3850 50  0000 C CNN
F 1 "ICSP" V 650 3450 50  0000 C CNN
F 2 "" H 750 3500 50  0001 C CNN
F 3 "~" H 750 3500 50  0001 C CNN
	1    750  3500
	1    0    0    -1  
$EndComp
Wire Wire Line
	950  3500 1500 3500
Connection ~ 1500 3500
Wire Wire Line
	1500 3500 1500 4450
Wire Wire Line
	950  3400 1200 3400
Wire Wire Line
	1500 1650 1500 2700
Wire Wire Line
	1000 1800 1200 1800
Wire Wire Line
	1000 2200 1750 2200
Wire Wire Line
	1100 2300 1750 2300
Wire Wire Line
	1200 1800 1200 3400
Connection ~ 1200 1800
Wire Wire Line
	1200 1800 2850 1800
Wire Wire Line
	950  3600 1700 3600
Wire Wire Line
	1700 3600 1700 3250
Connection ~ 1700 3250
Wire Wire Line
	950  3700 1600 3700
Wire Wire Line
	1600 3700 1600 3550
Connection ~ 1600 3550
Wire Wire Line
	950  3800 1300 3800
Connection ~ 1300 3800
Wire Wire Line
	1300 3800 1300 4650
Wire Wire Line
	4100 2600 4600 2600
Wire Wire Line
	4600 2600 4600 4250
Wire Wire Line
	4600 4250 1100 4250
Wire Wire Line
	1100 4250 1100 3300
Wire Wire Line
	1100 3300 950  3300
Connection ~ 4100 2600
$Comp
L Connector:Conn_01x02_Male J?
U 1 1 5C926E8E
P 700 1100
F 0 "J?" H 806 1278 50  0000 C CNN
F 1 "DC 5V" V 600 1050 50  0000 C CNN
F 2 "" H 700 1100 50  0001 C CNN
F 3 "~" H 700 1100 50  0001 C CNN
	1    700  1100
	1    0    0    -1  
$EndComp
Wire Wire Line
	900  1200 1500 1200
Wire Wire Line
	1500 1200 1500 1350
Connection ~ 1500 1350
Wire Wire Line
	900  1100 2750 1100
Wire Wire Line
	2750 1100 2750 1350
Wire Wire Line
	2750 1350 2850 1350
$Comp
L Device:R_PHOTO R?
U 1 1 5C979B6D
P 4900 2000
F 0 "R?" H 4970 2046 50  0000 L CNN
F 1 "R_PHOTO" H 4970 1955 50  0000 L CNN
F 2 "" V 4950 1750 50  0001 L CNN
F 3 "~" H 4900 1950 50  0001 C CNN
	1    4900 2000
	1    0    0    -1  
$EndComp
$Comp
L Device:R R?
U 1 1 5C979C94
P 4900 1600
F 0 "R?" H 4970 1646 50  0000 L CNN
F 1 "R" H 4970 1555 50  0000 L CNN
F 2 "" V 4830 1600 50  0001 C CNN
F 3 "~" H 4900 1600 50  0001 C CNN
	1    4900 1600
	1    0    0    -1  
$EndComp
Wire Wire Line
	4900 1750 4900 1800
Wire Wire Line
	4900 1450 4900 1350
Connection ~ 4900 1350
Wire Wire Line
	4900 1350 5550 1350
Wire Wire Line
	4600 1800 4900 1800
Connection ~ 4900 1800
Wire Wire Line
	4900 1800 4900 1850
Wire Wire Line
	900  2700 1500 2700
Connection ~ 1500 2700
Wire Wire Line
	1500 2700 1500 3500
$Comp
L Device:R R?
U 1 1 5CA9AC82
P 1200 5050
F 0 "R?" H 1050 5100 50  0000 L CNN
F 1 "1k" H 1050 5000 50  0000 L CNN
F 2 "" V 1130 5050 50  0001 C CNN
F 3 "~" H 1200 5050 50  0001 C CNN
	1    1200 5050
	1    0    0    -1  
$EndComp
$Comp
L Device:R R?
U 1 1 5CAC5BEE
P 1550 5050
F 0 "R?" H 1400 5100 50  0000 L CNN
F 1 "1k" H 1400 5000 50  0000 L CNN
F 2 "" V 1480 5050 50  0001 C CNN
F 3 "~" H 1550 5050 50  0001 C CNN
	1    1550 5050
	1    0    0    -1  
$EndComp
$Comp
L Device:R R?
U 1 1 5CAEFB02
P 1850 5050
F 0 "R?" H 1700 5100 50  0000 L CNN
F 1 "1k" H 1700 5000 50  0000 L CNN
F 2 "" V 1780 5050 50  0001 C CNN
F 3 "~" H 1850 5050 50  0001 C CNN
	1    1850 5050
	1    0    0    -1  
$EndComp
$Comp
L Device:R R?
U 1 1 5CB19A15
P 1950 5050
F 0 "R?" H 2000 5200 50  0000 L CNN
F 1 "1k" H 2000 4900 50  0000 L CNN
F 2 "" V 1880 5050 50  0001 C CNN
F 3 "~" H 1950 5050 50  0001 C CNN
	1    1950 5050
	1    0    0    -1  
$EndComp
Wire Wire Line
	1200 3900 1200 4900
Wire Wire Line
	1550 3800 1550 4900
Wire Wire Line
	1850 3700 1850 4900
Wire Wire Line
	1950 4000 1950 4900
Wire Wire Line
	1200 5200 1200 6400
Wire Wire Line
	1550 5200 1550 6500
Wire Wire Line
	1850 5200 1850 6200
Wire Wire Line
	1950 5200 1950 6100
$Comp
L Device:R R?
U 1 1 5CC980FE
P 5550 1600
F 0 "R?" H 5620 1646 50  0000 L CNN
F 1 "10k" H 5620 1555 50  0000 L CNN
F 2 "" V 5480 1600 50  0001 C CNN
F 3 "~" H 5550 1600 50  0001 C CNN
	1    5550 1600
	1    0    0    -1  
$EndComp
$Comp
L Switch:SW_MEC_5G SW?
U 1 1 5CC982D0
P 5550 2000
F 0 "SW?" V 5504 2148 50  0000 L CNN
F 1 "Mode" V 5595 2148 50  0000 L CNN
F 2 "" H 5550 2200 50  0001 C CNN
F 3 "http://www.apem.com/int/index.php?controller=attachment&id_attachment=488" H 5550 2200 50  0001 C CNN
	1    5550 2000
	0    1    1    0   
$EndComp
$Comp
L Device:R R?
U 1 1 5CCEE297
P 5550 2400
F 0 "R?" H 5620 2446 50  0000 L CNN
F 1 "100k" H 5620 2355 50  0000 L CNN
F 2 "" V 5480 2400 50  0001 C CNN
F 3 "~" H 5550 2400 50  0001 C CNN
	1    5550 2400
	1    0    0    -1  
$EndComp
Wire Wire Line
	5550 1750 5550 1800
Wire Wire Line
	5550 2200 5550 2250
Wire Wire Line
	5550 1450 5550 1350
Connection ~ 5550 1350
Wire Wire Line
	5550 1350 6000 1350
$Comp
L Device:R R?
U 1 1 5CDC614F
P 6000 1600
F 0 "R?" H 6070 1646 50  0000 L CNN
F 1 "10k" H 6070 1555 50  0000 L CNN
F 2 "" V 5930 1600 50  0001 C CNN
F 3 "~" H 6000 1600 50  0001 C CNN
	1    6000 1600
	1    0    0    -1  
$EndComp
$Comp
L Switch:SW_MEC_5G SW?
U 1 1 5CDC6156
P 6000 2000
F 0 "SW?" V 5954 2148 50  0000 L CNN
F 1 "+" V 6045 2148 50  0000 L CNN
F 2 "" H 6000 2200 50  0001 C CNN
F 3 "http://www.apem.com/int/index.php?controller=attachment&id_attachment=488" H 6000 2200 50  0001 C CNN
	1    6000 2000
	0    1    1    0   
$EndComp
$Comp
L Device:R R?
U 1 1 5CDC615D
P 6000 2400
F 0 "R?" H 6070 2446 50  0000 L CNN
F 1 "100k" H 6070 2355 50  0000 L CNN
F 2 "" V 5930 2400 50  0001 C CNN
F 3 "~" H 6000 2400 50  0001 C CNN
	1    6000 2400
	1    0    0    -1  
$EndComp
Wire Wire Line
	6000 1750 6000 1800
Wire Wire Line
	6000 2200 6000 2250
$Comp
L Device:R R?
U 1 1 5CDF2167
P 6400 1600
F 0 "R?" H 6470 1646 50  0000 L CNN
F 1 "10k" H 6470 1555 50  0000 L CNN
F 2 "" V 6330 1600 50  0001 C CNN
F 3 "~" H 6400 1600 50  0001 C CNN
	1    6400 1600
	1    0    0    -1  
$EndComp
$Comp
L Switch:SW_MEC_5G SW?
U 1 1 5CDF216E
P 6400 2000
F 0 "SW?" V 6354 2148 50  0000 L CNN
F 1 "-" V 6445 2148 50  0000 L CNN
F 2 "" H 6400 2200 50  0001 C CNN
F 3 "http://www.apem.com/int/index.php?controller=attachment&id_attachment=488" H 6400 2200 50  0001 C CNN
	1    6400 2000
	0    1    1    0   
$EndComp
$Comp
L Device:R R?
U 1 1 5CDF2175
P 6400 2400
F 0 "R?" H 6470 2446 50  0000 L CNN
F 1 "100k" H 6470 2355 50  0000 L CNN
F 2 "" V 6330 2400 50  0001 C CNN
F 3 "~" H 6400 2400 50  0001 C CNN
	1    6400 2400
	1    0    0    -1  
$EndComp
Wire Wire Line
	6400 1750 6400 1800
Wire Wire Line
	6400 2200 6400 2250
$Comp
L Device:R R?
U 1 1 5CE1E767
P 6800 1600
F 0 "R?" H 6870 1646 50  0000 L CNN
F 1 "10k" H 6870 1555 50  0000 L CNN
F 2 "" V 6730 1600 50  0001 C CNN
F 3 "~" H 6800 1600 50  0001 C CNN
	1    6800 1600
	1    0    0    -1  
$EndComp
$Comp
L Switch:SW_MEC_5G SW?
U 1 1 5CE1E76E
P 6800 2000
F 0 "SW?" V 6754 2148 50  0000 L CNN
F 1 "bb" V 6845 2148 50  0000 L CNN
F 2 "" H 6800 2200 50  0001 C CNN
F 3 "http://www.apem.com/int/index.php?controller=attachment&id_attachment=488" H 6800 2200 50  0001 C CNN
	1    6800 2000
	0    1    1    0   
$EndComp
$Comp
L Device:R R?
U 1 1 5CE1E775
P 6800 2400
F 0 "R?" H 6870 2446 50  0000 L CNN
F 1 "100k" H 6870 2355 50  0000 L CNN
F 2 "" V 6730 2400 50  0001 C CNN
F 3 "~" H 6800 2400 50  0001 C CNN
	1    6800 2400
	1    0    0    -1  
$EndComp
Wire Wire Line
	6800 1750 6800 1800
Wire Wire Line
	6800 2200 6800 2250
Wire Wire Line
	6000 1450 6000 1350
Connection ~ 6000 1350
Wire Wire Line
	6000 1350 6400 1350
Wire Wire Line
	6400 1450 6400 1350
Connection ~ 6400 1350
Wire Wire Line
	6400 1350 6800 1350
Wire Wire Line
	6800 1450 6800 1350
Connection ~ 6800 1350
Wire Wire Line
	6800 1350 7850 1350
Wire Wire Line
	5550 2550 5550 2650
Wire Wire Line
	5550 2650 6000 2650
Wire Wire Line
	6800 2650 6800 2550
Wire Wire Line
	6400 2550 6400 2650
Connection ~ 6400 2650
Wire Wire Line
	6400 2650 6800 2650
Wire Wire Line
	6000 2550 6000 2650
Connection ~ 6000 2650
Wire Wire Line
	6000 2650 6400 2650
$Comp
L power:GND #PWR?
U 1 1 5CF8F39D
P 6800 2750
F 0 "#PWR?" H 6800 2500 50  0001 C CNN
F 1 "GND" H 6950 2650 50  0000 C CNN
F 2 "" H 6800 2750 50  0001 C CNN
F 3 "" H 6800 2750 50  0001 C CNN
	1    6800 2750
	1    0    0    -1  
$EndComp
Wire Wire Line
	4900 2650 5550 2650
Wire Wire Line
	4900 2150 4900 2650
Connection ~ 5550 2650
Wire Wire Line
	6800 2650 6800 2750
Connection ~ 6800 2650
Wire Wire Line
	6800 2250 6700 2250
Wire Wire Line
	6700 2250 6700 3000
Wire Wire Line
	6700 3000 4000 3000
Connection ~ 6800 2250
Connection ~ 4000 3000
Wire Wire Line
	4000 3000 4000 2800
Wire Wire Line
	4100 2900 6300 2900
Wire Wire Line
	6300 2900 6300 2250
Wire Wire Line
	6300 2250 6400 2250
Connection ~ 4100 2900
Wire Wire Line
	4100 2900 4100 2700
Connection ~ 6400 2250
Wire Wire Line
	6000 2250 5900 2250
Wire Wire Line
	5900 2250 5900 2800
Wire Wire Line
	5900 2800 4200 2800
Connection ~ 6000 2250
Connection ~ 4200 2800
Wire Wire Line
	4200 2800 4200 2500
Wire Wire Line
	5550 2250 4800 2250
Wire Wire Line
	4800 2250 4800 2700
Wire Wire Line
	4800 2700 4300 2700
Connection ~ 5550 2250
Connection ~ 4300 2700
Wire Wire Line
	4300 2700 4300 4000
Wire Wire Line
	3950 2200 4600 2200
Wire Wire Line
	4600 1800 4600 2200
$Comp
L Device:Speaker_Crystal LS?
U 1 1 5D1AA6E4
P 8100 1950
F 0 "LS?" H 8050 2150 50  0000 L CNN
F 1 "Speaker_Crystal" V 8300 1600 50  0000 L CNN
F 2 "" H 8065 1900 50  0001 C CNN
F 3 "~" H 8065 1900 50  0001 C CNN
	1    8100 1950
	1    0    0    -1  
$EndComp
$Comp
L Transistor_BJT:MPSA42 Q?
U 1 1 5D1AA8E5
P 7750 2350
F 0 "Q?" H 7941 2396 50  0000 L CNN
F 1 "BC547" V 7950 2050 50  0000 L CNN
F 2 "Package_TO_SOT_THT:TO-92_Inline" H 7950 2275 50  0001 L CIN
F 3 "http://www.onsemi.com/pub_link/Collateral/MPSA42-D.PDF" H 7750 2350 50  0001 L CNN
	1    7750 2350
	1    0    0    -1  
$EndComp
$Comp
L Device:R R?
U 1 1 5D245620
P 7500 2150
F 0 "R?" H 7570 2196 50  0000 L CNN
F 1 "1k" H 7570 2105 50  0000 L CNN
F 2 "" V 7430 2150 50  0001 C CNN
F 3 "~" H 7500 2150 50  0001 C CNN
	1    7500 2150
	1    0    0    -1  
$EndComp
$Comp
L Transistor_BJT:MPSA42 Q?
U 1 1 5D2ACB61
P 7400 1700
F 0 "Q?" H 7591 1746 50  0000 L CNN
F 1 "BC547" V 7600 1400 50  0000 L CNN
F 2 "Package_TO_SOT_THT:TO-92_Inline" H 7600 1625 50  0001 L CIN
F 3 "http://www.onsemi.com/pub_link/Collateral/MPSA42-D.PDF" H 7400 1700 50  0001 L CNN
	1    7400 1700
	1    0    0    -1  
$EndComp
$Comp
L Device:R R?
U 1 1 5D314A92
P 6950 1200
F 0 "R?" V 6743 1200 50  0000 C CNN
F 1 "1k" V 6834 1200 50  0000 C CNN
F 2 "" V 6880 1200 50  0001 C CNN
F 3 "~" H 6950 1200 50  0001 C CNN
	1    6950 1200
	0    1    1    0   
$EndComp
Wire Wire Line
	7850 2550 7850 2650
Wire Wire Line
	7850 2650 6800 2650
Wire Wire Line
	7500 2300 7500 2350
Wire Wire Line
	7500 2350 7550 2350
Wire Wire Line
	7500 1900 7500 2000
Wire Wire Line
	7500 1500 7500 850 
Connection ~ 7500 850 
Wire Wire Line
	7500 850  9150 850 
Wire Wire Line
	7900 2050 7850 2050
Wire Wire Line
	7850 2050 7850 2150
Wire Wire Line
	7900 1950 7850 1950
Wire Wire Line
	7850 1950 7850 1350
Connection ~ 7850 1350
Wire Wire Line
	7850 1350 8700 1350
Wire Wire Line
	7100 1200 7150 1200
Wire Wire Line
	7150 1200 7150 1700
Wire Wire Line
	7150 1700 7200 1700
Wire Wire Line
	6800 1200 4000 1200
Wire Wire Line
	4000 1200 4000 2100
Wire Wire Line
	4000 2100 3950 2100
Wire Bus Line
	5050 3100 10150 3100
Wire Bus Line
	3750 3450 3750 4850
Wire Bus Line
	5350 6400 10000 6400
Wire Bus Line
	3450 6000 3450 7050
Wire Bus Line
	5600 5200 5600 6300
Wire Bus Line
	6500 5200 6500 6300
Wire Bus Line
	7400 5200 7400 6300
Wire Bus Line
	8300 5200 8300 6300
Wire Bus Line
	9200 5200 9200 6300
Wire Bus Line
	10100 5200 10100 6300
$EndSCHEMATC
