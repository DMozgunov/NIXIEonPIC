EESchema Schematic File Version 4
LIBS:NIXIEonPIC-cache
EELAYER 26 0
EELAYER END
$Descr A4 11693 8268
encoding utf-8
Sheet 1 1
Title "NIXIE clock on PIC16F628"
Date "2018-08-08"
Rev "1"
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
P 3150 1750
F 0 "D?" H 3150 1966 50  0000 C CNN
F 1 "1N4148" H 3150 1875 50  0000 C CNN
F 2 "Diode_THT:D_DO-35_SOD27_P7.62mm_Horizontal" H 3150 1575 50  0001 C CNN
F 3 "http://www.nxp.com/documents/data_sheet/1N4148_1N4448.pdf" H 3150 1750 50  0001 C CNN
	1    3150 1750
	1    0    0    -1  
$EndComp
$Comp
L Device:Battery_Cell BT?
U 1 1 5B6F4959
P 3600 1750
F 0 "BT?" V 3500 1700 50  0000 R CNN
F 1 "3V Li" V 3750 1850 50  0000 R CNN
F 2 "" V 3600 1810 50  0001 C CNN
F 3 "~" V 3600 1810 50  0001 C CNN
	1    3600 1750
	0    -1   -1   0   
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
	2850 1650 2850 1750
Wire Wire Line
	3000 1750 2850 1750
Connection ~ 2850 1750
Wire Wire Line
	2850 1750 2850 1800
Wire Wire Line
	3400 1750 3300 1750
Wire Wire Line
	2850 3300 2850 3250
Wire Wire Line
	3700 1750 4150 1750
Wire Wire Line
	4150 1750 4150 3250
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
F 1 "22pF" V 2400 3400 50  0000 L CNN
F 2 "" H 2188 3400 50  0001 C CNN
F 3 "~" H 2150 3550 50  0001 C CNN
	1    2150 3550
	0    1    1    0   
$EndComp
$Comp
L Device:C C?
U 1 1 5B6D810E
P 2150 3250
F 0 "C?" V 2250 3400 50  0000 L CNN
F 1 "22pF" V 2050 3350 50  0000 L CNN
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
P 4350 5600
F 0 "U?" H 4350 6197 60  0000 C CNN
F 1 "K155ID1" H 4350 6091 60  0000 C CNN
F 2 "" H 4350 5600 60  0000 C CNN
F 3 "" H 4350 5600 60  0000 C CNN
	1    4350 5600
	1    0    0    -1  
$EndComp
Entry Wire Line
	5050 5250 5150 5350
Entry Wire Line
	5050 5350 5150 5450
Entry Wire Line
	5050 5450 5150 5550
Entry Wire Line
	5050 5550 5150 5650
Entry Wire Line
	5050 5750 5150 5850
Entry Wire Line
	5050 5850 5150 5950
Entry Wire Line
	5050 5950 5150 6050
Wire Wire Line
	2850 3250 3450 3250
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
	3700 5650 3000 5650
Wire Wire Line
	5100 5650 5100 4950
Wire Wire Line
	3700 5450 3650 5450
Wire Wire Line
	3650 5450 3650 3350
Wire Wire Line
	3650 3350 4250 3350
Wire Wire Line
	4250 3350 4250 2200
Wire Wire Line
	4250 2200 3950 2200
Wire Wire Line
	3700 5750 3600 5750
Wire Wire Line
	3600 5750 3600 3300
Wire Wire Line
	3600 3300 4200 3300
Wire Wire Line
	4200 3300 4200 2100
Wire Wire Line
	4200 2100 3950 2100
Wire Wire Line
	3700 5550 3550 5550
Wire Wire Line
	3550 5550 3550 3200
Wire Wire Line
	3550 3200 4100 3200
Wire Wire Line
	4100 3200 4100 2700
Wire Wire Line
	4100 2700 3950 2700
Wire Wire Line
	3950 2800 4000 2800
Wire Wire Line
	4000 2800 4000 3150
Wire Wire Line
	4000 3150 3500 3150
Wire Wire Line
	3500 3150 3500 5850
Wire Wire Line
	3500 5850 3700 5850
$Comp
L Device:C C?
U 1 1 5B752056
P 2400 1500
F 0 "C?" H 2515 1546 50  0000 L CNN
F 1 "100nF" H 2150 1400 50  0000 L CNN
F 2 "" H 2438 1350 50  0001 C CNN
F 3 "~" H 2400 1500 50  0001 C CNN
	1    2400 1500
	1    0    0    -1  
$EndComp
Wire Wire Line
	2400 1650 2400 1750
Wire Wire Line
	2400 1750 2850 1750
Wire Wire Line
	2400 1350 1500 1350
Wire Wire Line
	1500 1350 1500 3700
Wire Wire Line
	1500 3700 2300 3700
Wire Wire Line
	2300 3250 2300 3550
Connection ~ 2300 3550
Wire Wire Line
	2300 3550 2300 3700
$Comp
L Device:C C?
U 1 1 5B765457
P 3000 5800
F 0 "C?" H 3115 5846 50  0000 L CNN
F 1 "100nF" H 3115 5755 50  0000 L CNN
F 2 "" H 3038 5650 50  0001 C CNN
F 3 "~" H 3000 5800 50  0001 C CNN
	1    3000 5800
	1    0    0    -1  
$EndComp
Connection ~ 3000 5650
$Comp
L power:GND #PWR?
U 1 1 5B765547
P 3000 6050
F 0 "#PWR?" H 3000 5800 50  0001 C CNN
F 1 "GND" H 3005 5877 50  0000 C CNN
F 2 "" H 3000 6050 50  0001 C CNN
F 3 "" H 3000 6050 50  0001 C CNN
	1    3000 6050
	1    0    0    -1  
$EndComp
Wire Wire Line
	3000 5950 3000 6050
Connection ~ 3450 3250
Wire Wire Line
	3450 3250 4150 3250
Entry Wire Line
	9250 3150 9350 3250
Entry Wire Line
	10150 3150 10250 3250
Entry Wire Line
	8350 3150 8450 3250
Entry Wire Line
	7450 3150 7550 3250
Entry Wire Line
	6550 3150 6650 3250
Entry Wire Line
	5650 3150 5750 3250
Wire Wire Line
	5750 3250 5750 3900
Wire Wire Line
	6650 3250 6650 3900
Wire Wire Line
	7550 3250 7550 3900
Wire Wire Line
	9350 3250 9350 3900
Wire Wire Line
	10250 3250 10250 3900
Wire Wire Line
	8450 3250 8450 3900
Entry Wire Line
	5350 3800 5450 3900
Entry Wire Line
	5350 3900 5450 4000
Entry Wire Line
	5350 4000 5450 4100
Entry Wire Line
	5350 4100 5450 4200
Entry Wire Line
	5350 4200 5450 4300
Entry Wire Line
	5350 4300 5450 4400
Wire Wire Line
	5300 3800 5350 3800
Wire Wire Line
	5300 3900 5350 3900
Wire Wire Line
	5350 4000 5300 4000
Wire Wire Line
	5300 4100 5350 4100
Wire Wire Line
	5350 4200 5300 4200
Wire Wire Line
	5300 4300 5350 4300
$Comp
L Device:C C?
U 1 1 5B8922A2
P 4300 4800
F 0 "C?" V 4048 4800 50  0000 C CNN
F 1 "100nF" V 4139 4800 50  0000 C CNN
F 2 "" H 4338 4650 50  0001 C CNN
F 3 "~" H 4300 4800 50  0001 C CNN
	1    4300 4800
	0    1    1    0   
$EndComp
Wire Wire Line
	4700 4400 4300 4400
Entry Wire Line
	5050 6050 5150 6150
Entry Wire Line
	5050 6150 5150 6250
Entry Wire Line
	5050 6250 5150 6350
Wire Wire Line
	3700 5950 3650 5950
Wire Wire Line
	3650 5950 3650 6050
Wire Wire Line
	3700 5350 3450 5350
Wire Wire Line
	3450 5350 3450 6150
Wire Wire Line
	3700 5250 3400 5250
Wire Wire Line
	3400 5250 3400 6250
Wire Wire Line
	3450 4950 4800 4950
Wire Wire Line
	5000 5250 5050 5250
Wire Wire Line
	5000 5350 5050 5350
Wire Wire Line
	5000 5450 5050 5450
Wire Wire Line
	5000 5550 5050 5550
Wire Wire Line
	5000 5650 5100 5650
Wire Wire Line
	5050 5750 5000 5750
Wire Wire Line
	5050 5850 5000 5850
Wire Wire Line
	5050 5950 5000 5950
Wire Wire Line
	3650 6050 5050 6050
Wire Wire Line
	3450 6150 5050 6150
Wire Wire Line
	3400 6250 5050 6250
$Comp
L 74xx:74LS138 U?
U 1 1 5B9D1B3E
P 4800 4100
F 0 "U?" H 4800 4878 50  0000 C CNN
F 1 "74LS138" H 4800 4787 50  0000 C CNN
F 2 "" H 4800 4100 50  0001 C CNN
F 3 "http://www.ti.com/lit/gpn/sn74LS138" H 4800 4100 50  0001 C CNN
	1    4800 4100
	1    0    0    -1  
$EndComp
Wire Wire Line
	3450 3250 3450 4950
Wire Wire Line
	4800 4800 4800 4950
Connection ~ 4800 4950
Wire Wire Line
	4800 4950 5100 4950
Wire Wire Line
	4450 4800 4800 4800
Connection ~ 4800 4800
Wire Wire Line
	4150 4800 4100 4800
Wire Wire Line
	4100 4800 4100 3500
Wire Wire Line
	4100 3500 4300 3500
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
Wire Wire Line
	10050 3250 10250 3250
Wire Wire Line
	10250 3250 10600 3250
Connection ~ 9700 3250
Wire Wire Line
	9150 3250 9350 3250
Wire Wire Line
	8250 3250 8450 3250
Wire Wire Line
	7350 3250 7550 3250
Wire Wire Line
	6450 3250 6650 3250
Wire Wire Line
	2850 1350 4300 1350
Wire Wire Line
	4300 1350 4300 3500
Connection ~ 2850 1350
Connection ~ 4300 3500
Wire Wire Line
	4300 3500 4800 3500
Wire Wire Line
	3000 5650 3000 4800
Wire Wire Line
	3000 4800 4100 4800
Connection ~ 4100 4800
Wire Wire Line
	2400 1350 2850 1350
Connection ~ 2400 1350
Wire Wire Line
	9350 3250 9700 3250
Wire Wire Line
	8450 3250 8800 3250
Wire Wire Line
	7550 3250 7900 3250
Wire Wire Line
	6650 3250 7000 3250
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
Wire Bus Line
	5450 3150 10150 3150
Wire Bus Line
	5450 3150 5450 4400
Wire Bus Line
	5150 6400 10000 6400
Wire Bus Line
	5150 5350 5150 6400
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
