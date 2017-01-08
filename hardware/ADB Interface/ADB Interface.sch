EESchema Schematic File Version 2
LIBS:ndp
LIBS:power
LIBS:device
LIBS:transistors
LIBS:conn
LIBS:linear
LIBS:regul
LIBS:74xx
LIBS:cmos4000
LIBS:adc-dac
LIBS:memory
LIBS:xilinx
LIBS:microcontrollers
LIBS:dsp
LIBS:microchip
LIBS:analog_switches
LIBS:motorola
LIBS:texas
LIBS:intel
LIBS:audio
LIBS:interface
LIBS:digital-audio
LIBS:philips
LIBS:display
LIBS:cypress
LIBS:siliconi
LIBS:opto
LIBS:atmel
LIBS:contrib
LIBS:valves
LIBS:ADB Interface-cache
EELAYER 25 0
EELAYER END
$Descr A4 11693 8268
encoding utf-8
Sheet 1 1
Title ""
Date ""
Rev ""
Comp ""
Comment1 ""
Comment2 ""
Comment3 ""
Comment4 ""
$EndDescr
$Comp
L +5V #PWR01
U 1 1 584B9C9D
P 5100 2800
F 0 "#PWR01" H 5100 2650 50  0001 C CNN
F 1 "+5V" H 5100 2940 50  0000 C CNN
F 2 "" H 5100 2800 50  0000 C CNN
F 3 "" H 5100 2800 50  0000 C CNN
	1    5100 2800
	1    0    0    -1  
$EndComp
$Comp
L R R2
U 1 1 584B9CB4
P 5300 3250
F 0 "R2" V 5380 3250 50  0000 C CNN
F 1 "470" V 5300 3250 50  0000 C CNN
F 2 "Resistors_ThroughHole:Resistor_Horizontal_RM10mm" V 5230 3250 50  0001 C CNN
F 3 "" H 5300 3250 50  0000 C CNN
	1    5300 3250
	1    0    0    -1  
$EndComp
$Comp
L GND #PWR02
U 1 1 584B9EC2
P 4200 2800
F 0 "#PWR02" H 4200 2550 50  0001 C CNN
F 1 "GND" H 4200 2650 50  0000 C CNN
F 2 "" H 4200 2800 50  0000 C CNN
F 3 "" H 4200 2800 50  0000 C CNN
	1    4200 2800
	1    0    0    -1  
$EndComp
$Comp
L +5V #PWR03
U 1 1 584BA184
P 5300 3400
F 0 "#PWR03" H 5300 3250 50  0001 C CNN
F 1 "+5V" H 5300 3540 50  0000 C CNN
F 2 "" H 5300 3400 50  0000 C CNN
F 3 "" H 5300 3400 50  0000 C CNN
	1    5300 3400
	-1   0    0    1   
$EndComp
$Comp
L +3.3V #PWR04
U 1 1 584BA220
P 5650 3350
F 0 "#PWR04" H 5650 3200 50  0001 C CNN
F 1 "+3.3V" H 5650 3490 50  0000 C CNN
F 2 "" H 5650 3350 50  0000 C CNN
F 3 "" H 5650 3350 50  0000 C CNN
	1    5650 3350
	-1   0    0    1   
$EndComp
$Comp
L R R1
U 1 1 584BA334
P 4000 2850
F 0 "R1" V 4080 2850 50  0000 C CNN
F 1 "470" V 4000 2850 50  0000 C CNN
F 2 "Resistors_ThroughHole:Resistor_Horizontal_RM10mm" V 3930 2850 50  0001 C CNN
F 3 "" H 4000 2850 50  0000 C CNN
	1    4000 2850
	1    0    0    -1  
$EndComp
$Comp
L +3.3V #PWR05
U 1 1 584BA39B
P 4000 2700
F 0 "#PWR05" H 4000 2550 50  0001 C CNN
F 1 "+3.3V" H 4000 2840 50  0000 C CNN
F 2 "" H 4000 2700 50  0000 C CNN
F 3 "" H 4000 2700 50  0000 C CNN
	1    4000 2700
	1    0    0    -1  
$EndComp
$Comp
L R R3
U 1 1 584BA3B9
P 5900 3200
F 0 "R3" V 5980 3200 50  0000 C CNN
F 1 "470" V 5900 3200 50  0000 C CNN
F 2 "Resistors_ThroughHole:Resistor_Horizontal_RM10mm" V 5830 3200 50  0001 C CNN
F 3 "" H 5900 3200 50  0000 C CNN
	1    5900 3200
	1    0    0    -1  
$EndComp
$Comp
L CONN_01X05 P1
U 1 1 584BAA73
P 4600 4400
F 0 "P1" H 4600 4700 50  0000 C CNN
F 1 "CONN_01X05" V 4700 4400 50  0000 C CNN
F 2 "Socket_Strips:Socket_Strip_Straight_1x05" H 4600 4400 50  0001 C CNN
F 3 "" H 4600 4400 50  0000 C CNN
	1    4600 4400
	0    1    1    0   
$EndComp
$Comp
L +3.3V #PWR06
U 1 1 584BAB09
P 4400 4200
F 0 "#PWR06" H 4400 4050 50  0001 C CNN
F 1 "+3.3V" H 4400 4340 50  0000 C CNN
F 2 "" H 4400 4200 50  0000 C CNN
F 3 "" H 4400 4200 50  0000 C CNN
	1    4400 4200
	0    -1   -1   0   
$EndComp
$Comp
L GND #PWR07
U 1 1 584BAB2D
P 4500 4050
F 0 "#PWR07" H 4500 3800 50  0001 C CNN
F 1 "GND" H 4500 3900 50  0000 C CNN
F 2 "" H 4500 4050 50  0000 C CNN
F 3 "" H 4500 4050 50  0000 C CNN
	1    4500 4050
	0    1    1    0   
$EndComp
$Comp
L +5V #PWR08
U 1 1 584BAB51
P 4600 3900
F 0 "#PWR08" H 4600 3750 50  0001 C CNN
F 1 "+5V" H 4600 4040 50  0000 C CNN
F 2 "" H 4600 3900 50  0000 C CNN
F 3 "" H 4600 3900 50  0000 C CNN
	1    4600 3900
	0    -1   -1   0   
$EndComp
Wire Wire Line
	4600 4200 4600 3900
Wire Wire Line
	4500 4050 4500 4200
Wire Wire Line
	4700 3650 3400 3650
Wire Wire Line
	4700 4200 4700 3650
Wire Wire Line
	6300 4200 4800 4200
Wire Wire Line
	6300 3050 6300 4200
Connection ~ 5300 3050
Wire Wire Line
	5300 3100 5300 3050
Connection ~ 4000 3050
Wire Wire Line
	4000 3000 4000 3050
Wire Wire Line
	3400 3050 4150 3050
Wire Wire Line
	5150 3050 5450 3050
Text Label 5150 4200 0    60   ~ 0
adb
Text Label 4050 3650 0    60   ~ 0
~poweron
Wire Wire Line
	3400 3650 3400 3050
$Comp
L PWR_FLAG #FLG09
U 1 1 5872AFB8
P 6350 7150
F 0 "#FLG09" H 6350 7245 50  0001 C CNN
F 1 "PWR_FLAG" H 6350 7330 50  0000 C CNN
F 2 "" H 6350 7150 50  0000 C CNN
F 3 "" H 6350 7150 50  0000 C CNN
	1    6350 7150
	1    0    0    -1  
$EndComp
$Comp
L PWR_FLAG #FLG010
U 1 1 5872B020
P 6050 6950
F 0 "#FLG010" H 6050 7045 50  0001 C CNN
F 1 "PWR_FLAG" H 6050 7130 50  0000 C CNN
F 2 "" H 6050 6950 50  0000 C CNN
F 3 "" H 6050 6950 50  0000 C CNN
	1    6050 6950
	-1   0    0    1   
$EndComp
$Comp
L PWR_FLAG #FLG011
U 1 1 5872B040
P 6650 6950
F 0 "#FLG011" H 6650 7045 50  0001 C CNN
F 1 "PWR_FLAG" H 6650 7130 50  0000 C CNN
F 2 "" H 6650 6950 50  0000 C CNN
F 3 "" H 6650 6950 50  0000 C CNN
	1    6650 6950
	-1   0    0    1   
$EndComp
$Comp
L GND #PWR012
U 1 1 5872B060
P 6350 7150
F 0 "#PWR012" H 6350 6900 50  0001 C CNN
F 1 "GND" H 6350 7000 50  0000 C CNN
F 2 "" H 6350 7150 50  0000 C CNN
F 3 "" H 6350 7150 50  0000 C CNN
	1    6350 7150
	1    0    0    -1  
$EndComp
$Comp
L +5V #PWR013
U 1 1 5872B080
P 6650 6950
F 0 "#PWR013" H 6650 6800 50  0001 C CNN
F 1 "+5V" H 6650 7090 50  0000 C CNN
F 2 "" H 6650 6950 50  0000 C CNN
F 3 "" H 6650 6950 50  0000 C CNN
	1    6650 6950
	1    0    0    -1  
$EndComp
$Comp
L +3.3V #PWR014
U 1 1 5872B0A0
P 6050 6950
F 0 "#PWR014" H 6050 6800 50  0001 C CNN
F 1 "+3.3V" H 6050 7090 50  0000 C CNN
F 2 "" H 6050 6950 50  0000 C CNN
F 3 "" H 6050 6950 50  0000 C CNN
	1    6050 6950
	1    0    0    -1  
$EndComp
$Comp
L ADB K1
U 1 1 5872B555
P 4650 2950
F 0 "K1" H 4650 2950 50  0000 C CNN
F 1 "ADB" H 4650 2800 50  0000 C CNN
F 2 "Socket_Strips:Socket_Strip_Straight_1x04" H 4650 2950 50  0001 C CNN
F 3 "" H 4650 2950 50  0000 C CNN
	1    4650 2950
	1    0    0    -1  
$EndComp
$Comp
L Q_NMOS_DGS Q1
U 1 1 5872B6F2
P 5650 3150
F 0 "Q1" H 5950 3200 50  0000 R CNN
F 1 "ZVNL110A" H 6300 3100 50  0000 R CNN
F 2 "TO_SOT_Packages_THT:TO-92_Inline_Wide" H 5850 3250 50  0001 C CNN
F 3 "" H 5650 3150 50  0000 C CNN
	1    5650 3150
	0    -1   -1   0   
$EndComp
Wire Wire Line
	5650 3350 5900 3350
Wire Wire Line
	5850 3050 6300 3050
Connection ~ 5900 3050
$EndSCHEMATC
