# NIXIEonPIC
NIXIE clock on pic16f628 with built in RTC

Current IDE MPLAB X

###done
* Simple RTC without time changing(starts from 00:00:00 and counts 24h)
* Indication on NIXIE
* Implementation in according to schematics Rev. 2 (HV generation is done by 555 timer though)


###to do

* Time set up (schematics Rev. 3, push buttons added)
* HV generation by PIC. PWM to step-up and INT to control overvoltage
* Alarm clock, maybe some of them.
* Power down detection (MCLR port)
* Lightness control with photo resistor (conparator + internal Vref as simple rough ADC)
* USART for bluetooth connection(well, dreams are dreams)
