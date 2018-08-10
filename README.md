# NIXIEonPIC
NIXIE clock on pic16f628 with built in RTC

Current IDE MPLAB X

### done
- [x] Simple RTC without time changing(starts from 00:00:00 and counts 24h)
- [x] Indication on NIXIE
- [x] Implementation in according to schematics Rev. 2 (HV generation is done by 555 timer though)


### to do

- [ ] Time set up (schematics Rev. 3, push buttons added)
- [ ] Power down detection (MCLR port)
- [ ] Lightness control with photo resistor (comparator + internal Vref as simple rough ADC)
- [ ] HV generation by PIC. PWM to step-up and INT to control overvoltage
- [ ] Alarm clock, maybe some of them.
- [ ] USART for bluetooth connection (well, dreams are dreams)
