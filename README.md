# HID Cloner

Program to clone 26 bit HID card using a cheap RFID reader (https://www.aliexpress.com/item/Cloner-125KHz-EM4100-RFID-Copier-Writer-Duplicator-Programmer-Reader-5-Pcs-EM4305-T5577-Rewritable-ID-Keyfobs/32630807675.html)

- Sample with your sound card (192000 Hz sampling rate MONO) the output of a RFID ASK demodulator with the HID card 
  You can find the place where you need to solder the GND and demodulator output wire in the "circuit.jpg" file
- Open "HID Cloner.exe"
- Read wave file and put a T5577 card on top of the reader to clone it
- Done!

You MUST download the SRF32.dll file together with "HID cloner.exe" to make it work
