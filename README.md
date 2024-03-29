# Processor6502

Scala 3 using JDK 11 and ScalaFX.

Not sure how to characterise this. Its not an emulator or a simulator as it only performs the functions of a 6502 opcode within the environment of the GUI.

##Features
* Built in code editor
  * 6502 nememics 
  * assembler commands.
* Assemble code to memory form editor or file.
* Save and load memory image.
* 3 modes to 'run' code
  * Run - as fast as your machine will go!
  * Run Slow - 50ms delay after each instruction
  * Single step


##Assembler Commands

BYT - define a byte value decimal or hex. Can define more than one seperated by commas.

WRD - define a word value decimal or hex. Can define more than one seperated by commas.

ADDR - define an address value (little endian) decimal or hex. Can define more than one seperated by commas.

ORIG - set assembly location.

DEF - define a value decimal or hex

CLR - clear all defined labels etc. Only valid as first line.

symbolname: - a symbolic reference (label) to the current location

## Main Screen

The main screen

<img width="518" alt="MainScreen" src="images/MainScreen.png">

## Main Screen Areas

<img width="518" alt="Areas of the MainScreen" src="images/MainScreen_areas.png">

### Number format <img width="16" alt="palrblue" src="images/palebluedot.png">

Select the format in which numbers are displayed for registers and memory

### Processor Registers <img width="16" alt="paleyellow" src="images/paleyellowdot.jpg">

Displays the current contens of the registers.
Clicking the set button for the PC sets th evalue of the programe counter to the entered value.

### Status Flags <img width="16" alt="paleyellow" src="images/paleorangedot.jpg">

Displays the curent state of the staus register flags.

Each flag has a toggle button which toggles the flag between it 2 possible states.

This enables for example a loop to be terminated during debugging by updating the approprite flags.

### Vectors <img width="16" alt="paleyellow" src="images/palereddot.jpg">

The 6502 vectors are the last 6 bytes of memory.
Each vector can be set using its set button or in assemple as in this example:

    def reset $FFFC
    orig 512
    start:
    ; your code here
    ...
    orig reset
    addr start

The **Reset**, **NMI** and **IRQ** buttons perform the associated interrupt action. The **Exe** button pops up the execute window enabling run or single step.

### Memory View <img width="16" alt="palegreen" src="images/palegreendot.jpg">

Displays the contents of all 65536 memory locations with disassembly of contents if valid.

**Memory Image** Menu button with options to save and load the current contents of memory to an image file.

On save the **Set Range Dialogue** pops up, the start and end address of the dump can be entered in the current selected Number Format (DEC, HEX, OCT or BIN).

On **Set Range** the file to save to can be selected / entered and a image of the memory range is written.

The **View PC** button scrolls the view to the current PC location.

The **View Location** button scrolls the view to the location entered using the set button which pops up a numeric input form enabling the value to be entered in the current selected Number Format (DEC, HEX, OCT or BIN).


### Actions Area <img width="16" alt="palepurple" src="images/palepurpledot.jpg">

**Config Actions** menu button.
* Load - Load a confoguration from a save file
* Save - save current configuration to a save file

**Code Actions** menu button.
* Open Editor - opens the code editor
* Assemble from file - assembles a file without loading into the editor.


***Disassemble From***
Disassembles the current memory contents from the given location until the first ZERO byte or the end of memory.
The current disassembly location is set using the set button which pops up a numeric input form enabling the value to be entered in the current selected Number Format (DEC, HEX, OCT or BIN).
