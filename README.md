# DEFN-System
A Sega Master System Emulator written in Clojure. 

Runs commercial games.

Supports the Sega Game Gear.

Supports battery saves and save states.

Uses a slightly modified version of the Z80Core found in this project: https://github.com/codesqueak/Z80Processor

The code for the modified Z80Core is not in this repo. It can be found here: https://github.com/netb258/custom-Z80Processor

# Controls
Z is mapped to Button 1.

X is mapped to Button 2.

Q is mapped to Save State (for the current game).

E is mapped to Load State (for the current game).

Use the arrow keys for movement and ENTER to pause.

# Running the code
lein run PATH_TO_ROM

# Release version
There is a standalone JAR file in the release section that adds GUI to this emulator.
It can be run like this:

java -jar defn-system-with-gui.jar

# Screenshots

<table>
 <tr>
    <td><img src="./screenshots/screen17.png?raw=true" alt="Screen 17" width="100%"></td>
    <td><img src="./screenshots/screen18.png?raw=true" alt="Screen 18" width="100%"></td>
  </tr>
  <tr>
    <td><img src="./screenshots/screen1.png?raw=true" alt="Screen 1" width="100%"></td>
    <td><img src="./screenshots/screen2.png?raw=true" alt="Screen 2" width="100%"></td>
  </tr>
  <tr>
    <td><img src="./screenshots/screen3.png?raw=true" alt="Screen 3" width="100%"></td>
    <td><img src="./screenshots/screen4.png?raw=true" alt="Screen 4" width="100%"></td>
  </tr>
  <tr>
    <td><img src="./screenshots/screen5.png?raw=true" alt="Screen 5" width="100%"></td>
    <td><img src="./screenshots/screen6.png?raw=true" alt="Screen 6" width="100%"></td>
  </tr>
  <tr>
    <td><img src="./screenshots/screen7.png?raw=true" alt="Screen 7" width="100%"></td>
    <td><img src="./screenshots/screen8.png?raw=true" alt="Screen 8" width="100%"></td>
  </tr>
  <tr>
    <td><img src="./screenshots/screen9.png?raw=true" alt="Screen 9" width="100%"></td>
    <td><img src="./screenshots/screen10.png?raw=true" alt="Screen 10" width="100%"></td>
  </tr>
  <tr>
    <td><img src="./screenshots/screen11.png?raw=true" alt="Screen 11" width="100%"></td>
    <td><img src="./screenshots/screen12.png?raw=true" alt="Screen 12" width="100%"></td>
  </tr>
  <tr>
    <td><img src="./screenshots/screen13.png?raw=true" alt="Screen 13" width="100%"></td>
    <td><img src="./screenshots/screen14.png?raw=true" alt="Screen 14" width="100%"></td>
  </tr>
  <tr>
    <td><img src="./screenshots/screen15.png?raw=true" alt="Screen 15" width="100%"></td>
    <td><img src="./screenshots/screen16.png?raw=true" alt="Screen 16" width="100%"></td>
  </tr>
  <tr>
    <td><img src="./screenshots/screen19.png?raw=true" alt="Screen 19" width="100%"></td>
    <td><img src="./screenshots/screen20.png?raw=true" alt="Screen 20" width="100%"></td>
  </tr>
</table>
