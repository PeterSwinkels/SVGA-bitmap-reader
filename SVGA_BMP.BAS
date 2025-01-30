DECLARE SUB DisplayHelp ()
DEFINT A-Z

CONST BITMAPDIMENSIONS = 19
CONST BITMAPHEADERSIZE = 1078
CONST BITMAPPALETTE = 55
CONST VESAVIDEOMODE = &H101

TYPE RegTypeX
 ax AS INTEGER
 bx AS INTEGER
 cx AS INTEGER
 dx AS INTEGER
 bp AS INTEGER
 si AS INTEGER
 di AS INTEGER
 flags AS INTEGER
 ds AS INTEGER
 es AS INTEGER
END TYPE

TYPE VBEInformationStr
 VESASignature AS STRING * 4
 VESAVersion AS INTEGER
 OEMStringPTR AS LONG
 Capabilities AS STRING * 4
 VideoModePTR AS LONG
 TotalMemory AS INTEGER
 Reserved AS STRING * 236
END TYPE

TYPE VESAInformationStr
 ModeAttributes AS INTEGER
 WinAAttributes AS STRING * 1
 WinBAttributes AS STRING * 1
 WinGranularity AS INTEGER
 WinSize AS INTEGER
 WinASegment AS INTEGER
 WinBSegment AS INTEGER
 WinFuncPointer AS LONG
 BytesPerScanLine AS INTEGER
 XResolution AS INTEGER
 YResolution AS INTEGER
 XCharSize AS STRING * 1
 YCharSize AS STRING * 1
 NumberOfPlanes AS STRING * 1
 BitsPerPixel AS STRING * 1
 NumberOfBanks AS STRING * 1
 MemoryModel AS STRING * 1
 BankSize AS STRING * 1
 NumberOfImagePages AS STRING * 1
 SizeOfBank AS STRING * 1
 RedMaskSize AS STRING * 1
 RedFieldPosition AS STRING * 1
 GreenMaskSize AS STRING * 1
 GreenFieldPosition AS STRING * 1
 BlueMaskSize AS STRING * 1
 BlueFieldPosition AS STRING * 1
 RsvdMaskSize AS STRING * 1
 RsvdFieldPosition AS STRING * 1
 DirectColorInformation AS STRING * 1
 Reserved AS STRING * 216
END TYPE

DECLARE FUNCTION OpenBinary (FileName AS STRING)
DECLARE FUNCTION SelectBitmap$ ()
DECLARE FUNCTION SetVESAMode (VideoMode AS INTEGER)
DECLARE FUNCTION VESAPresent ()
DECLARE SUB DisplayBitmap (FileH AS INTEGER, XOffset AS INTEGER, YOffset AS INTEGER, ScreenWidth AS INTEGER, ScreenHeight AS INTEGER, ImageWidth AS LONG, ImageHeight AS LONG)
DECLARE SUB DisplayBitmapInformation (FileName AS STRING, ImageWidth AS LONG, ImageHeight AS LONG)
DECLARE SUB GetVESAInformation (VideoMode AS INTEGER, Information AS VESAInformationStr)
DECLARE SUB INTERRUPTX (intnum AS INTEGER, inreg AS RegTypeX, outreg AS RegTypeX)
DECLARE SUB Main ()
DECLARE SUB ResetVideo ()
DECLARE SUB SetPalette (FileH AS INTEGER)
DECLARE SUB SetVESABank (Bank AS INTEGER)

ResetVideo
DisplayHelp
CALL Main
END

SUB DisplayBitmap (FileH AS INTEGER, XOffset AS INTEGER, YOffset AS INTEGER, ScreenWidth AS INTEGER, ScreenHeight AS INTEGER, ImageWidth AS LONG, ImageHeight AS LONG)
DIM Bank AS INTEGER
DIM CurrentBank AS INTEGER
DIM Offset AS LONG
DIM Position AS LONG
DIM Row AS STRING
DIM x AS INTEGER
DIM y AS INTEGER
STATIC Padding AS INTEGER

 IF ImageWidth = 0 AND ImageHeight = 0 THEN
  SEEK FileH, BITMAPDIMENSIONS
  ImageWidth = CVL(INPUT$(4, FileH))
  ImageHeight = CVL(INPUT$(4, FileH))
  IF ScreenHeight > ImageHeight THEN ScreenHeight = ImageHeight
  Padding = ((LOF(FileH) - BITMAPHEADERSIZE) / ImageHeight) - ImageWidth
 END IF

 Position = (LOF(FileH) - ((ImageWidth + Padding) * (YOffset + ScreenHeight)))
 IF Position > BITMAPHEADERSIZE THEN
  SEEK FileH, Position
 ELSE
  SEEK FileH, BITMAPHEADERSIZE
 END IF

 DEF SEG = &HA000
 CurrentBank = 0
 SetVESABank CurrentBank

 FOR y = ScreenHeight - 1 TO 0 STEP -1
  IF Position > 0 AND LOC(FileH) < LOF(FileH) THEN
   Row = INPUT$(ImageWidth + Padding, FileH)
   FOR x = 0 TO ScreenWidth - 1
    Offset = CLNG(CLNG(y) * CLNG(ScreenWidth)) + CLNG(x)
    Bank = Offset \ &H10000
    IF NOT Bank = CurrentBank THEN
     SetVESABank Bank
     CurrentBank = Bank
    END IF
    IF x + XOffset < ImageWidth AND y + YOffset < ImageHeight THEN
     POKE Offset AND &HFFFF&, ASC(MID$(Row, x + XOffset + 1, 1))
    ELSE
     POKE Offset AND &HFFFF&, &H0
    END IF
   NEXT x
  ELSE
   FOR x = 0 TO ScreenWidth - 1
    Offset = CLNG(CLNG(y) * CLNG(ScreenWidth)) + CLNG(x)
    Bank = Offset \ &H10000
    IF NOT Bank = CurrentBank THEN
     SetVESABank Bank
     CurrentBank = Bank
    END IF
    POKE Offset AND &HFFFF&, &H0
   NEXT x
  END IF
  Position = Position + (ImageWidth + Padding)
 NEXT y
END SUB

SUB DisplayBitmapInformation (FileName AS STRING, ImageWidth AS LONG, ImageHeight AS LONG)
 PRINT
 PRINT " Bitmap: "; FileName
 PRINT USING " X: #####"; x
 PRINT USING " Y: #####"; y
 PRINT USING " Width:  ##### pixels"; ImageWidth
 PRINT USING " Height: ##### pixels"; ImageHeight
 DO: LOOP WHILE INKEY$ = ""
END SUB

SUB DisplayHelp
 PRINT
 PRINT " SVGA_BMP v1.00 - by: Peter Swinkels, ***2025***"
 PRINT
 PRINT " Arrow keys     = Scroll."
 PRINT " F1             = Display this help."
 PRINT " Escape         = Quit."
 PRINT " I              = Display information about the bitmap."
 PRINT " 1-4/6-9        = Scroll the entire screen width/height."
 PRINT " 5              = Move to the bitmap's center."
 DO: LOOP WHILE INKEY$ = ""
END SUB

SUB GetVESAInformation (VideoMode AS INTEGER, Information AS VESAInformationStr)
DIM Registers AS RegTypeX

 Registers.ax = &H4F01
 Registers.cx = VideoMode
 Registers.es = VARSEG(Information)
 Registers.di = VARPTR(Information)
 INTERRUPTX &H10, Registers, Registers
END SUB

SUB Main
DIM FileH AS INTEGER
DIM FileName AS STRING
DIM ImageHeight AS LONG
DIM ImageWidth AS LONG
DIM KeyStroke AS STRING
DIM Success AS INTEGER
DIM VESAInformation AS VESAInformationStr
DIM x AS INTEGER
DIM y AS INTEGER
 
 IF VESAPresent THEN
  GetVESAInformation VESAVIDEOMODE, VESAInformation
  ResetVideo
  FileName = SelectBitmap$
  IF NOT LTRIM$(RTRIM$(FileName)) = "" THEN
   IF SetVESAMode(VESAVIDEOMODE) THEN
    FileH = OpenBinary(FileName)
    SetPalette FileH
    x = 0
    y = 0
    DO
     DisplayBitmap FileH, x, y, VESAInformation.XResolution, VESAInformation.YResolution, ImageWidth, ImageHeight
     DO
      KeyStroke = INKEY$
     LOOP WHILE KeyStroke = ""
     SELECT CASE KeyStroke
      CASE CHR$(0) + "H"
       y = y - (VESAInformation.YResolution / 4)
      CASE CHR$(0) + "P"
       y = y + (VESAInformation.YResolution / 4)
      CASE CHR$(0) + "K"
       x = x - (VESAInformation.XResolution / 4)
      CASE CHR$(0) + "M"
       x = x + (VESAInformation.YResolution / 4)
      CASE CHR$(0) + ";"
       ResetVideo
       DisplayHelp
       Success = SetVESAMode(VESAVIDEOMODE)
       SetPalette FileH
      CASE "1"
       x = x - VESAInformation.XResolution
       y = y + VESAInformation.YResolution
      CASE "2"
       y = y + VESAInformation.YResolution
      CASE "3"
       x = x + VESAInformation.XResolution
       y = y + VESAInformation.YResolution
      CASE "4"
       x = x - VESAInformation.XResolution
      CASE "5"
       x = ImageWidth / 2
       y = ImageHeight / 2
      CASE "6"
       x = x + VESAInformation.XResolution
      CASE "7"
       x = x - VESAInformation.XResolution
       y = y - VESAInformation.YResolution
      CASE "8"
       y = y - VESAInformation.YResolution
      CASE "9"
       x = x + VESAInformation.XResolution
       y = y - VESAInformation.YResolution
      CASE "i", "I"
       ResetVideo
       DisplayBitmapInformation FileName, ImageWidth, ImageHeight
       Success = SetVESAMode(VESAVIDEOMODE)
       SetPalette FileH
      CASE CHR$(27)
       ResetVideo
       EXIT DO
     END SELECT
     IF x < 0 THEN x = 0
     IF y < 0 THEN y = 0
    LOOP
    CLOSE FileH
   ELSE
    PRINT "Could not initialize VESA."
   END IF
  END IF
 ELSE
  PRINT "No VESA found."
 END IF
END SUB

FUNCTION OpenBinary (FileName AS STRING)
DIM FileH AS INTEGER

 FileH = FREEFILE
 OPEN FileName FOR INPUT LOCK READ WRITE AS FileH
 CLOSE FileH
 OPEN FileName FOR BINARY LOCK READ WRITE AS FileH

 OpenBinary = FileH
END FUNCTION

SUB ResetVideo
 SCREEN 1
 SCREEN 0
 PALETTE
 WIDTH 40
 WIDTH 80, 25
 COLOR 7, 0
 CLS
END SUB

FUNCTION SelectBitmap$
DIM FileName AS STRING

 FileName = COMMAND$
 IF FileName = "" THEN
  COLOR 0, 7
  PRINT " BITMAP FILES: " + SPACE$(65)
  COLOR 7, 0
  FILES "*.BMP"
  PRINT STRING$(80, "=")
  LINE INPUT "SPECIFY BITMAP: ", FileName
 END IF

 IF NOT FileName = "" THEN
  FileName = UCASE$(LTRIM$(RTRIM$(FileName)))
  IF NOT RIGHT$(FileName, 4) = ".BMP" THEN FileName = FileName + ".BMP"
 END IF

 SelectBitmap$ = FileName
END FUNCTION

SUB SetPalette (FileH AS INTEGER)
DIM Blue AS LONG
DIM ColorIndex
DIM Green AS LONG
DIM Null AS STRING
DIM Red AS LONG

 SEEK FileH, BITMAPPALETTE
 FOR ColorIndex = 0 TO 255
  Blue = ASC(INPUT$(1, FileH)) \ &H4
  Green = ASC(INPUT$(1, FileH)) \ &H4
  Red = ASC(INPUT$(1, FileH)) \ &H4
  OUT &H3C8, ColorIndex
  OUT &H3C9, Red
  OUT &H3C9, Green
  OUT &H3C9, Blue
  Null = INPUT$(1, FileH)
 NEXT ColorIndex
END SUB

SUB SetVESABank (Bank AS INTEGER)
DIM Registers AS RegTypeX

 Registers.ax = &H4F05
 Registers.bx = &H0
 Registers.dx = Bank
 INTERRUPTX &H10, Registers, Registers
END SUB

FUNCTION SetVESAMode (VideoMode AS INTEGER)
DIM Registers AS RegTypeX

 Registers.ax = &H4F02
 Registers.bx = VideoMode
 INTERRUPTX &H10, Registers, Registers

 SetVESAMode = (Registers.ax = &H4F)
END FUNCTION

FUNCTION VESAPresent
DIM Information AS VBEInformationStr
DIM Registers AS RegTypeX

 Registers.ax = &H4F00
 Registers.es = VARSEG(Information)
 Registers.di = VARPTR(Information)
 INTERRUPTX &H10, Registers, Registers
 
 VESAPresent = (Registers.ax = &H4F)
END FUNCTION

