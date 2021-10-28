'256 Color Bitmap Reader add-on, by: Peter Swinkels.
DEFINT A-Z

TYPE InformationType
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

TYPE VGAInfoType
 VESASignature AS STRING * 4
 VESAVersion AS INTEGER
 OEMStringPTR AS LONG
 Capabilities AS STRING * 4
 VideoModePTR AS LONG
 TotalMemory AS INTEGER
 Reserved AS STRING * 236
END TYPE

DECLARE FUNCTION OpenBinary (FileName$)
DECLARE FUNCTION SetVESAMode (VIDEOMODE)
DECLARE FUNCTION VESAPresent ()
DECLARE SUB DisplayBitmap (FileH, XOffset, YOffset, ScreenWidth, SCREENHEIGHT, ImageWidth AS LONG, ImageHeight AS LONG)
DECLARE SUB GetVESAInformation (VIDEOMODE, Information AS InformationType)
DECLARE SUB INTERRUPTX (intnum AS INTEGER, inreg AS RegTypeX, outreg AS RegTypeX)
DECLARE SUB Main ()
DECLARE SUB ResetVideo ()
DECLARE SUB SetPalette (FileH)
DECLARE SUB SetVESABank (Bank)

CALL Main

SUB DisplayBitmap (FileH, XOffset, YOffset, ScreenWidth, SCREENHEIGHT, ImageWidth AS LONG, ImageHeight AS LONG)
CONST BITMAP = 1078
CONST DIMENSIONS = 19
DIM Bank
DIM CurrentBank
DIM Offset AS LONG
DIM Position AS LONG
DIM Row$
DIM x
DIM y
STATIC Padding

 IF ImageWidth = 0 AND ImageHeight = 0 THEN
  SEEK FileH, DIMENSIONS
  ImageWidth = CVL(INPUT$(4, FileH))
  ImageHeight = CVL(INPUT$(4, FileH))
  IF SCREENHEIGHT > ImageHeight THEN SCREENHEIGHT = ImageHeight
  Padding = ((LOF(FileH) - BITMAP) / ImageHeight) - ImageWidth
 END IF

 Position = (LOF(FileH) - ((ImageWidth + Padding) * (YOffset + SCREENHEIGHT)))
 IF Position > BITMAP THEN SEEK FileH, Position ELSE SEEK FileH, BITMAP

 DEF SEG = &HA000
 CurrentBank = 0
 SetVESABank CurrentBank

 FOR y = SCREENHEIGHT - 1 TO 0 STEP -1
  IF Position > 0 AND LOC(FileH) < LOF(FileH) THEN
   Row$ = INPUT$(ImageWidth + Padding, FileH)
   FOR x = 0 TO ScreenWidth - 1
    Offset = CLNG(CLNG(y) * CLNG(ScreenWidth)) + CLNG(x)
    Bank = Offset \ &H10000
    IF NOT Bank = CurrentBank THEN
     SetVESABank Bank
     CurrentBank = Bank
    END IF
    IF x + XOffset < ImageWidth AND y + YOffset < ImageHeight THEN
     POKE Offset AND &HFFFF&, ASC(MID$(Row$, x + XOffset + 1, 1))
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

SUB GetVESAInformation (VIDEOMODE, Information AS InformationType)
DIM Registers AS RegTypeX

 Registers.ax = &H4F01
 Registers.cx = VIDEOMODE
 Registers.es = VARSEG(Information)
 Registers.di = VARPTR(Information)
 INTERRUPTX &H10, Registers, Registers
END SUB

SUB Main
CONST VIDEOMODE = &H101
DIM FileH
DIM FileName$
DIM ImageHeight AS LONG
DIM ImageWidth AS LONG
DIM Key$
DIM Success
DIM VESAInformation AS InformationType
DIM x
DIM y
 
 IF NOT VESAPresent THEN
  PRINT "No VESA found."
  END
 END IF

 GetVESAInformation VIDEOMODE, VESAInformation

 ResetVideo
 COLOR 0, 7: PRINT " BITMAP FILES: " + SPACE$(65)
 COLOR 7, 0: FILES "*.bmp"
 PRINT STRING$(80, "=")
 LINE INPUT "SPECIFY BITMAP: ", FileName$
 IF LTRIM$(RTRIM$(FileName$)) = "" THEN EXIT SUB
 Success = SetVESAMode(VIDEOMODE)
 IF NOT Success THEN
  PRINT "Could not initialize VESA."
  END
 END IF

 FileH = OpenBinary(FileName$)
 SetPalette FileH
 x = 0
 y = 0
 DO
  CLS
  DisplayBitmap FileH, x, y, VESAInformation.XResolution, VESAInformation.YResolution, ImageWidth, ImageHeight
  DO
   Key$ = INKEY$
  LOOP WHILE Key$ = ""
  SELECT CASE Key$
   CASE CHR$(0) + "H"
    y = y - (VESAInformation.YResolution / 4)
   CASE CHR$(0) + "P"
    y = y + (VESAInformation.YResolution / 4)
   CASE CHR$(0) + "K"
    x = x - (VESAInformation.XResolution / 4)
   CASE CHR$(0) + "M"
    x = x + (VESAInformation.YResolution / 4)
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
    PRINT USING "X: ####"; x
    PRINT USING "Y: ####"; y
    PRINT USING "Width:  ####"; ImageWidth
    PRINT USING "Height: #### "; ImageHeight
    DO: LOOP WHILE INKEY$ = ""
    Success = SetVESAMode(VIDEOMODE)
    SetPalette FileH
   CASE CHR$(27)
    ResetVideo
    EXIT DO
  END SELECT
  IF x < 0 THEN x = 0
  IF y < 0 THEN y = 0
 LOOP
 CLOSE FileH
END SUB

FUNCTION OpenBinary (FileName$)
DIM FileH

 FileH = FREEFILE
 OPEN FileName$ FOR INPUT AS FileH: CLOSE FileH
 OPEN FileName$ FOR BINARY AS FileH

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

SUB SetPalette (FileH)
CONST COLORS = 55
DIM Blue AS LONG
DIM ColorIndex
DIM Green AS LONG
DIM Null$
DIM Red AS LONG

 SEEK FileH, COLORS
 FOR ColorIndex = 0 TO 255
  Blue = ASC(INPUT$(1, FileH)) \ &H4
  Green = ASC(INPUT$(1, FileH)) \ &H4
  Red = ASC(INPUT$(1, FileH)) \ &H4
  OUT 968, ColorIndex
  OUT 969, Red
  OUT 969, Green
  OUT 969, Blue
  Null$ = INPUT$(1, FileH)
 NEXT ColorIndex
END SUB

SUB SetVESABank (Bank)
DIM Registers AS RegTypeX

 Registers.ax = &H4F05
 Registers.bx = &H0
 Registers.dx = Bank
 INTERRUPTX &H10, Registers, Registers
END SUB

FUNCTION SetVESAMode (VIDEOMODE)
DIM Registers AS RegTypeX

 Registers.ax = &H4F02
 Registers.bx = VIDEOMODE
 INTERRUPTX &H10, Registers, Registers

SetVESAMode = (Registers.ax = &H4F)
END FUNCTION

FUNCTION VESAPresent
DIM Information AS VGAInfoType
DIM Registers AS RegTypeX

 Registers.ax = &H4F00
 Registers.es = VARSEG(Information)
 Registers.di = VARPTR(Information)
 INTERRUPTX &H10, Registers, Registers
 
VESAPresent = (Registers.ax = &H4F)
END FUNCTION

